/* 
	Copyright (c) 2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
	
	All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, are permitted 
	provided that the following conditions are met:

	1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
		and the following disclaimer.

	2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
		and the following disclaimer in the documentation and/or other materials provided with the distribution.
	
	3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
		or promote products derived from this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
	WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
	PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
	LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN 
	IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.github.devconslejme.misc.jme;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.NotMainThread;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.game.CharacterI.CompositeControl;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.ICompositeRestrictedAccessControl;
import com.github.devconslejme.misc.InfoI.Info;
import com.github.devconslejme.misc.MainThreadI;
import com.github.devconslejme.misc.MatterI.EMatter;
import com.github.devconslejme.misc.MatterI.Matter;
import com.github.devconslejme.misc.MatterI.MatterStatus;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableWeak;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.TimeFormatI;
import com.github.devconslejme.misc.TimedDelay;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bullet.BulletAppState;
import com.jme3.bullet.BulletAppState.ThreadingType;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.PhysicsTickListener;
import com.jme3.bullet.collision.PhysicsCollisionEvent;
import com.jme3.bullet.collision.PhysicsCollisionGroupListener;
import com.jme3.bullet.collision.PhysicsCollisionListener;
import com.jme3.bullet.collision.PhysicsCollisionObject;
import com.jme3.bullet.collision.PhysicsRayTestResult;
import com.jme3.bullet.collision.shapes.BoxCollisionShape;
import com.jme3.bullet.collision.shapes.SphereCollisionShape;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.bullet.objects.PhysicsRigidBody;
import com.jme3.collision.CollisionResult;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Transform;
import com.jme3.math.Vector3f;
import com.jme3.scene.BatchNode;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Box;

/**
 * http://physics.nist.gov/cuu/Units/units.html
 * http://www.bulletphysics.org/mediawiki-1.5.8/index.php?title=Frequently_Asked_Questions
 * so: length meter m, mass kilogram kg, time second s
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PhysicsI implements PhysicsTickListener, PhysicsCollisionGroupListener, PhysicsCollisionListener{
	public static PhysicsI i(){return GlobalManagerI.i().get(PhysicsI.class);}
	
	public static class CompositeControl implements ICompositeRestrictedAccessControl{private CompositeControl(){};};
	private ICompositeRestrictedAccessControl	cc=new CompositeControl();
	
	private ArrayList<PhysicsData> apdPreventDisintegr = new ArrayList<PhysicsData>();
	
	private ArrayList<PhysicsData> apdDisintegrateAtMainThreadQueue = new ArrayList<PhysicsData>();
	private ArrayList<CallableWeak> acallUpdtPhysAtMainThreadQueue = new ArrayList<>();
//	private ArrayList<PhysicsData> apdGravityUpdtMainThreadQueue = new ArrayList<>();
//	private ArrayList<PhysicsData> apdLocationUpdtMainThreadQueue = new ArrayList<>();
//	private ArrayList<PhysicsData> apdRotationUpdtMainThreadQueue = new ArrayList<>();
	private ArrayList<PhysicsData> apdSafeSpotRestoreMainThreadQueue = new ArrayList<>();
	
	private long	lTickCount=0;
	private HashMap<PhysicsRigidBody,PhysicsData> hmDisintegratables=new HashMap<PhysicsRigidBody,PhysicsData>(); 
	private HashMap<PhysicsRigidBody,PhysicsData> hmProjectiles=new HashMap<PhysicsRigidBody,PhysicsData>(); 
	private BulletAppState	bullet;
	private PhysicsSpace	pspace;
	private BoundingBox	bbSpace;
	private LinkedHashMap<String, Info>	hmInfo;
	private TimedDelay tdChkOutOfWorldBoundsDisintegrationAllowed = new TimedDelay(10f).setActive(true);
	private TimedDelay tdSaveSafeSpotRot = new TimedDelay(3f).setActive(true);
	private TimedDelay tdInfo = new TimedDelay(1f).setActive(true);
	private TimedDelay tdDisintegrate = new TimedDelay(10f).setActive(true);
	private PhysicsData	pdLastThrownFromCam;
	private int	iThreadPhysTPS;
	private int	iThreadPhysTickSum;
	private long	lThreadPhysLastCalcTPS;
	private float	fThreadPhysTPF;
	private ArrayList<ImpTorForce> arbcThreadPhysicsPreTickQueue = new ArrayList();
	private boolean	bGlueAllowed=true;
//	private float	fDefaultProjectileMaxLife=2;
	private boolean bDisableCcdToLetCollisionGroupsWork;
	private float fDeflectionAngle=90f;

	private boolean bAllowGrabbedsPhysInterferences;

	
	public static class ImpTorForce{
//		private Spatial	spt;
//		private RigidBodyControl	rbc;
		/** @DefSelfNote dont expose it, this class is a simplifier */
		private PhysicsSpace	ps;
		
		private Vector3f	v3fForce;
		private Vector3f	v3fForceLocation;
		private Vector3f	v3fImpulseRelPos;
		private Vector3f	v3fImpulse;
		private Vector3f	v3fTorque;
		private Vector3f	v3fTorqueImpulse;
		private Float	fImpulseAtSelfDirection;
		private PhysicsData	pd;
		private  Float fImpulseAtSelfDirectionUpwardsDisplacement;
		
//		public Spatial getSpt() {
//			return spt;
//		}
//		public Impulse setSpt(Spatial spt) {
//			this.spt = spt;
//			return this;
//		}
//		public RigidBodyControl getRBC() {
//			return rbc;
//		}
//		public Impulse setRBC(RigidBodyControl rbc) {
//			this.rbc = rbc;
//			return this;
//		}
		
		public PhysicsSpace getPs() {
			return ps;
		}
		
		public Vector3f getForce() {
			return v3fForce;
		}
		public ImpTorForce setForce(Vector3f v3fForce) {
			this.v3fForce = v3fForce;
			return this;
		}
		public Vector3f getForceLocation() {
			return v3fForceLocation;
		}
		public ImpTorForce setForceLocation(Vector3f v3fForceLocation) {
			this.v3fForceLocation = v3fForceLocation;
			return this;
		}
		public Vector3f getImpulse() {
			return v3fImpulse;
		}
		public float getImpulseAtSelfDir() {
			return fImpulseAtSelfDirection;
		}
		/**
		 * 
		 * @param fImpulse
		 * @param fUpwardsDisplacement can be null
		 * @return
		 */
		public ImpTorForce setImpulseAtSelfDir(float fImpulse, Float fUpwardsDisplacement) {
			assert v3fImpulse==null;
			this.fImpulseAtSelfDirection=fImpulse;
			this.fImpulseAtSelfDirectionUpwardsDisplacement=fUpwardsDisplacement;
			return this;
		}
		/**
		 * 
		 * @param v3fImpulse
		 * @param v3fRelPos can be null (will default to zero)
		 * @return
		 */
		public ImpTorForce setImpulse(Vector3f v3fImpulse,Vector3f v3fRelPos) {
			assert fImpulseAtSelfDirection==null;
			this.v3fImpulse = v3fImpulse;
			this.v3fImpulseRelPos = v3fRelPos==null?Vector3f.ZERO:v3fRelPos;
			return this;
		}
		public Vector3f getTorque() {
			return v3fTorque;
		}
		public ImpTorForce setTorque(Vector3f v3fTorque) {
			this.v3fTorque = v3fTorque;
			return this;
		}
		public Vector3f getTorqueImpulse() {
			return v3fTorqueImpulse;
		}
		/**
		 * THIS IS NOT RELATIVE TO THE CURRENT ORIENTATION!!! this is a global orientation thing :/
		 * @param v3fTorqueImpulse
		 * @return
		 */
		public ImpTorForce setTorqueImpulse(Vector3f v3fTorqueImpulse) {
			this.v3fTorqueImpulse = v3fTorqueImpulse;
			return this;
		}
		
	}

	public void configure(){
		bullet = new BulletAppState();
		bullet.setThreadingType(ThreadingType.PARALLEL);
		AppI.i().attatchAppState(bullet);
		
		pspace = bullet.getPhysicsSpace();
		pspace.addTickListener(PhysicsI.this);
		pspace.addCollisionListener(this);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_01);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_02);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_03);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_04);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_05);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_06);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_07);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_08);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_09);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_10);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_11);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_12);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_13);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_14);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_15);
		pspace.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_16);
		
		bbSpace = new BoundingBox(pspace.getWorldMin(), pspace.getWorldMax());
		
		initMaintenanceUpdateLoop();
//		initUpdateGravity();
	}
	
//	private void initUpdateGravity() {
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				for(PhysicsData pd:apdListGravityUpdtMainThreadQueue) {
//					pd.applyNewGravityAtMainThread();
//				}
//				apdListGravityUpdtMainThreadQueue.clear();
//				return true;
//			}
//		}).enableLoopMode();
//	}
	
	private void initMaintenanceUpdateLoop() {
		hmInfo = new LinkedHashMap<String,Info>();
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(tdInfo.isReady(true))updateInfo();
				
				updateLevitators(pspace,getTPF());
				
				/**
				 * these queues may be harder to maintain than a single one, 
				 * but may help on debugging and to grant their execution order
				 * without having to filter a single list for the right order
				 * 
				 * TODO make such single list auto sorted in the right order?
				 * The ones with timed delay, would then be checked individually, instead of a single td chk 
				 */
				/**
				 * safe spot (being an auto workaround/fix) must be b4 new location and new rotation (that are requests) 
				 */
				for(PhysicsData pd:apdSafeSpotRestoreMainThreadQueue)pd.applyRestoreSafeSpotRotAtMainThread();
				apdSafeSpotRestoreMainThreadQueue.clear();
				for(CallableWeak call:acallUpdtPhysAtMainThreadQueue)call.call();
				acallUpdtPhysAtMainThreadQueue.clear();
//				for(PhysicsData pd:apdLocationUpdtMainThreadQueue)pd.applyNewPhysLocationAtMainThread();
//				apdLocationUpdtMainThreadQueue.clear();
//				for(PhysicsData pd:apdRotationUpdtMainThreadQueue)pd.applyNewPhysRotationAtMainThread();
//				apdRotationUpdtMainThreadQueue.clear();
//				for(PhysicsData pd:apdGravityUpdtMainThreadQueue)pd.applyNewGravityAtMainThread();
//				apdGravityUpdtMainThreadQueue.clear();
				
				updateProjectiles();  //b4 disintegration!!! (so it may even just disintegrate safely)
				if(tdDisintegrate.isReady(true))updateDisintegratablesAndItsQueue(); //LAST THING
				
				return true;
			}
		}).enableLoopMode();
	}
	
	protected void updateProjectiles() {
		for(PhysicsData pd:hmProjectiles.values()){
			/**
			 * some group collide() (phys thread) will not generate collisions() (main thread)
			 * when looks the projectile is being auto-deflected...
			 */
			PhysicsProjectileI.i().glueProjectileCheckApply(pd,pd.getGlueWhere(),null);
			
			/**
			 * fly direction apply, this cancels rotations, TODO bad for flying rotating knives btw
			 * if it just collided with anything, wait a little bit
			 */
			if(pd.isAlignFlyDirection() && pd.getFlyingDelay()>0.1f && pd.getGlueWhere()==null) {
				Vector3f v3fMvDir=pd.getLinearVelocityCopy().normalize();
				float fAngleBetweenMoveAndCurrentDeg = pd.getPhysicsRotationCopy().getRotationColumn(2).angleBetween(v3fMvDir)*FastMath.RAD_TO_DEG;
	//			System.out.println("deg="+fAngleBetweenMoveAndCurrentDeg);
				if(fAngleBetweenMoveAndCurrentDeg>10) {
					Spatial spt = pd.getSpatialWithPhysics();
	//				if(spt.getLocalScale().length()<5)spt.setLocalScale(1,1,16);System.out.println("l="+spt.getLocalScale().length());
					spt.lookAt(pd.getPhysicsLocationCopy().add(v3fMvDir), Vector3f.UNIT_Y);
					this.syncPhysTransfFromSpt(pd, false, true);
				}
			}
		}
	}
	
	protected void updateDisintegratablesAndItsQueue() {
		for(PhysicsData pd:hmDisintegratables.values()){
			if(!pd.isAllowDisintegration())apdPreventDisintegr.add(pd); //as the disintegration request may happen outside here too
			
			if(pd.getAgeNano() > pd.getProjectileMaxLifeTime() ){
				requestDisintegration(pd);
//				if(!apdDisintegrateAtMainThreadQueue.contains(pd))apdDisintegrateAtMainThreadQueue.add(pd);
			}
//			else{
//				updateIgnoredProjectileCollisions(pd);
//			}
		}
		
		for(PhysicsData pd:apdPreventDisintegr){
			hmDisintegratables.remove(pd.getPRB(cc));
			apdDisintegrateAtMainThreadQueue.remove(pd);
		}
		apdPreventDisintegr.clear();
		
		for(PhysicsData pd:apdDisintegrateAtMainThreadQueue){
			disintegrate(pd);
		}
		apdDisintegrateAtMainThreadQueue.clear();
	}
	
	protected void disintegrate(PhysicsData pd){
		if(EDebug.LogDisintegrations.b()){
			syso("Disintegrating:"+pd.getInfo()+",");
		}
		
		erasePhysicsFrom(pd.getSpatialWithPhysics());
		
		Node nodeParent = pd.getSpatialWithPhysics().getParent();
		pd.getSpatialWithPhysics().removeFromParent();
		if(nodeParent instanceof BatchNode)((BatchNode)nodeParent).batch();  
		
		pd.setDisintegrated(true);
	}

	public void updateInfo(){
		InfoJmeI.i().putAt(hmInfo,"TPS",iThreadPhysTPS);
		InfoJmeI.i().putAt(hmInfo,"PhysTPF",fThreadPhysTPF,3);
		InfoJmeI.i().putAt(hmInfo,"Disintegratables",hmDisintegratables.size());
		InfoJmeI.i().putAt(hmInfo,"TotRBCs",pspace.getRigidBodyList().size());
		InfoJmeI.i().putAt(hmInfo,"Spd",bullet.getSpeed(),2);
		InfoJmeI.i().putAt(hmInfo,"Grav",pspace.getGravity(new Vector3f()),1);
		InfoJmeI.i().putAt(hmInfo,"Min",pspace.getWorldMin(),0);
		InfoJmeI.i().putAt(hmInfo,"Max",pspace.getWorldMax(),0);
		InfoJmeI.i().putAt(hmInfo,"TotChars",pspace.getCharacterList().size());
		InfoJmeI.i().putAt(hmInfo,"TotGhosts",pspace.getGhostObjectList().size());
		InfoJmeI.i().putAt(hmInfo,"TotVehicles",pspace.getVehicleList().size());
		InfoJmeI.i().putAt(hmInfo,"TotJoints",pspace.getJointList().size());
		
		if(pdLastThrownFromCam!=null){
			InfoJmeI.i().putAt(hmInfo,"LtTrwSpd",pdLastThrownFromCam.getLastImpulse().fImpulseAtSelfDirection);
		}
		
		HWEnvironmentJmeI.i().putCustomInfo("Phys",InfoJmeI.i().prepareFullInfoRecursive(hmInfo));
	}
	
	synchronized public void requestDisintegration(PhysicsData pd){
		if(!apdDisintegrateAtMainThreadQueue.contains(pd))apdDisintegrateAtMainThreadQueue.add(pd);
	}
	
	/**
	 * TODO create a compound if it is a node with more than one geometry.
	 * @param geom
	 * @param mt
	 * @return
	 */
	public PhysicsData imbueFromWBounds(Geometry geom, MatterStatus mts, Node nodeStore){//, Vector3f v3fForceScaleCS){
		assert !UserDataI.i().contains(geom, PhysicsData.class);
		
		if(nodeStore!=null){
			/** to be on a node is important to let other things be attached to it like stuck projectiles */
			MiscJmeI.i().addToName(nodeStore, "PhysImbued:"+geom.getName(), false);
			
			Transform trf = geom.getWorldTransform();
			geom.setLocalTransform(new Transform());
			nodeStore.setLocalTransform(trf);
			
			Node nodeParent = geom.getParent();
			nodeStore.attachChild(geom);
			if(nodeParent!=null)nodeParent.attachChild(nodeStore);
		}
		
		PhysicsData pd = new PhysicsData(nodeStore,geom);
//		pd.nodexLink=spt;
		pd.saveSafePosRotFromSpatialLink();
		
		/*****************************************
		 * retrieve correct(default/Original/Aligned) bounding
		 * bkp rot
		 */
		pd.setWRotBkp(geom.getWorldRotation());
		/**
		 * reset rot: look at z+1 from where it is, and up to y=1
		 */
		geom.lookAt(geom.getWorldTranslation().add(0,0,1), Vector3f.UNIT_Y);
		
		/**
		 * get the bound related to an unmodified rotation 
		 */
		pd.setBoundingVolume(geom.getWorldBound().clone()); //the world bound will already be a scaled result...
		
		//restore rot
		geom.lookAt(geom.getWorldTranslation().add(pd.getWRotBkp().getRotationColumn(2)), pd.getWRotBkp().getRotationColumn(1));
		
		/***********************************************
		 *  create collision shape from bounds
		 */
		float fPseudoDiameter = 0f;
		if (pd.getBoundingVolume() instanceof BoundingBox) {
			pd.setAsBoundingBox();
			pd.setCollisionShape( new BoxCollisionShape(pd.getBoundingBox().getExtent(null)) );
			Vector3f v3fExtent = pd.getBoundingBox().getExtent(null);
			fPseudoDiameter=2f*Math.min(v3fExtent.x,Math.min(v3fExtent.y,v3fExtent.z)); //to make it sure wont fallthru by any direction
//			fPseudoDiameter=2f*pd.bb.getExtent(null).length();
		}else
		if (pd.getBoundingVolume() instanceof BoundingSphere) {
			pd.setAsBoundingSphere();
			pd.setCollisionShape( new SphereCollisionShape(pd.getBoundingSphere().getRadius()) );
			fPseudoDiameter=2f*pd.getBoundingSphere().getRadius();
		}else{
			throw new DetailedException("unsupported "+pd.getBoundingVolume().getClass(),geom);
		}
		
		RigidBodyControl rbc = new RigidBodyControl(pd.getCollisionShape());
		pd.setPRB(rbc);
		
		pd.setMatterStatus(mts);
		
		float fCCdMotionThreshold = fPseudoDiameter/2f;
		pd.setCcdMotionThresholdBkp(fCCdMotionThreshold);
		rbc.setCcdMotionThreshold(fCCdMotionThreshold);
		if(isDisableCcdToLetCollisionGroupsWork()) {
			disableCcdToLetCollisionGroupsWork(pd);
		}
		
		/**
		 * "Each time the object moves more than (motionThreshold) within one frame a sphere of radius 
		 * (sweptSphereRadius) is swept from the first position of the object to the position in the 
		 * next frame to check if there was any objects in between that were missed because the object 
		 * moved too fast." - https://hub.jmonkeyengine.org/t/ccd-usage/24655/13
		 * 
		 * "The radius is just the radius of the sphere that is swept to do this check, so make it so 
		 * large that it resembles your object." - https://hub.jmonkeyengine.org/t/ccd-usage/24655/2
		 */
		rbc.setCcdSweptSphereRadius(fPseudoDiameter/2f);
		
		pd.getSpatialWithPhysics().addControl(rbc); //this will put the rbc at spatial's W/L location/rotation
		
		pd.setPosAtPreviousTick(pd.getSpatialWithPhysics().getLocalTranslation());
		
		pd.updateMaterializedAtTime();
		
		UserDataI.i().putSafelyMustNotExist(pd.getSpatialWithPhysics(), pd); //BEFORE adding to phys space as its thread will be trying to retrieve it!
		pspace.add(pd.getSpatialWithPhysics()); //LAST THING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		
		return pd;
	}
	
	/**
	 * collision groups skippers/ignorers/collisioDeniers will not work without this... :(
	 * @param pd
	 */
	@Workaround
	@Bugfix
	public void disableCcdToLetCollisionGroupsWork(PhysicsData pd) {
		pd.getPRB(cc).setCcdMotionThreshold(0);
	}
	
	public PhysicsData getPhysicsDataFrom(PhysicsCollisionObject pco){
		Spatial spt=(Spatial)pco.getUserObject();
		PhysicsData pd = getPhysicsDataFrom(spt);
		return pd;
	}
	public PhysicsData getPhysicsDataFrom(Spatial spt){
		PhysicsData pd = UserDataI.i().getMustExistOrNull(spt, PhysicsData.class);
		if(pd==null){
			/**
			 * includes last to work even if detached, the top root will have no physics anyway
			 * this also allows childs with different phys data, like stuk projectiles
			 */
			for(Node node:SpatialHierarchyI.i().getAllParents(spt,true)){
				pd = UserDataI.i().getMustExistOrNull(node, PhysicsData.class);
//				if(pd!=null)return pd;
				break;
			}
		}
		return pd;
	}
	
	public void changeMatter(PhysicsData pd, Matter mt, Float fForceMassGramsIgnoringModelBoundVolume) {
		MatterStatus mtsNew = new MatterStatus(mt);
		mtsNew.setVolumeCm3(pd.getMts().getVolumeCm3());
		if(fForceMassGramsIgnoringModelBoundVolume!=null) {
			mtsNew.setMassGrams(fForceMassGramsIgnoringModelBoundVolume);
		}
		pd.setMatterStatus(mtsNew);
		pd.getPRB(cc).setMass((float) mtsNew.getMassKg());
	}
	
	public void putPhysicsDataInfo(Spatial sptSourceRetrieveFrom, HashMap<String,Info> hmStore){
		PhysicsData pd = getPhysicsDataFrom(sptSourceRetrieveFrom);
		if(pd!=null){
			InfoJmeI.i().putAt(hmStore,"mass",pd.getMatterStatus().getMassKg(),3);
			InfoJmeI.i().putAt(hmStore,"vol",pd.getMatterStatus().getVolumeM3(),3);
			InfoJmeI.i().putAt(hmStore,"grav",pd.getGravityCopy(),1);
			InfoJmeI.i().putAt(hmStore,"rest",pd.isResting());
			if(pd.getSBNodeGluedProjectiles()!=null)InfoJmeI.i().putAt(hmStore,"GluePrjc",pd.getSBNodeGluedProjectiles().getChildren().size());
			
			// last as may change too much
			InfoJmeI.i().putAt(hmStore,"spd",pd.getLinearVelocityCopy(),2);
			InfoJmeI.i().putAt(hmStore,"angv",pd.getAngularVelocityCopy(),1);
//			InfoJmeI.i().putAt(hmStore,"LtTrwSpd",pd.imp.fImpulseAtSelfDirection);
		}
	}
	
//	private void initUpdateLastTargetInfo() {
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				TargetGeom tgt = TargetI.i().getLastSingleTarget();
//				if(tgt!=null){
//				}
//				return true;
//			}
//		}).enableLoopMode().setDelaySeconds(0.5f);
//	}
	
	public PhysicsData erasePhysicsFrom(Spatial spt){
		RigidBodyControl rbc = spt.getControl(RigidBodyControl.class);
		
		if(hmDisintegratables.containsKey(rbc)){
			hmDisintegratables.remove(rbc);
		}
		
		if(hmProjectiles.containsKey(rbc)){
			hmProjectiles.remove(rbc);
		}
		
		removeFromPhysicsSpace(spt);
		spt.removeControl(rbc); //AFTER from space removal
		
		PhysicsData pd = getPhysicsDataFrom(spt);
		UserDataI.i().eraseAllOf(spt,pd);
		
		return pd;
	}
	
	public void removeFromPhysicsSpace(Spatial spt){
		pspace.remove(spt);
	}
	
	public void add(Spatial spt){
		pspace.add(spt);
	}
	
//	public void applyImpulseLater(Spatial spt, Impulse imp){
//		assert getPhysicsDataFrom(spt)!=null;
////		ps.enqueue(callable)
//		
//		imp.spt = spt;//(Spatial)obj;
//		imp.rbc = imp.spt.getControl(RigidBodyControl.class);
//		imp.ps=ps;
//		
//		synchronized (arbcThreadPhysicsPreTickQueue) {
//			arbcThreadPhysicsPreTickQueue.add(imp);
//		}
//	}
	public void applyImpulseLater(PhysicsData pd, ImpTorForce imp){
//		assert getPhysicsDataFrom(spt)!=null;
//		ps.enqueue(callable)
		
//		imp.spt = spt;//(Spatial)obj;
//		imp.rbc = imp.spt.getControl(RigidBodyControl.class);
		imp.pd=pd;
		imp.ps=pspace;
		
		synchronized (arbcThreadPhysicsPreTickQueue) {
			arbcThreadPhysicsPreTickQueue.add(imp);
		}
	}
	
	/**
	 * THIS IS ANOTHER THREAD
	 */
	@NotMainThread
	@Deprecated
	@Override
	public void prePhysicsTick(PhysicsSpace ps, float tpf) {
//		if(DetailedException.isExitRequested())return;
		try{threadPhysicsPreTick(ps,tpf);}catch(Exception ex){DetailedException.forceExitTrapIrrevocablySuspendCurrentThread(ex);}
	}
	@NotMainThread
	public void threadPhysicsPreTick(PhysicsSpace ps, float tpf) {
		/**
		 * apply impulses/forces requests queued
		 */
		synchronized(arbcThreadPhysicsPreTickQueue){
			for(ImpTorForce imp:arbcThreadPhysicsPreTickQueue){
				assert imp.ps==ps;
				
				PhysicsRigidBody prb = imp.pd.getPRB(cc);
				
				if(imp.v3fForce!=null){
					if(imp.v3fForceLocation==null){
						prb.applyCentralForce(imp.v3fForce);
					}else{
						prb.applyForce(imp.v3fForce, imp.v3fForceLocation);
					}
				}
				
				if(imp.fImpulseAtSelfDirection!=null){
					Vector3f v3fDispl = Vector3f.ZERO;
					
					if(imp.fImpulseAtSelfDirectionUpwardsDisplacement!=null) {
						v3fDispl = prb.getPhysicsRotation().getRotationColumn(1).mult(imp.fImpulseAtSelfDirectionUpwardsDisplacement);
					}
					
					prb.applyImpulse(
						prb.getPhysicsRotation().getRotationColumn(2).mult(imp.fImpulseAtSelfDirection), 
						v3fDispl
					);
				}
				
				if(imp.v3fImpulse!=null)
					prb.applyImpulse(imp.v3fImpulse, imp.v3fImpulseRelPos);
				
				if(imp.v3fTorque!=null)
					prb.applyTorque(imp.v3fTorque);
				
				if(imp.v3fTorqueImpulse!=null)
					prb.applyTorqueImpulse(imp.v3fTorqueImpulse);
				
			}
			
			arbcThreadPhysicsPreTickQueue.clear();
		}
		
//		for(PhysicsRigidBody prb:ps.getRigidBodyList()){
//			PhysicsData pd = getPhysicsDataFrom(prb);
//			if(pd!=null && pd.getGlueWhere()!=null)pd.rbc.setMass(0);
//		}
	}
	
	/**
	 * THIS IS ANOTHER THREAD
	 */
	@NotMainThread
	@Deprecated
	@Override
	public void physicsTick(PhysicsSpace ps, float tpf) {
//		if(DetailedException.isExitRequested())return;
		try{threadPhysicsTick(ps,tpf);}catch(Exception ex){DetailedException.forceExitTrapIrrevocablySuspendCurrentThread(ex);}
	}
	/** 
	 * just for naming clarity 
	 * TODO write current forces to spatials for easy access?
	 */
	@NotMainThread
	public void threadPhysicsTick(PhysicsSpace ps, float tpf) {
		threadPhysicsTickCalcTPS(ps,tpf);
		threadPhysicsTickDebugLogDisplacementPerTick(ps,tpf);
		threadPhysicsTickSaveSafeSpot(ps,tpf);
		threadPhysicsTickDisintegrateAtWBoundsOrRestoreToSafeSpot(ps,tpf);
//		threadPhysicsTickLevitators(ps,tpf);
	}
	
//	private void threadPhysicsTickLevitators(PhysicsSpace ps, float tpf) {
	protected void updateLevitators(PhysicsSpace ps, float tpf) {
		for(PhysicsRigidBody prb:ps.getRigidBodyList()){ //TODO create a pd list of levitators instead of check all prbs...
			PhysicsData pdLevi = getPhysicsDataFrom(prb);
			if(!pdLevi.isLevitating())continue;
			
			float fNearMargin = (pdLevi.getLevitationHeight()*0.05f);
			
			ArrayList<RayCastResultX> resultsx = rayCastSortNearest(
				prb.getPhysicsLocation(), 
				// will try a bit farer to prevent falling too often unnecessarily
				prb.getPhysicsLocation().add(0, -(pdLevi.getLevitationHeight()+2f*fNearMargin), 0), false, 
				true, true, pdLevi
			);
			
			/**
			 * ignore/skip these ones
			 */
			RayCastResultX resHitBelow = null;
			for(RayCastResultX rx:resultsx) { 
				if(rx.getPD().getLeviFollow()==pdLevi)continue; //ignore the followers
				if(rx.getPD().isGrabbedBy(pdLevi))continue; //ignored grabbeds, prevents levitation trick/glitch TODO allow this optionally as a funny trick/magic
//				if(r.getPd().isGrabbed())continue; //TODO allow this by applying a downwards force on the grabber?
				resHitBelow=rx;
				break;
			}
			
			PhysicsData pdFollow = pdLevi.getLeviFollow();
			if(pdFollow!=null || resHitBelow!=null) {
				if(prb.getGravity().length()>0)pdLevi.setNewGravityAtMainThread(Vector3f.ZERO);
			}else {
//			if(resHitBelow==null){
				pdLevi.setNewGravityAtMainThread(null); //restores default gravity, ex.: so it can fall
			}
			
			if(pdFollow!=null) {
//				PhysicsRigidBody prbFollow = pdFollow.getPRB();
				Quaternion quaFollow = pdFollow.getPhysicsRotationCopy();
				Vector3f v3fDisplOrientedFollowTo = pdFollow.getPhysicsLocationCopy();
				Vector3f v3fDispl = pdLevi.getLevitationDisplacement();
				v3fDisplOrientedFollowTo.addLocal(quaFollow.getRotationColumn(0).mult(v3fDispl.x));
				v3fDisplOrientedFollowTo.addLocal(quaFollow.getRotationColumn(1).mult(v3fDispl.y));
				v3fDisplOrientedFollowTo.addLocal(quaFollow.getRotationColumn(2).mult(v3fDispl.z));
				
				prb.setPhysicsRotation(pdFollow.getPhysicsRotationCopy());
				prb.setPhysicsLocation(v3fDisplOrientedFollowTo);
			}else
			if(resHitBelow!=null) {
				// positioning above
				float fDistCurrent = prb.getPhysicsLocation().subtract(resHitBelow.getWHitPos()).length();
				float fDistRemainingToReach = pdLevi.getLevitationHeight()-fDistCurrent;
				if(fDistRemainingToReach > fNearMargin) {
					float fFinalHeight = pdLevi.getLevitationHeight()-fNearMargin; //will be a bit lower, the imprecision helps on making it more stable
					pdLevi.setPhysicsLocationAtMainThread(resHitBelow.getWHitPos().add(0,fFinalHeight,0));
				}
				
				/**
				 * for pseudo levitators (beings with legs): 
				 * use some trick to let the weight of the levitator be applied on below objects
				 * may be an invisible sphere, with the same mass of the levitator and its followers, 
				 * that keeps endless falling where the levitator is?  
				 */
				applyImpulseLater(resHitBelow.getPD(), new ImpTorForce().setImpulse(
					new Vector3f(0,-resHitBelow.getPD().getMass(),0), resHitBelow.getLocalHitPos()));
				
			}
			
		}
	}
	
	public RayCastResultX applyLevitationAtCamTarget(Float fHeight) {
		RayCastResultX pdrtr = getPhysicsDataAtCamDir(false, true);
		if(pdrtr!=null && !pdrtr.pd.isTerrain() && pdrtr.pd.getMass()>0) {
			pdrtr.pd.setLevitation(null,fHeight);
		}
		return pdrtr;
	}
	
	public void applyLevitation(PhysicsData pd,Float fHeight) {
		pd.setLevitation(null,fHeight);
	}
	
	private void threadPhysicsTickDisintegrateAtWBoundsOrRestoreToSafeSpot(PhysicsSpace ps, float tpf) {
		// auto disintegrate at world bounds or restore last safe spot TODO do one check per frame instead of all at a delayed time?
		if(tdChkOutOfWorldBoundsDisintegrationAllowed.isReady(true)){
			for(PhysicsRigidBody prb:ps.getRigidBodyList()){
				if(!bbSpace.contains(prb.getPhysicsLocation())){
//					if(CharacterI.i().isCharacter(prb))continue;
//					if(prb.getUserObject() instanceof GeometryTestProjectile)continue;
					
					if(EDebug.TestDynamicPhysicsWithoutSpatialAndData.b() && prb.getUserObject()==null)continue;
					PhysicsData pd = getPhysicsDataFrom(prb);
					if(pd==null)continue;//other stuff
//					if(pd==null){
//						syso("breakpoint here");
//					}
					
					if(pd.isAllowDisintegration()){
						if(EDebug.LogDisintegrationByOutOfWorldBoundaries.b())syso(pd.getInfo()+","+prb.getPhysicsLocation());
						requestDisintegration(pd);
					}else{
						if(pd.getLastSafeSpot()!=null){
//							resetForces(pd);
//							pd.restoreSafeSpotRot();
							apdSafeSpotRestoreMainThreadQueue.add(pd);
//							pd.restoreSafeSpotRotAtMainThread();
						}
					}
				}
			}
		}
	}

	private void threadPhysicsTickSaveSafeSpot(PhysicsSpace ps, float tpf) {
		// save safe spot
		lTickCount++;
		for(PhysicsRigidBody prb:ps.getRigidBodyList()){
			if(EDebug.TestDynamicPhysicsWithoutSpatialAndData.b() && prb.getUserObject()==null)continue;
			PhysicsData pd = getPhysicsDataFrom(prb);
			if(pd==null)continue; //other stuff
			
			pd.setLastPhysUpdateNano(System.nanoTime()); //TODO use simulation time?
			
//			if(pd.getPRB().getGravity().length()==0)prb.activate();
			if(prb.getGravity().length()==0)prb.activate(); //no gravity, never rests
			if(pd.isForceAwakePhysTickCount())prb.activate();
			
			if(pd.isProjectile()){
				if(pd.getGlueWhere()!=null){
					/**
					 * not imediate to let the collision impulse/force be applied on what was hit
					 */
					if(pd.isWaitPhysTicksB4Glueing()){
						pd.setReadyToGlue(true);
					}
//					pd.iWaitPhysTicksB4Glueing--; //after the check
					
//					if(pd.bGlueApplied){ //not imediate to let the collision impulse/force be applied on what was hit
//						pd.prb.setMass(0f);
//					}else{
					if(!pd.isGlueApplied()){ 
						pd.getGlueWhere().forceAwakeSomeTicks();
					}
				}
				continue;
			}
			
			if(isResting(prb)){
//				if(!pd.isWasSafePosRotSavedAtPreviousTickAndUpdate(lTickCount)){
				if(!pd.isResting()){
					pd.saveSafePosRot(lTickCount, prb.getPhysicsLocation(),prb.getPhysicsRotation());
					pd.setResting(true);
				}
			}else{
				pd.setResting(false);
			}
		}
	}

	private void threadPhysicsTickDebugLogDisplacementPerTick(PhysicsSpace ps,float tpf) {
		if(EDebug.LogDisplacementPerTick.b()){
			for(PhysicsRigidBody prb:ps.getRigidBodyList()){
				PhysicsData pd = getPhysicsDataFrom(prb);
				if(pd!=null){
					float fDist=prb.getPhysicsLocation().distance(pd.getPosAtPreviousTick());
					if(fDist>0){
						syso(pd.getInfo()+":stepDist="+fDist+","+"CcdMT="+prb.getCcdMotionThreshold()+",");
						pd.setPosAtPreviousTick(prb.getPhysicsLocation());//it is a copy
					}
				}
			}
		}
	}

	private void threadPhysicsTickCalcTPS(PhysicsSpace ps, float tpf) {
		// TPS ticks per second
		fThreadPhysTPF=tpf;
		iThreadPhysTickSum++;
		if(System.currentTimeMillis() > (lThreadPhysLastCalcTPS+1000)){
			lThreadPhysLastCalcTPS=System.currentTimeMillis();
			iThreadPhysTPS=iThreadPhysTickSum;
			iThreadPhysTickSum=0;
		}
	}

	public void resetForces(PhysicsData pd){
		pd.resetForces();
//		pd.getPRB().setAngularVelocity(Vector3f.ZERO);
//		pd.getPRB().setLinearVelocity(Vector3f.ZERO);
	}
	
	public void setEnabled(boolean enabled) {
		bullet.setEnabled(enabled);
	}

	public boolean isEnabled() {
		return bullet.isEnabled();
	}
	
	public boolean toggleEnabled(){
		setEnabled(!isEnabled());
		return isEnabled();
	}
	
	public void setBulletDebugVisualsEnabled(boolean debugEnabled) {
		bullet.setDebugEnabled(debugEnabled);
	}

	public boolean isBulletDebugVisualsEnabled() {
		return bullet.isDebugEnabled();
	}

	public float getSpeed() {
		return bullet.getSpeed();
	}

	public void setSpeed(float speed) {
		bullet.setSpeed(speed);
	}
	
//	public Impulse throwAtSelfDirImpulse(PhysicsData pd, float fDesiredSpeed){
//		return throwAtSelfDirImpulse(
//			pd.getSpatialWithPhysics(), 
//			(float) (fDesiredSpeed*pd.getMatterStatus().getMassKg()) //the final speed depends on the mass
//		); 
//	}
	/**
	 * Do not use with bullets, they are too tiny, too little mass, too fast...
	 * For such bullets use raycast and apply forces on the hit target.
	 * @param spt
	 * @param fImpulseAtDirection
	 * @return 
	 */
	public ImpTorForce throwAtSelfDirImpulse(PhysicsData pd, float fDesiredSpeed){
		float fImpulseAtDirection=(float) (fDesiredSpeed*pd.getMatterStatus().getMassKg()); //the final speed depends on the mass
//		PhysicsData pd = getPhysicsDataFrom(spt);
		if(pd!=null && pd.getMatterStatus().getMassGrams()<50f && fImpulseAtDirection>300){ //9mm bullet weights 124 grains = 8 grams of lead
			MessagesI.i().warnMsg(this, "this looks like a bullet, avoid using this method!", pd, fImpulseAtDirection);
		}
		ImpTorForce imp = new ImpTorForce().setImpulseAtSelfDir(fImpulseAtDirection,null);
		PhysicsI.i().applyImpulseLater(pd, imp);
		pd.setLastImpuse(imp);
		return imp;
	}
	
	public Object debugTest(Object... aobj){
		return null; //keep even if emtpy
	}
	
	public void testDebugCreateMarker(ColorRGBA color, Vector3f v3f) {
		Geometry geom = GeometryI.i().create(MeshI.i().sphere(0.05f),color);
		geom.setLocalTranslation(v3f);
		AppI.i().getRootNode().attachChild(geom);
	}
	
	public ImpTorForce throwFromCam(PhysicsData pd,float fDesiredSpeed){
		// in front of cam pos
		AppI.i().placeAtCamWPos(pd, 1f, true); //orientated z
		ImpTorForce imp = throwAtSelfDirImpulse(pd,fDesiredSpeed);
		pdLastThrownFromCam=pd;
		return imp;
	}
	
	public void syncPhysTransfFromSpt(PhysicsData pd,boolean bLocation,boolean bRotation) {
		syncPhysTransfFromSpt(pd.getSpatialWithPhysics(), pd.getPRB(cc), bLocation, bRotation);
	}
	public void syncPhysTransfFromSpt(Spatial spt,PhysicsRigidBody prb, boolean bLocation, boolean bRotation) {
		if(bLocation)prb.setPhysicsLocation(spt.getWorldTranslation());
		if(bRotation)prb.setPhysicsRotation(spt.getWorldRotation());
	}
	public void syncPhysTransfFromSpt(Spatial spt) {
		RigidBodyControl rbc = spt.getControl(RigidBodyControl.class);
		syncPhysTransfFromSpt(spt, rbc, true, true);
	}
	
	/**
	 * generated collision events, not necessarily in the expected order :(
	 * it was necessary to make a raycast to determine the first collision :(
	 */
	@Override
	public void collision(PhysicsCollisionEvent event) {
		if(event.getNodeB()==null || event.getNodeA()==null)return;
		
		if(EDebug.LogCollisions.b()){
			syso(":collision():"+event.getNodeA().getName()+" <-> "+event.getNodeB().getName());
		}

		PhysicsData pdA = getPhysicsDataFrom(event.getNodeA());
		PhysicsData pdB = getPhysicsDataFrom(event.getNodeB());
		
		if(pdA!=null) {
			pdA.setLastCollisionNano(SimulationTimeI.i().getNanoTime());
		}
		if(pdB!=null) {
			pdB.setLastCollisionNano(SimulationTimeI.i().getNanoTime());
		}
		
		if(pdA==null || pdB==null) {
			return; //ignore non supported stuff
		}
		
		PhysicsProjectileI.i().glueProjectileCheckApply(pdA,pdB,event.getLocalPointB());
		PhysicsProjectileI.i().glueProjectileCheckApply(pdB,pdA,event.getLocalPointA());
		
		/**
		 * to let it continuously receive impact impulses, as it sleeps too fast
		 */
		if(pdA.isProjectile() && !pdB.isProjectile()) {
			pdB.forceAwakeSomeTicks(); //
		}
		if(pdB.isProjectile() && !pdA.isProjectile()) {
			pdA.forceAwakeSomeTicks();
		}
	}
	
	/**
	 * this is for collision groups {@link PhysicsCollisionGroupListener}
	 */
	@NotMainThread
	@Deprecated //in favor or naming clarity
	@Override
	public boolean collide(PhysicsCollisionObject nodeA,PhysicsCollisionObject nodeB) {
//		if(DetailedException.isExitRequested())return false; //may prevent further errors elsewhere by preventing all collisions?
		try{return threadPhysicsGroupCollideAllowedCheck(nodeA,nodeB);}catch(Exception ex){DetailedException.forceExitTrapIrrevocablySuspendCurrentThread(ex);}
		return false; //dummy, never reached...
	}
	/** 
	 * (new name also for clarity)
	 * 
	 * On a single tick it may be about to collide with more than one!!!
	 * And the collision order may NOT be the expected one!!! 
	 * @return if collision will be allowed and generate collision events at main thread TODO right?
	 */
	@NotMainThread
	protected boolean threadPhysicsGroupCollideAllowedCheck(PhysicsCollisionObject nodeA,PhysicsCollisionObject nodeB) {
//		if(CharacterI.i().isCharacter(nodeA))return true;
//		if(CharacterI.i().isCharacter(nodeB))return true;
		
		if(EDebug.TestDynamicPhysicsWithoutSpatialAndData.b()){
			if(nodeA.getUserObject()==null || nodeB.getUserObject()==null){
				return true;
			}
		}
		
		PhysicsData pdA = getPhysicsDataFrom((Spatial)nodeA.getUserObject());
		PhysicsData pdB = getPhysicsDataFrom((Spatial)nodeB.getUserObject());
		
		if(pdA==null || pdB==null){
			return false; //deny unsupported stuff TODO but... despite not generating collision events, it is colliding... :(
		}
		
		// spawned ignores its spawn source/parent
		if(pdA.containsPhysicsDataSkipCollisionGroup(pdB))return false;
		if(pdB.containsPhysicsDataSkipCollisionGroup(pdA))return false;
		
		//	ignore the followers
		if(pdA.getLeviFollow()==pdB)return false; 
		if(pdB.getLeviFollow()==pdA)return false; 
		
		//ignored grabbeds, prevents levitation trick/glitch 
		if(pdA.isGrabbedBy(pdB))return false; 
		if(pdB.isGrabbedBy(pdA))return false;
		
		if(!isAllowGrabbedsPhysInterferences()) {
			if(pdA.isGrabbed() || pdB.isGrabbed())return false;
		}
		
//		/** these two may not be working because the order of the collision may not be the expected, and the glue where would be set after other collisions on the same tick */
//		if(pdA.getGlueWhere()!=null)return pdA.getGlueWhere()==pdB; //TODO this seems useless
//		if(pdB.getGlueWhere()!=null)return pdB.getGlueWhere()==pdA; //TODO this seems useless
		
		if(pdA.isProjectile() && pdB.isProjectile()){
			return false;//prevent prjctle vs prjctle TODO allow if spawn source differs, allow also if already collided once with something else and is on a ricochet
		}
		
		if(bGlueAllowed){
//			if(true)return false;
			
			Boolean b=null;
			b=threadPhysicsGroupGlueDetectProjectileNextHit(pdA);
			if(b!=null){
				return b;
			}
			
			b=threadPhysicsGroupGlueDetectProjectileNextHit(pdB);
			if(b!=null){
				return b;
			}
			
			// skip if projectile didnt glue //TODO make it cause a scractch at least
			if(pdA.isProjectile() && !pdB.isProjectile()) {
				return false;
			}
			if(!pdA.isProjectile() && pdB.isProjectile()) {
				return false;
			}
		}
		
		return true; //allow all other collisions of PhysicsData
	}
	
	public static enum EDebug{
		AllowLog(true),
		
		/** use the temps to avoid having to restart the application, can even just rename them later! */
		Temp0,		Temp1,		Temp2,		Temp3,		Temp4,		Temp5,		Temp6,		Temp7,		Temp8, Temp9,
		
		LogDisintegrations, 
		
		LogDisplacementPerTick,
		LogCollisions, 
		TestDynamicPhysicsWithoutSpatialAndData, 
		LogDisintegrationByOutOfWorldBoundaries, 
		;
		EDebug(){}
		EDebug(boolean b){this.b=b;}
		boolean b;
		public void set(boolean b){this.b=b;}
		public boolean b() {
			return b;
		}
	}
	
	private Comparator<PhysicsRayTestResult> cmpRayNearest = new Comparator<PhysicsRayTestResult>() {
		@Override public int compare(PhysicsRayTestResult o1, PhysicsRayTestResult o2) {
			return Float.compare(o1.getHitFraction(),o2.getHitFraction());
		}
	};
	private float fPhysicsRayCastRange=300f;
	
//	@SuppressWarnings("unchecked")
//	public List<PhysicsRayTestResult> rayCastSortNearest(Vector3f v3fFrom, Vector3f v3fTo, boolean bIgnoreProjectiles) {
//		List<PhysicsRayTestResult> aprtrList = pspace.rayTest(v3fFrom,v3fTo);
//		if(bIgnoreProjectiles) {
//			
//		}
//		Collections.sort(aprtrList, cmpRayNearest);
//		return aprtrList;
//	}
	public static class RayCastResultX{
		private PhysicsRayTestResult resPhys;
		private CollisionResult resGeom;
		
		private PhysicsData pd;
		private Vector3f v3fWrldHit;

		private Vector3f v3fNormal;

		private float fDistance;
		private Geometry geom;
		
		private Vector3f v3fLocalHit;
		
		
		
//		private PhysicsRigidBody prb;
		
		public RayCastResultX(PhysicsRayTestResult resPhys,
			CollisionResult resGeom, PhysicsData pd, Geometry geom, Vector3f v3fWrldHit,
			Vector3f v3fNormal, float fDistance
		) {
			super();
			this.resPhys = resPhys;
			this.resGeom = resGeom;
			this.pd = pd;
			this.geom=geom;
			this.v3fWrldHit = v3fWrldHit;
			this.v3fNormal = v3fNormal;
			this.fDistance = fDistance;
			
			//TODO could the geometry not be aligned with the current physics position/rotation
			if(pd!=null)v3fLocalHit = pd.getGeomOriginalInitialLink().worldToLocal(getWHitPos(),null);
		}
		public CollisionResult getResGeom() {
			return resGeom;
		}
		public PhysicsRayTestResult getResPhys() {
			return resPhys;
		}
		public PhysicsData getPD() {
			return pd;
		}
		public Vector3f getWHitPos() {
			return v3fWrldHit;
		}
//		public PhysicsRigidBody getPrb() {
//			return prb;
//		}
		
		@Override
		public String toString() {
			return pd.toString();
		}
		public float getDistance() {
			return fDistance;
		}
		public Vector3f getNormal() {
			return v3fNormal;
		}
		public Geometry getGeom() {
			return geom;
		}
		public Vector3f getLocalHitPos() {
			return v3fLocalHit;//pd.getGeomOriginalInitialLink().worldToLocal(getWHitPos(),null);
		}
	}
	
	/**
	 * 
	 * @param v3fFrom
	 * @param v3fToOrDirection
	 * @param bIsDirection direction will be limited by the max range, if false you can decide how far it will be
	 * @param bIgnoreProjectiles
	 * @param bFirstOnly
	 * @param apdSkip
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<RayCastResultX> rayCastSortNearest(Vector3f v3fFrom, Vector3f v3fToOrDirection, boolean bIsDirection, boolean bIgnoreProjectiles, boolean bFirstOnly, PhysicsData... apdSkip) {
		ArrayList<RayCastResultX> apdrtrList = new ArrayList<RayCastResultX>();
		Vector3f v3fTo = bIsDirection ? 
			v3fFrom.add(v3fToOrDirection.normalize().mult(fPhysicsRayCastRange)) : 
			v3fToOrDirection;
		List<PhysicsRayTestResult> aprtrList = pspace.rayTest(v3fFrom,v3fTo);
		Collections.sort(aprtrList, cmpRayNearest);
		
		labelResults:for(PhysicsRayTestResult result:aprtrList){
			PhysicsData pdChk = getPhysicsDataFrom(result.getCollisionObject());
			if(pdChk!=null){ 
				if(bIgnoreProjectiles && pdChk.isProjectile())continue; //to skip/ignore projectile vs projectile
				for(PhysicsData pdSkip:apdSkip) {
					if(pdSkip==pdChk)continue labelResults;
				}
				
				assert pdChk.isPRB(result.getCollisionObject());
				Vector3f v3fHit = v3fFrom.clone().interpolateLocal(v3fTo,result.getHitFraction());
				RayCastResultX resultx = new RayCastResultX(
					result, null, pdChk, pdChk.getGeomOriginalInitialLink(), v3fHit, result.getHitNormalLocal().clone(), 
					v3fFrom.distance(v3fHit) 
				);
				apdrtrList.add(resultx);
				if(bFirstOnly)break;
			}
		}
		
		return apdrtrList;
	}
	
	protected Boolean threadPhysicsGroupGlueDetectProjectileNextHit(PhysicsData pdProjectile){
		if(!pdProjectile.isProjectile())return null; //skip
		
		if(pdProjectile.getGlueWhere()!=null)return false;//1st hit only
		
		Vector3f v3fFrom = pdProjectile.getPhysicsLocationCopy();
		ArrayList<RayCastResultX> aresxList = rayCastSortNearest( // TODO use shortest than max range?
			v3fFrom,pdProjectile.getLinearVelocityCopy(),true,true,true,pdProjectile);
		if(aresxList.size()>0) {
			RayCastResultX resx = aresxList.get(0);
			boolean bGlued = pdProjectile.checkGluedAt(resx);
			
//			boolean bDeflected = pdProjectile.isHasGlueTargetDeflected();
			
			/**
			 * while hitting dynamic objects, the mass must remain > 0f to let forces be applied
			 */
			if(resx.pd.isTerrain() && bGlued){
				/**
				 * will be removed from physics space later
				 * "GLUE" ON TERRAIN HERE (not actually  glue, but terrain wont move anyway...)
				 * TODO changing mass here seems safe right?
				 */
				pdProjectile.setStaticPhysics(); //no need to be nested on a spatial when glueing on static terrain TODO instead check if nearest has mass=0? but it may be temporary and be glued would work better...
				pdProjectile.setPhysicsLocationAtMainThread(pdProjectile.getWorldGlueSpot());
//				pdProjectile.prb.setPhysicsLocation(pdProjectile.v3fWorldGlueSpot); //this positioning works precisely if done here, np, is easier, keep it here...
			}
			
			/**
			 * to collision groups work, Ccd must be disabled, but..
			 * to let impact impulses be applied, the projectile must not fail hitting the dynamic objects.
			 * the only way is to re-enable Ccd just before it is glued and removed from physics space!
			 * 
			 * TODO alternatively, apply impulse force and redirect projectile movement manually? this would allow ignoring Ccd and stil use collision groups! this would basically do bullet engine work manually...
			 * 
			 * TODO after the 1st glue attempt, Ccd will remain enabled auto hitting everything thru native bullet... find a workaround...
			 */
			pdProjectile.restoreCcdMotionThreshold();
			
			return true; //to generate the collision event
		}
			
		return null; //to skip
	}
	
	public RayCastResultX applyImpulseHitTarget(Spatial sptRayCastFrom, Float fImpulse){
		Vector3f v3fDir = sptRayCastFrom.getWorldRotation().getRotationColumn(2).clone();
		Vector3f v3fPos = sptRayCastFrom.getWorldTranslation();
		return applyImpulseHitTarget(v3fPos,v3fDir,fImpulse);
	}
	public RayCastResultX applyImpulseHitTarget(PhysicsData pdRayCastFrom, Float fImpulse){
		Vector3f v3fDir = pdRayCastFrom.getPhysicsRotationCopy().getRotationColumn(2);
		Vector3f v3fPos = pdRayCastFrom.getPhysicsLocationCopy();
		return applyImpulseHitTarget(v3fPos,v3fDir,fImpulse);
	}
	public RayCastResultX applyImpulseHitTargetFromCam(Float fImpulse){
		Vector3f v3fDir = AppI.i().getCamLookingAtDir();
		Vector3f v3fPos = AppI.i().getCamWPos(0);
		return applyImpulseHitTarget(v3fPos,v3fDir,fImpulse);
	}
	/**
	 * ex.: use this for insta bullet shots or pushing things 
	 * @param fImpulse if null, the impulse will be based on the target's mass
	 * @return
	 */
	public RayCastResultX applyImpulseHitTarget(Vector3f v3fPos, Vector3f v3fDir, Float fImpulse){
		ArrayList<RayCastResultX> ares = rayCastSortNearest(v3fPos, v3fDir, true, false, true); 
		if(ares.size()>0){
			RayCastResultX res = ares.get(0);
			if(fImpulse==null)fImpulse=res.pd.getMass();
			v3fDir=v3fDir.normalize();
			v3fDir.multLocal(fImpulse);
			applyImpulseLater(res.pd,	new ImpTorForce().setImpulse(v3fDir,res.getLocalHitPos())	);
			return res;
		}
		
		return null;
	}
//	/**
//	 * ex.: use this for insta bullet shots or pushing things 
//	 * @param fImpulse if null, the impulse will be based on the target's mass
//	 * @return
//	 */
//	public RayCastResultX applyImpulseHitTargetAtCamDirection(Float fImpulse){
//		RayCastResultX res = getPhysicsDataAtCamDir(false, true);
//		if(res!=null){
//			if(fImpulse==null)fImpulse=res.pd.getPRB().getMass();
//			applyImpulseLater(res.pd,
//				new ImpTorForce()
//					.setImpulse(
//						AppI.i().getCamLookingAtDir().mult(fImpulse), 
//						res.pd.getGeomOriginalInitialLink().worldToLocal(res.getV3fWrldHit(),null)
//					)
//			);
//			
//			return res;
//		}
//		
//		return null;
//	}
	public RayCastResultX getPhysicsDataAtCamDir(boolean bIgnoreProjectiles, boolean bFirstOnly, PhysicsData... apdSkip){
//		for(CollisionResult cr:WorldPickingI.i().raycastPiercingAtCenter(null)){
//			PhysicsData pd = getPhysicsDataFrom(cr.getGeometry());
//			if(pd==null)continue;
//			return pd;
//		}
		ArrayList<RayCastResultX> a = rayCastSortNearest(
			AppI.i().getCamWPos(0f), 
//			AppI.i().getCamWPos(0f).add(AppI.i().getCamLookingAtDir().mult(getPhysicsRayCastRange())), 
			AppI.i().getCamLookingAtDir(), true,
			bIgnoreProjectiles, bFirstOnly, apdSkip);
		if(a.size()>0)return a.get(0);
		return null;
	}
	
	public void syso(String str){
		if(EDebug.AllowLog.b()){
			String strSep="/";
			System.out.println("[Phys]"
				+"Tk"+lTickCount+strSep
//				+"Tm"+System.nanoTime()+strSep
				+TimeFormatI.i().getRealTimeFormatted(null,"HH:mm:ss.SSS")+strSep
				+Thread.currentThread().getName()+strSep
				+str);
		}
	}
	
	public boolean isResting(PhysicsRigidBody prb){
		return (prb.getLinearVelocity().length()==0 && prb.getAngularVelocity().length()==0);
	}
	
	@Deprecated
	private void checkAndSaveSafeSpot(
		Spatial sptA, 
		Vector3f v3fA, 
		Quaternion quaRotA, 
		Spatial sptB, 
		Vector3f v3fB,  
		Quaternion quaRotB
	){
		PhysicsData pdA = getPhysicsDataFrom(sptA);
		PhysicsData pdB = getPhysicsDataFrom(sptB);
		
		if(pdA.isTerrain())pdB.saveSafePosRot(v3fB,quaRotB);
		if(pdB.isTerrain())pdA.saveSafePosRot(v3fA,quaRotA);
	}

	public void wakeUp(PhysicsData pd) {
		pd.wakeUpPhysics();
	}

	public void cancelDisintegration(PhysicsData pdWhat) {
		// prevent disintegration if glued on dynamic //TODO just increase the timeout or limit the amount per dynamic parent
		pdWhat.setAllowDisintegration(false);
		hmDisintegratables.remove(pdWhat.getPRB(cc));
		apdDisintegrateAtMainThreadQueue.remove(pdWhat);
	}

	public Vector3f getGravityCopy() {
		return pspace.getGravity(new Vector3f());
	}
	
	public PhysicsData spawnVolumeBox(float fVolumeM3, Float fMassKg){
		return spawnVolumeBox(null, fVolumeM3, "", null, fMassKg);
	}
	public PhysicsData spawnVolumeBox(ColorRGBA color, float fVolumeM3, String strName, Vector3f v3fPos, Float fMassKg){
		if(color==null) {
			color=ColorRGBA.Gray.clone();
			color.a=0.5f;
		}
		
		Geometry geom = GeometryI.i().create(MeshI.i().box((float) (Math.cbrt(fVolumeM3)/2f)), color);
		geom.setName("Box"+strName);
		
//		/** to be on a node is important to let other things be attached to it like stuck projectiles */
//		Node node = new Node("TestBox"+str);
//		node.attachChild(geom);
		
		if(v3fPos==null) {
			ArrayList<RayCastResultX> resx = WorldPickingI.i().raycastPiercingAtCenter(null);
			if(resx.size()>0) {
				v3fPos = resx.get(0).getWHitPos();
			}else {
				return null;
			}
		}
		geom.move(v3fPos); //b4 physics
		
		Matter mt = EMatter.Generic1KgPerM3.get();
		if(fMassKg!=null) {
//			mt = new Matter(strName+StringI.i().getNextUniqueGlobalId(),(fMassKg*1000)/fVolumeM3);
			mt = new Matter(strName+StringI.i().getNextUniqueGlobalId(), fMassKg, fVolumeM3);
		}
		
		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geom,new MatterStatus(mt),new Node());
		
		AppI.i().getRootNode().attachChild(pd.getSpatialWithPhysics());
		
		return pd;
	}
	
	public PhysicsData spawnWall(Vector3f v3fFrom, Vector3f v3fTo) {
		return spawnWall(v3fFrom, v3fTo, false, null,null,null);
	}
	/**
	 * this is easy to use with terrain editors
	 * @param v3fFrom
	 * @param v3fTo
	 * @param bFloorRamp true for ramps/floors, will be rotated in z 90deg. false for walls, will be moved upwards by half height (so its bottom is on the target spots)
	 * @return
	 */
	public PhysicsData spawnWall(Vector3f v3fFrom, Vector3f v3fTo, boolean bFloorRamp, Float fHeightOrWidth, Float fThickness, ColorRGBA color) {
		if(fThickness==null)fThickness=0.25f; //a common brick wall
		if(fHeightOrWidth==null)fHeightOrWidth=2f; // a simple common wall
		if(color==null)color=ColorRGBA.Gray;//concrete looks
		
		float fLength = v3fTo.distance(v3fFrom);
		Vector3f v3fDir = v3fTo.subtract(v3fFrom);
		
		Geometry geomWall=GeometryI.i().create(new Box(fThickness/2f, fHeightOrWidth/2f, fLength/2f), color);
		geomWall.setName("OrthoWall");
		AppI.i().getRootNode().attachChild(geomWall);
		
		geomWall.setLocalTranslation(v3fFrom);
		geomWall.lookAt(v3fTo, Vector3f.UNIT_Y);
		
		geomWall.move(geomWall.getLocalRotation().getRotationColumn(2).mult(fLength/2f));
		
		if(bFloorRamp) {
			geomWall.rotate(0, 0, 90*FastMath.DEG_TO_RAD);
		}else {
			//wall bottom
			geomWall.move(geomWall.getLocalRotation().getRotationColumn(1).mult(fHeightOrWidth/2f));
		}
		
		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geomWall,new MatterStatus(EMatter.Generic1KgPerM3.get()),null).setTerrain(true);
		pd.setStaticPhysics(); //rbc
		
		return pd;
	}
	
//	/**
//	 * 
//	 * @param iPlane 0=x floor/ceiling, 1=y walls, 2=z easyRampsOriginOnBase 
//	 * @param fWidth
//	 * @param fHeight
//	 * @param fThickness can be null, will be default
//	 * @param v3fPos
//	 * @return
//	 */
//	@Deprecated //too messy..
//	public PhysicsData spawnWall(int iPlane,float fWidth,float fHeight,float fRadRotation,Float fThickness, Vector3f v3fPos){
//		if(fThickness==null)fThickness=0.1f;
//		
//		Geometry geomWall=GeometryI.i().create(new Box(fWidth/2f,fHeight/2f,fThickness/2f), ColorRGBA.Gray);
//		geomWall.setName("OrthoWall");
//		AppI.i().getRootNode().attachChild(geomWall);
////		Spatial sptAtRoot = geomWall;
//		
//		switch(iPlane){
//			case 0:
//				MiscJmeI.i().addToName(geomWall, "floor", false);
//				geomWall.rotate(90*FastMath.DEG_TO_RAD, 0, 0); //floors/ceilings
//				geomWall.rotate(0, fRadRotation, 0); //floors/ceilings
//				break;
//			case 1:
//				geomWall.rotate(0, fRadRotation, 0);
//				break; //default walls
//			case 2:
//				geomWall.rotate(0, 0, 90*FastMath.DEG_TO_RAD);
//				geomWall.setLocalTranslation(0,fWidth/2f,0);
//				
//				Node nodePivot = new Node("tmpPivotRot");
//				nodePivot.setLocalRotation(geomWall.getLocalRotation());
//				geomWall.getParent().attachChild(nodePivot);
//				RotateI.i().rotateAroundPivot(geomWall, nodePivot, fRadRotation, true);
//				nodePivot.removeFromParent();
//				
////				geomWall.rotate(0, 0, 90*FastMath.DEG_TO_RAD);
//				break;
//		}
//		
//		if(v3fPos!=null)geomWall.move(v3fPos); //b4 physics
//		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geomWall).setTerrain(true);
//		pd.getPRB().setMass(0f); //rbc
//		
////		AppI.i().getRootNode().attachChild(sptAtRoot);
//		
//		return pd;
//	}

	public boolean isGlueAllowed() {
		return bGlueAllowed;
	}

	public PhysicsI setGlueAllowed(boolean bGlueAllowed) {
		this.bGlueAllowed = bGlueAllowed;
		return this; 
	}

	public boolean isDisableCcdToLetCollisionGroupsWork() {
		return bDisableCcdToLetCollisionGroupsWork;
	}

	public PhysicsI setDisableCcdToLetCollisionGroupsWork(boolean bDisableCcdToLetCollisionGroupsWork) {
		this.bDisableCcdToLetCollisionGroupsWork = bDisableCcdToLetCollisionGroupsWork;
		return this; 
	}

	public float getPhysicsRayCastRange() {
		return fPhysicsRayCastRange;
	}

	public PhysicsI setPhysicsRayCastRange(float fPhysicsRayCastRange) {
		this.fPhysicsRayCastRange = fPhysicsRayCastRange;
		return this; 
	}

	public float getDefaultDeflectionAngle() {
		return fDeflectionAngle;
	}

	public PhysicsI setDeflectionAngle(float fDeflectionAngle) {
		this.fDeflectionAngle = fDeflectionAngle;
		return this; 
	}

	public void enqueueUpdatePhysicsAtMainThread(CallableWeak cw) {
		if(MainThreadI.i().isCurrentMainThread()) {
			cw.call();
		}else {
			/**
			 * if it is always enqueued, the apply order will be granted to work properly
			 * considering other enqueable things there, but.. this is only really necessary when
			 * requested from another thread...
			 */
			acallUpdtPhysAtMainThreadQueue.add(cw);
		}
		
	}

	public void assimilatePhysicsData(PhysicsData pd) {
		PhysicsRigidBody prb = pd.getPRB(cc);
		if(pd.isAllowDisintegration())hmDisintegratables.put(prb, pd);
		if(pd.isProjectile())hmProjectiles.put(prb, pd);
	}

	public boolean isAllowGrabbedsPhysInterferences() {
		return bAllowGrabbedsPhysInterferences;
	}

	public PhysicsI setAllowGrabbedsPhysInterferences(boolean bAllowGrabbedsPhysInterferences) {
		this.bAllowGrabbedsPhysInterferences = bAllowGrabbedsPhysInterferences;
		return this; 
	}


}
