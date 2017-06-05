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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import com.github.devconslejme.misc.Annotations.NotMainThread;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.InfoI.Info;
import com.github.devconslejme.misc.MatterI.EMatter;
import com.github.devconslejme.misc.MatterI.Matter;
import com.github.devconslejme.misc.MatterI.MatterStatus;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.TimeConvertI;
import com.github.devconslejme.misc.TimeFormatI;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.GeometryI.GeometryX;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bounding.BoundingVolume;
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
import com.jme3.bullet.collision.shapes.CollisionShape;
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
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PhysicsI implements PhysicsTickListener, PhysicsCollisionGroupListener, PhysicsCollisionListener{
	public static PhysicsI i(){return GlobalManagerI.i().get(PhysicsI.class);}
	
	private ArrayList<PhysicsData> apdDisintegrate = new ArrayList<PhysicsData>();
	private long	lTickCount=0;
	private HashMap<PhysicsRigidBody,PhysicsData> hmDisintegratables=new HashMap<PhysicsRigidBody,PhysicsData>(); 
	private BulletAppState	bullet;
	private PhysicsSpace	ps;
	private BoundingBox	bbSpace;
	private LinkedHashMap<String, Info>	hmInfo;
	private TimedDelay tdDisintegrate = new TimedDelay(10f).setActive(true);
	private TimedDelay tdSaveSafeSpotRot = new TimedDelay(3f).setActive(true);
	private PhysicsData	pdLastThrownFromCam;
	private int	iThreadPhysTPS;
	private int	iThreadPhysTickSum;
	private long	lThreadPhysLastCalcTPS;
	private float	fThreadPhysTPF;
	private ArrayList<Impulse> arbcThreadPhysicsPreTickQueue = new ArrayList();
	private boolean	bGlueAllowed=true;
	private float	fDefaultProjectileMaxLife=2;
	
	public static class Impulse{
		private Spatial	spt;
		private RigidBodyControl	rbc;
		/** @DefSelfNote dont expose it, this class is a simplifier */
		private PhysicsSpace	ps;
		
		private Vector3f	v3fForce;
		private Vector3f	v3fForceLocation;
		private Vector3f	v3fImpulseRelPos;
		private Vector3f	v3fImpulse;
		private Vector3f	v3fTorque;
		private Vector3f	v3fTorqueImpulse;
		private Float	fImpulseAtSelfDirection;
		
		public Spatial getSpt() {
			return spt;
		}
		public Impulse setSpt(Spatial spt) {
			this.spt = spt;
			return this;
		}
		public RigidBodyControl getRBC() {
			return rbc;
		}
		public Impulse setRBC(RigidBodyControl rbc) {
			this.rbc = rbc;
			return this;
		}
		
		public PhysicsSpace getPs() {
			return ps;
		}
		
		public Vector3f getForce() {
			return v3fForce;
		}
		public Impulse setForce(Vector3f v3fForce) {
			this.v3fForce = v3fForce;
			return this;
		}
		public Vector3f getForceLocation() {
			return v3fForceLocation;
		}
		public Impulse setForceLocation(Vector3f v3fForceLocation) {
			this.v3fForceLocation = v3fForceLocation;
			return this;
		}
		public Vector3f getImpulse() {
			return v3fImpulse;
		}
		public Impulse setImpulseAtSelfDir(float fImpulse) {
			assert v3fImpulse==null;
			this.fImpulseAtSelfDirection=fImpulse;
			return this;
		}
		/**
		 * 
		 * @param v3fImpulse
		 * @param v3fRelPos can be null (will default to zero)
		 * @return
		 */
		public Impulse setImpulse(Vector3f v3fImpulse,Vector3f v3fRelPos) {
			assert fImpulseAtSelfDirection==null;
			this.v3fImpulse = v3fImpulse;
			this.v3fImpulseRelPos = v3fRelPos==null?Vector3f.ZERO:v3fRelPos;
			return this;
		}
		public Vector3f getTorque() {
			return v3fTorque;
		}
		public Impulse setTorque(Vector3f v3fTorque) {
			this.v3fTorque = v3fTorque;
			return this;
		}
		public Vector3f getTorqueImpulse() {
			return v3fTorqueImpulse;
		}
		public Impulse setTorqueImpulse(Vector3f v3fTorqueImpulse) {
			this.v3fTorqueImpulse = v3fTorqueImpulse;
			return this;
		}
		
	}

	public void configure(){
		bullet = new BulletAppState();
		bullet.setThreadingType(ThreadingType.PARALLEL);
		AppI.i().attatchAppState(bullet);
		
		ps = bullet.getPhysicsSpace();
		ps.addTickListener(PhysicsI.this);
		ps.addCollisionListener(this);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_01);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_02);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_03);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_04);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_05);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_06);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_07);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_08);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_09);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_10);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_11);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_12);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_13);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_14);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_15);
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_16);
		
		bbSpace = new BoundingBox(ps.getWorldMin(), ps.getWorldMax());
		
		initMaintenanceUpdateLoop();
	}
	
	private void initMaintenanceUpdateLoop() {
		hmInfo = new LinkedHashMap<String,Info>();
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				updateInfo();
				
//				updateGlue(); //b4 disintegration!!! (so it may even just disintegrate safely)
				updateDisintegration();
				
				return true;
			}
		}).enableLoopMode().setDelaySeconds(0.5f);
	}
	
	protected void updateDisintegration() {
		long lSTime = SimulationTimeI.i().getNanoTime();
		for(PhysicsData pd:hmDisintegratables.values()){
			if((lSTime - pd.lMaterializedSTime) > (pd.lProjectileMaxLifeTime *(pd.bGlueApplied?10:1)) ){
				if(!apdDisintegrate.contains(pd))apdDisintegrate.add(pd);
			}
		}
		
		for(PhysicsData pd:apdDisintegrate){
			disintegrate(pd);
		}
		apdDisintegrate.clear();
	}
	
	protected void disintegrate(PhysicsData pd){
		if(EDebug.Temp9.b()){
			syso("Disintegrating:"+pd.getInfo()+",");
		}
		
		erasePhysicsFrom(pd.getSpatialWithPhysics());
		
		Node nodeParent = pd.getSpatialWithPhysics().getParent();
		pd.getSpatialWithPhysics().removeFromParent();
		if(nodeParent instanceof BatchNode)((BatchNode)nodeParent).batch();  
		
		pd.bDisintegrated=true;
	}

	public void updateInfo(){
		InfoJmeI.i().putAt(hmInfo,"TPS",iThreadPhysTPS);
		InfoJmeI.i().putAt(hmInfo,"PhysTPF",fThreadPhysTPF,3);
		InfoJmeI.i().putAt(hmInfo,"Disintegratables",hmDisintegratables.size());
		InfoJmeI.i().putAt(hmInfo,"TotRBCs",ps.getRigidBodyList().size());
		InfoJmeI.i().putAt(hmInfo,"Spd",bullet.getSpeed(),2);
		InfoJmeI.i().putAt(hmInfo,"Grav",ps.getGravity(new Vector3f()),1);
		InfoJmeI.i().putAt(hmInfo,"Min",ps.getWorldMin(),0);
		InfoJmeI.i().putAt(hmInfo,"Max",ps.getWorldMax(),0);
		InfoJmeI.i().putAt(hmInfo,"TotChars",ps.getCharacterList().size());
		InfoJmeI.i().putAt(hmInfo,"TotGhosts",ps.getGhostObjectList().size());
		InfoJmeI.i().putAt(hmInfo,"TotVehicles",ps.getVehicleList().size());
		InfoJmeI.i().putAt(hmInfo,"TotJoints",ps.getJointList().size());
		
		if(pdLastThrownFromCam!=null){
			InfoJmeI.i().putAt(hmInfo,"LtTrwSpd",pdLastThrownFromCam.imp.fImpulseAtSelfDirection);
		}
		
		HWEnvironmentJmeI.i().putCustomInfo("Phys",InfoJmeI.i().prepareFullInfoRecursive(hmInfo));
		/*
		HWEnvironmentJmeI.i().putCustomInfo("Phys",""
			+"Disintegratables="+hmDisintegratables.size()+", "
			+"Spd="+bullet.getSpeed()+", "
			+"Grav="+ps.getGravity(new Vector3f())+", "
			+"Min="+ps.getWorldMin()+", "
			+"Max="+ps.getWorldMax()+", "
			+"TotChars="+ps.getCharacterList().size()+", "
			+"TotGhosts="+ps.getGhostObjectList().size()+", "
			+"TotRBCs="+ps.getRigidBodyList().size()+", "
			+"TotVehicles="+ps.getVehicleList().size()+", "
			+"TotJoints="+ps.getJointList().size()+", "
			+"LtTrwSpd="+pdLastThrownFromCam.imp.fImpulseAtSelfDirection+", "
		);
		*/
	}
	
	public static class PhysicsData{
		protected long lProjectileMaxLifeTime;
		protected boolean	bDisintegrated;
		protected Quaternion	quaWRotBkp;
		protected BoundingVolume	bv;
		protected BoundingBox	bb;
		protected CollisionShape	cs;
		protected BoundingSphere	bs;
		protected RigidBodyControl	rbc;
//		protected Spatial	sptLink;
		protected boolean	bAllowDisintegration=false;
		protected Vector3f	v3fLastSafeSpot;
		protected Quaternion	quaLastSafeRot;
		protected boolean	bTerrain;
		protected long	lRestingAtTickCount;
		protected boolean	bResting;
		protected Impulse	imp;
		protected long	lMaterializedSTime;
		protected Vector3f	v3fWorldGlueSpot;
		protected PhysicsData	pdGlueWhere;
		protected boolean	bGlueApplied;
		protected boolean	bProjectile;
		protected Vector3f	v3fEventCollOtherLocalPos;
		protected Vector3f	v3fLocalGlueSpot;
		protected Quaternion	quaLocalGlueRot;
		protected Vector3f	v3fPosAtPreviousTick;
		protected Matter	mt;
		protected MatterStatus	mts;
		protected Geometry	geomLink;
		protected NodeX	nodexLink;
		
		/**
		 * 
		 * @param fX
		 * @param fY
		 * @param fZ
		 * @return new final rotation
		 */
		public Quaternion rotate(float fX,float fY,float fZ){
			Quaternion qua = new Quaternion();
			qua.fromAngles(fX,fY,fZ);
			getRBC().setPhysicsRotation(getRBC().getPhysicsRotation().mult(qua));
			return getRBC().getPhysicsRotation();
		}
		
		public long getProjectileMaxLifeTime() {
			return lProjectileMaxLifeTime;
		}

		public PhysicsData setProjectileMaxLifeTime(float fSeconds) {
			this.lProjectileMaxLifeTime = TimeConvertI.i().secondsToNano(fSeconds);
			return this; 
		}
		
		public PhysicsData(NodeX nodex, Geometry geom) {
			this.nodexLink=nodex;
			this.geomLink=geom;
//			if(spt instanceof NodeX)nodexLink=(NodeX)spt;
//			this.sptLink = spt;
			
			setProjectileMaxLifeTime(PhysicsI.i().getDefaultProjectileMaxLife());
		}

		public PhysicsRigidBody getRBC() {
			return rbc;
		}

		public boolean isAllowDisintegration() {
			return bAllowDisintegration;
		}

		public PhysicsData setAllowDisintegration(boolean bAllowDisintegration) {
			this.bAllowDisintegration = bAllowDisintegration;
			PhysicsI.i().hmDisintegratables.put(rbc, this);
			return this; 
		}

//		public void disintegrateLater() {
//			PhysicsI.i().requestDisintegration(this);
////			apdDisintegrate.add(this);
////			bDisintegrate=true;
//		}

		public Vector3f getLastSafeSpot() {
			return v3fLastSafeSpot;
		}

		public boolean isTerrain() {
			return bTerrain;
		}

		public PhysicsData setTerrain(boolean bTerrain) {
			this.bTerrain = bTerrain;
			return this; 
		}
		
		public void saveSafePosRotFromSpatialLink() {
			saveSafePosRot(getSpatialWithPhysics().getWorldTranslation(), getSpatialWithPhysics().getWorldRotation());
		}

		public void restoreSafeSpotRot() {
			rbc.setPhysicsLocation(v3fLastSafeSpot);
			rbc.setPhysicsRotation(quaLastSafeRot);
		}
		
		public boolean isWasSafePosRotSavedAtPreviousTickAndUpdate(long lTickCount){
			boolean b = lTickCount-1 == this.lRestingAtTickCount;
			if(b)this.lRestingAtTickCount=lTickCount;
			return b;
		}
		
		public void saveSafePosRot(Vector3f v3fPos, Quaternion quaRot) {
			saveSafePosRot(-1, v3fPos, quaRot);
		}
		public void saveSafePosRot(long lRestingAtTickCount, Vector3f v3fPos, Quaternion quaRot) {
			this.lRestingAtTickCount = lRestingAtTickCount;
//			if(v3fPos!=null)
			assert v3fPos!=null;
			this.v3fLastSafeSpot=v3fPos.clone();
			
			// rot may not be available
			if(quaRot!=null)this.quaLastSafeRot=quaRot.clone();
		}

		public boolean isResting() {
			return bResting;
		}

		public void setResting(boolean b) {
			this.bResting=b;
		}

		public void setLastImpuse(Impulse imp) {
			this.imp = imp;
		}

		public void updateMaterializedAtTime() {
			this.lMaterializedSTime=SimulationTimeI.i().getNanoTime();
		}

		public boolean isExplodeIfHit(PhysicsData pdWhere) {
			return bAllowDisintegration && pdWhere.bAllowDisintegration;
		}

//		public void setGlueWhere(PhysicsData pdWhere) {
//			this.v3fGlueAt = rbc.getPhysicsLocation();
//			this.pdGlueWhere = pdWhere;
//			if(!PhysicsI.i().apdGlue.contains(this))PhysicsI.i().apdGlue.add(this);
//		}

		public void setGlueWhere(PhysicsData pdWhere) {
			this.pdGlueWhere=pdWhere;
		}

		public boolean isProjectile() {
			return bProjectile;
		}
		
		@Override
		public String toString() {
			return getSpatialWithPhysics().getName();
		}

		public String getInfo() {
			return getSpatialWithPhysics().getName()+","+getSpatialWithPhysics().getClass().getSimpleName()+","+rbc.getClass().getSimpleName();
		}
		
		public Spatial getSpatialWithPhysics(){
			if(nodexLink!=null)return nodexLink;
			return geomLink;
		}
		
		public NodeX getEnclosingNode() {
			return nodexLink;
		}

		public Geometry getGeometry() {
			return geomLink;
		}
	}
	
	
//	ArrayList<PhysicsData> apdGlue = new ArrayList<PhysicsData>();
	
	synchronized public void requestDisintegration(PhysicsData pd){
		if(!apdDisintegrate.contains(pd))apdDisintegrate.add(pd);
	}
	
	/**
	 * 
	 * @param spt
	 * @return dynamic: mass 1f
	 */
	public PhysicsData imbueFromWBounds(Geometry geom){
		return imbueFromWBounds(geom,null,true);
	}
	/**
	 * TODO create a compound if it is a node with more than one geometry.
	 * @param geom
	 * @param mt
	 * @return
	 */
	public PhysicsData imbueFromWBounds(Geometry geom, Matter mt, boolean bEncloseInNode){//, Vector3f v3fForceScaleCS){
		assert !UserDataI.i().contains(geom, PhysicsData.class);
		
//		Spatial spt = geom;
		NodeX nodex=null;
		if(bEncloseInNode){
			/** to be on a node is important to let other things be attached to it like stuck projectiles */
			nodex = new NodeX("PhysImbued:"+geom.getName());
			
			Transform trf = geom.getWorldTransform();
			geom.setLocalTransform(new Transform());
			nodex.setLocalTransform(trf);
			
			Node nodeParent = geom.getParent();
			nodex.attachChild(geom);
			if(nodeParent!=null)nodeParent.attachChild(nodex);
		}
		
		PhysicsData pd = new PhysicsData(nodex,geom);
//		pd.nodexLink=spt;
		pd.saveSafePosRotFromSpatialLink();
		if(mt!=null){
			pd.mts=new MatterStatus(mt);
		}else{
			pd.mts=new MatterStatus(EMatter.Custom1KgPerM3.get());
		}
		
		/*****************************************
		 * retrieve correct bounding
		 */
		//bkp rot
		pd.quaWRotBkp = geom.getWorldRotation().clone();
		// reset rot: look at z+1 from where it is, and up to y=1
		geom.lookAt(geom.getWorldTranslation().add(0,0,1), Vector3f.UNIT_Y);
		
		/**
		 * get the bound related to an unmodified rotation 
		 */
		pd.bv = geom.getWorldBound().clone(); //the world bound will already be a scaled result...
		
		//restore rot
		geom.lookAt(geom.getWorldTranslation().add(pd.quaWRotBkp.getRotationColumn(2)), pd.quaWRotBkp.getRotationColumn(1));
		
		/***********************************************
		 *  create collision shape from bounds
		 */
		float fPseudoDiameter = 0f;
		if (pd.bv instanceof BoundingBox) {
			pd.bb = (BoundingBox) pd.bv.clone();
			pd.cs = new BoxCollisionShape(pd.bb.getExtent(null));
			fPseudoDiameter=2f*pd.bb.getExtent(null).length();
		}else
		if (pd.bv instanceof BoundingSphere) {
			pd.bs = (BoundingSphere) pd.bv.clone();
//			assert v3fForceScaleCS==null;
//			if(v3fForceScaleCS==null){
				pd.cs = new SphereCollisionShape(pd.bs.getRadius());
//			}else{
//				pd.cs = new CapsuleCollisionShape(pd.bs.getRadius());
//			}
			fPseudoDiameter=2f*pd.bs.getRadius();
		}else{
			throw new DetailedException("unsupported "+pd.bv.getClass(),geom);
		}
		
		pd.mts.setVolumeM3(pd.bv.getVolume());
		
		pd.rbc=new RigidBodyControl(pd.cs);
//		pd.rbc.getCollideWithGroups();
//		pd.rbc.getCollisionGroup();
		
		pd.rbc.setMass((float) pd.mts.getMass());
		
//		float fCCdMotionThreshold = fPseudoDiameter;
//		float fCCdMotionThreshold = fPseudoDiameter*0.75f;
		float fCCdMotionThreshold = fPseudoDiameter/2f;
		pd.rbc.setCcdMotionThreshold(fCCdMotionThreshold);
		
		/**
		 * "Each time the object moves more than (motionThreshold) within one frame a sphere of radius 
		 * (sweptSphereRadius) is swept from the first position of the object to the position in the 
		 * next frame to check if there was any objects in between that were missed because the object 
		 * moved too fast." - https://hub.jmonkeyengine.org/t/ccd-usage/24655/13
		 * 
		 * "The radius is just the radius of the sphere that is swept to do this check, so make it so 
		 * large that it resembles your object." - https://hub.jmonkeyengine.org/t/ccd-usage/24655/2
		 */
		pd.rbc.setCcdSweptSphereRadius(fPseudoDiameter/2f);
		
//		Spatial spt=nodex;
//		if(spt==null)spt=geom;
//		if(bEncloseInNode){
		pd.getSpatialWithPhysics().addControl(pd.rbc); //this will put the rbc at spatial's W/L location/rotation
//		}else{
//			geom.addControl(pd.rbc); //this will put the rbc at spatial's W/L location/rotation
//		}
//		if(bEncloseInNode){
//			/** to be on a node is important to let other things be attached to it like stuck projectiles */
//			Node nodeParent = geom.getParent();
//			Transform trf = geom.getWorldTransform();
//			NodeX nodex = new NodeX("PhysImbued:"+geom.getName());
//			nodex.attachChild(geom);
//			nodex.setLocalTransform(trf);
//			if(nodeParent!=null)nodeParent.attachChild(nodex);
//			nodex.addControl(pd.rbc); //this will put the rbc at spatial's W/L location/rotation
//		}else{
//			geom.addControl(pd.rbc); //this will put the rbc at spatial's W/L location/rotation
//		}
		
		pd.v3fPosAtPreviousTick=pd.getSpatialWithPhysics().getLocalTranslation().clone();
		
		pd.updateMaterializedAtTime();
		
		UserDataI.i().putSafelyMustNotExist(pd.getSpatialWithPhysics(), pd); //BEFORE adding to phys space as its thread will be trying to retrieve it!
		ps.add(pd.getSpatialWithPhysics()); //LAST THING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		
		return pd;
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
	
	public void putPhysicsData(Spatial sptSourceRetrieveFrom, HashMap<String,Info> hmStore){
		PhysicsData pd = getPhysicsDataFrom(sptSourceRetrieveFrom);
		if(pd!=null){
			InfoJmeI.i().putAt(hmStore,"mass",pd.mts.getMass(),3);
			InfoJmeI.i().putAt(hmStore,"vol",pd.mts.getVolumeM3(),3);
			InfoJmeI.i().putAt(hmStore,"spd",pd.rbc.getLinearVelocity(),2);
			InfoJmeI.i().putAt(hmStore,"angv",pd.rbc.getAngularVelocity(),1);
			InfoJmeI.i().putAt(hmStore,"grav",pd.rbc.getGravity(),1);
			InfoJmeI.i().putAt(hmStore,"rest",pd.isResting());
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
		
		removeFromPhysicsSpace(spt);
		spt.removeControl(rbc); //AFTER from space removal
		
		PhysicsData pd = getPhysicsDataFrom(spt);
		UserDataI.i().eraseAllOf(spt,pd);
		
		return pd;
	}
	
	public void removeFromPhysicsSpace(Spatial spt){
		ps.remove(spt);
	}
	
	public void add(Spatial spt){
		ps.add(spt);
	}
	
	public void applyImpulseLater(Spatial spt, Impulse imp){
		assert getPhysicsDataFrom(spt)!=null;
//		ps.enqueue(callable)
		
		imp.spt = spt;//(Spatial)obj;
		imp.rbc = imp.spt.getControl(RigidBodyControl.class);
		imp.ps=ps;
		
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
		try{threadPhysicsDoPrePhysicsTick(ps,tpf);}catch(Exception ex){DetailedException.forceExitTrapIrrevocablySuspendCurrentThread(ex);}
	}
	@NotMainThread
	public void threadPhysicsDoPrePhysicsTick(PhysicsSpace ps, float tpf) {
		synchronized(arbcThreadPhysicsPreTickQueue){
			for(Impulse imp:arbcThreadPhysicsPreTickQueue){
				assert imp.ps==ps;
				
				if(imp.v3fForce!=null){
					if(imp.v3fForceLocation==null){
						imp.rbc.applyCentralForce(imp.v3fForce);
					}else{
						imp.rbc.applyForce(imp.v3fForce, imp.v3fForceLocation);
					}
				}
				
				if(imp.fImpulseAtSelfDirection!=null){
					imp.rbc.applyImpulse(
						imp.rbc.getPhysicsRotation().getRotationColumn(2).mult(imp.fImpulseAtSelfDirection), 
						Vector3f.ZERO
					);
				}
				
				if(imp.v3fImpulse!=null)
					imp.rbc.applyImpulse(imp.v3fImpulse, imp.v3fImpulseRelPos);
				
				if(imp.v3fTorque!=null)
					imp.rbc.applyTorque(imp.v3fTorque);
				
				if(imp.v3fTorqueImpulse!=null)
					imp.rbc.applyTorqueImpulse(imp.v3fTorqueImpulse);
				
			}
			
			arbcThreadPhysicsPreTickQueue.clear();
		}
	}
	
	/**
	 * THIS IS ANOTHER THREAD
	 */
	@NotMainThread
	@Deprecated
	@Override
	public void physicsTick(PhysicsSpace ps, float tpf) {
//		if(DetailedException.isExitRequested())return;
		try{threadPhysicsDoPhysicsTick(ps,tpf);}catch(Exception ex){DetailedException.forceExitTrapIrrevocablySuspendCurrentThread(ex);}
	}
	/** 
	 * just for naming clarity 
	 * TODO write current forces to spatials for easy access?
	 */
	@NotMainThread
	public void threadPhysicsDoPhysicsTick(PhysicsSpace ps, float tpf) {
		// TPS
		fThreadPhysTPF=tpf;
		iThreadPhysTickSum++;
		if(System.currentTimeMillis() > (lThreadPhysLastCalcTPS+1000)){
			lThreadPhysLastCalcTPS=System.currentTimeMillis();
			iThreadPhysTPS=iThreadPhysTickSum;
			iThreadPhysTickSum=0;
		}
		
		if(EDebug.LogDisplacementPerTick.b()){
			for(PhysicsRigidBody prb:ps.getRigidBodyList()){
				PhysicsData pd = getPhysicsDataFrom(prb);
				if(pd!=null){
					float fDist=prb.getPhysicsLocation().distance(pd.v3fPosAtPreviousTick);
					if(fDist>0){
						syso(pd.getInfo()+":stepDist="+fDist+","+"CcdMT="+prb.getCcdMotionThreshold()+",");
						pd.v3fPosAtPreviousTick=prb.getPhysicsLocation();//it is a copy
					}
				}
			}
		}
		
//		if(true)return;
		
		// save safe spot
		lTickCount++;
		for(PhysicsRigidBody prb:ps.getRigidBodyList()){
//			if(CharacterI.i().isCharacter(prb))continue;
//			if(prb.getUserObject() instanceof GeometryTestProjectile)continue;
			
			if(EDebug.TestDynamicPhysicsWithoutSpatialAndData.b() && prb.getUserObject()==null)continue;
			PhysicsData pd = getPhysicsDataFrom(prb);
			if(pd==null)continue; //other stuff
//			if(pd==null){
//				syso("breakpoint here");
//			}
			
			if(pd.isProjectile())continue;
			
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
			
		// auto disintegrate at world bounds or restore last safe spot
		if(tdDisintegrate.isReady(true)){
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
							resetForces(pd);
							pd.restoreSafeSpotRot();
						}
					}
				}
			}
		}
		
	}
	
	public void resetForces(PhysicsData pd){
		pd.getRBC().setAngularVelocity(Vector3f.ZERO);
		pd.getRBC().setLinearVelocity(Vector3f.ZERO);
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
	
	/**
	 * Do not use with bullets, they are too tiny, too little mass, too fast...
	 * For such bullets use raycast and apply forces on the hit target.
	 * @param spt
	 * @param fImpulseAtDirection
	 * @return 
	 */
	public Impulse throwAtSelfDirImpulse(Spatial spt, float fImpulseAtDirection){
		PhysicsData pd = getPhysicsDataFrom(spt);
		if(pd!=null && pd.mts.getMass()<0.01f && fImpulseAtDirection>300){
			MessagesI.i().warnMsg(this, "this looks like a bullet, avoid using this method!", spt, pd, fImpulseAtDirection);
		}
		Impulse imp = new Impulse().setImpulseAtSelfDir(fImpulseAtDirection);
		PhysicsI.i().applyImpulseLater(spt, imp);
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
	
	public Impulse throwFromCam(PhysicsData pd,float fDesiredSpeed){
		AppI.i().placeAtCamWPos(pd.getSpatialWithPhysics(), 1f, true); //orientated z
		syncPhysTransfFromSpt(pd.getSpatialWithPhysics());
		Impulse imp = throwAtSelfDirImpulse(pd.getSpatialWithPhysics(), (float) (fDesiredSpeed*pd.mts.getMass())); //the final speed depends on the mass
		pdLastThrownFromCam=pd;
		return imp;
	}
	
	public void syncPhysTransfFromSpt(Spatial spt) {
		RigidBodyControl rbc = spt.getControl(RigidBodyControl.class);
		rbc.setPhysicsLocation(spt.getWorldTranslation());
		rbc.setPhysicsRotation(spt.getWorldRotation());
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
		if(pdA==null || pdB==null)return;
		
		PhysicsProjectileI.i().glueProjectileCheckApply(pdA,pdB,event.getLocalPointB());
		PhysicsProjectileI.i().glueProjectileCheckApply(pdB,pdA,event.getLocalPointA());
	}
	
	/**
	 * this is for collision groups {@link PhysicsCollisionGroupListener}
	 */
	@NotMainThread
	@Deprecated //in favor or naming clarity
	@Override
	public boolean collide(PhysicsCollisionObject nodeA,PhysicsCollisionObject nodeB) {
//		if(DetailedException.isExitRequested())return false; //may prevent further errors elsewhere by preventing all collisions?
		try{return threadPhysicsDoAboutToCollideWithGroupChkAllowed(nodeA,nodeB);}catch(Exception ex){DetailedException.forceExitTrapIrrevocablySuspendCurrentThread(ex);}
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
	protected boolean threadPhysicsDoAboutToCollideWithGroupChkAllowed(PhysicsCollisionObject nodeA,PhysicsCollisionObject nodeB) {
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
			return true; //other stuff
		}
		
		if(pdA.isProjectile() && pdB.isProjectile()){
			return false;//prevent prjctle vs prjctle
		}
		
		if(bGlueAllowed){
			Boolean b=null;
			b=threadPhysicsGlueDetectProjectileNextHit(pdA);
			if(b!=null){
				return b;
			}
			
			b=threadPhysicsGlueDetectProjectileNextHit(pdB);
			if(b!=null){
				return b;
			}
		}
		
		return true; //allow all other collisions
	}
	
	public static enum EDebug{
		AllowLog(true),
		
		/** use the temps to avoid having to restart the application, can even just rename them later! */
		Temp0,		Temp1,		Temp2,		Temp3,		Temp4,		Temp5,		Temp6,		Temp7,		Temp8,		
		Temp9, //TODO rename LogDisintegrations
		
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
	
//	protected Boolean threadPhysicsChkProjectileMovingTowards(PhysicsCollisionObject node, PhysicsData pd){
	protected Boolean threadPhysicsGlueDetectProjectileNextHit(PhysicsData pd){
//		if(node.getUserObject() instanceof GeometryTestProjectile){
//			PhysicsData pd = getPhysicsDataFrom((Spatial)node.getUserObject());
		if(!pd.isProjectile())return null; //skip
		
		if(pd.pdGlueWhere!=null)return false;//1st hit only
//			RigidBodyControl rbc = (RigidBodyControl)node;
		
		Vector3f v3fFrom = pd.rbc.getPhysicsLocation();
		Vector3f v3fTo = pd.rbc.getPhysicsLocation().add(pd.rbc.getLinearVelocity().normalize().mult(10)); //mult 10 is a guess for high speed, TODO confirm this
//			Vector3f v3fTo = pd.rbc.getPhysicsLocation().add(pd.rbc.getLinearVelocity());
		@SuppressWarnings("unchecked")List<PhysicsRayTestResult> rayTest = ps.rayTest(v3fFrom,v3fTo);
		
		Vector3f v3fWrldHitNearest = null;
		RigidBodyControl pcoNearest = null;
		PhysicsData pdNearest=null;
		for(PhysicsRayTestResult prtr:rayTest){
//				if(prtr.getCollisionObject() instanceof RigidBodyControl){
//					RigidBodyControl rbcMovingTowards = (RigidBodyControl)prtr.getCollisionObject();
//				}
			PhysicsCollisionObject pco = prtr.getCollisionObject();
			PhysicsData pdChk = getPhysicsDataFrom(prtr.getCollisionObject());
			if(pdChk!=null){ //1st
				if(pdChk.isProjectile())continue; //to skip/ignore projectile vs projectile
//					if(pdChk.isProjectile())return false; //prevent each other
				
				Vector3f v3fWrldHitChk = v3fFrom.clone().interpolateLocal(v3fTo,prtr.getHitFraction());
				if(v3fWrldHitNearest==null || v3fFrom.distance(v3fWrldHitChk)<v3fFrom.distance(v3fWrldHitNearest)){
					v3fWrldHitNearest=v3fWrldHitChk;
					pcoNearest = (RigidBodyControl)pco;
					pdNearest=pdChk;
				}
			}
		}
		
//			if(v3fWHitNearest!=null && pcoNearest!=null){
		if(pdNearest!=null){
//				if(pdNearest.isProjectile())return false; //prevent each other
			
			pd.pdGlueWhere=(pdNearest);
			pd.v3fWorldGlueSpot=v3fWrldHitNearest;
			pd.v3fLocalGlueSpot=v3fWrldHitNearest.subtract(pcoNearest.getPhysicsLocation());
			pd.quaLocalGlueRot=pcoNearest.getPhysicsRotation();
			
			if(pdNearest.isTerrain()){
//					resetForces(pd); //prevents delayed ricochet
//					pd.rbc.setGravity(Vector3f.ZERO);//prevent falling
				pd.rbc.setMass(0f); //no need to nested spatial glue on static terrain TODO instead check if nearest has mass=0? but it may be temporary and be glued would work better...
				pd.rbc.setPhysicsLocation(pd.v3fWorldGlueSpot);
				return true; //to generate the collision event
			}
		}
			
		return null; //to skip
	}
	
	/**
	 * ex.: use this for insta bullet shots or pushing things 
	 * @param fImpulse
	 * @return
	 */
	public CollisionResult applyImpulseHitTargetAtCamDirection(float fImpulse){
		for(CollisionResult cr:WorldPickingI.i().raycastPiercingAtCenter(null)){
			if(getPhysicsDataFrom(cr.getGeometry())==null)continue;
			
			applyImpulseLater(
				cr.getGeometry(),
				new Impulse().setImpulse(AppI.i().getCamLookingAtDir(), cr.getGeometry().worldToLocal(cr.getContactPoint(),null))
			);
			
			return cr;
		}
		
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
		pd.rbc.activate();
	}

	public void cancelDisintegration(PhysicsData pdWhat) {
		// prevent disintegration if glued on dynamic //TODO just increase the timeout or limit the amount per dynamic parent 
		hmDisintegratables.remove(pdWhat.rbc);
		apdDisintegrate.remove(pdWhat);
	}

	public Vector3f getGravity() {
		return ps.getGravity(new Vector3f());
	}
	
	/**
	 * 
	 * @param color can be null
	 * @param fVolume
	 * @param str
	 * @param v3fPos
	 * @return
	 */
	public PhysicsData spawnVolumeBox(ColorRGBA color, float fVolume, String str, Vector3f v3fPos){
		if(color==null)color=ColorRGBA.Gray;
		
		Geometry geom = GeometryI.i().create(MeshI.i().box((float) (Math.cbrt(fVolume)/2f)), color);
		geom.setName("Box"+str);
		
//		/** to be on a node is important to let other things be attached to it like stuck projectiles */
//		Node node = new Node("TestBox"+str);
//		node.attachChild(geom);
		
		if(v3fPos!=null)geom.move(v3fPos); //b4 physics
		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geom,EMatter.Custom1KgPerM3.get(),true);
		
		AppI.i().getRootNode().attachChild(pd.getSpatialWithPhysics());
		
		return pd;
	}

	/**
	 * 
	 * @param iPlane 0=x floor/ceiling, 1=y walls, 2=z easyRampsOriginOnBase will have a node parent 
	 * @param fWidth
	 * @param fHeight
	 * @param fThickness can be null, will be default
	 * @param v3fPos
	 * @return
	 */
	public PhysicsData spawnOrthoWall(int iPlane,float fWidth,float fHeight,Float fThickness, Vector3f v3fPos){
		if(fThickness==null)fThickness=0.1f;
		
		GeometryX geomWall=GeometryI.i().create(new Box(fWidth/2f,fHeight/2f,fThickness/2f), ColorRGBA.Gray);
		geomWall.setName("OrthoWall");
		AppI.i().getRootNode().attachChild(geomWall);
//		Spatial sptAtRoot = geomWall;
		
		switch(iPlane){
			case 0:
				MiscJmeI.i().addToName(geomWall, "floor", false);
				geomWall.rotate(90*FastMath.DEG_TO_RAD, 0, 0); //floors/ceilings
				break;
			case 1:break; //default wall
			case 2:
				geomWall.rotate(0, 0, 90*FastMath.DEG_TO_RAD);
//				NodeX nodexParent = new NodeX(geomWall.getName()+":Ramp");
//				nodexParent.attachChild(geomWall);
				geomWall.move(0,fWidth/2f,0);
//				sptAtRoot=nodexParent;
				break;
		}
		
		if(v3fPos!=null)geomWall.move(v3fPos); //b4 physics
		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geomWall).setTerrain(true);
		pd.getRBC().setMass(0f); //rbc
		
//		AppI.i().getRootNode().attachChild(sptAtRoot);
		
		return pd;
	}

	public boolean isGlueAllowed() {
		return bGlueAllowed;
	}

	public PhysicsI setGlueAllowed(boolean bGlueAllowed) {
		this.bGlueAllowed = bGlueAllowed;
		return this; 
	}

	public float getDefaultProjectileMaxLife() {
		return fDefaultProjectileMaxLife;
	}

	public PhysicsI setDefaultProjectileMaxLife(float fDefaultProjectileMaxLife) {
		this.fDefaultProjectileMaxLife = fDefaultProjectileMaxLife;
		return this; 
	}
}
