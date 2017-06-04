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
import java.util.List;

import com.github.devconslejme.game.CharacterI;
import com.github.devconslejme.misc.Annotations.NotMainThread;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.InfoI.Info;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.TimeConvertI;
import com.github.devconslejme.misc.TimeFormatI;
import com.github.devconslejme.misc.TimedDelay;
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
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.BatchNode;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

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
	private HashMap<String, Info>	hmInfo;
	private TimedDelay tdDisintegrate = new TimedDelay(10f).setActive(true);
	private TimedDelay tdSaveSafeSpotRot = new TimedDelay(3f).setActive(true);
	private PhysicsData	pdLastThrownFromCam;
	private int	iThreadPhysTPS;
	private int	iThreadPhysTickSum;
	private long	lThreadPhysLastCalcTPS;
	private float	fThreadPhysTPF;
	private ArrayList<Impulse> arbcThreadPhysicsPreTickQueue = new ArrayList();
	
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
		hmInfo = new HashMap<String,Info>();
		
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
		
		for(PhysicsData pd:apdDisintegrate)disintegrate(pd);
		apdDisintegrate.clear();
	}
	
	protected void disintegrate(PhysicsData pd){
		erasePhysicsFrom(pd.sptLink);
		
		Node nodeParent = pd.sptLink.getParent();
		pd.sptLink.removeFromParent();
		if(nodeParent instanceof BatchNode)((BatchNode)nodeParent).batch();  
		
		pd.bDisintegrated=true;
	}

	public void updateInfo(){
		InfoJmeI.i().putAt(hmInfo,"Disintegratables",hmDisintegratables.size());
		InfoJmeI.i().putAt(hmInfo,"Spd",bullet.getSpeed(),2);
		InfoJmeI.i().putAt(hmInfo,"Grav",ps.getGravity(new Vector3f()),1);
		InfoJmeI.i().putAt(hmInfo,"Min",ps.getWorldMin(),0);
		InfoJmeI.i().putAt(hmInfo,"Max",ps.getWorldMax(),0);
		InfoJmeI.i().putAt(hmInfo,"TotChars",ps.getCharacterList().size());
		InfoJmeI.i().putAt(hmInfo,"TotGhosts",ps.getGhostObjectList().size());
		InfoJmeI.i().putAt(hmInfo,"TotRBCs",ps.getRigidBodyList().size());
		InfoJmeI.i().putAt(hmInfo,"TotVehicles",ps.getVehicleList().size());
		InfoJmeI.i().putAt(hmInfo,"TotJoints",ps.getJointList().size());
		InfoJmeI.i().putAt(hmInfo,"TPS",iThreadPhysTPS);
		InfoJmeI.i().putAt(hmInfo,"PhysTPF",fThreadPhysTPF,3);
		
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
	
	/**
	 * 
	 * @param spt
	 * @return dynamic: mass 1f
	 */
	public PhysicsData imbueFromWBounds(Spatial spt){
		return imbueFromWBounds(spt,null);
	}
	public static class PhysicsData{
		long lProjectileMaxLifeTime;
		boolean	bDisintegrated;
		float fMass;
		Quaternion	quaWRotBkp;
		BoundingVolume	bv;
		BoundingBox	bb;
		CollisionShape	cs;
		BoundingSphere	bs;
		RigidBodyControl	rbc;
		Spatial	sptLink;
		Float	fDensity;
		float	fVolume;
		boolean	bAllowDisintegration=false;
//		private boolean	bDisintegrate;
		Vector3f	v3fLastSafeSpot;
		Quaternion	quaLastSafeRot;
		boolean	bTerrain;
		long	lRestingAtTickCount;
		boolean	bResting;
		Impulse	imp;
		long	lMaterializedSTime;
		Vector3f	v3fWorldGlueSpot;
		PhysicsData	pdGlueWhere;
//		private boolean	bApplyGluedMode;
		boolean	bGlueApplied;
		boolean	bProjectile;
		Vector3f	v3fEventCollOtherLocalPos;
		Vector3f	v3fLocalGlueSpot;
		Quaternion	quaLocalGlueRot;
		
		public long getProjectileMaxLifeTime() {
			return lProjectileMaxLifeTime;
		}

		public PhysicsData setProjectileMaxLifeTime(float fSeconds) {
			this.lProjectileMaxLifeTime = TimeConvertI.i().secondsToNano(fSeconds);
			return this; 
		}
		
		public PhysicsData(Spatial spt) {
			this.sptLink = spt;
			
			setProjectileMaxLifeTime(2);
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
			saveSafePosRot(sptLink.getWorldTranslation(), sptLink.getWorldRotation());
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
			return sptLink.getName();
		}
	}
	
	
//	ArrayList<PhysicsData> apdGlue = new ArrayList<PhysicsData>();
	
	synchronized public void requestDisintegration(PhysicsData pd){
		if(!apdDisintegrate.contains(pd))apdDisintegrate.add(pd);
	}
	
	public PhysicsData imbueFromWBounds(Spatial spt, Float fDensityAutoMassFromVolume){//, Vector3f v3fForceScaleCS){
		assert !UserDataI.i().contains(spt, PhysicsData.class);
		
		PhysicsData pd = new PhysicsData(spt);
		pd.fDensity=fDensityAutoMassFromVolume;
		pd.saveSafePosRotFromSpatialLink();
		
		//bkp rot
		pd.quaWRotBkp = spt.getWorldRotation().clone();
		// reset rot: look at z+1 from where it is, and up to y=1
		spt.lookAt(spt.getWorldTranslation().add(0,0,1), Vector3f.UNIT_Y);
		
		pd.bv = spt.getWorldBound().clone(); //the world bound will already be a scaled result...
		
		//restore rot
		spt.lookAt(spt.getWorldTranslation().add(pd.quaWRotBkp.getRotationColumn(2)), pd.quaWRotBkp.getRotationColumn(1));
		
		// create collision shape from bounds
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
			throw new DetailedException("unsupported "+pd.bv.getClass(),spt);
		}
		
//		if(v3fForceScaleCS!=null){
//			pd.cs.setScale(v3fForceScaleCS);
//		}
		
//		if(false)assert spt.getWorldScale().lengthSquared()==3f : "scaled collision shape may cause imprecision and even ccd will fail";
//		if(spt.getWorldScale().lengthSquared()!=3f){ //TODO enable this one day?
//			cs.setScale(spt.getWorldScale());
//		}
		
		pd.fVolume = pd.bv.getVolume();
		
		pd.rbc=new RigidBodyControl(pd.cs);
//		pd.rbc.getCollideWithGroups();
//		pd.rbc.getCollisionGroup();
		
		pd.fMass=1f;
		if(pd.fDensity!=null){
			pd.fMass=pd.fVolume*pd.fDensity;
		}
		pd.rbc.setMass(pd.fMass);
		
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
		
		spt.addControl(pd.rbc); //this will put the rbc at spatial's W/L location/rotation
		
		pd.updateMaterializedAtTime();
		
		UserDataI.i().putSafelyMustNotExist(spt, pd); //BEFORE adding to phys space as its thread will be trying to retrieve it!
		ps.add(spt); //LAST THING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		
		return pd;
	}
	
	public PhysicsData getPhysicsDataFrom(PhysicsCollisionObject pco){
		Spatial spt=(Spatial)pco.getUserObject();
		PhysicsData pd = getPhysicsDataFrom(spt);
		return pd;
	}
	public PhysicsData getPhysicsDataFrom(Spatial spt){
		return UserDataI.i().getMustExistOrNull(spt, PhysicsData.class);
	}
	
	public void putPhysicsData(Spatial sptSourceRetrieveFrom, HashMap<String,Info> hmStore){
		PhysicsData pd = getPhysicsDataFrom(sptSourceRetrieveFrom);
		if(pd!=null){
			InfoJmeI.i().putAt(hmStore,"mass",pd.fMass,3);
			InfoJmeI.i().putAt(hmStore,"vol",pd.fVolume,3);
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
		
//		if(true)return;
		
		// save safe spot
		lTickCount++;
		for(PhysicsRigidBody prb:ps.getRigidBodyList()){
			if(CharacterI.i().isCharacter(prb))continue;
//			if(prb.getUserObject() instanceof GeometryTestProjectile)continue;
			
			PhysicsData pd = getPhysicsDataFrom(prb);
			if(pd==null){
				syso("breakpoint here");
			}
			
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
					if(CharacterI.i().isCharacter(prb))continue;
//					if(prb.getUserObject() instanceof GeometryTestProjectile)continue;
					
					PhysicsData pd = getPhysicsDataFrom(prb);
					if(pd==null){
						syso("breakpoint here");
					}
					
					if(pd.isAllowDisintegration()){
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

	public void setDebugEnabled(boolean debugEnabled) {
		bullet.setDebugEnabled(debugEnabled);
	}

	public boolean isDebugEnabled() {
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
		if(pd!=null && pd.fMass<0.01f && fImpulseAtDirection>300){
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
		AppI.i().placeAtCamWPos(pd.sptLink, 1f, true); //orientated z
		syncPhysTransfFromSpt(pd.sptLink);
		Impulse imp = throwAtSelfDirImpulse(pd.sptLink, fDesiredSpeed*pd.fMass); //the final speed depends on the mass
		pdLastThrownFromCam=pd;
		return imp;
	}
	
	public void syncPhysTransfFromSpt(Spatial spt) {
		RigidBodyControl rbc = spt.getControl(RigidBodyControl.class);
		rbc.setPhysicsLocation(spt.getWorldTranslation());
		rbc.setPhysicsRotation(spt.getWorldRotation());
	}
	
	
	@Override
	public void collision(PhysicsCollisionEvent event) {
		if(event.getNodeB()==null || event.getNodeA()==null)return;
		if(false)syso(":collision():"+event.getNodeA().getName()+" <-> "+event.getNodeB().getName());
		
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
	 */
	@NotMainThread
	protected boolean threadPhysicsDoAboutToCollideWithGroupChkAllowed(PhysicsCollisionObject nodeA,PhysicsCollisionObject nodeB) {
		if(CharacterI.i().isCharacter(nodeA))return true;
		if(CharacterI.i().isCharacter(nodeB))return true;
		
		PhysicsData pdA = getPhysicsDataFrom((Spatial)nodeA.getUserObject());
		PhysicsData pdB = getPhysicsDataFrom((Spatial)nodeB.getUserObject());
		
		if(pdA.isProjectile() && pdB.isProjectile())return false;//prevent prjctle vs prjctle
		
		Boolean b=null;
		b=threadPhysicsChkProjectileMovingTowards(pdA);
		if(b!=null)return b;
		
		b=threadPhysicsChkProjectileMovingTowards(pdB);
		if(b!=null)return b;
		
		return true; //allow all other collisions
	}
	
//	protected Boolean threadPhysicsChkProjectileMovingTowards(PhysicsCollisionObject node, PhysicsData pd){
	protected Boolean threadPhysicsChkProjectileMovingTowards(PhysicsData pd){
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
					pd.rbc.setMass(0f);
					return true; //to generate the collision event
				}
			}
			
//		}
		
		return null; //to skip
	}
//		if(nodeA.getUserObject() instanceof GeometryTestProjectile && nodeB.getUserObject() instanceof GeometryTestProjectile){
//			return false;
//		}
//		
////		boolean bGlued=
////			( nodeA.getUserObject() instanceof GeometryTestProjectile) && !(nodeB.getUserObject() instanceof GeometryTestProjectile)
////			||
////			!(nodeA.getUserObject() instanceof GeometryTestProjectile) &&  (nodeB.getUserObject() instanceof GeometryTestProjectile)
////		;
//		boolean bGlued=(nodeA.getUserObject() instanceof GeometryTestProjectile || nodeB.getUserObject() instanceof GeometryTestProjectile);
//		if(bGlued){
//			PhysicsData pdA = getPhysicsDataFrom(nodeA);
//			PhysicsData pdB = getPhysicsDataFrom(nodeB);
//			boolean bSkip=false; //disintegrated or already set glue
//			if(pdA.bDisintegrated || pdA.pdGlueWhere!=null){
////				resetForces(pdA);
////				pdA.rbc.setEnabled(false);
//				bSkip=true;
//			}
//			if(pdB.bDisintegrated || pdB.pdGlueWhere!=null){
////				resetForces(pdB);
////				pdB.rbc.setEnabled(false);
//				bSkip=true;
//			}
////			syso(pdA.sptLink.getName()+","+pdB.sptLink.getName());
//			if(bSkip)return false; //only the 1st matters
//			
////			if(false){
//			if(nodeA.getUserObject() instanceof GeometryTestProjectile){
////				ps.remove(nodeA); //remove physics control to stop moving NOW, to "stay" where it is
//				pdA.setGlueWhere(pdB);
//			}
//			
//			if(nodeB.getUserObject() instanceof GeometryTestProjectile){
////				ps.remove(nodeB); //remove physics control to stop moving NOW, to "stay" where it is
//				pdB.setGlueWhere(pdA);
//			}
////			}
//		
//			return true; //true to generate the collision event with the applied force, hit spot etc
//		}
		
//		PhysicsData pdA = getPhysicsDataFrom(nodeA);
//		PhysicsData pdB = getPhysicsDataFrom(nodeB);
//		if(pdA==null){
//			syso("put break point here");
//		}
//		if(pdB==null){
//			syso("put break point here");
//		}
//		
//		assert pdA!=null && pdB!=null;
		
//		if(false)syso(":[byGroup]collide():"
//			+pdA.sptLink.getName()
//			+" <-> "+pdB.sptLink.getName());
		
//		return true; //allow all collisions
//	}
	
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
//		ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
//		if(acr.size()>0){
//			CollisionResult cr = acr.get(0);
//			if(getPhysicsDataFrom(cr.getGeometry())==null)return null;
//			
//			applyImpulseLater(
//				cr.getGeometry(),
//				new Impulse().setImpulse(AppI.i().getCamLookingAtDir(), cr.getGeometry().worldToLocal(cr.getContactPoint(),null))
//			);
//			
//			return cr;
//		}
		return null;
	}
	
	public void syso(String str){
//		if(false)// this is weighty!
			System.out.println(lTickCount+":"+System.nanoTime()+":"+TimeFormatI.i().getRealTimeFormatted()+":"+Thread.currentThread().getName()+":"+str);
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

}
