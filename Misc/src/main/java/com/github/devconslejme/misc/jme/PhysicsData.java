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

import com.github.devconslejme.game.CharacterI.LeviCharacter;
import com.github.devconslejme.misc.MainThreadI;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.TimeConvertI;
import com.github.devconslejme.misc.MatterI.MatterStatus;
import com.github.devconslejme.misc.jme.PhysicsI.ImpTorForce;
import com.github.devconslejme.misc.jme.PhysicsI.PhysicsData;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bounding.BoundingVolume;
import com.jme3.bullet.collision.shapes.CollisionShape;
import com.jme3.bullet.objects.PhysicsRigidBody;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.SimpleBatchNode;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public  class PhysicsData{
	private SimpleBatchNode	sbnGluedProjectiles;
	private long lProjectileMaxLifeTime;
	private boolean	bDisintegrated;
	private Quaternion	quaWRotBkp;
	private BoundingVolume	bv;
	private BoundingBox	bb;
	private CollisionShape	cs;
	private BoundingSphere	bs;
	private PhysicsRigidBody	prb;
	private boolean	bAllowDisintegration=false;
	private Vector3f	v3fLastSafeSpot;
	private Quaternion	quaLastSafeRot;
	private boolean	bTerrain;
	private long	lRestingAtTickCount;
	private boolean	bResting;
	private ImpTorForce	imp;
	private long	lLastPhysUpdateNano;
	private long	lLastRenderUpdateNano;
	private long	lMaterializedSimulTimeNano;
	private Vector3f	v3fWorldGlueSpot;
	private PhysicsData	pdGlueWhere;
	private ArrayList<PhysicsData>	apdPhysicsDataSkipCollisionGroup = new ArrayList<>();
	private boolean	bGlueApplied;
	private boolean	bProjectile;
	private Vector3f	v3fEventCollOtherLocalPos;
	private Vector3f	v3fGlueWherePhysLocalPos;
	private Quaternion	quaGlueWherePhysWRotAtImpact;
	private Vector3f	v3fPosAtPreviousTick;
	private MatterStatus	mts;
	private Geometry	geomOriginalInitialLink;
	private Node	nodexLink;
	private int	iForceAwakePhysTickCount;
	private int	iWaitPhysTicksB4Glueing=1;//1;
	private float	fGrabDist;
//		private CollisionResult cr;
//		private Vector3f v3fGravityBkp;
	private LeviCharacter lcGrabber;
	private Vector3f v3fLevitationDisplacement=null;
	private float fCCdMotionThreshold;
	private Vector3f v3fNewGravity=null;
	private boolean bSuspendLevitation;
	private Vector3f v3fNewPhysLocation;
	public boolean bReadyToGlue;
	private PhysicsData pdLevitationFollow;
	private RayCastResultX resxGlueTarget;
	private boolean bGlueTargetDeflected;
	private float fLastHitAngleAtGluableDeg;
	private boolean bAlignFlyDirection=true;
	
	/**
	 * radians
	 * @param fX
	 * @param fY
	 * @param fZ
	 * @return new final rotation
	 */
	public Quaternion addRotation(float fX,float fY,float fZ){
		Quaternion qua = new Quaternion();
		qua.fromAngles(fX,fY,fZ);
		getPRB().setPhysicsRotation(getPRB().getPhysicsRotation().mult(qua));
		return getPRB().getPhysicsRotation();
	}
	
	public boolean isAlignFlyDirection() {
		return bAlignFlyDirection;
	}

	public long getAgeNano() {
		return (SimulationTimeI.i().getNanoTime() - lMaterializedSimulTimeNano);
	}

	public long getProjectileMaxLifeTime() {
		if(bGlueApplied)return lProjectileMaxLifeTime*PhysicsProjectileI.i().getGluedProjectileMaxLifeTimeMultiplier();
		return lProjectileMaxLifeTime;
	}

	public PhysicsData setProjectileMaxLifeTime(float fSeconds) {
		this.lProjectileMaxLifeTime = TimeConvertI.i().secondsToNano(fSeconds);
		return this; 
	}
	
	public PhysicsData(Node nodex, Geometry geom) {
		this.nodexLink=nodex;
		this.geomOriginalInitialLink=geom;
//			this.v3fGravityBkp = new Vector3f(PhysicsI.i().getGravity());
//			if(spt instanceof NodeX)nodexLink=(NodeX)spt;
//			this.sptLink = spt;
		
		setProjectileMaxLifeTime(PhysicsProjectileI.i().getDefaultProjectileMaxLife());
	}

//	public PhysicsRigidBody getPRB() {
//		return prb;
//	}

	public boolean isAllowDisintegration() {
		return bAllowDisintegration;
	}

	public PhysicsData setAllowDisintegration(boolean bAllowDisintegration) {
		this.bAllowDisintegration = bAllowDisintegration;
		if(bAllowDisintegration) {
			PhysicsI.i().hmDisintegratables.put(prb, this);
		}else {
			PhysicsI.i().hmDisintegratables.remove(prb);
		}
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

	public void setLastImpuse(ImpTorForce imp) {
		this.imp = imp;
	}

	public void updateMaterializedAtTime() {
		this.lMaterializedSimulTimeNano=SimulationTimeI.i().getNanoTime();
	}

	public boolean isExplodeIfHit(PhysicsData pdWhere) {
		return bAllowDisintegration && pdWhere.bAllowDisintegration;
	}

//		public void setGlueWhere(PhysicsData pdWhere) {
//			this.v3fGlueAt = rbc.getPhysicsLocation();
//			this.pdGlueWhere = pdWhere;
//			if(!PhysicsI.i().apdGlue.contains(this))PhysicsI.i().apdGlue.add(this);
//		}

	public PhysicsData getGlueWhere() {
		return pdGlueWhere;
	}
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
		return getSpatialWithPhysics().getName()+","+getSpatialWithPhysics().getClass().getSimpleName()+","+prb.getClass().getSimpleName();
	}
	
	public boolean isEnclosed() {
		return nodexLink!=null;
	}
	
	public Spatial getSpatialWithPhysics(){
		if(isEnclosed())return nodexLink;
		return geomOriginalInitialLink;
	}
	
//		public NodeX getEnclosingNode() {
//			return nodexLink;
//		}

//		public Geometry getGeometry() {
//			return geomLink;
//		}

	public void setMatterStatus(MatterStatus mts) {
//			if(mt!=null){
//				mts=new MatterStatus(mt);
//			}else{
//				mts=new MatterStatus(EMatter.Generic1KgPerM3.get());
//			}
		this.mts=mts;
		if(this.mts.getMassGrams()==0) {
			/**
			 * only uses the volume if the mass is not set,
			 * this way, a mass can override the volume,
			 * making it not real tho.
			 * 
			 * good to prevent too tiny physics colliders, 
			 * and to prevent physics engine problems derived of too discrepant masses (one too high and other too low)
			 * TODO confirm, the good mass ratio for collisions should not exceed 20??? (like 1kg vs 20kg), or just check that and reduce applied forces while creating damaged spots on both colliders?
			 */
			this.mts.setVolumeM3(bv.getVolume());
		}
		prb.setMass((float)this.mts.getMassKg());
	}

	public Geometry getInitialOriginalGeometry() {
		return geomOriginalInitialLink;
	}

	public void forceAwakeSomeTicks() {
		iForceAwakePhysTickCount=10;
	}

	public boolean isStatic() {
		return prb.getMass()==0;
	}

	public boolean isActivatable() {
		return ActivatorI.i().isActivetable(getInitialOriginalGeometry());
	}

	public PhysicsData setGrabDist(float fGrabDist) {
		this.fGrabDist = fGrabDist;
		return this;
	}
	
	public float getGrabDist(){
		return fGrabDist;
	}
	
	public void restoreSafeSpotRotAtMainThread() {
		if(MainThreadI.i().isCurrentMainThread()) {
			applyRestoreSafeSpotRotAtMainThread();
		}else {
			PhysicsI.i().apdSafeSpotRestoreMainThreadQueue.add(this);
		}
	}
	public void applyRestoreSafeSpotRotAtMainThread() {
		MainThreadI.i().assertEqualsCurrentThread();
		PhysicsI.i().resetForces(this);
		prb.setPhysicsLocation(v3fLastSafeSpot);
		prb.setPhysicsRotation(quaLastSafeRot);
	}
	/**
	 * bullet native crashes otherwise..
	 */
	public void applyNewGravityAtMainThread() {
		MainThreadI.i().assertEqualsCurrentThread();
		this.prb.setGravity(v3fNewGravity);
	}
	/**
	 * @param v3f null to restore bkp
	 */
	public void setNewGravityAtMainThread(Vector3f v3f) {
		if(v3f==null)v3f = PhysicsI.i().getGravityCopy().clone(); //restore the bkp
		if(v3fNewGravity==null || !v3fNewGravity.equals(v3f)) {
			v3fNewGravity = v3f.clone();
			if(MainThreadI.i().isCurrentMainThread()) {
				applyNewGravityAtMainThread();
			}else {
				PhysicsI.i().apdGravityUpdtMainThreadQueue.add(this);
			}
		}
	}
	public PhysicsData setTempGravityTowards(Vector3f v3fGravityTargetSpot, Float fAcceleration) {
		Vector3f v3fNewGravity=null;
		if(v3fGravityTargetSpot==null) {
			v3fNewGravity=(PhysicsI.i().getGravityCopy());
		}else {
			v3fNewGravity = v3fGravityTargetSpot.subtract(prb.getPhysicsLocation()).normalize();
			v3fNewGravity.multLocal(fAcceleration!=null?fAcceleration:PhysicsI.i().getGravityCopy().length());
		}
		
		setNewGravityAtMainThread(v3fNewGravity);
		prb.activate();
		
		return this;
	}
	/**
	 * otherwise, will glitch impulses applied to it, making them inverted in rotation/torque! 
	 */
	public void setPhysicsLocationAtMainThread(Vector3f v3f) {
		v3fNewPhysLocation=v3f.clone();
		if(MainThreadI.i().isCurrentMainThread()) {
			applyNewPhysLocationAtMainThread();
		}else {
			PhysicsI.i().apdLocationUpdtMainThreadQueue.add(this);
		}
	}

	private void applyNewPhysLocationAtMainThread() {
		MainThreadI.i().assertEqualsCurrentThread();
		this.prb.setPhysicsLocation(v3fNewPhysLocation);
	}

	public void setPRB(PhysicsRigidBody prb) {
		this.prb=prb;
	}

	public PhysicsData setGrabbedBy(LeviCharacter bccxGrabber) {
		this.lcGrabber = bccxGrabber;
		return this;
	}
//		public PhysicsData setGrabbedBy(BetterCharacterControlX bccxGrabber) {
//			this.bccxGrabber = bccxGrabber;
//			return this;
//		}
	
	public boolean isGrabbed() {
		return lcGrabber!=null;
	}

//		public CollisionResult getCollisionResult() {
//			return cr;
//		}
//
//		public PhysicsData setCollisionResult(CollisionResult cr) {
//			this.cr = cr;
//			return this; 
//		}

	public SimpleBatchNode getSBNodeGluedProjectiles() {
		return sbnGluedProjectiles;
	}

	public PhysicsData setSBNodeGluedProjectiles(SimpleBatchNode sbnGluedProjectiles) {
		this.sbnGluedProjectiles = sbnGluedProjectiles;
		return this; 
	}

	public PhysicsData setProjectileMaxLifeTime(long lProjectileMaxLifeTime) {
		this.lProjectileMaxLifeTime = lProjectileMaxLifeTime;
		return this; 
	}

	public boolean isDisintegrated() {
		return bDisintegrated;
	}

	public PhysicsData setDisintegrated(boolean bDisintegrated) {
		this.bDisintegrated = bDisintegrated;
		return this;
	}

	public Quaternion getWRotBkp() {
		return quaWRotBkp;
	}

	public PhysicsData setWRotBkp(Quaternion quaWRotBkp) {
		this.quaWRotBkp = quaWRotBkp;
		return this;
	}

	public BoundingVolume getBoundingVolume() {
		return bv;
	}

	public PhysicsData setBoundingVolume(BoundingVolume bv) {
		this.bv = bv;
		return this;
	}

	public BoundingBox getBoundingBox() {
		return bb;
	}

	public PhysicsData setBoundingBox(BoundingBox bb) {
		this.bb = bb;
		return this;
	}

	public CollisionShape getCollisionShape() {
		return cs;
	}

	public PhysicsData setCollisionShape(CollisionShape cs) {
		this.cs = cs;
		return this;
	}

	public BoundingSphere getBoundingSphere() {
		return bs;
	}

	public PhysicsData setBoundingSphere(BoundingSphere bs) {
		this.bs = bs;
		return this;
	}

	public boolean isbAllowDisintegration() {
		return bAllowDisintegration;
	}

	public PhysicsData setbAllowDisintegration(boolean bAllowDisintegration) {
		this.bAllowDisintegration = bAllowDisintegration;
		return this;
	}

	public Vector3f getV3fLastSafeSpot() {
		return v3fLastSafeSpot;
	}

	public PhysicsData setV3fLastSafeSpot(Vector3f v3fLastSafeSpot) {
		this.v3fLastSafeSpot = v3fLastSafeSpot;
		return this;
	}

	public Quaternion getQuaLastSafeRot() {
		return quaLastSafeRot;
	}

	public PhysicsData setQuaLastSafeRot(Quaternion quaLastSafeRot) {
		this.quaLastSafeRot = quaLastSafeRot;
		return this;
	}

	public boolean isbTerrain() {
		return bTerrain;
	}

	public PhysicsData setbTerrain(boolean bTerrain) {
		this.bTerrain = bTerrain;
		return this;
	}

	public long getlRestingAtTickCount() {
		return lRestingAtTickCount;
	}

	public PhysicsData setlRestingAtTickCount(long lRestingAtTickCount) {
		this.lRestingAtTickCount = lRestingAtTickCount;
		return this;
	}

	public boolean isbResting() {
		return bResting;
	}

	public PhysicsData setbResting(boolean bResting) {
		this.bResting = bResting;
		return this;
	}

	public ImpTorForce getImp() {
		return imp;
	}

	public PhysicsData setImp(ImpTorForce imp) {
		this.imp = imp;
		return this;
	}

	public long getLastPhysUpdateNano() {
		return lLastPhysUpdateNano;
	}

	public PhysicsData setlLastPhysUpdateNano(long lLastPhysUpdateNano) {
		this.lLastPhysUpdateNano = lLastPhysUpdateNano;
		return this;
	}

	public long getlLastRenderUpdateNano() {
		return lLastRenderUpdateNano;
	}

	public PhysicsData setlLastRenderUpdateNano(long lLastRenderUpdateNano) {
		this.lLastRenderUpdateNano = lLastRenderUpdateNano;
		return this;
	}

	public long getlMaterializedSTime() {
		return lMaterializedSimulTimeNano;
	}

	public PhysicsData setlMaterializedSTime(long lMaterializedSTime) {
		this.lMaterializedSimulTimeNano = lMaterializedSTime;
		return this;
	}

	public Vector3f getV3fWorldGlueSpot() {
		return v3fWorldGlueSpot;
	}

	public PhysicsData setV3fWorldGlueSpot(Vector3f v3fWorldGlueSpot) {
		this.v3fWorldGlueSpot = v3fWorldGlueSpot;
		return this;
	}

	public boolean containsPhysicsDataSkipCollisionGroup(PhysicsData pd) {
		return apdPhysicsDataSkipCollisionGroup.contains(pd);
	}

	public void addPhysicsDataSkipCollisionGroup(PhysicsData pd) {
		this.apdPhysicsDataSkipCollisionGroup.add(pd);
	}

	public boolean isbGlueApplied() {
		return bGlueApplied;
	}

	public PhysicsData setbGlueApplied(boolean bGlueApplied) {
		this.bGlueApplied = bGlueApplied;
		return this;
	}

	public boolean isbProjectile() {
		return bProjectile;
	}

	public PhysicsData setProjectile(boolean bProjectile) {
		this.bProjectile = bProjectile;
		if(bProjectile) {
			PhysicsI.i().hmProjectiles.put(prb, this);
		}else {
			PhysicsI.i().hmProjectiles.remove(prb);
		}
		return this;
	}

	public Vector3f getV3fEventCollOtherLocalPos() {
		return v3fEventCollOtherLocalPos;
	}

	public PhysicsData setV3fEventCollOtherLocalPos(Vector3f v3fEventCollOtherLocalPos) {
		this.v3fEventCollOtherLocalPos = v3fEventCollOtherLocalPos;
		return this;
	}

	public Vector3f getV3fGlueWherePhysLocalPos() {
		return v3fGlueWherePhysLocalPos;
	}

	public PhysicsData setV3fGlueWherePhysLocalPos(Vector3f v3fGlueWherePhysLocalPos) {
		this.v3fGlueWherePhysLocalPos = v3fGlueWherePhysLocalPos;
		return this;
	}

	public Quaternion getQuaGlueWherePhysWRotAtImpact() {
		return quaGlueWherePhysWRotAtImpact;
	}

	public PhysicsData setQuaGlueWherePhysWRotAtImpact(
		Quaternion quaGlueWherePhysWRotAtImpact) {
		this.quaGlueWherePhysWRotAtImpact = quaGlueWherePhysWRotAtImpact;
		return this;
	}

	public Vector3f getV3fPosAtPreviousTick() {
		return v3fPosAtPreviousTick;
	}

	public PhysicsData setV3fPosAtPreviousTick(Vector3f v3fPosAtPreviousTick) {
		this.v3fPosAtPreviousTick = v3fPosAtPreviousTick;
		return this;
	}

	public MatterStatus getMts() {
		return mts;
	}

//		public PhysicsData setMatterStatus(MatterStatus mts) {
//			this.mts = mts;
//			return this;
//		}

	public Geometry getGeomOriginalInitialLink() {
		return geomOriginalInitialLink;
	}

	public PhysicsData setGeomOriginalInitialLink(Geometry geomOriginalInitialLink) {
		this.geomOriginalInitialLink = geomOriginalInitialLink;
		return this;
	}

	public Node getNodexLink() {
		return nodexLink;
	}

	public PhysicsData setNodexLink(Node nodexLink) {
		this.nodexLink = nodexLink;
		return this;
	}

	public int getiForceAwakePhysTickCount() {
		return iForceAwakePhysTickCount;
	}

	public PhysicsData setiForceAwakePhysTickCount(int iForceAwakePhysTickCount) {
		this.iForceAwakePhysTickCount = iForceAwakePhysTickCount;
		return this;
	}

	public int getiForceStaticPhysTickCount() {
		return iWaitPhysTicksB4Glueing;
	}

	public PhysicsData setiForceStaticPhysTickCount(int iForceStaticPhysTickCount) {
		this.iWaitPhysTicksB4Glueing = iForceStaticPhysTickCount;
		return this;
	}

//		public Vector3f getV3fGravityBkp() {
//			return v3fGravityBkp;
//		}

	public LeviCharacter getGrabber() {
		return lcGrabber;
	}

	public boolean isLevitating() {
		if(bSuspendLevitation)return false;
		return v3fLevitationDisplacement!=null;
	}
	public Float getLevitationHeight() {
		return v3fLevitationDisplacement.y;
	}
	
	/**
	 * 
	 * @param pdFollow can be null, otherwise will be a reference for the height
	 * @param fLevitationHeight
	 * @return
	 */
	public PhysicsData setLevitation(PhysicsData pdLevitationFollow,Float fLevitationHeight) {
		setLevitation(pdLevitationFollow, new Vector3f(0,fLevitationHeight,0));
		return this;
	}
	
	/**
	 * 
	 * @param pdLevitationFollow
	 * @param v3fLeviDispl it will be relative to where the followed is looking at, including it's up vector
	 * @return
	 */
	public PhysicsData setLevitation(PhysicsData pdLevitationFollow,Vector3f v3fLeviDispl) {
		this.pdLevitationFollow=pdLevitationFollow;
		this.v3fLevitationDisplacement = v3fLeviDispl!=null ? v3fLeviDispl.clone() : null;
		if(this.v3fLevitationDisplacement==null)setNewGravityAtMainThread(null);
		return this; 
	}

	public void setCcdMotionThresholdBkp(float fCCdMotionThreshold) {
		this.fCCdMotionThreshold=fCCdMotionThreshold;
	}
	public float getCCdMotionThresholdBkp() {
		return fCCdMotionThreshold;
	}

	public void suspendLevitationIfItIs() {
		bSuspendLevitation=true;
	}

	public void resumeLevitationIfItWas() {
		bSuspendLevitation=false;
	}

	public boolean isReadyToGlue() {
		return bReadyToGlue;
	}

	public Quaternion getPhysicsRotationCopy() {
		return prb.getPhysicsRotation(); // is a copy already
	}

	public PhysicsData getLeviFollow() {
		return pdLevitationFollow;
	}

	public boolean isGrabbedBy(PhysicsData pdLevi) {
		return lcGrabber!=null && lcGrabber.pdTorso==pdLevi;
	}

	public float getMass() {
		return prb.getMass();
	}

	public boolean checkGluedAt(RayCastResultX resx) {
		// TODO use info like target surface hardness, projectile piercability, projectile shape/pointyness, impact energy/velocity etc
		Vector3f v3fHitDir = prb.getPhysicsRotation().getRotationColumn(2);
		fLastHitAngleAtGluableDeg = resx.getNormal().angleBetween(v3fHitDir.negate()) * FastMath.RAD_TO_DEG;
		fLastHitAngleAtGluableDeg = 90f-fLastHitAngleAtGluableDeg;
		boolean bDeflected = fLastHitAngleAtGluableDeg < PhysicsI.i().getDefaultDeflectionAngle();
		
		if(!bDeflected) {
			this.resxGlueTarget=resx;
			
			//easifiers
			pdGlueWhere=(resx.pd);
			
			v3fWorldGlueSpot=resx.v3fWrldHit;
			
			v3fGlueWherePhysLocalPos=resx.v3fWrldHit.subtract(resx.pd.prb.getPhysicsLocation());
			quaGlueWherePhysWRotAtImpact=resx.pd.prb.getPhysicsRotation();
		}		
		
		return !bDeflected;
	}
	
	@Deprecated
	public boolean isHasGlueTargetDeflected() {
		return bGlueTargetDeflected;
	}

}

