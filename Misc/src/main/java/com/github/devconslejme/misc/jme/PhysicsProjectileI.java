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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.MatterI.EMatterStatus;
import com.github.devconslejme.misc.MatterI.MatterStatus;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ActivatorI.ActivetableListenerAbs;
import com.github.devconslejme.misc.jme.ColorI.EColor;
import com.github.devconslejme.misc.jme.DecalI.EDecal;
import com.github.devconslejme.misc.jme.GeometryI.GeometryX;
import com.github.devconslejme.misc.jme.ParticlesI.EParticle;
import com.github.devconslejme.misc.jme.PhysicsI.ImpTorForce;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bounding.BoundingVolume;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.SimpleBatchNode;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Sphere;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PhysicsProjectileI {
	public static PhysicsProjectileI i(){return GlobalManagerI.i().get(PhysicsProjectileI.class);}
	
	private SimpleBatchNode	sbnProjectilesAtWorld;
	private float	fDefaultProjectileMaxLife=2;
	private int iProjectileMaxLifeTimeMultiplier=100;
//	private PhysicsThrowProjectiles ppCamDevDbgTst;
	private PhysicsThrowProjectiles ppFromCamCurrent;
	private GeometryX geomProjectileFactory;
	private float fDefaultGravityDivTrick=1f;
	private float fDefaultDesiredSpeed=10f;
	private int iDefaultProjectilesPerSecond=1;
	private boolean bGlowingProjectile;
	private boolean bAllowExplosionCascade;
	private boolean bAllProjectilesAreInstableFun;
	
//	public static class SimpleBatchNode
	
	public static class PhysicsThrowProjectiles{
		private int	iProjectilesPerSecond; 
		private GeometryX	geomProjectileFactory;
//		private Matter mt;
		private float fDesiredSpeed;
//		private float fRadius;
//		private float	fPhysBoundsScaleDiv;
		/**
		 * this trick simulates/helps requiring less speed for a good flying/drop curve w/o stressing the phys engine too much
		 */
		private float	fGravityDivTrick;
//		protected boolean	bFiring;
		private MatterStatus mts;
		private float fRealVelocityMetersPerSecond;
		
		@Override
		public PhysicsThrowProjectiles clone(){
			return new PhysicsThrowProjectiles(mts,fRealVelocityMetersPerSecond);
//			return new PhysicsThrowProjectiles(geomProjectileFactory, fDesiredSpeed, fGravityDivTrick, mt);
		}
		
		public PhysicsThrowProjectiles(MatterStatus mts, float fRealVelocityMetersPerSecond) {
			this.fDesiredSpeed = PhysicsProjectileI.i().getDefaultDesiredSpeed(); //for the physics engine
			this.fRealVelocityMetersPerSecond = fRealVelocityMetersPerSecond;
			iProjectilesPerSecond = PhysicsProjectileI.i().getDefaultProjectilesPerSecond(); 
			this.mts=mts;
			this.fGravityDivTrick=PhysicsProjectileI.i().getDefaultGravityDivTrick(); //TODO if the velocity becomes too low, like a shot straight up that begins to fall, or after hitting something, it's gravity should be restored to normal gravity to not look weird like anything else falling faster than a bullet...
			this.geomProjectileFactory=PhysicsProjectileI.i().getDefaultProjectileFactory();
//			this.geomProjectileFactory.setMeshWhenStatic(new BoundingBox())
//			if(bForceBox || Box.class.isInstance(mesh)) {
//				mesh.setBound(new BoundingBox());
//			}else {
//				mesh.setBound(new BoundingSphere());
//			}

		}

		public int getProjectilesPerSecond() {
			return iProjectilesPerSecond;
		}

		public PhysicsThrowProjectiles setProjectilesPerSecond(int iTestProjectilesPerSecond) {
			this.iProjectilesPerSecond = iTestProjectilesPerSecond;
			return this; 
		}
	}
	
	public void configure(){
		sbnProjectilesAtWorld = new SimpleBatchNode("StaticTerrainWorldBatchNode");
		AppI.i().getRootNode().attachChild(sbnProjectilesAtWorld);
		
    KeyBindCommandManagerI.i().putBindCommandsLater("Space",new CallBoundKeyCmd(){
  		@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
  			PhysicsI.i().applyImpulseHitTargetFromCam(null);
  			return true;
  		}}.setName("CamPushDynamicForce").holdKeyPressedForContinuousCmd().setDelaySeconds(1f/3f)
		);
	}
	
	public GeometryX getDefaultProjectileFactory() {
		if(this.geomProjectileFactory==null) {
			float fRadius=0.1f;
//			float fPhysBoundsScaleDiv=4f;
			
			float fScaleXY=0.25f;
			geomProjectileFactory = GeometryI.i().create(new Sphere(3,4,fRadius), ColorRGBA.Cyan, false, new GeometryX("projectile"));
			geomProjectileFactory.scale(fScaleXY,fScaleXY,1f); //this wont affect the bounding sphere
			geomProjectileFactory.setModelBound(new BoundingSphere(fRadius*fScaleXY,new Vector3f())); //this will become a tiny collider
			if(isGlowingProjectile()) {
				geomProjectileFactory.getMaterial().setColor(EColor.GlowColor.s(), ColorRGBA.Blue.mult(10)); //requires the bloom post processor with glow objects mode
			}
			Mesh meshStatic = geomProjectileFactory.getMesh().clone();
			meshStatic.setBound(new BoundingBox());
			meshStatic.updateBound();
			geomProjectileFactory.setMeshWhenStatic(meshStatic);
		}
		
		return geomProjectileFactory;
	}
	
	public static class Gun{
		private EMatterStatus eprojectile;
		private float fRealVelocityMetersPerSecond;
		private float fEffectiveFireRange;
		private EMatterStatus egun;
		private float fLengthMeters;
		
		public Gun(EMatterStatus egun, EMatterStatus eprojectile, float fRealVelocityMetersPerSecond, float fEffectiveFireRange, float fLengthMeters) {
			super();
			this.eprojectile = eprojectile;
			this.fRealVelocityMetersPerSecond = fRealVelocityMetersPerSecond;
			this.egun = egun;
			this.fEffectiveFireRange=fEffectiveFireRange;
			this.fLengthMeters=fLengthMeters;
		}
		
	}
	
	private static HashMap<String,Gun> hmGun = new HashMap<>();
	public static enum EGun{
		AK47(new Gun(EMatterStatus.GunAK47, EMatterStatus.Bullet762x39mm, 710f, 350f, 0.875f)),
		Glock17(new Gun(EMatterStatus.GunGlock17, EMatterStatus.Bullet9mm, 375f, 50f, 0.186f)),
		;
//		EGun(EMatterStatus egun, EMatterStatus eproj, float fRealVelocityMetersPerSecond, float fEffectiveFireRange){
		EGun(Gun gun){
//			PhysicsProjectileI.i().putGun(this.toString(), new Gun(egun, eproj, fRealVelocityMetersPerSecond, fEffectiveFireRange));
			PhysicsProjectileI.i().putGun(this.toString(), gun);
		}
		public Gun get() {
//			return hmGun.get(this.toString());
			return PhysicsProjectileI.i().getGun(this.toString());
		}
	}
	
	public void putGun(String strId, Gun gun) {
		hmGun.put(strId, gun);
	}
	public Gun getGun(String strId) {
		return hmGun.get(strId);
	}
	
//	public PhysicsGun createGun(EGun egun){
//		Gun gun = hmGun.get(egun.toString());
//		return createGun(gun);
//	}
	public PhysicsGun createGun(Gun gun){
		return createGun(
			new PhysicsThrowProjectiles(gun.eprojectile.get(), gun.fRealVelocityMetersPerSecond),
			gun.egun.get(),
			gun.fLengthMeters);
	}
	//	public PhysicsGun createGun(PhysicsThrowProjectiles pp,float fOverallGunDensity){
	protected PhysicsGun createGun(PhysicsThrowProjectiles pp,MatterStatus mts, float fLengthMeters){
		PhysicsGun pg = new PhysicsGun();
		pg.pp=pp;
		
		GeometryX geom = GeometryI.i().create(MeshI.i().cylinder(fLengthMeters,0.15f), ColorRGBA.Yellow, false, new GeometryX("PhysicsGun"));
		geom.scale(0.25f,0.5f,1f);
		geom.setName("PhysicsGun");
		AppI.i().getRootNode().attachChild(geom);
		pg.pd=PhysicsI.i().imbueFromWBounds(geom,	mts, new Node());
//		pg.pd=PhysicsI.i().imbueFromWBounds(geom,	new Matter("Density="+fOverallGunDensity, fOverallGunDensity),	true);
		
		CallableXAnon cx = new CallableXAnon() {
			@Override
			public Boolean call() {
				throwProjectileFrom(pg);
				return true;
			}
		}.setDelaySeconds(1f/pg.pp.iProjectilesPerSecond).enableLoopMode();
		
		ActivatorI.i().applyActivetableListener(geom, new ActivetableListenerAbs() {
			@Override public boolean activateEvent	(Spatial sptSource) {
				QueueI.i().enqueue(cx);
				return true;
			}
			@Override public boolean deactivateEvent(Spatial sptSource) {
				QueueI.i().removeLoopFromQueue(cx);
				return true;
			}
//				throwProjectile(pg);
//				return true;
//			}
		});
		
		return pg;
	}
	
	public static class PhysicsGun {
		PhysicsData pd;
		PhysicsThrowProjectiles pp;
	}
	
	protected void applyRecoil(PhysicsGun gun) {
		// gun recoil
//		float fMassRatio = pdPjtl.getPRB().getMass()/(gun.pd.getPRB().getMass());
		float fImpulseRecoil = 0f;
		float fDisplacementUpwards = 1f; //TODO this should be a result of the projectile powder/mass force VS gun mass, and be configurable outside here to make it sure it is working.
		int iTest=6; // best is 6 til now
		switch(iTest){
//			case 0:
//				fImpulseRecoil = impPjtl.getImpulseAtSelfDir()*fMassRatio;
//				fDisplacementUpwards = 1000f; //TODO this should be a result of the projectile powder/mass force VS gun mass, and be configurable outside here to make it sure it is working.
//				break;
//			case 2:
//				/**
//				 * Kinectic Energy = m/2 * v^2
//				 * how much would be the velocity at the gun? thats the mass ratio used for.
//				 * TODO confirm if this calc will be enough to be automatic for all guns
//				 */
//				fImpulseRecoil = FastMath.pow(gun.pp.fRealVelocityMetersPerSecond*fMassRatio,2f)*(gun.pd.getPRB().getMass()/2f);
//				fDisplacementUpwards = 0.5f;
//				break;
			case 3:
				/**
				 * the good resulting values are similar to the gun mass!
				 * so well, lets just use it!
				 */
				fImpulseRecoil = gun.pd.getMass();
				fDisplacementUpwards = 0.5f;
				break;
			case 4:
				/**
				 * the good resulting values are similar to the gun mass!
				 * so well, lets just use it!
				 * This effect looks excellent!!!
				 */
				fImpulseRecoil = gun.pd.getMass()/2f;
				fDisplacementUpwards = 0.25f;
				break;
			case 5:
				/**
				 * the good resulting values are similar to the gun mass!
				 * so well, lets just use it!
				 */
				fImpulseRecoil = gun.pd.getMass()/4f;
				fDisplacementUpwards = 0.25f;
				break;
			case 6:
				/**
				 * the good resulting values are similar to the gun mass!
				 * so well, lets just use it!
				 * This effect looks excellent!!!
				 */
				fImpulseRecoil = gun.pd.getMass()/2f;
				gun.pd.setFrictionAtMainThread(1f); //TODO friction may require to be adjusted depending on the terrain?
				fDisplacementUpwards = 0.5f;
				break;
		}
		
		ImpTorForce impRecoil = new ImpTorForce()
			.setImpulseAtSelfDir(-fImpulseRecoil,fDisplacementUpwards)
			;
		PhysicsI.i().applyImpulseLater(gun.pd, impRecoil);
	}
	
	public PhysicsData throwProjectileFrom(PhysicsGun pgun){
		PhysicsData pdPjtl = prepareProjectile(pgun.pp);
		pdPjtl.addPhysicsDataSkipCollisionGroup(pgun.pd);
		
		pdPjtl.setPhysicsLocationAtMainThread(pgun.pd.getPhysicsLocationCopy());
//		pdPjtl.applyNewPhysLocationAtMainThread();
		pdPjtl.setPhysicsRotationAtMainThread(pgun.pd.getPhysicsRotationCopy());
//		pdPjtl.applyNewPhysRotationAtMainThread();
//		pdPjtl.getPRB().setPhysicsLocation(pgun.pd.getPRB().getPhysicsLocation());
//		pdPjtl.getPRB().setPhysicsRotation(pgun.pd.getPRB().getPhysicsRotation());
		
		ImpTorForce impPjtl = PhysicsI.i().throwAtSelfDirImpulse(pdPjtl, pgun.pp.fDesiredSpeed);
		
		applyRecoil(pgun);
		
		return pdPjtl;
	}
	public PhysicsData throwProjectileFromCamera(PhysicsThrowProjectiles pp){
		PhysicsData pd = prepareProjectile(pp);
		PhysicsI.i().throwFromCam(pd,pp.fDesiredSpeed);
		return pd;
	}
	public PhysicsData prepareProjectile(PhysicsThrowProjectiles pp){
		GeometryX geomClone = pp.geomProjectileFactory.clone();
		geomClone.setName("Projectile");
		sbnProjectilesAtWorld.attachChild(geomClone); //AppI.i().getRootNode().attachChild(geomClone);
		sbnProjectilesAtWorld.batch();
		
		assert geomClone.isWorldBoundingSphere() : "the fastest collider calculus is for spheres, so projectiles must use it";
		
		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geomClone, pp.mts, null);
//		geomClone.scale(pp.fPhysBoundsScaleDiv); //to restore the good looking size
		
		pd.setAllowDisintegration(true);
//		PhysicsI.i().hmDisintegratables.put(pd.getPRB(), pd);
		
		pd.setProjectile(true);
//		PhysicsI.i().hmProjectiles.put(pd.getPRB(), pd);
		
		if(isAllProjectilesAreInstableFun())pd.setInstableExplode(true);
		
		PhysicsI.i().assimilatePhysicsData(pd);
		
//		pd.getPRB().setGravity(PhysicsI.i().getGravityCopy().divide(pp.fGravityDivTrick));
		pd.setNewGravityAtMainThread(PhysicsI.i().getGravityCopy().divide(pp.fGravityDivTrick));
		
//		disableCcdToLetCollisionGroupsWork(pd);
		
		return pd;
	}
	
//	protected void reparentProjectileLater(SimpleBatchNode nodeNewParent, Geometry sptProjectile){
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				reparentProjectile(nodeNewParent, sptProjectile);
//				return true;
//			}
//		});
//	}
	
	private void destroyProjectile(PhysicsData pdWhat) {
		reparentProjectile(null, pdWhat);
	}
	
	protected void reparentProjectile(SimpleBatchNode nodeNewParent, PhysicsData pdWhat){
		GeometryX geomWhat = pdWhat.getInitialOriginalGeometry();
		SimpleBatchNode previousParent = (SimpleBatchNode)geomWhat.getParent();
		
		if(nodeNewParent!=null){
			if(nodeNewParent==sbnProjectilesAtWorld) {
//				Vector3f v3fWPos=previousParent.localToWorld(geomWhat.getLocalTranslation(),null);
//				geomWhat.setLocalTranslation(v3fWPos);
				geomWhat.setLocalTranslation(geomWhat.getWorldTranslation());
			}
			nodeNewParent.attachChild(geomWhat);
			nodeNewParent.batch();
		}else {
			geomWhat.removeFromParent();
			PhysicsI.i().removeFromPhysicsSpace(geomWhat);
		}
		
		if(previousParent!=null)previousParent.batch();
	}
//	protected void reparentProjectile(Node nodeNewParent, Geometry sptProjectile){
//		boolean b = sptProjectile.getParent() instanceof SimpleBatchNode;
//		
//		if(nodeNewParent!=null){
//			nodeNewParent.attachChild(sptProjectile);
//			if(nodeNewParent instanceof SimpleBatchNode){
//				((SimpleBatchNode)nodeNewParent).batch();
//			}
//		}else{
//			sptProjectile.removeFromParent();
//		}
//		
//		if(b)sbnProjectilesAtWorld.batch();
//		
//	}

	public void applyGluedMode(PhysicsData pdWhat){
		assert pdWhat.isProjectile();
		
		PhysicsData pdGlueWhere = pdWhat.getGlueWhere();
		
		Geometry geomWhat = pdWhat.getInitialOriginalGeometry();
		
		if(pdGlueWhere.isTerrain()) { //terrain ones are insta static with mass 0 np
			PhysicsI.i().applyStaticColliderFromGeometryBounds(pdWhat);
		}else {
			PhysicsI.i().removeFromPhysicsSpace(geomWhat); //this prevents further updates from physics space
		}
		
		if(pdGlueWhere.isEnclosed() && !pdGlueWhere.isTerrain()){ //will glue at dynamic parent surface
			if(pdGlueWhere.getSBatchNodeGluedProjectilesOnMe()==null){
				pdGlueWhere.setSBNodeGluedProjectiles(new SimpleBatchNode(SimpleBatchNode.class.getName()+":LocalGluedProjectiles"));
				((Node)pdGlueWhere.getSpatialWithPhysics()).attachChild(pdGlueWhere.getSBatchNodeGluedProjectilesOnMe());
			}
			
			Quaternion quaWhatWRotBkp = geomWhat.getWorldRotation().clone();
			reparentProjectile(pdGlueWhere.getSBatchNodeGluedProjectilesOnMe(), pdWhat);
			
			PhysicsI.i().cancelDisintegration(pdWhat);
			
			QueueI.i().enqueue(new CallableXAnon() {
				long lLastGlueSetPosNano=-1; //1st time will always try
				@Override
				public Boolean call() {
					boolean bTryAgain=false;
					if(pdWhat.getLastPhysUpdateNano()>lLastGlueSetPosNano){
						bTryAgain=true;
					}
					
					if(bTryAgain){
						/**
						 *  restores the location, based on the target rotation at the physics impact moment
						 */
						Node node = new Node();
						node.setLocalRotation(pdWhat.getQuaGlueWherePhysWRotAtImpact());
						geomWhat.setLocalTranslation(
							node.worldToLocal(pdWhat.getV3fGlueWherePhysLocalPos(),null));
						
						/**
						 * applies the world rotation at the impact moment
						 */
						geomWhat.setLocalRotation(
							geomWhat.getParent().getWorldRotation().inverse().mult(
								quaWhatWRotBkp));
						
						lLastGlueSetPosNano=System.nanoTime();
						return false; // to make it sure the physics have not changed it after that 
					}
					
					checkBuggyMissPlacedFew(pdWhat);
					
					checkProjectilesClashInstabilityExplode(pdWhat);
					
					return true;
				}
			});
		}
	
		pdWhat.setGlueApplied(true);
	}
	
	public void checkProjectilesClashInstabilityExplode(PhysicsData pdWhat) {
		if(!pdWhat.isProjectile())return;
		
		GeometryX geomWhat = pdWhat.getInitialOriginalGeometry();
		SimpleBatchNode sbParent = (SimpleBatchNode)geomWhat.getParent();
		if(sbParent==null)return;
//		sbParent.batch();
		
		/**
		 * IMPORTANT!!!
		 * inside the batch node, the geometries are not updated as that batch node moves on the world,
		 * this means their world bound are of the last glue; the last batch() update doesnt change that!
		 * so the world bound stored is of before being added to the batch node!
		 */
		BoundingVolume bvWhatFixed = pdWhat.getInitialOriginalGeometry().getWorldBound().clone();
		bvWhatFixed.setCenter(geomWhat.getLocalTranslation());
		
		float fPowerfulRadius=0.1f;
//		boolean bExploded=false;
		ArrayList<PhysicsData> apd = new ArrayList<PhysicsData>(); 
		boolean bDoExplode=false;
		for(Spatial sptOther:sbParent.getChildren()) {
			if(!GeometryX.class.isInstance(sptOther))continue;
			GeometryX geomOther = (GeometryX)sptOther;
			if(geomOther==geomWhat)continue; //ignore self
			PhysicsData pdOther = PhysicsI.i().getPhysicsDataFrom(sptOther);
			if(pdOther==null)continue; // the batched mesh/geometry is also a child of the batch node, this skips it too
			if(!pdOther.isProjectile())continue;
			
			BoundingVolume bvOtherFixed = geomOther.getWorldBound().clone();
			bvOtherFixed.setCenter(geomOther.getLocalTranslation());
			
			if(bvWhatFixed.getCenter().distance(bvOtherFixed.getCenter()) < fPowerfulRadius*pdWhat.getPowerfulRadiusMultiplier()) {
				apd.add(pdOther);
			}
			
			if(!bDoExplode) {
				if(pdWhat.isMarkedToExplode()) {
					bDoExplode=true;
				}else
				if(pdWhat.isInstableExplode() && bvOtherFixed.intersects(bvWhatFixed)) { //Instability: this begins the automatic explosion cascade
					if(!apd.contains(pdWhat))apd.add(pdWhat); //so do it later
					apd.add(pdOther);
					bDoExplode=true;
				}
				
				if(bDoExplode) {
					if(pdWhat.isMarkedToExplode())explode(pdWhat,fPowerfulRadius,2f);
//					//TODO terrain ones are not working why?
//					ParticlesI.i().createAtMainThread(EParticle.Smoke.s(), pdWhat.getSpatialWithPhysics(), 0.1f, 100f);
//					if(!pdWhat.isMarkedToExplode())break; //wont break to look for others nearby
				}
			}
		}
		
		if(bDoExplode && isAllowExplosionCascade()) {
			//cascade
			for(PhysicsData pd:apd) {
				pd.markToExplode();
				pd.checkExplodeAtMainThread();
				ParticlesI.i().createAtMainThread(EParticle.Smoke.s(), pd.getSpatialWithPhysics(), 0.1f, 100f);
			}
		}
	}
	
	public void explode(PhysicsData pdWhat, float fMinRadius, float fMaxRadius) {
		Vector3f v3fPos = null;
		GeometryX geomWhat = pdWhat.getInitialOriginalGeometry();
		if(pdWhat.getGlueWhere().isTerrain()) {
//			v3fPos = pdWhat.getInstaTempWorldGlueSpot();
			v3fPos = geomWhat.getWorldTranslation();
			DecalI.i().createAtMainThread(null,v3fPos,
				geomWhat.getWorldRotation().getRotationColumn(2),
				geomWhat.getWorldRotation().getRotationColumn(1),
				EDecal.Exploded);
			destroyProjectile(pdWhat);
		}else {
			reparentProjectile(sbnProjectilesAtWorld, pdWhat); //to position properly in the world
			v3fPos = geomWhat.getWorldTranslation().clone();
			destroyProjectile(pdWhat);
		}
		ParticlesI.i().createAtMainThread(EParticle.ShockWave.s(), v3fPos, 1f, null);
		PhysicsI.i().pushAllAround(v3fPos, pdWhat.getExplosionForce(), fMinRadius, fMaxRadius);
	}

	protected void checkBuggyMissPlacedFew(PhysicsData pdWhat) {
		BoundingVolume bvWhat = pdWhat.getInitialOriginalGeometry().getWorldBound();
		BoundingVolume bvWhere = pdWhat.getGlueWhere().getInitialOriginalGeometry().getWorldBound();
//		if(!pdWhat.getBoundingVolume().intersects(pdWhat.getGlueWhere().getBoundingVolume())){
		if(!bvWhat.intersects(bvWhere)){
			destroyProjectile(pdWhat);
			MessagesI.i().warnMsg(this, "purging still bugly placed projectile, TODO improve this..", bvWhat, bvWhere, bvWhat.getCenter().distance(bvWhere.getCenter()));
		}
	}

	protected void glueProjectileCheckApply(PhysicsData pd, PhysicsData pdWhere, Vector3f v3fEventCollPos){
		if(
			pd.isProjectile() &&
			pdWhere!=null &&
			!pdWhere.isProjectile() && 
			!pd.isDisintegrated() && 
			!pd.isGlueApplied() && 
			pd.getGlueWhere()==pdWhere &&
			pd.isReadyToGlue() 
//			&& !pd.isHasGlueTargetDeflected()
		){
			if(v3fEventCollPos!=null)pd.setV3fEventCollOtherLocalPos(v3fEventCollPos.clone());
			applyGluedMode(pd);
		}
	}
	
	public Object debugTest(Object... aobj){//keep even if empty
//		EDebug.TestDynamicPhysicsWithoutSpatialAndData.set(true);
//		sbnBatchTestProjectiles.removeFromParent();
//		for (Spatial spt : sbnBatchTestProjectiles.getChildren()) {
//			spt.removeControl(RigidBodyControl.class);
//		}
//		sbnBatchTestProjectiles.detachAllChildren();
//		sbnBatchTestProjectiles.batch();
		return sbnProjectilesAtWorld.getChildren().size();
	}
	public float getDefaultProjectileMaxLife() {
		return fDefaultProjectileMaxLife;
	}

	public PhysicsProjectileI setDefaultProjectileMaxLife(float fDefaultProjectileMaxLife) {
		this.fDefaultProjectileMaxLife = fDefaultProjectileMaxLife;
		return this; 
	}


	public int getGluedProjectileMaxLifeTimeMultiplier() {
		return iProjectileMaxLifeTimeMultiplier;
	}

	/**
	 * when glued/stuck on terrain
	 * @param iProjectileMaxLifeTimeMultiplier
	 * @return
	 */
	public PhysicsProjectileI setProjectileMaxLifeTimeMultiplier(		int iProjectileMaxLifeTimeMultiplier) {
		this.iProjectileMaxLifeTimeMultiplier = iProjectileMaxLifeTimeMultiplier;
		return this; 
	}

	public PhysicsThrowProjectiles getProjectileFromCamCurrent() {
		return ppFromCamCurrent;
	}

	public PhysicsProjectileI setProjectileFromCamCurrent(PhysicsThrowProjectiles ppFromCamCurrent) {
		this.ppFromCamCurrent = ppFromCamCurrent;
		return this; 
	}

	public float getDefaultGravityDivTrick() {
		return fDefaultGravityDivTrick;
	}

	public PhysicsProjectileI setDefaultGravityDivTrick(float fDefaultGravityDivTrick) {
		this.fDefaultGravityDivTrick = fDefaultGravityDivTrick;
		return this; 
	}

	public float getDefaultDesiredSpeed() {
		return fDefaultDesiredSpeed;
	}

	public PhysicsProjectileI setDefaultDesiredSpeed(float fDefaultDesiredSpeed) {
		this.fDefaultDesiredSpeed = fDefaultDesiredSpeed;
		return this; 
	}

	public int getDefaultProjectilesPerSecond() {
		return iDefaultProjectilesPerSecond;
	}

	public PhysicsProjectileI setDefaultProjectilesPerSecond(int iDefaultProjectilesPerSecond) {
		this.iDefaultProjectilesPerSecond = iDefaultProjectilesPerSecond;
		return this; 
	}

	public boolean isGlowingProjectile() {
		return bGlowingProjectile;
	}

	public PhysicsProjectileI setGlowingProjectile(boolean bGlowingProjectile) {
		this.bGlowingProjectile = bGlowingProjectile;
		return this; 
	}

	public boolean isAllowExplosionCascade() {
		return bAllowExplosionCascade;
	}

	public PhysicsProjectileI setAllowExplosionCascade(boolean bAllowExplosionCascade) {
		this.bAllowExplosionCascade = bAllowExplosionCascade;
		return this; 
	}

	public boolean isAllProjectilesAreInstableFun() {
		return bAllProjectilesAreInstableFun;
	}

	public PhysicsProjectileI setAllProjectilesAreInstableFun(
		boolean bAllProjectilesAreInstableFun) {
		this.bAllProjectilesAreInstableFun = bAllProjectilesAreInstableFun;
		return this; 
	}
	
}
