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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.MatterI.EMatter;
import com.github.devconslejme.misc.MatterI.Matter;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ActivatorI.ActivetableListenerAbs;
import com.github.devconslejme.misc.jme.ColorI.EColor;
import com.github.devconslejme.misc.jme.PhysicsI.Impulse;
import com.github.devconslejme.misc.jme.PhysicsI.PhysicsData;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
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
	private PhysicsThrowProjectiles ppCamDevDbgTst;
	private PhysicsThrowProjectiles ppFromCamCurrent;
	
	public static class PhysicsThrowProjectiles{
		private int	iProjectilesPerSecond = (10); //10 seems the default of many guns
		private Geometry	geomProjectileFactory;
		private Matter mt;
		private float fDesiredSpeed;
		private float fRadius;
		private float	fPhysBoundsScaleDiv;
		private float	fGravityDiv;
//		protected boolean	bFiring;
		
		@Override
		public PhysicsThrowProjectiles clone(){
			return new PhysicsThrowProjectiles(geomProjectileFactory, fDesiredSpeed, fRadius, fPhysBoundsScaleDiv, fGravityDiv, mt);
		}
		
		/**
		 * 
		 * @param geomProjectileFactory
		 * @param fDesiredSpeed
		 * @param fRadius
		 * @param fPhysBoundsScaleDiv (can be null) to initially lower the mass and collision shape size, but will be restored just after to show the geometry properly, it will just have a smaller collider than what it looks
		 * @param fGravityDiv (can be null) this trick simulates/helps requiring less speed for a good flying/drop curve w/o stressing the phys engine
		 * @param mt
		 */
		public PhysicsThrowProjectiles(Geometry geomProjectileFactory, float fDesiredSpeed, float fRadius, Float fPhysBoundsScaleDiv, Float fGravityDiv, Matter mt) {
			this.fDesiredSpeed = fDesiredSpeed;
			this.fRadius = fRadius;
			this.mt = mt;
			this.fGravityDiv=fGravityDiv==null?1f:fGravityDiv;
			this.fPhysBoundsScaleDiv=fPhysBoundsScaleDiv==null?1f:fPhysBoundsScaleDiv;
			
			if(geomProjectileFactory==null){
				geomProjectileFactory = GeometryI.i().create(new Sphere(3,4,fRadius), ColorRGBA.Cyan);
				geomProjectileFactory.scale(0.25f,0.25f,1f);
				geomProjectileFactory.scale(1f/fPhysBoundsScaleDiv);
				geomProjectileFactory.getMaterial().setColor(EColor.GlowColor.s(), ColorRGBA.Blue.mult(10)); //requires the bloom post processor with glow objects mode
			}
			this.geomProjectileFactory=geomProjectileFactory;
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
		sbnProjectilesAtWorld = new SimpleBatchNode("BatchNode");
		AppI.i().getRootNode().attachChild(sbnProjectilesAtWorld);
		
		ppCamDevDbgTst = new PhysicsThrowProjectiles(null,250,0.1f,4f,4f,EMatter.Generic100KgPerM3.get());
		
    KeyBindCommandManagerI.i().putBindCommandsLater("Space",new CallBoundKeyCmd(){
  		@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
  			PhysicsProjectileI.i().throwProjectileFromCamera(ppFromCamCurrent);
  			setDelaySeconds(1f/ppCamDevDbgTst.iProjectilesPerSecond); //so it becomes dynamicly changeable
  			return true;
  		}}.setName("ShootProjectile").holdKeyPressedForContinuousCmd().setDelaySeconds(1f/ppCamDevDbgTst.iProjectilesPerSecond)
		);
	}
	
//	public PhysicsGun createGun(PhysicsThrowProjectiles pp,float fOverallGunDensity){
	public PhysicsGun createGun(PhysicsThrowProjectiles pp,Matter mtGunRelativeOverallMatter){
		PhysicsGun pg = new PhysicsGun();
		pg.pp=pp;
		
		Geometry geom = GeometryI.i().create(MeshI.i().cylinder(1f,0.05f), ColorRGBA.Yellow);
		geom.setName("PhysicsGun");
		AppI.i().getRootNode().attachChild(geom);
		pg.pd=PhysicsI.i().imbueFromWBounds(geom,	mtGunRelativeOverallMatter,	true);
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
	
	public PhysicsData throwProjectileFrom(PhysicsGun gun){
		PhysicsData pdPjtl = prepareProjectile(gun.pp);
		pdPjtl.setPdSpawnedFrom(gun.pd);
		
		pdPjtl.getPRB().setPhysicsLocation(gun.pd.getPRB().getPhysicsLocation());
		pdPjtl.getPRB().setPhysicsRotation(gun.pd.getPRB().getPhysicsRotation());
		
		Impulse impPjtl = PhysicsI.i().throwAtSelfDirImpulse(pdPjtl, gun.pp.fDesiredSpeed);
		
		float fMassRatio = pdPjtl.getPRB().getMass()/gun.pd.getPRB().getMass();
//		float fImpulseRecoil = -1f * impPjtl.getImpulseAtSelfDir().mult(fMassRatio).length();
		float fImpulseRecoil = -1f * impPjtl.getImpulseAtSelfDir()*fMassRatio;
//		float fTorque = 0.01f;
		float fY = 2f;
//		if(gun.pd.f)
//		if(gun.pd.isGrabbed())fTorque*=1000f;
//		if(gun.pd.isGrabbed())fY*=100000f;
		Impulse impRecoil = new Impulse()
//			.setim
			.setImpulseAtSelfDir(fImpulseRecoil,fY)
//			.setTorqueImpulse(new Vector3f(fTorque,0,0))
			;
		PhysicsI.i().applyImpulseLater(gun.pd, impRecoil);
		
		return pdPjtl;
	}
	public PhysicsData throwProjectileFromCamera(PhysicsThrowProjectiles pp){
		PhysicsData pd = prepareProjectile(pp);
		PhysicsI.i().throwFromCam(pd,pp.fDesiredSpeed);
		return pd;
	}
	public PhysicsData prepareProjectile(PhysicsThrowProjectiles pp){
		Geometry geomClone = pp.geomProjectileFactory.clone();
		geomClone.setName("Projectile");
		sbnProjectilesAtWorld.attachChild(geomClone); //AppI.i().getRootNode().attachChild(geomClone);
		sbnProjectilesAtWorld.batch();
		
		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geomClone,pp.mt,false);
		geomClone.scale(pp.fPhysBoundsScaleDiv); //to restore the good looking size
		pd.setAllowDisintegration(true);
		pd.setbProjectile(true);
		pd.getPRB().setGravity(PhysicsI.i().getGravity().divide(pp.fGravityDiv));
		
//		disableCcdToLetCollisionGroupsWork(pd);
		
		return pd;
	}
	
//	/**
//	 * collision groups skippers will not work without this... :(
//	 * @param pd
//	 */
//	@Workaround
//	private void disableCcdToLetCollisionGroupsWork(PhysicsData pd) {
//		pd.getPRB().setCcdMotionThreshold(0);
//	}

	protected void reparentProjectile(Node nodeNewParent, Geometry sptProjectile){
//		assert nodeNewParent!=sbnProjectilesAtWorld;
		
		boolean b = sptProjectile.getParent() instanceof SimpleBatchNode;
		
		if(nodeNewParent!=null){
			nodeNewParent.attachChild(sptProjectile);
			if(nodeNewParent instanceof SimpleBatchNode){
				((SimpleBatchNode)nodeNewParent).batch();
			}
		}else{
			sptProjectile.removeFromParent();
		}
		
		if(b)sbnProjectilesAtWorld.batch();
		
	}

	public void applyGluedMode(PhysicsData pdWhat){
		assert pdWhat.isProjectile();
		
		PhysicsData pdGlueWhere = pdWhat.getPdGlueWhere();
		
		Geometry geomWhat = pdWhat.getInitialOriginalGeometry();
		
		PhysicsI.i().removeFromPhysicsSpace(geomWhat); //this prevents further updates from physics space
		
		if(pdGlueWhere.isEnclosed() && !pdGlueWhere.isTerrain()){ //will glue at dynamic parent surface
			if(pdGlueWhere.getSBNodeGluedProjectiles()==null){
				pdGlueWhere.setSBNodeGluedProjectiles(new SimpleBatchNode(SimpleBatchNode.class.getName()+":LocalGluedProjectiles"));
				((Node)pdGlueWhere.getSpatialWithPhysics()).attachChild(pdGlueWhere.getSBNodeGluedProjectiles());
			}
			
			Quaternion quaWhatWRotBkp = geomWhat.getWorldRotation().clone();
			reparentProjectile(pdGlueWhere.getSBNodeGluedProjectiles(), pdWhat.getInitialOriginalGeometry());
			
			PhysicsI.i().cancelDisintegration(pdWhat);
			
			QueueI.i().enqueue(new CallableXAnon() {
				long lLastGlueSetPosNano=-1; //1st time will always try
				@Override
				public Boolean call() {
//					/**
//					 * this can be inside a loop over the disintegratables
//					 */
//					if(pdWhat.bAllowDisintegration)PhysicsI.i().cancelDisintegration(pdWhat);
					
//					if(geomWhat.getControl(RigidBodyControl.class).getPhysicsSpace()!=null){
//						return false;
//					}
					boolean bTryAgain=false;
					if(pdWhat.getlLastPhysUpdateNano()>lLastGlueSetPosNano){
						bTryAgain=true;
					}
					
//					if(geomWhat.getLocalTranslation().distance(pdWhat.v3fGlueWherePhysLocalPos)>0){
//						bTryAgain=true;
//					}
					
					if(bTryAgain){
//						Quaternion quaBkp = geomWhat.getWorldRotation().clone();
						
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
					
					return true;
				}
			});
//		}else{
//			pdWhat.rbc.setPhysicsLocation(pdWhat.v3fWorldGlueSpot);
//		}else{
//			geomWhat.setLocalTranslation(pdWhat.v3fWorldGlueSpot);
		}
	
		pdWhat.setbGlueApplied(true);
	}
	
	protected void glueProjectileCheckApply(PhysicsData pd, PhysicsData pdWhere, Vector3f v3fEventCollPos){
		if(
			pd.isProjectile() &&
			pdWhere!=null &&
			!pdWhere.isProjectile() && 
			!pd.isDisintegrated() && 
			!pd.isbGlueApplied() && 
			pd.getPdGlueWhere()==pdWhere
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


	public int getProjectileMaxLifeTimeMultiplier() {
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
	
	public PhysicsProjectileI setProjectileFromCamToDevTestDbg() {
		this.ppFromCamCurrent = ppCamDevDbgTst;
		return this; 
	}
	
	public PhysicsThrowProjectiles getProjectileThrowerDevTestDbgCopy(){
		return ppCamDevDbgTst.clone();
	}
}
