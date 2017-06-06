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
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ColorI.EColor;
import com.github.devconslejme.misc.jme.PhysicsI.PhysicsData;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.SimpleBatchNode;
import com.jme3.scene.shape.Sphere;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PhysicsProjectileI {
	public static PhysicsProjectileI i(){return GlobalManagerI.i().get(PhysicsProjectileI.class);}
	
//	public static class GeometryTestProjectile extends Geometry{
//		@Override	public GeometryTestProjectile clone() {return (GeometryTestProjectile)super.clone();}
//	}
	
	private int	iProjectilesPerSecond;
	private long	lTestProjectilesMaxLifeTime;// = TimeConvertI.i().secondsToNano(5);
	private SimpleBatchNode	sbnProjectilesAtWorld;
	private Geometry	geomTestProjectileFactory;
	private Matter mtProjectile = EMatter.Generic100KgPerM3.get();//EMatter.Lead.get();
	
	public void configure(){
		setTestProjectilesPerSecond(10); //10 seems the default of many guns
		
    KeyBindCommandManagerI.i().putBindCommandsLater("Space",new CallBoundKeyCmd(){
  		@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
//  			PhysicsProjectileI.i().throwProjectileFromCamera(250,0.1f,6f);
  			PhysicsProjectileI.i().throwProjectileFromCamera(250,0.1f,mtProjectile);
  			setDelaySeconds(1f/iProjectilesPerSecond); //dynamicly changeable
  			return true;
  		}}.setName("ShootProjectile").holdKeyPressedForContinuousCmd().setDelaySeconds(1f/iProjectilesPerSecond)
		);
	}
	

	public int getTestProjectilesPerSecond() {
		return iProjectilesPerSecond;
	}

	public PhysicsProjectileI setTestProjectilesPerSecond(int iTestProjectilesPerSecond) {
		this.iProjectilesPerSecond = iTestProjectilesPerSecond;
		return this; 
	}

	public PhysicsData throwProjectileFromCamera(float fDesiredSpeed, float fRadius, Matter mt){
		if(sbnProjectilesAtWorld==null){
			sbnProjectilesAtWorld = new SimpleBatchNode("BatchNode");
			AppI.i().getRootNode().attachChild(sbnProjectilesAtWorld);
		}
		
//		Vector3f v3fScale = new Vector3f(0.25f,0.25f,1f);
		if(geomTestProjectileFactory==null){
			geomTestProjectileFactory = GeometryI.i().create(new Sphere(3,4,fRadius), ColorRGBA.Cyan);
//			geomTestProjectileFactory = GeometryI.i().create(MeshI.i().sphere(fRadius), ColorRGBA.Cyan, false, new GeometryTestProjectile());
//			geomTestProjectileFactory = GeometryI.i().create(MeshI.i().box(fRadius), ColorRGBA.Cyan, false, new GeometryTestProjectile());
			geomTestProjectileFactory.scale(0.25f,0.25f,1f);
			geomTestProjectileFactory.scale(1f/4f); // to lower the automatic mass
			geomTestProjectileFactory.getMaterial().setColor(EColor.GlowColor.s(), ColorRGBA.Blue.mult(10)); //requires the bloom post processor with glow objects mode
		}
		
		Geometry geomClone = geomTestProjectileFactory.clone();
		geomClone.setName("Projectile");
		sbnProjectilesAtWorld.attachChild(geomClone); //AppI.i().getRootNode().attachChild(geomClone);
		sbnProjectilesAtWorld.batch();
		
//		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geomClone,fDensity,geomTestProjectileFactory.getLocalScale());
		PhysicsData pd = PhysicsI.i().imbueFromWBounds(geomClone,mt,false);
		geomClone.scale(4f); //to restore the good looking size
		//TODO scale
//		ps.remove(pd.rbc);
//		pd.rbc.getCollisionShape().setScale(geomTestProjectileFactory.getLocalScale());
//		ps.add(pd.rbc);
		pd.setAllowDisintegration(true);
		pd.bProjectile=(true);
		pd.getRBC().setGravity(PhysicsI.i().getGravity().mult(0.25f));
		
		PhysicsI.i().throwFromCam(pd,fDesiredSpeed);
		
		return pd;
	}

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
		
		PhysicsData pdGlueWhere = pdWhat.pdGlueWhere;
		
		PhysicsI.i().removeFromPhysicsSpace(pdWhat.getSpatialWithPhysics()); //this prevents further updates from physics space
		
		if(pdGlueWhere.isEnclosed() && !pdGlueWhere.isTerrain()){ //will glue at dynamic parent surface
			if(pdGlueWhere.sbnGluedProjectiles==null){
				pdGlueWhere.sbnGluedProjectiles = new SimpleBatchNode(SimpleBatchNode.class.getName()+":LocalGluedProjectiles");
				((Node)pdGlueWhere.getSpatialWithPhysics()).attachChild(pdGlueWhere.sbnGluedProjectiles);
			}
			
			reparentProjectile(pdGlueWhere.sbnGluedProjectiles, pdWhat.getInitialOriginalGeometry());
			
			PhysicsI.i().cancelDisintegration(pdWhat);
			
			QueueI.i().enqueue(new CallableXAnon() {
				long lLastGlueSetPosNano=-1; //1st time will always try
				@Override
				public Boolean call() {
//					if(pdWhat.getSpatialWithPhysics().getControl(RigidBodyControl.class).getPhysicsSpace()!=null){
//						return false;
//					}
					boolean bTryAgain=false;
					if(pdWhat.lLastPhysUpdateNano>lLastGlueSetPosNano){
						bTryAgain=true;
					}
					
//					if(pdWhat.getSpatialWithPhysics().getLocalTranslation().distance(pdWhat.v3fGlueWherePhysLocalPos)>0){
//						bTryAgain=true;
//					}
					
					if(bTryAgain){
						Node node = new Node();
						node.setLocalRotation(pdWhat.quaGlueWherePhysWRotAtImpact);
						pdWhat.getSpatialWithPhysics().setLocalTranslation(
//							node.localToWorld(pdWhat.v3fGlueWherePhysLocalPos,null));
							node.worldToLocal(pdWhat.v3fGlueWherePhysLocalPos,null));
						
						lLastGlueSetPosNano=System.nanoTime();
						return false; // to make it sure the physics have not changed it after that 
					}
					
					return true;
				}
			});
//		}else{
//			pdWhat.rbc.setPhysicsLocation(pdWhat.v3fWorldGlueSpot);
//		}else{
//			pdWhat.getSpatialWithPhysics().setLocalTranslation(pdWhat.v3fWorldGlueSpot);
		}
	
		pdWhat.bGlueApplied=true;
	}
	
	protected void glueProjectileCheckApply(PhysicsData pd, PhysicsData pdWhere, Vector3f v3fEventCollPos){
		if(
			pd.isProjectile() && 
			!pdWhere.isProjectile() && 
			!pd.bDisintegrated && 
			!pd.bGlueApplied && 
			pd.pdGlueWhere==pdWhere
		){
			pd.v3fEventCollOtherLocalPos=v3fEventCollPos.clone();
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
}
