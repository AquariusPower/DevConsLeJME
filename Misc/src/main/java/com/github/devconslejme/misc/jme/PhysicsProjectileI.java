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
import com.jme3.math.Quaternion;
import com.jme3.math.Transform;
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

	public void reparentProjectile(Node nodeNewParent, Spatial sptProjectile){
		assert nodeNewParent!=sbnProjectilesAtWorld;
		
		boolean b=sptProjectile.getParent()==sbnProjectilesAtWorld;
		
		if(nodeNewParent!=null){
			nodeNewParent.attachChild(sptProjectile);
		}else{
			sptProjectile.removeFromParent();
		}
		
		if(b)sbnProjectilesAtWorld.batch();
	}

	public void applyGluedMode(PhysicsData pdWhat){
		assert pdWhat.isProjectile();
		
		Node nodeParentest = SpatialHierarchyI.i().getParentestOrSelf(
			pdWhat.pdGlueWhere.getSpatialWithPhysics(), Node.class, true, false);
		
		if(nodeParentest!=null){ //will glue at dynamic parent surface
			reparentProjectile(nodeParentest,pdWhat.getSpatialWithPhysics());
			
			PhysicsI.i().cancelDisintegration(pdWhat);
		}
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				Vector3f v3fChk=null;
				
				if(nodeParentest!=null){ //will glue at dynamic parent surface
					
					boolean bUseRayTestPos=true;
					if(bUseRayTestPos){
						boolean bRayTestLocalPos=true;
						if(bRayTestLocalPos){
							v3fChk = pdWhat.v3fGlueWherePhysLocalPos;
							attachChildRotated(nodeParentest,pdWhat);
//							// rot 
//							Node node = new Node();
//							node.attachChild(pdWhat.sptLink);
//							node.setLocalRotation(pdWhat.quaLocalGlueRot);
//							nodeParentest.attachChild(node);
						}else{ 
							/**
							 * for dynamic parents this is bad as they may have moved after the
							 * pos being set at the phys thread...
							 */
							v3fChk = nodeParentest.worldToLocal(pdWhat.v3fWorldGlueSpot,null);
						}
					}else{ //event local pos
						v3fChk = pdWhat.v3fEventCollOtherLocalPos;
						attachChildRotated(nodeParentest,pdWhat);
					}
					
				}else{
					//simple at world on some static surface
					v3fChk = pdWhat.v3fWorldGlueSpot;
//					pdWhat.sptLink.setLocalTranslation(pdWhat.v3fWorldGlueSpot); 
				}
				
//				if(pdWhat.getSpatialWithPhysics().getLocalTranslation().distance(v3fChk)>0f){
					/**
					 * shall have no physics anymore at this point
					 */
					assert pdWhat.getSpatialWithPhysics().getControl(RigidBodyControl.class).getPhysicsSpace()==null;
					pdWhat.getSpatialWithPhysics().setLocalTranslation(v3fChk);
//					return false; //to re-check
//				}
				
				return true;
			}
		});
	
		PhysicsI.i().removeFromPhysicsSpace(pdWhat.getSpatialWithPhysics()); //this prevents further updates from physics space
		
		pdWhat.bGlueApplied=true;
	}
	
	String strSBNKey=SimpleBatchNode.class.getName()+":LocalGluedProjectiles";
	protected void attachChildRotated(Node nodeParentest, PhysicsData pdWhat){
		if(pdWhat.sbnGluedProjectiles==null){
			pdWhat.sbnGluedProjectiles = new SimpleBatchNode(strSBNKey);
			nodeParentest.attachChild(pdWhat.sbnGluedProjectiles);
		}
		
		Quaternion quaDiff = pdWhat.sbnGluedProjectiles.getWorldRotation().subtract(pdWhat.quaGlueWherePhysWRotAtImpact);
		
		pdWhat.getSpatialWithPhysics().setLocalTranslation(pdWhat.v3fGlueWherePhysLocalPos);
		Node nodeGlueWherePhysRotAtImpact = new Node();
		nodeGlueWherePhysRotAtImpact.setLocalRotation(quaDiff);
		nodeGlueWherePhysRotAtImpact.attachChild(pdWhat.getSpatialWithPhysics());
		Vector3f v3fSimulatedPos=pdWhat.getSpatialWithPhysics().getWorldTranslation();
		Quaternion quaSimulatedRot=pdWhat.getSpatialWithPhysics().getWorldRotation();
		
		pdWhat.sbnGluedProjectiles.attachChild(pdWhat.getSpatialWithPhysics());
		pdWhat.getSpatialWithPhysics().setLocalRotation(quaSimulatedRot);
		pdWhat.getSpatialWithPhysics().setLocalTranslation(v3fSimulatedPos);
		pdWhat.sbnGluedProjectiles.batch();
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
