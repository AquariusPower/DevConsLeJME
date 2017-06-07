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
package com.github.devconslejme.game;

import java.util.ArrayList;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.ICompositeRestrictedAccessControl;
import com.github.devconslejme.misc.Key;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.FlyByCameraX;
import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.MeshI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.PhysicsI;
import com.github.devconslejme.misc.jme.PhysicsI.PhysicsData;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.WorldPickingI;
import com.jme3.bullet.collision.PhysicsCollisionObject;
import com.jme3.bullet.control.BetterCharacterControl;
import com.jme3.collision.CollisionResult;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Box;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class CharacterI {
	public static CharacterI i(){return GlobalManagerI.i().get(CharacterI.class);}
	
	/**
	 * see  {@link BetterCharacterControl}
	 */
	public static class BetterCharacterControlX extends BetterCharacterControl{
		private Spatial	nodeHead;

		/**
		 * see {@link BetterCharacterControl#BetterCharacterControl(float, float, float)}
		 */
		public BetterCharacterControlX(float radius, float height, float mass) {
			super(radius, height, mass);
		}
		
		/**
		 * restriction based on info at
		 * see {@link BetterCharacterControl#BetterCharacterControl(float, float, float)}
		 */
		@Override
		public void setDuckedFactor(float factor) {
//			float fBkp = getDuckedFactor();
			super.setDuckedFactor(factor);
			assert (getFinalRadius()*2f < getFinalHeight());
		}
		
		public Spatial getSpatial() {
			return super.spatial;
		}

		public void setHead(Spatial nodeHead) {
			this.nodeHead = nodeHead;
		}

		@Override
		protected void setPhysicsLocation(Vector3f vec) {
			super.setPhysicsLocation(vec);
		}
	}

	public static class CompositeControl implements ICompositeRestrictedAccessControl{private CompositeControl(){};};
	private ICompositeRestrictedAccessControl	cc=new CompositeControl();
	
	private BetterCharacterControlX	bccLast;
	private Key	keyContext;
	private Boolean	bStrafeLeft;
	private Boolean	bForward;
	private float	fSpeed=5f;

	private FlyByCameraX	flycamx;
	
	public static class NodeBody extends Node{
		public NodeBody(String name) {
			super(name);
		}
	}
	
	public BetterCharacterControlX create(Vector3f v3fSpawnAt){
		if(v3fSpawnAt==null){
			/**
			 * user target spot
			 */
			ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
			if(acr.size()==0)return null;
			CollisionResult cr = acr.get(0);
			PhysicsData pd = PhysicsI.i().getPhysicsDataFrom(cr.getGeometry());
			if(!pd.isTerrain())return null;
			v3fSpawnAt = cr.getContactPoint();
		}
		
//		PhysicsCharacter pc = new PhysicsCharacter(new CapsuleCollisionShape(), 2f);
		float fHeight=1.7f;
		BetterCharacterControlX bcc = new BetterCharacterControlX(0.25f, fHeight, 70f);
		
		NodeBody nodeBody = new NodeBody("CharacterBody");
		
		Geometry geomBody = GeometryI.i().create(new Box(0.25f,fHeight/2f,0.25f), ColorRGBA.Orange);
		geomBody.setLocalTranslation(0, fHeight/2f, 0);
		nodeBody.attachChild(geomBody);
		
		Geometry nodeHead=GeometryI.i().create(MeshI.i().sphere(0.15f), ColorRGBA.Yellow);
		nodeHead.setLocalTranslation(0, fHeight, 0);
		nodeBody.attachChild(nodeHead);
		
		bcc.setHead(nodeHead);
		
		nodeBody.addControl(bcc);
		
		AppI.i().getRootNode().attachChild(nodeBody);
		
		PhysicsI.i().add(nodeBody);
		
		bcc.setPhysicsLocation(v3fSpawnAt.add(0,0.25f,0)); //a bit above
		
		bccLast = bcc;
		
		return bcc;
	}
	
	public void update(float fTPF){
		keyContext.setPressedSpecialExternalContextKeyMode(cc, bccLast!=null);
		
		if(keyContext.isPressed()){
			Vector3f v3fMove = new Vector3f();
			
			if(bForward!=null){
				Vector3f v3f=AppI.i().getCamLookingAtDir();
				v3f.multLocal(getSpeed());
				v3f.y=0;
				if(!bForward)v3f.negateLocal();
				v3fMove.addLocal(v3f);
			}
			
			if(bStrafeLeft!=null){
				Vector3f v3f=AppI.i().getCamLeftDir();
				v3f.multLocal(getSpeed());
				v3f.y=0;
				if(!bStrafeLeft)v3f.negateLocal();
				v3fMove.addLocal(v3f);
			}
			
			bccLast.setWalkDirection(v3fMove);
		}
			
		AppI.i().setCamFollow(keyContext.isPressed() ? bccLast.nodeHead : null);
		flycamx.setAllowMove(!keyContext.isPressed());
	}
	
	public void configure(FlyByCameraX flycamx){
		this.flycamx = flycamx;
		
		keyContext = KeyCodeManagerI.i().createSpecialExternalContextKey(cc, "CharacterPossessionContext");
		
		KeyBindCommandManagerI.i().putBindCommandsLater("Ctrl+I",
			new CallBoundKeyCmd(){
				@Override public Boolean callOnKeyReleased(int iClickCountIndex) {
					if(bccLast!=null){
						bccLast=null;
					}else{
						ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
						if(acr.size()>0){
							Geometry geom = acr.get(0).getGeometry();
							BetterCharacterControlX bcc = getBCCFrom(geom);
							if(bcc!=null)bccLast=bcc;
						}
					}
					return true;
				}
			}.holdKeyPressedForContinuousCmd().setName("CharacterPossession")
		);
		
		bindLater("Left", true, true);
		bindLater("Right", true, false);
		bindLater("Up", false, true);
		bindLater("Down", false, false);
		
		initUpdateCharacterMovement();
	}
	
	public BetterCharacterControlX getBCCFrom(Spatial spt){
		NodeBody nb = getBodyFrom(spt);
		if(nb==null)return null;
		BetterCharacterControlX bcc = nb.getControl(BetterCharacterControlX.class);
		return bcc;
	}
	
	public NodeBody getBodyFrom(Spatial spt){
		return SpatialHierarchyI.i().getParentestOrSelf(spt, NodeBody.class, true, false);
	}
	
	private void initUpdateCharacterMovement() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}).enableLoopMode().setDelaySeconds(0.1f);
	}

	protected void bindLater(String strKey, boolean bStrafe, boolean bPositive){
		KeyBindCommandManagerI.i().putBindCommandsLater(
			KeyCodeManagerI.i().getKeyForId(strKey).composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){
				@Override public Boolean callOnKeyPressed(int iClickCountIndex) {
					if(bStrafe){
						bStrafeLeft=bPositive; 
					}else{
						bForward=bPositive;
					}
					return true;
				}
				@Override public Boolean callOnKeyReleased(int iClickCountIndex) {
					if(bStrafe){
						bStrafeLeft=null; 
					}else{
						bForward=null;
					}
					return true;
				}
			}.holdKeyPressedForContinuousCmd().setName("CharacterMove"+(bStrafe?"StrafeLeft":"Trust")+(bPositive?"":"Negate"))
		);
	}

	public float getSpeed() {
		return fSpeed;
	}

	public CharacterI setSpeed(float fSpeed) {
		this.fSpeed = fSpeed;
		return this; 
	}
	
	
	public boolean isCharacter(PhysicsCollisionObject pco) {
		return (pco.getUserObject() instanceof NodeBody);
	}
	@Deprecated 
	public boolean isCharacterForSure(PhysicsCollisionObject pco) {
		return ((Spatial)pco.getUserObject()).getControl(BetterCharacterControlX.class)!=null;
	}
	
}