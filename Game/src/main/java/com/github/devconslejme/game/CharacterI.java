/* 
	Copyright (c) 2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
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
import com.github.devconslejme.misc.KeyBind;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.MatterI.EMatter;
import com.github.devconslejme.misc.MatterI.MatterStatus;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.FlyByCameraX;
import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.MeshI;
import com.github.devconslejme.misc.jme.NodeX;
import com.github.devconslejme.misc.jme.PhysicsData;
import com.github.devconslejme.misc.jme.PhysicsI;
import com.github.devconslejme.misc.jme.PhysicsI.ImpTorForce;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.jme.WorldPickingI;
import com.jme3.bullet.collision.PhysicsCollisionObject;
import com.jme3.bullet.control.BetterCharacterControl;
import com.jme3.bullet.objects.PhysicsRigidBody;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Spatial;
import com.jme3.scene.Spatial.CullHint;
import com.jme3.scene.shape.Box;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class CharacterI {
	public static CharacterI i(){return GlobalManagerI.i().get(CharacterI.class);}
	
	public static class CompositeControl implements ICompositeRestrictedAccessControl{private CompositeControl(){};};
	private ICompositeRestrictedAccessControl	cc=new CompositeControl();
	
	private Key	keyContext;
	private Boolean	bStrafeLeft;
	private Boolean	bForward;
	private float	fSpeed=1f;
	private float	fRunSpeedMult=6f;
	private boolean bRunning=false;
	private FlyByCameraX	flycamx;
	private LeviCharacter leviPossessed;
	private TimedDelay tdMoveImpulseInterval = new TimedDelay(1f).setActive(true);
	private float fDivMassRotToDir=1;
	private long lMaxJumpHoldDelay=10000;
	private float fJumpMultImpulse = 1f;
	private ArrayList<LeviCharacter> alc = new ArrayList<>();
	private Vector3f v3fMoveDirection = new Vector3f();
	private Long lJumpStartSimulTimeMilis=null;
	private Long lJumpEndSimulTimeMilis=null;
	private boolean bHideHeadOnPossession;
	
	/**
	 * see  {@link BetterCharacterControl}
	 */
	public static class BetterCharacterControlX extends BetterCharacterControl{
		private Spatial	sptHead;
		public NodeBodyPart nodeBody;

		/**
		 * see {@link BetterCharacterControl#BetterCharacterControl(float, float, float)}
		 */
		public BetterCharacterControlX(float radius, float height, float mass) {
			super(radius, height, mass);
		}
		
		/**
		 * restriction based on info at
		 * see {@link BetterCharacterControl#BetterCharacterControl(float, float, float)}
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
			this.sptHead = nodeHead;
		}

		@Override
		protected void setPhysicsLocation(Vector3f vec) {
			super.setPhysicsLocation(vec);
		}

		public PhysicsRigidBody getPRB() {
			return rigidBody;
		}
	}

	public static class NodeBodyPart extends NodeX{
		private LeviCharacter levi;

		public NodeBodyPart(LeviCharacter lc, String name) {
			super(name);
			this.levi=lc;
		}

		public LeviCharacter getLevi() {
			return levi;
		}
	}
	
	public static class LeviCharacter{
		float fHeight=1.7f;
		float fHeadRadius=0.15f;
		float fNeckHeight=0.10f;
		float fTorsoHeight=fHeight/2f;
		NodeBodyPart nodeTorso;
		NodeBodyPart nodeHead;
		public PhysicsData pdTorso;
		public PhysicsData pdHead;
		
		public void update(float tpf) {
			if(pdTorso.getPhysicsRotationCopy().getRotationColumn(1).distance(Vector3f.UNIT_Y)>0.1f) {
				pdTorso.getSpatialWithPhysics().rotateUpTo(Vector3f.UNIT_Y);
				PhysicsI.i().syncPhysTransfFromSpt(pdTorso,false,true);
			}
			
			float fLinearDamp=0.85f;
			if(pdTorso.getGravityCopy().length()>0) {
				fLinearDamp=0f;
			}
			pdTorso.setNewDampingAtMainThread(fLinearDamp, 0.75f); //TODO understand and improve this..., lower damping when over slipping surfaces like ice
//			pdHead.getPRB().setPhysicsLocation(
//				pdTorso.getPRB().getPhysicsLocation()
//					.add(0,fTorsoHeight/2f+fNeckHeight+fHeadRadius,0));
		}
	}
	
	public LeviCharacter create(Vector3f v3fSpawnAt){
		if(v3fSpawnAt==null){
			/**
			 * user target spot
			 */
			ArrayList<RayCastResultX> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
			if(acr.size()==0)return null;
			RayCastResultX cr = acr.get(0);
			if(!cr.getPD().isTerrain())return null;
			v3fSpawnAt = cr.getWHitPos();
		}
		
		LeviCharacter lc = new LeviCharacter();
		
		Geometry geomBody = GeometryI.i().create(new Box(0.25f,lc.fTorsoHeight/2f,0.075f), ColorRGBA.Orange);
		lc.nodeTorso = new NodeBodyPart(lc,"Torso");
		lc.pdTorso = PhysicsI.i().imbueFromWBounds(geomBody,new MatterStatus(EMatter.OrganicBody.get()),lc.nodeTorso);
		lc.pdTorso.setLevitation(null,lc.fHeight-lc.fTorsoHeight/2f-lc.fHeadRadius*2f);
//		lc.pdTorso.getPRB().setDamping(0.75f, 0.75f); //TODO understand and improve this..., lower damping when over slipping surfaces like ice
		
		Geometry geomHead=GeometryI.i().create(MeshI.i().sphere(lc.fHeadRadius), ColorRGBA.Yellow);
		lc.nodeHead = new NodeBodyPart(lc,"Head");
		lc.nodeHead.setLocalTranslation(0, lc.fHeight/4f+lc.fHeadRadius, 0);
		lc.pdHead = PhysicsI.i().imbueFromWBounds(geomHead,new MatterStatus(EMatter.OrganicBody.get()),lc.nodeHead);
		lc.pdHead.setLevitation(lc.pdTorso, lc.fTorsoHeight/2f+lc.fHeadRadius+lc.fNeckHeight); //will be above the torso!
		lc.pdHead.addPhysicsDataSkipCollisionGroup(lc.pdTorso);
		//		PhysicsJoint pj = new PhysicsJoint() {		};
		
		lc.nodeTorso.attachChild(lc.nodeHead);

		AppI.i().getRootNode().attachChild(lc.nodeTorso);
		
		lc.pdTorso.setPhysicsLocationAtMainThread(v3fSpawnAt.add(0,0.25f,0)); //a bit above
		
		if(leviPossessed==null)setPossessed(lc);
		
		alc.add(lc);
		
		return lc;
	}
	
	@Deprecated
	public BetterCharacterControlX createBCCX(Vector3f v3fSpawnAt){
		if(v3fSpawnAt==null){
			/**
			 * user target spot
			 */
			ArrayList<RayCastResultX> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
			if(acr.size()==0)return null;
			RayCastResultX cr = acr.get(0);
			if(!cr.getPD().isTerrain())return null;
			v3fSpawnAt = cr.getWHitPos();
		}
		
//		PhysicsCharacter pc = new PhysicsCharacter(new CapsuleCollisionShape(), 2f);
		float fHeight=1.7f;
		BetterCharacterControlX bcc = new BetterCharacterControlX(0.25f, fHeight, 70f);
		
		bcc.nodeBody = new NodeBodyPart(null,"CharacterBody");
		
		Geometry geomBody = GeometryI.i().create(new Box(0.25f,fHeight/2f,0.25f), ColorRGBA.Orange);
		geomBody.setLocalTranslation(0, fHeight/2f, 0);
		bcc.nodeBody.attachChild(geomBody);
		
		float fHeadRadius=0.15f;
		bcc.sptHead=GeometryI.i().create(MeshI.i().sphere(fHeadRadius), ColorRGBA.Yellow);
		bcc.sptHead.setLocalTranslation(0, fHeight+fHeadRadius, 0);
		bcc.nodeBody.attachChild(bcc.sptHead);
		
		bcc.nodeBody.addControl(bcc);
		
		AppI.i().getRootNode().attachChild(bcc.nodeBody);
		
		PhysicsData pd = new PhysicsData(bcc.nodeBody, geomBody);
		pd.setPRB(bcc.getPRB());
		UserDataI.i().putSafelyMustNotExist(pd.getSpatialWithPhysics(), pd); //BEFORE adding to phys space as its thread will be trying to retrieve it!
		PhysicsI.i().add(bcc.nodeBody);
		
		bcc.setPhysicsLocation(v3fSpawnAt.add(0,0.25f,0)); //a bit above
		
//		bccPossessed = bcc;
		
		return bcc;
	}
	
	protected void updateAllLevis(float tpf) {
		for(LeviCharacter lc : alc) {
			lc.update(tpf);
		}
	}
//	@Deprecated
//	public void updatePossessedBCC(float fTPF){
//		//TODO if(bccPossessed==null)return;
//		
//		Vector3f v3fMove = new Vector3f();
//		
//		if(bForward!=null){
//			Vector3f v3f=AppI.i().getCamLookingAtDir();
//			v3f.multLocal(getSpeed());
//			v3f.y=0;
//			if(!bForward)v3f.negateLocal();
//			v3fMove.addLocal(v3f);
//		}
//		
//		if(bStrafeLeft!=null){
//			Vector3f v3f=AppI.i().getCamLeftDir();
//			v3f.multLocal(getSpeed());
//			v3f.y=0;
//			if(!bStrafeLeft)v3f.negateLocal();
//			v3fMove.addLocal(v3f);
//		}
//		
//		//TODO bccPossessed.setWalkDirection(v3fMove);
//	}
	
	public void configure(FlyByCameraX flycamx){
		this.flycamx = flycamx;
		
		keyContext = KeyCodeManagerI.i().createSpecialExternalContextKey(cc, "CharacterPossessionContext");
		
		KeyBindCommandManagerI.i().putBindCommandsLater("Ctrl+I",
			new CallBoundKeyCmd(){
				@Override public Boolean callOnKeyReleased(int iClickCountIndex) {
					if(isPossessing()){
						releasePossessed();
					}else{
						ArrayList<RayCastResultX> ar = WorldPickingI.i().raycastPiercingAtCenter(null);
						if(ar.size()>0){
							Geometry geom = ar.get(0).getPD().getGeomOriginalInitialLink();
							LeviCharacter levi = getLeviFrom(geom);
							if(levi!=null)setPossessed(levi);
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
		
		KeyBindCommandManagerI.i().putBindCommandsLater(
			KeyCodeManagerI.i().getKeyForId("End").composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){
				@Override public Boolean callOnKeyPressed(int iClickCountIndex) {
					lJumpStartSimulTimeMilis=SimulationTimeI.i().getMillis();
					return true;
				}
				@Override public Boolean callOnKeyReleased(int iClickCountIndex) {
					lJumpEndSimulTimeMilis=SimulationTimeI.i().getMillis();
					return true;
				}
			}.setName("CharacterJump")
		);

		KeyBindCommandManagerI.i().putBindCommandsLater( //TODO use Shift alone in some way? or keep it just as a modifier only key?
			KeyCodeManagerI.i().getKeyForId("R").composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){
				@Override public Boolean callOnKeyPressed(int iClickCountIndex) {
					setRunning(true);
					return true;
				}
				@Override public Boolean callOnKeyReleased(int iClickCountIndex) {
					setRunning(false);
					return true;
				}
			}.setName("CharacterRun")
		);
		
		initUpdateCharacterMovement();
	}
	
	protected void setPossessed(LeviCharacter levi) {
		releasePossessed();
		this.leviPossessed=levi;
		if(leviPossessed!=null) {
			if(isHideHeadOnPossession())leviPossessed.nodeHead.setCullHint(CullHint.Always);
		}
	}

	protected void releasePossessed() {
		if(leviPossessed!=null) {
			leviPossessed.nodeHead.setCullHint(CullHint.Inherit);
		}
		leviPossessed=null;
	}

	protected LeviCharacter getLeviFrom(Spatial spt) {
		NodeBodyPart nb = getBodyFrom(spt);
		if(nb==null)return null;
		return nb.getLevi();
	}

	public BetterCharacterControlX getBCCFrom(Spatial spt){
		NodeBodyPart nb = getBodyFrom(spt);
		if(nb==null)return null;
		BetterCharacterControlX bcc = nb.getControl(BetterCharacterControlX.class);
		return bcc;
	}
	
	public NodeBodyPart getBodyFrom(Spatial spt){
		return SpatialHierarchyI.i().getParentestOrSelf(spt, NodeBodyPart.class, true, false);
	}
	
	private void initUpdateCharacterMovement() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				keyContext.setPressedSpecialExternalContextKeyMode(cc, isPossessing());
				
				updateAllLevis(getTPF());
				
				updatePossessed(getTPF());
//				updatePossessedBCC(getTPF());
				
				AppI.i().setCamFollow(getPossessedHead());
				flycamx.setAllowMove(!keyContext.isPressed());
				
				return true;
			}
		}).enableLoopMode();//.setDelaySeconds(0.1f);
	}
	
	protected void updatePossessed(float tpf) {
		v3fMoveDirection.set(0,0,0);
		if(!isPossessing())return;
		
		if(bForward!=null){
			Vector3f v3f=AppI.i().getCamLookingAtDir();
			v3f.y=0;
			v3f.normalizeLocal();//.multLocal(getSpeed());
			if(!bForward)v3f.negateLocal();
			v3fMoveDirection.addLocal(v3f);
		}
		
		if(bStrafeLeft!=null){
			Vector3f v3f=AppI.i().getCamLeftDir();
			v3f.y=0;
			v3f.normalizeLocal();//.multLocal(getSpeed());
			if(!bStrafeLeft)v3f.negateLocal();
			v3fMoveDirection.addLocal(v3f);
		}
		
		if(v3fMoveDirection.length()>0) {
			if(tdMoveImpulseInterval.isReady(true)) {
				Vector3f v3fDisplacement = v3fMoveDirection.subtract(leviPossessed.pdTorso.getPhysicsRotationCopy().getRotationColumn(2));
				v3fDisplacement.multLocal(leviPossessed.pdTorso.getMass()/getDivMassRotToDir()).negateLocal();
				PhysicsI.i().applyImpulseLater(leviPossessed.pdTorso,	new ImpTorForce()
					.setImpulse(calcMoveImpulse(v3fMoveDirection), v3fDisplacement)
				);
			}
		}
		
		if(lJumpEndSimulTimeMilis!=null) {
			long lJumpDelayMilis = lJumpEndSimulTimeMilis-lJumpStartSimulTimeMilis;
			PhysicsI.i().applyImpulseLater(leviPossessed.pdTorso,new ImpTorForce().setImpulse(new Vector3f(0,getJumpImpulse(lJumpDelayMilis),0), null));
			HWEnvironmentJmeI.i().putCustomInfo("CharLastJump", ""+lJumpEndSimulTimeMilis+","+lJumpStartSimulTimeMilis+","+lJumpDelayMilis+","+getJumpImpulse(lJumpDelayMilis));
			lJumpStartSimulTimeMilis=null;
			lJumpEndSimulTimeMilis=null;
		}
	}
	
	private Vector3f calcMoveImpulse(Vector3f v3fMoveDir) {
		v3fMoveDir=v3fMoveDir.clone();
		v3fMoveDir.normalizeLocal();
		v3fMoveDir.multLocal(leviPossessed.pdTorso.getMass());
		v3fMoveDir.multLocal(getSpeed());
		return v3fMoveDir;
	}

	/**
	 * average final height shall be 45cm
	 * @param lJumpDelayMilis max height will be reached for 1000ms
	 * @return
	 */
	private float getJumpImpulse(long lJumpDelayMilis) {
		float fImpulse=leviPossessed.pdTorso.getMass();
		if(lJumpDelayMilis>getMaxJumpHoldDelay())lJumpDelayMilis=getMaxJumpHoldDelay(); //max
		fImpulse*=(lJumpDelayMilis/(float)getMaxJumpHoldDelay());
		fImpulse*=getJumpMultImpulse() ;
		return fImpulse;
	}

	public boolean isPossessing() {
		return leviPossessed!=null;
	}
	public Spatial getPossessedHead() {
		if(!isPossessing())return null;
		return leviPossessed.nodeHead;
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
	
	public boolean toggleRunning() {
		return bRunning = !bRunning;
	}

	public float getSpeed() {
		return isRunning() ? fSpeed*fRunSpeedMult : fSpeed;
	}

	public CharacterI setSpeed(float fSpeed) {
		this.fSpeed = fSpeed;
		return this; 
	}
	
	
	public boolean isCharacter(PhysicsCollisionObject pco) {
		return (pco.getUserObject() instanceof NodeBodyPart);
	}
	@Deprecated 
	public boolean isCharacterForSure(PhysicsCollisionObject pco) {
		return ((Spatial)pco.getUserObject()).getControl(BetterCharacterControlX.class)!=null;
	}

	public LeviCharacter getPossessed() {
		return leviPossessed;
	}
//	public BetterCharacterControlX getPossessed() {
//		return bccLast;
//	}

	public String prependPossessedContextKeyMod(String strK) {
		return new KeyBind().setFromKeyCfg(strK).addModifier(keyContext).getBindCfg();
	}

	public boolean isRunning() {
		return bRunning;
	}

	public CharacterI setRunning(boolean bRunning) {
		this.bRunning = bRunning;
		return this; 
	}

	public float getDivMassRotToDir() {
		return fDivMassRotToDir;
	}

	public CharacterI setDivMassRotToDir(float fDivMassRotToDir) {
		this.fDivMassRotToDir = fDivMassRotToDir;
		return this; 
	}

	public boolean isHideHeadOnPossession() {
		return bHideHeadOnPossession;
	}

	public CharacterI setHideHeadOnPossession(boolean bHideHeadOnPossession) {
		this.bHideHeadOnPossession = bHideHeadOnPossession;
		return this; 
	}

	public long getMaxJumpHoldDelay() {
		return lMaxJumpHoldDelay;
	}

	public CharacterI setMaxJumpHoldDelay(long lMaxJumpHoldDelay) {
		this.lMaxJumpHoldDelay = lMaxJumpHoldDelay;
		return this; 
	}

	public float getJumpMultImpulse() {
		return fJumpMultImpulse;
	}

	public CharacterI setJumpMultImpulse(float fJumpMultImpulse) {
		this.fJumpMultImpulse = fJumpMultImpulse;
		return this; 
	}
	
	public CharacterI setMoveImpulseInterval(float f) {
		tdMoveImpulseInterval.resetAndChangeDelayTo(f).setActive(true);
		return this;
	}
}