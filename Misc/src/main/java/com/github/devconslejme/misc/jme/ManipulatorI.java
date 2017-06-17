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

import com.github.devconslejme.game.CharacterI;
import com.github.devconslejme.game.TargetI;
import com.github.devconslejme.game.TargetI.TargetGeom;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.ICompositeRestrictedAccessControl;
import com.github.devconslejme.misc.Key;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;

/**
 * TODO after picking, using the mouse, move and rotate using wheel and moving the mouse, aligned with whatever is just below it
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ManipulatorI {
	public static ManipulatorI i(){return GlobalManagerI.i().get(ManipulatorI.class);}

	public static class CompositeControl implements ICompositeRestrictedAccessControl{private CompositeControl(){};};
	private ICompositeRestrictedAccessControl	cc=new CompositeControl();
		
//	private CollisionResult	crManipulating;
	private Key	keyContext;

	protected PhysicsData pdManipulating;
	private boolean bAllowManipulationWithoutPhysics=false;
	private boolean bAllowManipulationOfTerrain=false;
	private boolean bAllowManipulationOfStatics=false;

	private float fRotateSpeedDegAngle=10f;

	private boolean	bPullGrabbed=true;

	private float	fPullSpeed=20f;
	private float	fCurrentSpeed;

	private Float	fMaxGrabDist=null;

	private float fMinDist=2f;

	private boolean bGrabForcePlacement=true;
	
	/**
	 * 
	 * @return if did dropped something
	 */
	public boolean drop() {
		boolean bIsGrabbing=pdManipulating!=null;
		if(bIsGrabbing) {
			PhysicsI.i().wakeUp(pdManipulating);
			if(!bGrabForcePlacement)pdManipulating.setTempGravityTowards(null,null);
			pdManipulating.setGrabbedBy(null);
			
			pdManipulating.resumeLevitationIfItWas();
			
//			pdManipulating.getPRB().setKinematic(false);
		}
		pdManipulating=null;
		return bIsGrabbing;
	}
	
	public void grab(PhysicsData pd, float fCurrentDistance) {
		boolean bOk=true;
		if(pd!=null) {
			if(!bAllowManipulationOfTerrain && pd.isTerrain())bOk=false;
			if(!bAllowManipulationOfStatics && pd.isStatic ())bOk=false;
		}else {
			if(!bAllowManipulationWithoutPhysics)bOk=false;
		}
		if(getMaxGrabDist()!=null && fCurrentDistance>getMaxGrabDist()){
			bOk=false;
		}
		
		if(bOk) {
			pdManipulating=pd;
			pdManipulating.suspendLevitationIfItIs();
			pdManipulating.setGrabDist(fCurrentDistance);
			fCurrentSpeed=0f;
			pdManipulating.setGrabbedBy(CharacterI.i().getPossessed());
//			pdManipulating.getPRB().setKinematic(true);
		}
	}
	
	public boolean isGrabbed(PhysicsData pd) {
		return pdManipulating == pd;
	}
	
	public void configure(){
		keyContext = KeyCodeManagerI.i().createSpecialExternalContextKey(cc, "ContextManipulatorGrab");
		
		KeyBindCommandManagerI.i().putBindCommandsLater("Ctrl+G", new CallBoundKeyCmd(){
			@Override
			public Boolean callOnKeyReleased(int iClickCountIndex) {
				if(!drop()) {
					ArrayList<RayCastResultX> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
					if(acr.size()>0) {
						PhysicsData pd = acr.get(0).getPD();
						if(pd!=null) {
							grab(pd,acr.get(0).getDistance());
						}
					}
				}
				return true;
			};
		}.setName("ManipulatorGrabAimed"));
		
		KeyBindCommandManagerI.i().putBindCommandsLater("Shift+Ctrl+G", new CallBoundKeyCmd(){
			@Override
			public Boolean callOnKeyReleased(int iClickCountIndex) {
				if(!drop()) {
					TargetGeom tg = TargetI.i().getLastSingleTarget();
					if(tg!=null) {
						grab(
							tg.getPhysicsData(), 
							AppI.i().getCamWPos(0f).distance( tg.getPhysicsData().getPhysicsLocationCopy() )
						);
					}
				}
				return true;
			};
		}.setName("ManipulatorGrabSelectedSingleTarget"));
		
		KeyBindCommandManagerI.i().putBindCommandsLater(
			KeyCodeManagerI.i().getMouseAxisKey(2,true).composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){@Override	public Boolean callOnKeyReleased(int iClickCountIndex) {
				rotateManipulated(true); return true;};}.setName("ManipGrabRotCW")
		);
		KeyBindCommandManagerI.i().putBindCommandsLater(
			KeyCodeManagerI.i().getMouseAxisKey(2,false).composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){@Override	public Boolean callOnKeyReleased(int iClickCountIndex) {
				rotateManipulated(false); return true;};}.setName("ManipGrabRotCCW")
		);
		
		KeyBindCommandManagerI.i().putBindCommandsLater(
			KeyCodeManagerI.i().getKeyForId("Space").composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){
				private PhysicsData pdTryActivate;
				@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
//					ActivatorI.i().activateIfPossible(crManipulating.getGeometry()); //parentest spt will also be considered (least Root Node)
					pdTryActivate=null;//pre-reset
					if(pdManipulating!=null) {
						pdTryActivate=pdManipulating;
						ActivatorI.i().activateIfPossible(pdTryActivate); //parentest spt will also be considered (least Root Node)
					}
					return true;
				}
				@Override
				public Boolean callOnKeyReleased(int iClickCountIndex) {
					if(pdTryActivate!=null)ActivatorI.i().deactivateIfPossible(pdTryActivate); //parentest spt will also be considered (least Root Node)
					pdTryActivate=null; //just to be sure...
					return true;
				};
			}.setName("ActivateGrabbed").holdKeyPressedForContinuousCmd()
		);
		
		initUpdateManipulated();
	}
	
	protected void rotateManipulated(boolean bCW){
		float fRotYRad=(bCW?1:-1)*fRotateSpeedDegAngle*FastMath.DEG_TO_RAD;
//		Spatial spt= pdManipulating!=null ? pdManipulating.getSpatialWithPhysics() : crManipulating.getGeometry();
		Spatial spt= pdManipulating.getSpatialWithPhysics();
		spt.rotate(0,fRotYRad,0);
		if(pdManipulating!=null)PhysicsI.i().syncPhysTransfFromSpt(pdManipulating,false,true);
//		if(pdManipulating!=null)PhysicsI.i().syncPhysTransfFromSpt(pdManipulating.getSpatialWithPhysics());
	}

	protected void initUpdateManipulated() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				keyContext.setPressedSpecialExternalContextKeyMode(cc, pdManipulating!=null);
				if(!keyContext.isPressed())return true;//skip
				updateManipulated(getTPF());
				return true;
			}
		}).enableLoopMode();
	}
	
	private TimedDelay tdResetForce = new TimedDelay(0.1f).setActive(true);
	protected void updateManipulated(float tpf) {
		Spatial spt= pdManipulating.getSpatialWithPhysics();
		
		if(bGrabForcePlacement) {
			float fDist = pdManipulating.getGrabDist();
			fCurrentSpeed+=getPullSpeed()*tpf;
//			fCurrentSpeed=getPullSpeed()*tpf;
			if(isPullGrabbed())fDist-=fCurrentSpeed;
			
			if(fDist<fMinDist) {
				fDist=fMinDist;
				fCurrentSpeed=0f;
			}
			pdManipulating.setGrabDist(fDist);
			
			spt.rotateUpTo(Vector3f.UNIT_Y);
			AppI.i().placeAtCamWPos(pdManipulating, fDist, pdManipulating.isActivatable());
			
//			PhysicsI.i().resetForces(pdManipulating); //prevent falling flickering glitch
			pdManipulating.setNewGravityAtMainThread(Vector3f.ZERO); //prevent falling flickering glitch
		}else {
			Vector3f v3fFrom = pdManipulating.getPhysicsLocationCopy();
			Vector3f v3fInfrontCamPos = AppI.i().getCamWPos(fMinDist);
			ArrayList<RayCastResultX> aresx = PhysicsI.i().rayCastSortNearest(v3fFrom, v3fInfrontCamPos, false, true, true, pdManipulating);//TODO possessed pds
			if(aresx.size()==0) { //must have a clean line of sight!
				float fDistRest=0.01f;
				float fDist = v3fInfrontCamPos.distance(v3fFrom);
	//			float fDist = pdManipulating.getGrabDist();
	//			pdManipulating.setTempGravityTowards(v3fCamPos,fDist);
				float fAcceleration = 200f;
				if(fDist < 0.10f)fAcceleration/=10f;
				else
				if(fDist < 0.25f)fAcceleration/=5f;
				else
				if(fDist < 0.50f)fAcceleration/=2f;
				pdManipulating.setTempGravityTowards(v3fInfrontCamPos, fDist>fDistRest ? fAcceleration : 0f);
				
				Vector3f v3fStraightDir = v3fInfrontCamPos.subtract(v3fFrom).normalize();
				Vector3f v3fMoveDir = pdManipulating.getLinearVelocityCopy().normalize();
				
				boolean bResetF=false;
				if(v3fStraightDir.distance(v3fMoveDir)>0.1f)bResetF=true; /** if the direction is too way off, reset once */
				if(fDist<fDistRest)bResetF=true;
				if(fDist<10f && tdResetForce.isReady(true))bResetF=true;
	//			if(tdResetForce.isReady(true))bResetF=true;
				if(bResetF) {
					PhysicsI.i().resetForces(pdManipulating);
				}
	//			if(fDist<fDistRest || (fDist>10f && tdResetForce.isReady(true)))PhysicsI.i().resetForces(pdManipulating);
	//			tdResetForce.resetAndChangeDelayTo(0.1f).setActive(true);
			}
		}
		
		if(pdManipulating.isActivatable()) {
			spt.lookAt(spt.getLocalTranslation().add(AppI.i().getCamLookingAtDir()), Vector3f.UNIT_Y);
			PhysicsI.i().syncPhysTransfFromSpt(pdManipulating,false,true);
		}
		
		HWEnvironmentJmeI.i().putCustomInfo("Grabbed", pdManipulating.getInfo());
	}

	public boolean isAllowManipulationWithoutPhysics() {
		return bAllowManipulationWithoutPhysics;
	}

	public ManipulatorI setAllowManipulationWithoutPhysics(
		boolean bAllowManipulationWithoutPhysics) {
		this.bAllowManipulationWithoutPhysics = bAllowManipulationWithoutPhysics;
		return this; 
	}

	public boolean isAllowManipulationOfTerrain() {
		return bAllowManipulationOfTerrain;
	}

	public ManipulatorI setAllowManipulationOfTerrain(boolean bAllowManipulationOfTerrain) {
		this.bAllowManipulationOfTerrain = bAllowManipulationOfTerrain;
		return this; 
	}

	public boolean isAllowManipulationOfStatics() {
		return bAllowManipulationOfStatics;
	}

	public ManipulatorI setAllowManipulationOfStatics(boolean bAllowManipulationOfStatics) {
		this.bAllowManipulationOfStatics = bAllowManipulationOfStatics;
		return this; 
	}

	public float getRotateSpeedDegAngle() {
		return fRotateSpeedDegAngle;
	}

	public ManipulatorI setRotateSpeedDegAngle(float fRotateSpeedDegAngle) {
		this.fRotateSpeedDegAngle = fRotateSpeedDegAngle;
		return this; 
	}

	public boolean isPullGrabbed() {
		return bPullGrabbed;
	}

	public ManipulatorI setPullGrabbed(boolean bPullGrabbed) {
		this.bPullGrabbed = bPullGrabbed;
		return this; 
	}

	public Float getMaxGrabDist() {
		return fMaxGrabDist;
	}
	
	/**
	 * 
	 * @param fMaxGrabDist null is unlimited
	 * @return
	 */
	public ManipulatorI setMaxGrabDist(Float fMaxGrabDist) {
		this.fMaxGrabDist = fMaxGrabDist;
		return this; 
	}

	public float getPullSpeed() {
		return fPullSpeed;
	}

	public ManipulatorI setPullSpeed(float fPullSpeed) {
		this.fPullSpeed = fPullSpeed;
		return this; 
	}

	public float getMinDist() {
		return fMinDist;
	}

	public ManipulatorI setMinDist(float fMinDist) {
		this.fMinDist = fMinDist;
		return this; 
	}

	public boolean isGrabForcePlacement() {
		return bGrabForcePlacement;
	}

	public ManipulatorI setGrabForcePlacement(boolean bGrabForcePlacement) {
		this.bGrabForcePlacement = bGrabForcePlacement;
		return this; 
	}
	
	
}
