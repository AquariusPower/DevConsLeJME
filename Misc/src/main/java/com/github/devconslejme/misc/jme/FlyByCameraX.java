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

import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.ICompositeRestrictedAccessControl;
import com.github.devconslejme.misc.Key;
import com.github.devconslejme.misc.KeyBind;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.TimedDelay;
import com.jme3.app.Application;
import com.jme3.collision.MotionAllowedListener;
import com.jme3.input.CameraInput;
import com.jme3.input.FlyByCamera;
import com.jme3.input.InputManager;
import com.jme3.input.MouseInput;
import com.jme3.input.controls.MouseAxisTrigger;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;

/**
 * TODO complete to be attachable to player's head or flying ship (limiting/requesting(listener?) the movement etc..)
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class FlyByCameraX extends FlyByCamera {
	private boolean	bAllowZooming=false; //only with scope or eagle eye
	private boolean	bAllowMove=true; //as it is a flycam, must be default true
	private boolean	bOverrideKeepFlyCamDisabled;
	
	/** stores user is constantly flying for how much time, until keys are released */
	private float	fAccMvTm;
//	private float	fAccMvTm = 0.05f;
	
	private ArrayList<CallableX> acxList=new ArrayList<CallableX>();
	private boolean	bRotationAllowed=true;
	private float fFlyAcceleration=0.1f;
	private InputManager	inputman;
	private float	fMinFOVdeg;
	private float	fMaxFOVdeg;
//	private Float	fFOVstep;
	private int	iZoomSteps;
	ArrayList<Float> afListZoom = new ArrayList<Float>();
	private int iCurrentZoomStep;
	private boolean	bEnableZoomStepsAndLimits;
	private TimedDelay tdMouseGrab=new TimedDelay(0.5f,"update mouse grab").setActive(true);
	private TimedDelay tdMI=new TimedDelay(1f,"update mouse info").setActive(true);
	private float fChangeZoomStepSpeed=500f;
	private float	fZoomedRotationSpeed=1f; //no zoom
//	protected boolean	bZoomApplied;
	private int	iBkpZoomStep;
	private Key	keyFlyCamMod;
	
	public boolean lazyPrepareKeyBindings(){
		Key keyMWU=KeyCodeManagerI.i().getMouseAxisKey(2,true);
		if(keyMWU==null)return false;
		String strMouseWheelUp=keyMWU.getFullId();
		
		Key keyMWD=KeyCodeManagerI.i().getMouseAxisKey(2,false);
		if(keyMWD==null)return false;
		String strMouseWheelDown=keyMWD.getFullId();
		
    MiscJmeI.i().enqueueUnregisterKeyMappings( //these were set at super
  		CameraInput.FLYCAM_LEFT,
  		CameraInput.FLYCAM_RIGHT,
  		CameraInput.FLYCAM_UP,
  		CameraInput.FLYCAM_DOWN
    		
    	,CameraInput.FLYCAM_STRAFELEFT,
    	CameraInput.FLYCAM_STRAFERIGHT,
    	CameraInput.FLYCAM_FORWARD,
    	CameraInput.FLYCAM_BACKWARD,
    	CameraInput.FLYCAM_RISE,
    	CameraInput.FLYCAM_LOWER
    	
      ,CameraInput.FLYCAM_ZOOMIN,
      CameraInput.FLYCAM_ZOOMOUT
    );
    
    // FlyCam key! Contextualized keybindings!
//    Key key = new Key("FlyCam", -1);
    keyFlyCamMod = KeyCodeManagerI.i().createSpecialExternalContextKey(cc,"FlyCamContext");
    
    // fly cam temp disabler (must work outside of the flycam context too!!!)
    KeyBindCommandManagerI.i().putBindCommandLater("F5","hold to keep mouse cursor visible",new CallBoundKeyCmd(){
			@Override	public Boolean callOnKeyPressed(){
				bOverrideKeepFlyCamDisabled=true;
				setEnabled(false);
				return true;
			}
			
			@Override public Boolean callOnKeyReleased() {
				bOverrideKeepFlyCamDisabled=false;
				return true;
			};
			
		}.holdKeyPressedForContinuousCmd());
    
    // zoom
		putFlyCamBindCmdLater(strMouseWheelUp,CameraInput.FLYCAM_ZOOMIN,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			zoomCamera( getAnalogValue());return true;}});
		putFlyCamBindCmdLater(strMouseWheelDown,CameraInput.FLYCAM_ZOOMOUT,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			zoomCamera(-getAnalogValue());return true;}});
		putFlyCamBindCmdLater(
//				KeyCodeManagerI.i().getMouseTriggerKey(2).composeCfgPrependModifiers(keyFlyCamMod),
			KeyCodeManagerI.i().getMouseTriggerKey(2).getFullId(),
//			keyFlyCamMod.getFullId()+"+"+KeyCodeManagerI.i().getMouseTriggerKey(2).getFullId(),
			"toggleZoom",
			new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){toggleZoom();return true;}});
		
    // rotation
		putFlyCamBindCmdLater("Left",CameraInput.FLYCAM_LEFT,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			rotateCamera( getTPF(),initialUpVec);return true;}}.holdKeyPressedForContinuousCmd());
		putFlyCamBindCmdLater("Right",CameraInput.FLYCAM_RIGHT,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			rotateCamera(-getTPF(),initialUpVec);return true;}}.holdKeyPressedForContinuousCmd());
		putFlyCamBindCmdLater("Up",CameraInput.FLYCAM_UP,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			rotateCamera(-getTPF() * (invertY ? -1 : 1), cam.getLeft());return true;}}.holdKeyPressedForContinuousCmd());
		putFlyCamBindCmdLater("Down",CameraInput.FLYCAM_DOWN,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			rotateCamera( getTPF() * (invertY ? -1 : 1), cam.getLeft());return true;}}.holdKeyPressedForContinuousCmd());
		
		// re-add later the mouse rotation JME native mappings config TODO use the Key bind system (after axis are working perfectly)
		restoreMouseAxisLater(CameraInput.FLYCAM_LEFT,MouseInput.AXIS_X,true);
		restoreMouseAxisLater(CameraInput.FLYCAM_RIGHT,MouseInput.AXIS_X,false);
		restoreMouseAxisLater(CameraInput.FLYCAM_UP,MouseInput.AXIS_Y,false);
		restoreMouseAxisLater(CameraInput.FLYCAM_DOWN,MouseInput.AXIS_Y,true);
		
    // movement, each key requires an independent queue object instance as they can be combined
		putFlyCamBindCmdLater("A",CameraInput.FLYCAM_STRAFELEFT ,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			moveCamera( getTPF(),true );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.holdKeyPressedForContinuousCmd());
		putFlyCamBindCmdLater("D",CameraInput.FLYCAM_STRAFERIGHT,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			moveCamera(-getTPF(),true );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.holdKeyPressedForContinuousCmd());
		
		putFlyCamBindCmdLater("W",CameraInput.FLYCAM_FORWARD    ,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			moveCamera( getTPF(),false);accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.holdKeyPressedForContinuousCmd());
		putFlyCamBindCmdLater("S",CameraInput.FLYCAM_BACKWARD   ,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			moveCamera(-getTPF(),false);accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.holdKeyPressedForContinuousCmd());
		
		putFlyCamBindCmdLater("Q",CameraInput.FLYCAM_RISE       ,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			riseCamera( getTPF()      );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.holdKeyPressedForContinuousCmd());
		putFlyCamBindCmdLater("Z",CameraInput.FLYCAM_LOWER      ,new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(){
			riseCamera(-getTPF()      );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.holdKeyPressedForContinuousCmd());
		
		return true;
	}
	
	public void putFlyCamBindCmdLater(String strKeyCfg, String strName, CallBoundKeyCmd callcmd) {
		KeyBindCommandManagerI.i().putBindCommandLater(getCfgAddFlyCamMod(strKeyCfg),strName,callcmd);
	}

	public String getCfgAddFlyCamMod(String strOtherModsAndActionKeyCfg){
		return getKeyAddFlyCamMod(strOtherModsAndActionKeyCfg).getBindCfg();
	}
	public KeyBind getKeyAddFlyCamMod(String strOtherModsAndActionKeyCfg){
		return new KeyBind().setFromKeyCfg(strOtherModsAndActionKeyCfg).addModifier(keyFlyCamMod);
	}
	
	protected void toggleZoom() {
		if(iBkpZoomStep!=iCurrentZoomStep){
			iCurrentZoomStep=iBkpZoomStep;
		}else{
			iCurrentZoomStep=getNoZoomStepIndex();
		}
	}

	@Override
	protected void rotateCamera(float value, Vector3f axis) {
		if(!isRotationAllowed())return;
		super.rotateCamera(value*fZoomedRotationSpeed, axis);
	}
	
	/**
	 * reset acceleration for constant fly
	 * @param cx
	 */
	protected void resetMvTm(CallBoundKeyCmd cx) {
		acxList.remove(cx);
		if(acxList.size()==0)fAccMvTm=0f;
	}
	/**
	 * accumulates user constantly holding flying controls to accelerate
	 * @param cx
	 */
	protected void accMvTrsTm(CallBoundKeyCmd cx){
		if(!acxList.contains(cx))acxList.add(cx);
		fAccMvTm+=cx.getTPF();
	}

	public boolean toggleInvertY(){
		invertY=!invertY;
		return invertY;
	}
	
	public boolean getInvertY(){
		return invertY;
	}
	
	public FlyByCameraX setInvertY(boolean b){
		invertY=b;
		return this;
	}
	
	private void restoreMouseAxisLater(String strMapping, int iAxis, boolean bNegativeOnAxis) {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override	public Boolean call() {
				if(inputManager.hasMapping(strMapping))return false; //wait clearing
		    inputManager.addMapping(strMapping, new MouseAxisTrigger(iAxis, bNegativeOnAxis));
		    inputManager.addListener(FlyByCameraX.this, strMapping);
				return true;
			}
		});
	}

	public FlyByCameraX(Camera cam) {
		super(cam);
		
//		setMoveSpeed(1f);
		
//		reBindKeys();
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
//				if(KeyCodeManagerI.i().getKeyListCopy().size()==0)return false;
				if(!lazyPrepareKeyBindings())return false;
				return true;
			}
		});
		
//		KeyBindCommandManagerI.i().putBindCommandLater("F5","hold to keep mouse cursor visible",new CallBoundKeyCmd(){
//			@Override	public Boolean callOnKeyPressed(){
//				bOverrideKeepFlyCamDisabled=true;
//				setEnabled(false);
//				return true;
//			}
//			
//			@Override public Boolean callOnKeyReleased() {
//				bOverrideKeepFlyCamDisabled=false;
//				return true;
//			};
//			
//		}.holdKeyPressedForContinuousCmd());
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}.enableLoopMode().setName("EnsureMouseGrab"));
		
		setEnabledRaw(isEnabled()); //to initially set it properly
//		setEnabled(!isEnabled());setEnabled(isEnabled()); //trick to setup more things
		
		/**
		 * min is some default that is not so random... 
		 * it is not necessarily useful tho 
		 * TODO improve this concept?
		 */
		setZoomLimits(getFOV()/2f, getFOV(), 2); // default 2 steps: min and max
	}
	
	@Override
	protected void zoomCamera(float value) {
		if(!isAllowZooming())return;
		
		if(bEnableZoomStepsAndLimits){
			if(value>0){
				iCurrentZoomStep+=(isZoomInverted()?-1:1);
			}else{
				iCurrentZoomStep+=(isZoomInverted()?1:-1);
			}
			fixZoom();
			//			bZoomApplied=true;
			iBkpZoomStep=iCurrentZoomStep;
		}else{ //direct controls it
			super.zoomCamera(value);
		}
	}
	
	/**
	 * 
	 * @return degrees
	 */
	public float getFOV(){
    return 
    	FastMath.atan(cam.getFrustumTop() / cam.getFrustumNear()) 
    	/ 
    	(FastMath.DEG_TO_RAD * .5f);
	}
	
	@Override
	protected void moveCamera(float value, boolean sideways) {
		if(!isAllowMove())return;
//		if(!sideways)fAccumulatedMoveTrust+=gettpf
		super.moveCamera(getCurrentVelocity(value), sideways);
	}
	
	protected float getCurrentVelocity(float value){
		float f=1;
		if(value<0)f=-1;
		return value+(f*fAccMvTm*fFlyAcceleration);
	}
	
	@Override
	protected void riseCamera(float value) {
		if(!isAllowMove())return;
		super.riseCamera(getCurrentVelocity(value));
	}
	
	public boolean isAllowZooming() {
		return bAllowZooming;
	}
	
	/**
	 * false if is not using zooming equipment (or is not an eagle)
	 * @param bAllowZooming
	 * @return
	 */
	/**
	 * 
	 * @param bAllowZooming
	 * @return
	 */
	public FlyByCameraX setAllowZooming(boolean bAllowZooming) {
		this.bAllowZooming = bAllowZooming;
		return this; //for beans setter
	}
	/**
	 * 
	 * @param fMinFOVdeg
	 * @param fMaxFOVdeg
	 * @param fFOVstep min of 2 steps (min fov, max fov)
	 * @return
	 */
	public FlyByCameraX setZoomLimits(float fMinFOVdeg, float fMaxFOVdeg, int iZoomSteps) {
		DetailedException.assertIsTrue("min steps", iZoomSteps>=2, iZoomSteps, fMinFOVdeg, fMaxFOVdeg);
		this.bEnableZoomStepsAndLimits=true;
		
		this.fMinFOVdeg = fMinFOVdeg;
		if(this.fMinFOVdeg<0.01f)this.fMinFOVdeg=0.01f; //TODO find a better value?
		
		this.fMaxFOVdeg = fMaxFOVdeg;
		
		this.iZoomSteps = iZoomSteps;
		
		afListZoom.clear();
		iZoomSteps--;
		float fZoomMaxDelta=fMaxFOVdeg-fMinFOVdeg;
		float fZoomStep=fZoomMaxDelta/iZoomSteps;
		for(int i=0;i<=iZoomSteps;i++){
			afListZoom.add(fMinFOVdeg+(i*fZoomStep));
		}
		iCurrentZoomStep=afListZoom.size()-1; // last/max one = no zoom
		
		return this; //for beans setter
	}
	
	public boolean isAllowMove() {
		return bAllowMove;
	}
	
	/**
	 * false when attached at the head of the player's entity 
	 * @param bAllowMove 
	 * @return
	 */
	public FlyByCameraX setAllowMove(boolean bAllowMove) {
		this.bAllowMove = bAllowMove;
		return this; //for beans setter
	}
	
	@Override
	public void setEnabled(boolean bEnable) {
		if(bOverrideKeepFlyCamDisabled && bEnable==true)return;
		
		if(isEnabled()!=bEnable){
			setEnabledRaw(bEnable);
		}
	}
	
	/**
	 * no other checks will be performed here
	 * @param bEnable
	 */
	protected void setEnabledRaw(boolean bEnable) {
		super.setEnabled(bEnable); //the super will also show the cursor but not hide it back!
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(getInputman()==null)return false;
				setEnabledRawTrick(bEnable);
				return true;
			}
		});
	}
	
	@Workaround
	protected void setEnabledRawTrick(boolean bEnable){
		/**
		 * Inversed request first!
		 * this trick is required to grant the cursor visibility state will
		 * actually be modified/applied/changed, otherwise it would be ignored as 
		 * the internal boolean would not have changed!
		 */
		getInputman().setCursorVisible(bEnable);
		
		// apply requested state
		getInputman().setCursorVisible(!bEnable);
	}

	protected InputManager getInputman(){
		if(inputman==null){
			if(G.i(Application.class)!=null){
				inputman=G.i(Application.class).getInputManager();
			}
		}
		return inputman;
	}
	
	private static class CompositeControl implements ICompositeRestrictedAccessControl{
		private CompositeControl(){}; }; private CompositeControl cc;
	
	public void update(float fTPF){
		if(tdMouseGrab.isReady(true)){
			if(isEnabled() && EnvironmentJmeI.i().getMouse().isCursorVisible()){
				//this is useful when debugging thru IDE that has an watch evaluation to ungrab the mouse directly thru lwjgl
				setEnabledRaw(true); //to grant it is properly set
			}
		}
		
		keyFlyCamMod.setPressedSpecialExternalContextKeyMode(cc,isEnabled());
		
		// fix/apply fov
		if(bEnableZoomStepsAndLimits){
			fixZoom();
			
			/**
			 * automatically zoom in/out to reach the requested zoom step value
			 */
			if(Math.abs(getFOV() - afListZoom.get(iCurrentZoomStep)) > 1f){ //compare with error margin
				float fZooming = (isZoomInverted()?1:-1)*(fChangeZoomStepSpeed*fTPF);
				if(getFOV() < afListZoom.get(iCurrentZoomStep)){
					fZooming*=-1f;
				}
				super.zoomCamera(fZooming);
			}
			
			fZoomedRotationSpeed=afListZoom.get(iCurrentZoomStep)/fMaxFOVdeg;
		}
		
		if(tdMI.isReady(true)){
			String strFOV="{ ";
			strFOV+="fov="+StringI.i().fmtFloat(getFOV(),2);
			if(bEnableZoomStepsAndLimits){
				/** (current zoom/toggleBkpZoom) [zoom list], zoomRotateSpeed */
				strFOV+="("+StringI.i().fmtFloat(afListZoom.get(iCurrentZoomStep),2)
					+"/"+afListZoom.get(iBkpZoomStep)+") ";
				strFOV+=afListZoom.toString()+", ";
				strFOV+="zmRtSpd="+fZoomedRotationSpeed;
			}
			strFOV+=" }";
			EnvironmentJmeI.i().putCustomInfo("CamFOVdeg", strFOV);
		}
	}

	private void fixZoom() {
		if(iCurrentZoomStep<0)iCurrentZoomStep=0;
		if(iCurrentZoomStep>getNoZoomStepIndex())iCurrentZoomStep=getNoZoomStepIndex();
	}

	private int getNoZoomStepIndex() {
		return afListZoom.size()-1;
	}

	public boolean isOverrideKeepFlyCamDisabled() {
		return bOverrideKeepFlyCamDisabled;
	}

	public float getAcceleration() {
		return fFlyAcceleration;
	}

	public FlyByCameraX setAcceleration(float fAcceleration) {
		this.fFlyAcceleration = fAcceleration;
		return this; //for beans setter
	}
	
	/**
	 * This actually sets a custom motion listener that can use the current position
	 * and the current velocity to move the camera in a customized way. 
	 * @param listener
	 */
	@Override
	public void setMotionAllowedListener(MotionAllowedListener listener) {
		/**
		 * KEEP because of the javadoc explanation!
		 */
		super.setMotionAllowedListener(listener);
	}

	public boolean isRotationAllowed() {
		return bRotationAllowed;
	}

	public FlyByCameraX setRotationAllowed(boolean bRotationAllowed) {
		this.bRotationAllowed = bRotationAllowed;
		return this; //for beans setter
	}
	
	/**
	 * if negative, will invert how the mouse wheel works
	 */
	@Override
	public void setZoomSpeed(float zoomSpeed) {
		/**
		 * keep this at least for the java doc
		 */
		super.setZoomSpeed(zoomSpeed);
	}
	
	/**
	 * 
	 * @return is control inverted
	 */
	public boolean toggleZoomInvert(){
		setZoomSpeed(-getZoomSpeed());
		return isZoomInverted();
	}
	
	public boolean isZoomInverted(){
		return getZoomSpeed()<0;
	}

	public float getChangeZoomStepSpeed() {
		return fChangeZoomStepSpeed;
	}

	public FlyByCameraX setChangeZoomStepSpeed(float fChangeZoomStepSpeed) {
		this.fChangeZoomStepSpeed = fChangeZoomStepSpeed;
		return this; 
	}

	public boolean isEnableZoomLimits() {
		return bEnableZoomStepsAndLimits;
	}

	public FlyByCameraX setEnableZoomLimits(boolean bEnableZoomLimits) {
		this.bEnableZoomStepsAndLimits = bEnableZoomLimits;
		return this; 
	}
}
