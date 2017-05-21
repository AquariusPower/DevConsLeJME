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
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.app.Application;
import com.jme3.collision.MotionAllowedListener;
import com.jme3.input.CameraInput;
import com.jme3.input.FlyByCamera;
import com.jme3.input.InputManager;
import com.jme3.input.MouseInput;
import com.jme3.input.controls.MouseAxisTrigger;
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
	private float	fAccMvTm = 0.05f;
	private ArrayList<CallableX> acxList=new ArrayList<CallableX>();
	private boolean	bRotationAllowed=true;
	
	public void reBindKeys(){
    MiscJmeI.i().enqueueUnregisterKeyMappings( //these were set at super
  		CameraInput.FLYCAM_LEFT,
  		CameraInput.FLYCAM_RIGHT,
  		CameraInput.FLYCAM_UP,
  		CameraInput.FLYCAM_DOWN,
    		
    	CameraInput.FLYCAM_STRAFELEFT,
    	CameraInput.FLYCAM_STRAFERIGHT,
    	CameraInput.FLYCAM_FORWARD,
    	CameraInput.FLYCAM_BACKWARD,
    	CameraInput.FLYCAM_RISE,
    	CameraInput.FLYCAM_LOWER
    );
    
    // rotation
		KeyBindCommandManagerI.i().putBindCommandLater("Left",CameraInput.FLYCAM_LEFT,new CallableXAnon(){@Override	public Boolean call(){
			rotateCamera( getTPF(),initialUpVec);return true;}}.enableLoopMode());
		KeyBindCommandManagerI.i().putBindCommandLater("Right",CameraInput.FLYCAM_RIGHT,new CallableXAnon(){@Override	public Boolean call(){
			rotateCamera(-getTPF(),initialUpVec);return true;}}.enableLoopMode());
		KeyBindCommandManagerI.i().putBindCommandLater("Up",CameraInput.FLYCAM_UP,new CallableXAnon(){@Override	public Boolean call(){
			rotateCamera(-getTPF() * (invertY ? -1 : 1), cam.getLeft());return true;}}.enableLoopMode());
		KeyBindCommandManagerI.i().putBindCommandLater("Down",CameraInput.FLYCAM_DOWN,new CallableXAnon(){@Override	public Boolean call(){
			rotateCamera( getTPF() * (invertY ? -1 : 1), cam.getLeft());return true;}}.enableLoopMode());
		
		// re-add later the mouse rotation JME native mappings config
		restoreMouseAxisLater(CameraInput.FLYCAM_LEFT,MouseInput.AXIS_X,true);
		restoreMouseAxisLater(CameraInput.FLYCAM_RIGHT,MouseInput.AXIS_X,false);
		restoreMouseAxisLater(CameraInput.FLYCAM_UP,MouseInput.AXIS_Y,false);
		restoreMouseAxisLater(CameraInput.FLYCAM_DOWN,MouseInput.AXIS_Y,true);
		
    // movement, each key requires an independent queue object instance as they can be combined
		KeyBindCommandManagerI.i().putBindCommandLater("A",CameraInput.FLYCAM_STRAFELEFT ,new CallableXAnon(){@Override	public Boolean call(){
			moveCamera( getTPF(),true );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.enableLoopMode());
		KeyBindCommandManagerI.i().putBindCommandLater("D",CameraInput.FLYCAM_STRAFERIGHT,new CallableXAnon(){@Override	public Boolean call(){
			moveCamera(-getTPF(),true );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.enableLoopMode());
		
		KeyBindCommandManagerI.i().putBindCommandLater("W",CameraInput.FLYCAM_FORWARD    ,new CallableXAnon(){@Override	public Boolean call(){
			moveCamera( getTPF(),false);accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.enableLoopMode());
		KeyBindCommandManagerI.i().putBindCommandLater("S",CameraInput.FLYCAM_BACKWARD   ,new CallableXAnon(){@Override	public Boolean call(){
			moveCamera(-getTPF(),false);accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.enableLoopMode());
		
		KeyBindCommandManagerI.i().putBindCommandLater("Q",CameraInput.FLYCAM_RISE       ,new CallableXAnon(){@Override	public Boolean call(){
			riseCamera( getTPF()      );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.enableLoopMode());
		KeyBindCommandManagerI.i().putBindCommandLater("Z",CameraInput.FLYCAM_LOWER      ,new CallableXAnon(){@Override	public Boolean call(){
			riseCamera(-getTPF()      );accMvTrsTm(this);return true;}
			@Override	public void callAfterRemovedFromQueue() {resetMvTm(this);}}.enableLoopMode());
		
	}
	
	@Override
	protected void rotateCamera(float value, Vector3f axis) {
		if(!isRotationAllowed())return;
		super.rotateCamera(value, axis);
	}
	
	protected void resetMvTm(CallableXAnon cx) {
		acxList.remove(cx);
		if(acxList.size()==0)fAccMvTm=0f;
	}
	protected void accMvTrsTm(CallableXAnon cx){
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
		
		reBindKeys();
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				if(KeyCodeManagerI.i().getKeyListCopy().size()==0)return false;
//				reBindKeys();
//				return true;
//			}
//		});
		
		KeyBindCommandManagerI.i().putBindCommandLater("F5","hold to keep mouse cursor visible",new CallableXAnon(){
			@Override	public Boolean call(){
				bOverrideKeepFlyCamDisabled=true;
				setEnabled(false);
				return true;
			}
			
			@Override public void callAfterRemovedFromQueue() {
				bOverrideKeepFlyCamDisabled=false;
			};
			
		}.enableLoopMode());
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}.enableLoopMode().setDelaySeconds(0.5f).setName("EnsureMouseGrab"));
		
		setEnabledRaw(isEnabled()); //to initially set it properly
//		setEnabled(!isEnabled());setEnabled(isEnabled()); //trick to setup more things
	}
	
	@Override
	protected void zoomCamera(float value) {
		if(!isAllowZooming())return;
		super.zoomCamera(value);
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
		return value+(f*fAccMvTm*fAcceleration);
	}
	
	private float fAcceleration=0.1f;
	private InputManager	inputman;
	
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
	public FlyByCameraX setAllowZooming(boolean bAllowZooming) {
		this.bAllowZooming = bAllowZooming;
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
	
//	TimedDelay td=new TimedDelay(0.5f,"update mouse grab").setActive(true);
	
	public void update(float fTPF){
//		if(td.isReady(true)){
//		if(isEnabled() && getInputman().isCursorVisible()){
			if(isEnabled() && EnvironmentJmeI.i().getMouse().isCursorVisible()){
				//this is useful when debugging thru IDE that has an watch evaluation to ungrab the mouse directly thru lwjgl
				setEnabledRaw(true); //to grant it is properly set
			}
//		}
	}

	public boolean isOverrideKeepFlyCamDisabled() {
		return bOverrideKeepFlyCamDisabled;
	}

	public float getAcceleration() {
		return fAcceleration;
	}

	public FlyByCameraX setAcceleration(float fAcceleration) {
		this.fAcceleration = fAcceleration;
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
}
