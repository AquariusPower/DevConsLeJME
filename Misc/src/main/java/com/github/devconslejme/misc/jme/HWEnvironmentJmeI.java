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
import java.util.Map.Entry;

import org.lwjgl.input.Mouse;
import org.lwjgl.opengl.Display;

import com.github.devconslejme.misc.Annotations.SimpleVarReadOnly;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.HWEnvironmentI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.TimedDelay;
import com.jme3.app.SimpleApplication;
import com.jme3.font.BitmapText;
import com.jme3.font.LineWrapMode;
import com.jme3.font.Rectangle;
import com.jme3.input.InputManager;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Quad;

/**
 * Hardware environment info.
 * 
 * This way lwjgl3 may replace lwjgl more easily... or any other ways to collect the required values
 * can be used.
 * 
 * TODO prefer using whatever is not a direct read from lwjgl (while still being precise/uptodate) if possible
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HWEnvironmentJmeI extends HWEnvironmentI{
	public static HWEnvironmentJmeI i(){return GlobalManagerI.i().retrieveOverridingSupers(HWEnvironmentJmeI.class,null,HWEnvironmentI.class);}
	
	private MouseI mouse;
	private DisplayI display = new DisplayI();
	private ArrayList<IEnvironmentListener> alisteners = new ArrayList<IEnvironmentListener>();
	
	private BitmapText	btInfo;
	private boolean	bShowFPS;
	private Vector3f	v3fInfoLocation;
	private boolean	bShowCamPos;
	private boolean	bShowCamRot;
	private Node	nodeGui;
	private Node nodeInfo=new Node();
	private Geometry geomInfoBkg = new Geometry();
//	private Application	app;
//	private InputManager	inputman;
//	private Camera	cam;
	private boolean	bShowMouseCursorPos;
	private SimpleApplication	sappOpt;
	private boolean bShowCustomInfo;
	private boolean bShowInfoOverride;
	
	public void configure(Node nodeGui){
//		GlobalManagerI.i().putGlobal(EnvironmentI.class, this);
		
//		app=G.i(Application.class);
//		cam=app.getCamera();
//		inputman=app.getInputManager();
//		mouse = new MouseI(inputman);
		mouse = new MouseI();
		
//		app.getStateManager().attach(new EnvState());
		AppI.i().attatchAppState(new EnvState());
		this.nodeGui=nodeGui;
		this.sappOpt=G.i(SimpleApplication.class);
		if(this.nodeGui==null){
			if(sappOpt!=null){
				this.nodeGui=G.i(SimpleApplication.class).getGuiNode();
			}
		}
		
		btInfo = StringTextJmeI.i().createBitmapTextMono("", ColorRGBA.White);
		
		geomInfoBkg.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(new ColorRGBA(0,0,0,0.5f)));
		btInfo.setLocalTranslation(new Vector3f(0,0,1));
		btInfo.setLineWrapMode(LineWrapMode.Word);
		nodeInfo.attachChild(btInfo);
		nodeInfo.attachChild(geomInfoBkg);
		nodeInfo.setName(HWEnvironmentJmeI.class.getSimpleName()+":info");
		
		KeyBindCommandManagerI.i().putBindCommandsLater("F6",new CallBoundKeyCmd() {
			@Override
			public Boolean callOnKeyReleased(int iClickCountIndex) {
				toggleShowInfoOverride();
				return true;
			}
		}.setName("ToggleShowDeveloperInfo"));
	}
	
	public class EnvState extends SimpleAppState{
		@Override
		public void update(float tpf) {
			super.update(tpf);
			HWEnvironmentJmeI.super.update(tpf);
			
			if(getDisplay().wasResized()){
				for(IEnvironmentListener l:alisteners){
					l.displayResizedEvent(getDisplay().getWidth(), getDisplay().getHeight());
				}
			}
			
			updateInfo();
		}
	}
	
//	public Vector3f getMouseCursorPosition(){
//		Vector2f v2f = inputman.getCursorPosition();
//		return new Vector3f(v2f.x,v2f.y,0);
//	}

	public static interface IEnvironmentListener{
		void displayResizedEvent(int iW, int iH);
	}
	public void addListener(IEnvironmentListener l){
		if(!alisteners.contains(l))alisteners.add(l);
	}
	
	public static class DisplayI{
		public int getWidth(){
			return Display.getWidth();
		}
		
		public int getHeight(){
			return Display.getHeight();
		}
		
		public void setResizable(boolean b){
			Display.setResizable(b);
		}
		public boolean isResizable(){
			return Display.isResizable();
		}
		
		/**
		 * this must be called from a single place!
		 * @return
		 */
		protected boolean wasResized(){
			return Display.wasResized();
		}

		public Vector3f getAppWindowSize() {
			return new Vector3f(getWidth(),getHeight(),0);
		}

		public Vector3f getTopLeftCorner() {
			return new Vector3f(0,getHeight(),0);
		}

		public Vector3f getCenter(Float fZ) {
			return new Vector3f(getWidth()/2,getHeight()/2,fZ==null ? MiscJmeI.i().getZAboveAllAtGuiNode() : fZ);
		}

		public int getMinSize() {
			return Math.min(getWidth(),getHeight());
		}		
		public int getMaxSize() {
			return Math.max(getWidth(),getHeight());
		}		
	}
	public DisplayI getDisplay(){
		return display;
	}

	public static class MouseI{
//		private InputManager	inputman;
		private Vector2f	v2fLastPosWhileVisible = new Vector2f();

//		public MouseI(InputManager inputman) {
//			this.inputman = inputman;
//		}

		public boolean isButtonDown(int i){
			return Mouse.isButtonDown(i);
		}
		
		/**
		 * z is above everything else
		 * @return
		 */
		public Vector3f getPos3D() {
//			return new Vector3f(Mouse.getX(), Mouse.getY(), MiscJmeI.i().getZAboveAllAtGuiNode());
			Vector2f v2f = getPos2D();
			return new Vector3f(v2f.x,v2f.y,MiscJmeI.i().getZAboveAllAtGuiNode());
		}
		
		public int isMouseCursorPressedButtons(){
			int i2=0;
			for(int i=0;i<9;i++){
				if(isButtonDown(i))i2++;
			}
	    return i2;
		}

		public Vector3f getPosWithMouseOnCenter(Vector3f v3fSize) {
			Vector3f v3fPos = getPos3D();
			v3fPos.x -= v3fSize.x/2f;
			v3fPos.y += v3fSize.y/2f;
			return v3fPos;
		}
		
		public Vector2f getPos2D() {
			/**
			 * TODO inputman.getCursorPosition() keeps changing even with flycam enabled, and when \ 
			 * it gets disabled, the last position with flycam previously disabled is restored as \
			 * soon the mouse moves any little bit, why that? is that good/useful in some way?
			 */
			if(isCursorVisible()){
				/**
				 * directly collet from lwjgl will avoid the inputman.getCursorPosition() 
				 * accumulated movement while the cursor was NOT visible ...
				 */
				v2fLastPosWhileVisible.set(Mouse.getX(),Mouse.getY()); // 
			}
			return v2fLastPosWhileVisible;
		}
		
		/**
		 * {@link InputManager#isCursorVisible()} is not insta updated
		 * @return
		 */
		public boolean isCursorVisible() {
			return !Mouse.isGrabbed();
		}
		
		/**
		 * use sparingly, mainly for debug pourposes
		 */
		public void forceUngrab(){
			Mouse.setGrabbed(false);
		}

		public void setCursorVisible(boolean bEnable) {
			AppI.i().setCursorVisible(bEnable);
		}
	}
	
	public MouseI getMouse() {
		return mouse;
	}

//	/**
//	 * 
//	 * @param v3fInfo if null will re-use last one
//	 * @param bShowFPS
//	 */
//	public void showInfo(Vector3f v3fInfo, boolean bShowFPS){
//		this.v3fInfo = v3fInfo;
//		this.bShowFPS = bShowFPS;
//	}
	
	private TimedDelay tdInfo = new TimedDelay(1f).setActive(true);
	protected void updateInfo(){
		if(!tdInfo.isReady(true))return;
		
		boolean bShow=false;
		if(bShowFPS)bShow=true;
		if(bShowCamPos)bShow=true;
		if(bShowCamRot)bShow=true;
		if(bShowCustomInfo)bShow=true;
		if(!bShowInfoOverride)bShow=false;
		if(!bShow){
			nodeInfo.removeFromParent();
			return;
		}
		
		StringBuilder sb=new StringBuilder();
		String strSep="\n";
		
		{//one line
			String strSep2=", ";
			if(bShowFPS)sb.append("FPS="+getFPS()+strSep2);
			if(bShowMouseCursorPos)sb.append("MouseXY="+getMouse().getPos2D()+strSep2);
			if(bShowCamPos){
				sb.append("CamPos="
					+StringTextJmeI.i().fmtVector3f(AppI.i().getCamWPosCopy(0f),2)
					+strSep2);
			}
			if(bShowCamRot){
				//TODO show a drawn line about Z at XY plane rotation, and another about up/downwards degrees
				sb.append("CamRotDeg="
					+StringTextJmeI.i().fmtToDegrees(AppI.i().getCamRotCopy(),1)
					+strSep2);
			}
			
			//TODO if not empty..
			sb.append(strSep);
		}
		
		if(hmCustomInfo.size()>0){
			for(Entry<String, String> entry:hmCustomInfo.entrySet()){
				sb.append(entry.getKey()+"="+entry.getValue()+strSep);
			}
		}
		
		/////////////////// prepare  the bitmap text //////////////////////
		btInfo.setBox(null); //to recalculate the height properly
//		btInfo.setText(sb.toString().trim()); //trim to remove last NL
		sb.append("\n");
		btInfo.setText(sb.toString()); // it requies the last NL to work properly
		//btInfo.setText("");btInfo.updateLogicalState()
//		btInfo.setColor("(?m)^(?=[^=]*)=",ColorRGBA.Yellow); //(?m) multiline mode
		btInfo.setColor("(?m)^[^=]*=",ColorRGBA.Cyan); //(?m) multiline mode
		btInfo.setColor("[0-9.]*",ColorRGBA.Green); //(?m) multiline mode
		geomInfoBkg.setMesh(new Quad(btInfo.getLineWidth(),btInfo.getHeight()));
//		geomInfoBkg.setLocalTranslation(new Vector3f());
//		geomInfoBkg.rotate(xAngle, yAngle, zAngle)
		geomInfoBkg.setLocalTranslation(0,-btInfo.getHeight(),0);
		
		Vector3f v3f=v3fInfoLocation;
		if(v3f==null){
			v3f=new Vector3f(0,0,MiscJmeI.i().getZAboveAllAtGuiNode());
			v3f.y=btInfo.getHeight();
//			v3f.y+=10;
		}
		nodeInfo.setLocalTranslation(v3f);
		
		btInfo.setBox(new Rectangle(0,0,HWEnvironmentJmeI.i().getDisplay().getWidth(),btInfo.getHeight()));
//		btInfo.setSize(15);
		
		if(nodeInfo.getParent()==null)nodeGui.attachChild(nodeInfo);
	}
	
	@SimpleVarReadOnly
	public String getInfo(){
		return btInfo.getText();
	}
	
	public boolean isShowFPS() {
		return bShowFPS;
	}

	public HWEnvironmentJmeI setShowFPS(boolean bShowFPS) {
		this.bShowFPS = bShowFPS;
		return this; //for beans setter
	}

	@SimpleVarReadOnly
	public Vector3f getInfoLocationCopy() {
		return v3fInfoLocation.clone();
	}

	/**
	 * 
	 * @param v3fInfo if null will be auto lower left corner
	 * @return
	 */
	public HWEnvironmentJmeI setInfoLocation(Vector3f v3fInfo) {
		this.v3fInfoLocation = v3fInfo;
		return this; //for beans setter
	}

	public boolean isShowCamPos() {
		return bShowCamPos;
	}

	public HWEnvironmentJmeI setShowCamPos(boolean bShowCamPos) {
		this.bShowCamPos = bShowCamPos;
		return this; //for beans setter
	}

	public boolean isShowCamRot() {
		return bShowCamRot;
	}
	
	public HWEnvironmentJmeI setShowCamRot(boolean bShowCamRot) {
		this.bShowCamRot = bShowCamRot;
		return this; //for beans setter
	}

	public boolean isShowMouseCursorPos() {
		return bShowMouseCursorPos;
	}

	public HWEnvironmentJmeI setShowMouseCursorPos(boolean bShowMouseCursorPos) {
		this.bShowMouseCursorPos = bShowMouseCursorPos;
		return this; 
	}
	
	@SimpleVarReadOnly
	public long getCurrentFrameId() {
		return getTotalFrameCount();
	}

	public long getFrameId(int iAddOrSub) {
		return getTotalFrameCount()-iAddOrSub;
	}

	public boolean isShowCustomInfo() {
		return bShowCustomInfo;
	}

	public HWEnvironmentJmeI setShowCustomInfo(boolean bShowCustomInfo) {
		this.bShowCustomInfo = bShowCustomInfo;
		return this; 
	}

	public boolean isShowInfoOverride() {
		return bShowInfoOverride;
	}

	public boolean toggleShowInfoOverride() {
		bShowInfoOverride=!bShowInfoOverride;
		return bShowInfoOverride;
	}
	public HWEnvironmentJmeI setShowInfoOverride(boolean bShowInfoOverride) {
		this.bShowInfoOverride = bShowInfoOverride;
		return this; 
	}


}
