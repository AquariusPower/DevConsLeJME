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
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import org.lwjgl.input.Mouse;
import org.lwjgl.opengl.Display;

import com.github.devconslejme.misc.Annotations.SimpleVarReadOnly;
import com.github.devconslejme.misc.EnvironmentI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.font.BitmapText;
import com.jme3.input.InputManager;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Quad;

/**
 * This way lwjgl3 may replace lwjgl more easily... or any other ways to collect the required values
 * can be used.
 * 
 * TODO prefer using whatever is not a direct read from lwjgl (while still being precise/actual) if possible
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class EnvironmentJmeI extends EnvironmentI{
	public static EnvironmentJmeI i(){return GlobalManagerI.i().retrieveOverridingSupers(EnvironmentJmeI.class,true,EnvironmentI.class);}
	
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
	private Application	app;
	private InputManager	inputman;
	private Camera	cam;
	private LinkedHashMap<String,String> hmCustomInfo = new LinkedHashMap<String,String>();
	private boolean	bShowMouseCursorPos;
	
	public void configure(Node nodeGui){
//		GlobalManagerI.i().putGlobal(EnvironmentI.class, this);
		
		app=G.i(Application.class);
		cam=app.getCamera();
		inputman=app.getInputManager();
		mouse = new MouseI(inputman);
		
		app.getStateManager().attach(new EnvState());
		this.nodeGui=nodeGui;
		if(this.nodeGui==null){
			if(G.i(SimpleApplication.class)!=null){
				this.nodeGui=G.i(SimpleApplication.class).getGuiNode();
			}
		}
		
		btInfo = new BitmapText(TextI.i().loadDefaultFont());
		btInfo.setSize(12);
		
		geomInfoBkg.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(new ColorRGBA(0,0,0,0.25f)));
		btInfo.setLocalTranslation(new Vector3f(0,0,1));
		nodeInfo.attachChild(btInfo);
		nodeInfo.attachChild(geomInfoBkg);
	}
	
	public class EnvState extends SimpleAppState{
		@Override
		public void update(float tpf) {
			super.update(tpf);
			EnvironmentJmeI.super.update(tpf);
			
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
		
		public boolean wasResized(){
			return Display.wasResized();
		}

		public Vector3f getAppWindowSize() {
			return new Vector3f(getWidth(),getHeight(),0);
		}

		public Vector3f getTopLeftCorner() {
			return new Vector3f(0,getHeight(),0);
		}

		public Vector3f getCenter() {
			return new Vector3f(getWidth()/2,getHeight()/2,0);
		}		
	}
	public DisplayI getDisplay(){
		return display;
	}

	public static class MouseI{
		private InputManager	inputman;
		private Vector2f	v2fLastPosWhileVisible = new Vector2f();

		public MouseI(InputManager inputman) {
			this.inputman = inputman;
		}

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
	
	public void putCustomInfo(String strKey,String strValue){
		hmCustomInfo.put(strKey, strValue);
	}
	
	protected void updateInfo(){
		if(
				!bShowFPS &&
				!bShowCamPos &&
				!bShowCamRot
		){
			btInfo.removeFromParent();
			return;
		}
		
		StringBuilder sb=new StringBuilder();
		String strSep="\n";
		
		if(bShowFPS)sb.append("FPS="+getFPS()+strSep);
		if(bShowMouseCursorPos)sb.append("MouseXY="+getMouse().getPos2D()+strSep);
		if(bShowCamPos){
			sb.append("CamPos="
				+TextI.i().fmtVector3f(cam.getLocation(),2)
				+strSep);
		}
		if(bShowCamRot){
			//TODO show a drawn line about Z at XY plane rotation, and another about up/downwards degrees
			sb.append("CamRotDeg="
				+TextI.i().fmtToDegrees(cam.getRotation(),1)
				+strSep);
		}
		if(hmCustomInfo.size()>0){
			for(Entry<String, String> entry:hmCustomInfo.entrySet()){
				sb.append(entry.getKey()+"="+entry.getValue()+strSep);
			}
		}
		
		btInfo.setText(sb.toString());
//		btInfo.setColor("(?m)^(?=[^=]*)=",ColorRGBA.Yellow); //(?m) multiline mode
		btInfo.setColor("(?m)^[^=]*=",ColorRGBA.Cyan); //(?m) multiline mode
		btInfo.setColor("[0-9.]*",ColorRGBA.Green); //(?m) multiline mode
		geomInfoBkg.setMesh(new Quad(btInfo.getLineWidth(),btInfo.getHeight()));
		
		Vector3f v3f=v3fInfoLocation;
		if(v3f==null){
			v3f=new Vector3f(0,0,MiscJmeI.i().getZAboveAllAtGuiNode());
			v3f.y=btInfo.getHeight();
		}
		nodeInfo.setLocalTranslation(v3f);
		
		if(nodeInfo.getParent()==null)nodeGui.attachChild(nodeInfo);
	}
	
	@SimpleVarReadOnly
	public String getInfo(){
		return btInfo.getText();
	}
	
	public boolean isShowFPS() {
		return bShowFPS;
	}

	public EnvironmentJmeI setShowFPS(boolean bShowFPS) {
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
	public EnvironmentJmeI setInfoLocation(Vector3f v3fInfo) {
		this.v3fInfoLocation = v3fInfo;
		return this; //for beans setter
	}

	public boolean isShowCamPos() {
		return bShowCamPos;
	}

	public EnvironmentJmeI setShowCamPos(boolean bShowCamPos) {
		this.bShowCamPos = bShowCamPos;
		return this; //for beans setter
	}

	public boolean isShowCamRot() {
		return bShowCamRot;
	}
	
	public EnvironmentJmeI setShowCamRot(boolean bShowCamRot) {
		this.bShowCamRot = bShowCamRot;
		return this; //for beans setter
	}

	public boolean isShowMouseCursorPos() {
		return bShowMouseCursorPos;
	}

	public EnvironmentJmeI setShowMouseCursorPos(boolean bShowMouseCursorPos) {
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

}
