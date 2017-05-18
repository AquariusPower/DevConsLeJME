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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.state.AbstractAppState;
import com.jme3.font.BitmapText;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
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
public class EnvironmentJmeI extends AbstractAppState{
	public static EnvironmentJmeI i(){return GlobalManagerI.i().get(EnvironmentJmeI.class);}
	
	private MouseI mouse = new MouseI();
	private DisplayI display = new DisplayI();
	private ArrayList<IEnvironmentListener> alisteners = new ArrayList<IEnvironmentListener>();
	
	private float	fTPF;
	private float	fSumTPF;
	private int	iFrameCount;
	private int	iFPS;
	private BitmapText	btInfo;
	private boolean	bShowFPS;
	private Vector3f	v3fInfoLocation;
	private boolean	bShowCamPos;
	private boolean	bShowCamRot;
	private Node	nodeGui;
	private Node nodeInfo=new Node();
	private Geometry geomInfoBkg = new Geometry();
	
	public void configure(Node nodeGui){
		G.i(Application.class).getStateManager().attach(this);
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
	
	@Override
	public void update(float fTPF) {
		super.update(fTPF);
		
		this.fTPF=(fTPF);
		
		this.fSumTPF+=fTPF;
		this.iFrameCount++;
		if(fSumTPF>=1f){
			iFPS=iFrameCount;
			fSumTPF-=1f;
			iFrameCount=0;
		}
		
		if(getDisplay().wasResized()){
			for(IEnvironmentListener l:alisteners){
				l.displayResizedEvent(getDisplay().getWidth(), getDisplay().getHeight());
			}
		}
		
		updateInfo();
	}
	
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
	}
	public DisplayI getDisplay(){
		return display;
	}

	public static class MouseI{
		public boolean isButtonDown(int i){
			return Mouse.isButtonDown(i);
		}
		
		/**
		 * z is above everything else
		 * @return
		 */
		public Vector3f getPos3D() {
//			return MiscJmeI.i().toV3f(app.getInputManager().getCursorPosition(), MiscJmeI.i().getZAboveAllAtGuiNode());
			return new Vector3f(Mouse.getX(), Mouse.getY(), MiscJmeI.i().getZAboveAllAtGuiNode());
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
			return new Vector2f(Mouse.getX(), Mouse.getY());
		}
	}
	
	public MouseI getMouse() {
		return mouse;
	}

	public float getTPF() {
		return fTPF;
	}
	
	public float getFPS() {
		return iFPS;
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
	
	LinkedHashMap<String,String> hmCustomInfo = new LinkedHashMap<String,String>(); 
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
		
		if(bShowFPS)sb.append("FPS="+iFPS+strSep);
		if(bShowCamPos){
			sb.append("CamPos="
				+TextI.i().fmtVector3f(G.i(Application.class).getCamera().getLocation(),2)
				+strSep);
		}
		if(bShowCamRot){
			//TODO show a drawn line about Z at XY plane rotation, and another about up/downwards degrees
			sb.append("CamRotDeg="
				+TextI.i().fmtToDegrees(G.i(Application.class).getCamera().getRotation(),1)
				+strSep);
		}
		if(hmCustomInfo.size()>0){
			for(Entry<String, String> entry:hmCustomInfo.entrySet()){
				sb.append(entry.getKey()+"="+entry.getValue());
			}
		}
		
		btInfo.setText(sb.toString());
		geomInfoBkg.setMesh(new Quad(btInfo.getLineWidth(),btInfo.getHeight()));
		
		Vector3f v3f=v3fInfoLocation;
		if(v3f==null){
			v3f=new Vector3f(0,0,MiscJmeI.i().getZAboveAllAtGuiNode());
			v3f.y=btInfo.getHeight();
		}
		nodeInfo.setLocalTranslation(v3f);
		
		if(nodeInfo.getParent()==null)nodeGui.attachChild(nodeInfo);
	}

	public boolean isShowFPS() {
		return bShowFPS;
	}

	public EnvironmentJmeI setShowFPS(boolean bShowFPS) {
		this.bShowFPS = bShowFPS;
		return this; //for beans setter
	}

	public Vector3f getInfoLocation() {
		return v3fInfoLocation;
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
	
}
