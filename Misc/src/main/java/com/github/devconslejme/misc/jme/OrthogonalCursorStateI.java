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
import com.github.devconslejme.misc.StringI;
import com.jme3.app.Application;
import com.jme3.app.state.AppStateManager;
import com.jme3.font.BitmapFont.Align;
import com.jme3.font.BitmapFont.VAlign;
import com.jme3.font.BitmapText;
import com.jme3.font.Rectangle;
import com.jme3.input.InputManager;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.debug.Grid;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class OrthogonalCursorStateI extends SimpleAppState{
	public static OrthogonalCursorStateI i(){return GlobalManagerI.i().get(OrthogonalCursorStateI.class);}

	private Geometry	geom;
//	private Application	app;
	private Node	nodeParent;
//	private InputManager	inputman;
	private int	fSize;
//	private SimpleApplication	sappOpt;
	private BitmapText	bt;
	private ColorRGBA	color;
	private boolean	bRotating;
	private boolean	bRotateOnlyIfMouseMoves;
	private Node	nodeInfo;
	private Node	nodeHook;
//	private float	fAboveLemurCursorRayCast;
	private float	fRotateSpeed;
	private float	fDistanceToCursor;
//	private Vector2f v2fCursorPosPrevious;
	private Vector3f v3fCursorPosPrevious;
	private float	fGoodReadableRotateSpeedZ;
//	private Vector2f	v2fCursorPos;
	private Vector3f	v3fCursorPos;
	private boolean	bShowCursorInfo;
	
	public OrthogonalCursorStateI() {
		bRotateOnlyIfMouseMoves=true;
		fRotateSpeed=1f;
		fDistanceToCursor=100;
		v3fCursorPosPrevious = new Vector3f();
		fGoodReadableRotateSpeedZ = -0.0025f;
	}
	
	public void configure(Node nodeParent){
		AppI.i().attatchAppState(this);
		this.nodeParent=nodeParent;
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		super.initialize(stateManager, app);
		
		// cross
		geom=new Geometry(OrthogonalCursorStateI.class.getSimpleName());
		fSize=Math.max(HWEnvironmentJmeI.i().getDisplay().getWidth(),HWEnvironmentJmeI.i().getDisplay().getHeight())+10; //a bit more to hide the borders
		geom.setMesh(new Grid(3,3,fSize));
//		geom.lookAt(new Vector3f(v2f.x,v2f.y-1000000,0), Vector3f.UNIT_Z.mult(1000));
		geom.lookAt(new Vector3f(0,-1000000,0), Vector3f.UNIT_Z.mult(1000)); //TODO the huge Y and Z gave precision/quality, why?
		color = ColorRGBA.Yellow.clone();
		geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(
			ColorI.i().colorChangeCopy(color, 0f, 0.1f)));
		nodeParent.attachChild(geom);
		
		// info
		bt = StringTextJmeI.i().createBitmapTextMono("", ColorI.i().colorChangeCopy(color, 0f, 0.3f));
		nodeInfo=new Node("OrthoInfo");
		nodeHook = new Node();
		nodeHook.attachChild(bt);
		nodeInfo.attachChild(nodeHook);
		nodeParent.attachChild(nodeInfo);
	}
	
	private void updateCursorInfo(){
		String str=StringI.i().createTable(3,
				"xy", String.format("%.0f",v3fCursorPos.x), String.format("%.0f",v3fCursorPos.y),
				"max", ""+HWEnvironmentJmeI.i().getDisplay().getWidth(), ""+HWEnvironmentJmeI.i().getDisplay().getHeight(),
				"diff", ""+(HWEnvironmentJmeI.i().getDisplay().getWidth()-(int)v3fCursorPos.x), ""+(HWEnvironmentJmeI.i().getDisplay().getHeight()-(int)v3fCursorPos.y),
				"%",	String.format("%.1f", (v3fCursorPos.x/(float)HWEnvironmentJmeI.i().getDisplay().getWidth ())*100),
							String.format("%.1f", (v3fCursorPos.y/(float)HWEnvironmentJmeI.i().getDisplay().getHeight())*100)
			);
			
			bt.setText(str);
			Vector3f v3fSize=null;
			if(bRotating){
				v3fSize=new Vector3f(200,200,0);
				bt.setBox(new Rectangle(0, 0, v3fSize.x, v3fSize.y));
				bt.setAlignment(Align.Center);
				bt.setVerticalAlignment(VAlign.Center);
			}else{
				v3fSize = MiscJmeI.i().getBoundingBoxSizeCopy(bt);
				bt.setBox(null);
			}
			
			Vector3f v3fPos = new Vector3f();
			if(bRotating){
				boolean bRotateNow=true;
				float fSpeedMultWhileMovingCursor=1f;
				if(v3fCursorPosPrevious.equals(v3fCursorPos)){
					if(bRotateOnlyIfMouseMoves){
						bRotateNow=false;
					}
				}else{
					fSpeedMultWhileMovingCursor=v3fCursorPosPrevious.distance(v3fCursorPos)*5f;
				}
				
					v3fPos.addLocal(fDistanceToCursor,0,0);
					
				if(bRotateNow){
					Vector3f v3fRot = new Vector3f(0,0,fGoodReadableRotateSpeedZ*fRotateSpeed*fSpeedMultWhileMovingCursor);
					nodeInfo.rotate(v3fRot.x,v3fRot.y,v3fRot.z);
					
					v3fRot.negateLocal(); //invert the rotation to compensate the parent
					nodeHook.rotate(v3fRot.x,v3fRot.y,v3fRot.z);
				}
					
					v3fSize.y*=-1;
					bt.setLocalTranslation(v3fSize.mult(0.5f).negate()); //the center of the text at the hook spot
			}else{
				//resets
				nodeInfo.setLocalRotation(new Quaternion());
				nodeHook.setLocalRotation(new Quaternion());
				bt.setLocalTranslation(new Vector3f());
				
//				v3fPos.subtractLocal(v3fSize);
				v3fSize.x=0;
				v3fPos.addLocal(v3fSize);
			}
			nodeHook.setLocalTranslation(v3fPos);
//			nodeInfo.setLocalTranslation(MiscJmeI.i().toV3f(v2fCursorPos, MiscJmeI.i().getZAboveAllAtGuiNode()));
			nodeInfo.setLocalTranslation(v3fCursorPos);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		if(HWEnvironmentJmeI.i().getMouse().isCursorVisible()){
//			v2fCursorPos = EnvironmentJmeI.i().getMouse().getPos2D();//inputman.getCursorPosition();
			v3fCursorPos = HWEnvironmentJmeI.i().getMouse().getPos3D();
			geom.setLocalTranslation(v3fCursorPos.x-fSize, v3fCursorPos.y+fSize, v3fCursorPos.z);
//			geom.setLocalTranslation(v2fCursorPos.x-fSize, v2fCursorPos.y+fSize, MiscJmeI.i().getZAboveAllAtGuiNode() );
			
			if(isShowCursorInfo())updateCursorInfo();
			
			v3fCursorPosPrevious.set(v3fCursorPos);
		}
		
		setShow(HWEnvironmentJmeI.i().getMouse().isCursorVisible());
	}
	
	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		
		setShow(enabled);
	}
	
	public void setShow(boolean b){
		if(b){
			if(geom.getParent()==null)nodeParent.attachChild(geom);
		}else{
			if(geom.getParent()!=null)geom.removeFromParent();
		}
	}

	public boolean isRotating() {
		return bRotating;
	}

	public void setRotating(boolean bRotating) {
		this.bRotating = bRotating;
	}

	public float getRotateSpeed() {
		return fRotateSpeed;
	}

	public void setRotateSpeed(float fRotateSpeed) {
		this.fRotateSpeed = fRotateSpeed;
	}

	public float getDistanceToCursor() {
		return fDistanceToCursor;
	}

	public void setDistanceToCursor(float fDistanceToCursor) {
		this.fDistanceToCursor = fDistanceToCursor;
	}

	public float getGoodReadableRotateSpeedZ() {
		return fGoodReadableRotateSpeedZ;
	}

	public void setGoodReadableRotateSpeedZ(float fGoodReadableRotateSpeedZ) {
		this.fGoodReadableRotateSpeedZ = fGoodReadableRotateSpeedZ;
	}

	public boolean isRotateOnlyIfMouseMoves() {
		return bRotateOnlyIfMouseMoves;
	}

	public void setRotateOnlyIfMouseMoves(boolean bRotateOnlyIfMouseMoves) {
		this.bRotateOnlyIfMouseMoves = bRotateOnlyIfMouseMoves;
	}

	public boolean isShowCursorInfo() {
		return bShowCursorInfo;
	}

	public void setShowCursorInfo(boolean bShowCursorInfo) {
		this.bShowCursorInfo = bShowCursorInfo;
	}
	
}
