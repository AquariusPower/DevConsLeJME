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

package com.github.devconslejme.misc.lemur;

import java.util.ArrayList;

import org.lwjgl.opengl.Display;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.jme3.app.Application;
import com.jme3.font.BitmapText;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.component.TextComponent;
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.core.GuiComponent;
import com.simsilica.lemur.core.GuiControl;
import com.simsilica.lemur.core.VersionedReference;
import com.simsilica.lemur.event.AbstractCursorEvent;
import com.simsilica.lemur.grid.GridModel;
import com.simsilica.lemur.style.ElementId;

/**
 * @DevSelfNote Misc lib class should not exist. As soon coehsion is possible, do it!
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MiscLemurI {
	public static MiscLemurI i(){return GlobalManagerI.i().get(MiscLemurI.class);}
	
	public Integer getEntryHeightPixels(ListBox lstbx){
		GridModel<Panel> gm = lstbx.getGridPanel().getModel();
		if(gm.getRowCount()==0)throw new DetailedException("list must not be empty");
		Panel pnl = gm.getCell(0, 0, null); // create a new cell
		float fHeight = pnl.getPreferredSize().getY();
		
		return (int)FastMath.ceil(fHeight);
	}
	
	public int getFontCharWidthForStyle(String strStyle){ //TODO check as it should be mono spaced font...
		// this detached label has no preferred size etc, so the line width will be as shrinked as possible
		String str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
		Label lbl = new Label(str,strStyle);
//		float fLineWidth = MiscJmeI.i().getBitmapTextFrom(lbl).getLineWidth();
		float fLineWidth = SpatialHierarchyI.i().getChildRecursiveExactMatch(lbl,BitmapText.class).getLineWidth();
		int iCharWidthPixels = Math.round(fLineWidth/=str.length());
		
		return iCharWidthPixels;
	}
	
	public TextEntryComponent getTextEntryComponentFrom(TextField tf){
		return (TextEntryComponent)tf.getControl(GuiControl.class).getComponent(TextField.LAYER_TEXT);
	}
	
	public TextComponent getTextComponentFrom(Label lbl){
		return lbl.getControl(GuiControl.class).getComponent(Label.LAYER_TEXT);
	}

	public boolean isMouseCursorOver(Panel pnl) {
		Vector2f v2f = GlobalManagerI.i().get(Application.class).getInputManager().getCursorPosition();
		
		Vector3f v3fPos = pnl.getLocalTranslation();
		Vector3f v3fSize = pnl.getSize();
		
		if(
				(v2f.x >= (v3fPos.x)) &&
				(v2f.x <= (v3fPos.x+v3fSize.x)) &&
				(v2f.y <= (v3fPos.y)) &&
				(v2f.y >= (v3fPos.y-v3fSize.y))
		){
			return true;
		}
		
		return false;
	}
	
	public Vector3f getCursorPosCopy(AbstractCursorEvent event){
		return new Vector3f(event.getX(),event.getY(),0);
	}
	
	public ArrayList<Panel> getAllListBoxItems(ListBox lstbx, boolean bIncludeSelector){
//		return MiscJmeI.i().getAllChildrenRecursiveFrom(lstbx.getGridPanel(), Panel.class, 1);
		ElementId elid = new ElementId(ListBox.ELEMENT_ID);
		ElementId elidItem = elid.child("item");
		ElementId elidSelector = elid.child(ListBox.SELECTOR_ID);
		
		ArrayList<Panel> apnl = SpatialHierarchyI.i().getAllChildrenRecursiveFrom(lstbx, Panel.class, null);
		for(Panel pnl:apnl.toArray(new Panel[0])){
			if(pnl.getElementId().equals(elidItem))continue;
			if(bIncludeSelector){
				if(pnl.getElementId().equals(elidSelector))continue;
			}
			apnl.remove(pnl);
		}
		
		return apnl;
	}
	
	/**
	 * this was just a guesser,
	 * maximization scope/limits would be required,
	 * nah... use a boolean!
	 * @param pnl
	 * @return
	 */
	@Deprecated
	private boolean isMaximized(Panel pnl) {
		Vector3f v3fSize = pnl.getSize();
		Vector3f v3fPos = pnl.getLocalTranslation();
		return 
			v3fSize.x==Display.getWidth() && 
			v3fSize.y==Display.getHeight() &&
			v3fPos.x==0 &&
			v3fPos.y==Display.getHeight()
			;
	}

	public void maximize(ResizablePanel pnl) {
		
	}
	public void maximize(ResizablePanel pnl, Vector3f v3fPos, Vector3f v3fSize) {
//		Vector3f v3fPos = pnl.getLocalTranslation(); //do not mess with z!!!
//		pnl.setLocalTranslation(new Vector3f(0,Display.getHeight(),v3fPos.z));
		pnl.setLocalTranslationXY(new Vector3f(0,Display.getHeight(),0));
		
//		Vector3f v3fSize = pnl.getSize(); //do not mess with z!!!
		pnl.setPreferredSizeWH(new Vector3f(Display.getWidth(),Display.getHeight(),v3fSize.z));
	}

	public void changeBackgroundColor(Button btnTitleText, ColorRGBA color) {
		changeBackgroundColor(btnTitleText, color, false);
	}
	public void changeBackgroundColor(Button btnTitleText, ColorRGBA color, boolean bForceNewBackground) {
		GuiComponent gc = btnTitleText.getBackground();
		QuadBackgroundComponent qbg =null;
		if(gc instanceof QuadBackgroundComponent){
			qbg = (QuadBackgroundComponent)gc;
		}
		
		if(qbg!=null){
			qbg.setColor(color);
		}else{
			if(bForceNewBackground){
				btnTitleText.setBackground(new QuadBackgroundComponent(color));
			}else{
				MessagesI.i().warnMsg(this, "background type should be", QuadBackgroundComponent.class, gc, btnTitleText, color);
			}
		}
	}
	
	public void moveToScreenCenterXY(PanelBase pnl) {
		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSizeCopy(pnl);
		pnl.setLocalTranslationXY(new Vector3f(
			Display.getWidth()/2f - v3fSize.x/2f, 
			Display.getHeight()/2f + v3fSize.y/2f, 
			0//			pnl.getLocalTranslation().z 
		));
//		pnl.getWorldBound().setCenter(new Vector3f(Display.getWidth()/2f,Display.getHeight()/2f,pnl.getLocalTranslation().z));
	}
	
	public static enum EReSizeApplyMode{
		Save,
		Restore,
		RestoreDefault,
		UpdateDefaultToCurrent,
		;
		public String s(){return toString();}
	}
	public static class SafeSize{
		public SafeSize(){};
		Vector3f v3fSafeSizeLast=null;
		Vector3f v3fSafeSizeDefault=null;
	}
//	private String strUDKeySafeSizeLast=ResizablePanel.class.getName()+"/SafeSize";
//	private String strUDKeySafeSizeDefault=ResizablePanel.class.getName()+"/SafeSizeDefault";
	public void safeSizeRecursively(EReSizeApplyMode eapply, Panel pnl) {
		SafeSize ss = UserDataI.i().getUserDataPSH(pnl, SafeSize.class, true);
		switch(eapply){
			case Restore:{
				Vector3f v3fSafeSize = ss.v3fSafeSizeLast;
				if(v3fSafeSize!=null)pnl.setPreferredSize(v3fSafeSize);
			}break;
			case RestoreDefault:{
				Vector3f v3fSafeSize = ss.v3fSafeSizeDefault;
				if(v3fSafeSize!=null)pnl.setPreferredSize(v3fSafeSize);
			}break;
			case Save:{
				ss.v3fSafeSizeLast=pnl.getPreferredSize().clone();
			}break;
			case UpdateDefaultToCurrent:{
				ss.v3fSafeSizeDefault=pnl.getPreferredSize().clone();
			}break;
		}
		
		for(Spatial sptChild:pnl.getChildren()){
			if (sptChild instanceof Panel) {
				safeSizeRecursively(eapply,(Panel)sptChild);
			}
		}
	}
	public void safeSizeInitialize(Panel pnl){
		SafeSize ss = UserDataI.i().getUserDataPSH(pnl, SafeSize.class, true);
		if(ss.v3fSafeSizeLast==null){ // 1st/initial safe size will be default
			MiscLemurI.i().safeSizeRecursively(EReSizeApplyMode.UpdateDefaultToCurrent,pnl);
		}
	}
	
	public void createLisbBoxVisibleItemsUpdater(ListBox lstbx){
		QueueI.i().enqueue(new CallableX(){
				private VersionedReference<Vector3f> vrv3f = new VersionedVector3f(lstbx.getSize()).createReference();
				
				@Override
				public Boolean call() {
					if(!vrv3f.update())return true;
					
					float fHeight = lstbx.getSize().y; //TODO inner container needs some time to be setup by lemur?
					
					int iLines = (int) (fHeight/MiscLemurI.i().getEntryHeightPixels(lstbx));
					iLines--; //to avoid the text being too close to each other
					
					if(lstbx.getVisibleItems()!=iLines){
						lstbx.setVisibleItems(iLines);
					}
					
					return true;
				}
			}
			.setName("UpdVisRows:"+lstbx.getName())
			.setDelaySeconds(0.25f)
			.enableLoop()
		);
	}
}
