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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.function.Function;

import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.jme.EnvironmentI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI.SpatialInfo;
import com.github.devconslejme.misc.jme.UserDataI;
import com.google.common.base.Strings;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.bounding.BoundingBox;
import com.jme3.font.BitmapText;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.component.TbtQuadBackgroundComponent;
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
	
	/** theoretically, if such offset is ever updated, everywhere on lemur will also be */
	@Workaround
	private float	fMinSizeZ = new QuadBackgroundComponent().getZOffset();
	
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
			v3fSize.x==EnvironmentI.i().getDisplay().getWidth() && 
			v3fSize.y==EnvironmentI.i().getDisplay().getHeight() &&
			v3fPos.x==0 &&
			v3fPos.y==EnvironmentI.i().getDisplay().getHeight()
			;
	}

	public void maximize(PanelBase pnl) {
		maximize(pnl, 
			new Vector3f(0,EnvironmentI.i().getDisplay().getHeight(),0), 
			new Vector3f(EnvironmentI.i().getDisplay().getWidth(),EnvironmentI.i().getDisplay().getHeight(),0));
	}
	public void maximize(PanelBase pnl, Vector3f v3fPosXY, Vector3f v3fSizeWH) {
		pnl.setLocalTranslationXY(v3fPosXY);
		pnl.setPreferredSizeWH(v3fSizeWH);
	}

	public void changeBackgroundColor(Button btnTitleText, ColorRGBA color) {
		changeBackgroundColor(btnTitleText, color, false);
	}
	public void changeBackgroundColor(Button btnTitleText, ColorRGBA color, boolean bForceNewBackground) {
		GuiComponent gc = btnTitleText.getBackground();
		if(gc instanceof QuadBackgroundComponent){
			((QuadBackgroundComponent)gc).setColor(color);
		}else
		if(gc instanceof TbtQuadBackgroundComponent){
			((TbtQuadBackgroundComponent)gc).setColor(color);
		}else{
			if(bForceNewBackground){
				btnTitleText.setBackground(new QuadBackgroundComponent(color));
			}else{
				MessagesI.i().warnMsg(this, "while not overriding, background type should be", QuadBackgroundComponent.class, TbtQuadBackgroundComponent.class, gc, btnTitleText, color);
			}
		}
	}
//	public void changeBackgroundColor(Button btnTitleText, ColorRGBA color, boolean bForceNewBackground) {
//		GuiComponent gc = btnTitleText.getBackground();
//		QuadBackgroundComponent qbg =null;
//		if(gc instanceof QuadBackgroundComponent){
//			qbg = (QuadBackgroundComponent)gc;
//		}
//		
//		if(qbg!=null){
//			qbg.setColor(color);
//		}else{
//			if(bForceNewBackground){
//				btnTitleText.setBackground(new QuadBackgroundComponent(color));
//			}else{
//				MessagesI.i().warnMsg(this, "background type should be", QuadBackgroundComponent.class, gc, btnTitleText, color);
//			}
//		}
//	}
	
	public void moveToScreenCenterXY(PanelBase pnl) {
		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSizeCopy(pnl);
		pnl.setLocalTranslationXY(new Vector3f(
			EnvironmentI.i().getDisplay().getWidth()/2f - v3fSize.x/2f, 
			EnvironmentI.i().getDisplay().getHeight()/2f + v3fSize.y/2f, 
			0//			pnl.getLocalTranslation().z 
		));
//		pnl.getWorldBound().setCenter(new Vector3f(DisplayI.i()..getWidth()/2f,DisplayI.i()..getHeight()/2f,pnl.getLocalTranslation().z));
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
	/**
	 * TODO confirm: this method cannot be called every frame or may mess alignment and line wrap mode on BitmapText
	 * @param eapply
	 * @param pnl
	 */
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
	
	public void createListBoxVisibleItemsUpdater(ListBox lstbx){
		QueueI.i().enqueue(new CallableX(){
				private VersionedReference<Vector3f> vrv3f = new VersionedVector3f(lstbx.getSize()).createReference();
				
				@Override
				public Boolean call() {
					if(!vrv3f.update())return true;
					if(lstbx.getModel().size()==0)return true; //is empty
					
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

	public boolean isListBoxItem(Spatial capture) {
//		if(GridPanel.class.isInstance(capture.getParent())){
//			GridPanel gp=(GridPanel)capture;
//			if(ListBox.class.isInstance(gp.getParent())){
//				return true;
//			}
//		}
		for(Node node:SpatialHierarchyI.i().getAllParents(capture)){
			if(ListBox.class.isInstance(node))return true;
		}
		return false;
	}

	public Vector3f toV3f(Vector2f v2fTo) {
		return new Vector3f(v2fTo.x,v2fTo.y,MiscJmeI.i().getZAboveAllAtGuiNode());
	}
	
	/**
	 * @return
	 */
	public float getMinSizeZ() {
		return fMinSizeZ;
	}
	
	/**
	 * see {@link #debugPanelsZOrderRecursiveInfo(int, boolean, String)}
	 * @param iMaxDepth
	 * @param bOnlyPanels
	 * @param strNameFilter
	 * @return
	 */
	public String debugPanelsZOrderRecursiveInfoStr(int iMaxDepth, boolean bOnlyPanels,String strNameFilter){
		StringBuilder sb = new StringBuilder("");
		Collection<SpatialInfo> list = debugPanelsZOrderRecursiveInfo(iMaxDepth, bOnlyPanels, strNameFilter).values();
		sb.append("Total="+list.size()+"\n");
		for(SpatialInfo spti:list){
			sb.append(spti.getCustomValue()+"\n");
		}
		return sb.toString();
	}
	
	/**
	 * 
	 * @param iMaxDepth if 0 is unlimited
	 * @param bOnlyPanels
	 * @param strStartAt can be null. can be a hashcode ex.: "#12345", can be a spatial debug name
	 * @return
	 */
	public LinkedHashMap<Spatial,SpatialInfo> debugPanelsZOrderRecursiveInfo(int iMaxDepth, boolean bOnlyPanels, String strStartAt){
		Function<SpatialInfo,Boolean> funcDo = new Function<SpatialInfo,Boolean>() {
			@Override
			public Boolean apply(SpatialInfo spti) {
				if(iMaxDepth>0 && spti.getDepth()>iMaxDepth)return false;
				if(bOnlyPanels && !Panel.class.isInstance(spti.getSpatial()))return false;
//				if(strNameFilter!=null && !spti.getSpatial().getName().contains(strNameFilter))return false;
				
				Class cl = spti.getSpatial().getClass();
				String str=(Strings.repeat(" ",spti.getDepth())
					+"#='"+spti.getSpatial().hashCode()+"', "
					+"Wz="+spti.getSpatial().getWorldTranslation().z+", "
					+"Bh="+((BoundingBox)spti.getSpatial().getWorldBound()).getZExtent()*2f+", "
					+"cl='"+cl.getSimpleName()+"', "
					+"nm='"+spti.getSpatial().getName()+"', "
				);
				
				if(BitmapText.class.isAssignableFrom(cl)){
					str+="txt='"+((BitmapText)spti.getSpatial()).getText()+"'";
				}else
				if(Label.class.isAssignableFrom(cl)){
					str+="txt='"+((Label)spti.getSpatial()).getText()+"'";
				}else
				if(TextField.class.isAssignableFrom(cl)){
					str+="txt='"+((TextField)spti.getSpatial()).getText()+"'";
				}
				
				spti.setCustomValue(str);
				
				return true;
			}
		};
		
		Node node = GlobalManagerI.i().get(SimpleApplication.class).getGuiNode();
		if(strStartAt!=null){
			node = SpatialHierarchyI.i().getChildRecursiveExactMatch(node, new Function<Spatial,Boolean>() {
				@Override
				public Boolean apply(Spatial spt) {
					if(strStartAt.equals("#"+spt.hashCode()))return true;
					if(strStartAt.equals(spt.getName()))return true;
					return false;
				}
			});
		}
		
		//		ArrayList<String> astrList = new ArrayList<String>();
//		astrList.add(nodeFrom.getLocalTranslation().z);
//		return astrList;
//		SimpleApplication sapp = GlobalManagerI.i().get(SimpleApplication.class);
		return SpatialHierarchyI.i().doSomethingRecursively(node, funcDo, 0, null);
	}
}
