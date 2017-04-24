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

package com.github.devconslejme.gendiag;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import com.github.devconslejme.es.DialogHierarchySystemI;
import com.github.devconslejme.es.HierarchyComp;
import com.github.devconslejme.es.HierarchyComp.EField;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchy;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.es.EntityId;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorMotionEvent;
import com.simsilica.lemur.event.DefaultCursorListener;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class ContextMenuI {
	public static ContextMenuI i(){return GlobalManagerI.i().get(ContextMenuI.class);}
	
//	private Node	nodeParent;
	private ResizablePanel	rzpContextMenu;
	private String	strStyle;
	private Container	cntrContextOptions;
//	private Vector3f	v3fHierarchyParentInitialDisplacement;
	private EntityId	entid;
	private Label	lbl;
	private boolean	bShowDbgInfo = false;
	private Command<Button>	btncmd = new Command<Button>() {
		@Override
		public void execute(Button source) {
			rzpContextMenu.close();
		}
	};
	private CallableX	cxDbgInfo = new CallableX() {
		@Override
		public Boolean call() {
			HierarchyComp hc = DialogHierarchyStateI.i().getHierarchyComp(rzpContextMenu);
			
			if(!rzpContextMenu.isOpened())return true;
			
			//after diag hierarchy is ready
			if(hc.getLastFocusTime()==-1)return true;
			
			String str = hc.toString().replace(",", "\n");
			PopupHintHelpListenerI.i().setPopupHelp(lbl, str);
			MessagesI.i().debugInfo(ContextMenuI.this, str);
			
			return true;
		}
	}.setDelaySeconds(1f).setLoop(true);
	
	public static class ContextMenu{
		LinkedHashMap<String,Button> hmContextOptions = new LinkedHashMap<String,Button>();
		private Panel pnlSource;
		private ResizablePanel	hrpParent;
		
		/**
		 * to be set only when clicking from the listener here
		 * @param pnlSource
		 */
		private void setContextSource(Panel pnlSource){
			this.pnlSource=pnlSource;
		}
		
		public Panel getContextOwner(){
			return pnlSource;
		}
		
		@SuppressWarnings("unchecked")
		public void addNewEntry(String strTextKey, Command<Button> cmd){
			Button btn = new Button(strTextKey);
			btn.addClickCommands(cmd);
			hmContextOptions.put(strTextKey, btn);
		}

		public ResizablePanel getHierarchyParent() {
			return hrpParent;
		}
		
		public void setHierarchyParent(ResizablePanel hrpParent) {
			this.hrpParent=hrpParent;
		}
	}
	
	private class ContextMenuListenerI extends DefaultCursorListener{
		@Override
		public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {
			super.cursorExited(event, target, capture);
			//TODO not working well: hideContextMenu(); 
		}
	}
	
	public static class ContextMenuOwnerListenerI extends DefaultCursorListener{
		public static ContextMenuOwnerListenerI i(){return GlobalManagerI.i().get(ContextMenuOwnerListenerI.class);}
		
		@Override
		protected void click(CursorButtonEvent event, Spatial target,				Spatial capture) {
			super.click(event, target, capture);
			
			if(event.getButtonIndex()!=1)return; //right mouse button
			if(!Button.class.isInstance(capture))return;
			
			Button btn = (Button)capture;
			
			ContextMenu cm = UserDataI.i().getUserDataPSH(btn, ContextMenu.class);
			
			if(cm!=null){
				cm.setContextSource(btn);
				ContextMenuI.i().showContextMenu(event,btn,cm);
				event.setConsumed();
			}
		}
	}

	public void configure(){//Node nodeParent) {
		strStyle = GuiGlobals.getInstance().getStyles().getDefaultStyle();
		
		rzpContextMenu = DialogHierarchyStateI.i().createDialog(ContextMenuI.class.getSimpleName(), strStyle);
		entid = DialogHierarchyStateI.i().getEntityId(rzpContextMenu); //DialogHierarchySystemI.i().createEntity(ContextMenuI.class.getSimpleName());
		
		DialogHierarchyStateI.i().getVisuals(rzpContextMenu).ignorePositionRelativeToParent();
		
		MiscJmeI.i().addToName(rzpContextMenu, ContextMenuI.class.getSimpleName(), true);
		
		DialogHierarchySystemI.i().setHierarchyComp(entid, 
			EField.eHierarchyType, EHierarchy.Top,
			EField.bVolatileModal, true
		);
		
		rzpContextMenu.setAllEdgesEnabled(false); //it is here for the hierarchy (not the resizing)
		
		cntrContextOptions = new Container(strStyle);
		rzpContextMenu.setContents(cntrContextOptions);
		rzpContextMenu.setBackground(new QuadBackgroundComponent(ColorRGBA.Cyan));
		
		lbl = new Label("");
		
		CursorEventControl.addListenersToSpatial(rzpContextMenu, new ContextMenuListenerI());
		
		DialogHierarchyStateI.i().addRequestAutoFocus(rzpContextMenu);
		
		if(bShowDbgInfo )QueueI.i().enqueue(cxDbgInfo);
		
//		QueueI.i().enqueue(new CallableX() {
//				@Override
//				public Boolean call() {
//					if(rzp.isOpened()){
//	//					ResizablePanel rzpParent = DialogHierarchySystemI.i().getHierarchyParentGuiLinkFor(entid).getResizablePanel();
//						EntityId entidParent = DialogHierarchySystemI.i().getHierarchyComp(entid).getHierarchyParent();
//						ResizablePanel rzpParent = DialogHierarchyStateI.i().getOpenDialog(entidParent);
//						
//						if(rzpParent!=null){
//							rzp.setLocalTranslation(
//								rzpParent.getLocalTranslation().subtract(
//									v3fHierarchyParentInitialDisplacement));
//						}else{
//							rzp.close();
//						}
//					}
//					return true;
//				}
//			}	.setName("ContextMenuUpdate")
//				.setLoop(true)
//				.setDelaySeconds(0.1f)
//		);
	}
	
	
	public void attachContextMenuAtListBoxItems(ListBox lstbx, ContextMenu cm){
		ArrayList<Panel> apnl = MiscJmeI.i().getAllChildrenRecursiveFrom(lstbx.getGridPanel(), Panel.class, null);
		for(Panel pnl:apnl){
			attachContextMenuAt(pnl, cm);
		}
	}
	
	public void attachContextMenuAt(Spatial spt, ContextMenu cm){
		UserDataI.i().setUserDataPSH(spt, cm);
		CursorEventControl.addListenersToSpatial(spt,ContextMenuOwnerListenerI.i());
	}
	
	@SuppressWarnings("unchecked")
	private void showContextMenu(CursorButtonEvent event, Button btnOwner, ContextMenu cm) {
		//TODO populate context menu
		cntrContextOptions.clearChildren();
		
		int i=0;
		lbl.setText("Context:"+btnOwner.getText());
		DragParentestPanelListenerI.i().applyAt(lbl);
		cntrContextOptions.addChild(lbl, i++, 0);
		for(Entry<String, Button> entry : (cm.hmContextOptions).entrySet()){
			Button btnOption = entry.getValue();
			btnOption.addClickCommands(btncmd);
			cntrContextOptions.addChild(btnOption, i++, 0);
		}
		
//		_HierarchyComponent comp = cm.getOwner().getComponent(_HierarchyComponent.class);
		DialogHierarchyStateI.i().showDialogAsModal(cm.getHierarchyParent(), rzpContextMenu);
//		QueueI.i().enqueue(cxDbgInfo);

//		comp=_HierarchySystemI.i().workOn(comp).showAsHierarchyModal(hrp.getComponent(_HierarchyComponent.class));
//		nodeParent.attachChild(hrp);
		
		rzpContextMenu.setPreferredSize(new Vector3f(200, 30*cm.hmContextOptions.size(), rzpContextMenu.getPreferredSize().z));
		int iDisplacement=20;
		rzpContextMenu.setLocalTranslation(event.getX()-iDisplacement, event.getY()+iDisplacement, 0); // z will be fixed by diag hierarchy
		
//		v3fHierarchyParentInitialDisplacement = cm.getHierarchyParent().getLocalTranslation().subtract(
//			rzp.getLocalTranslation());
	}
	
	public void hideContextMenu() {
		rzpContextMenu.removeFromParent();
	}
	
	public boolean isTheContextMenu(ResizablePanel hs) {
		return (hs==rzpContextMenu);
	}


	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ContextMenuI [rzp=");
		builder.append(rzpContextMenu);
		builder.append(", strStyle=");
		builder.append(strStyle);
		builder.append(", cntr=");
		builder.append(cntrContextOptions);
		builder.append(", entid=");
		builder.append(entid);
		builder.append(", lbl=");
		builder.append(lbl);
		builder.append(", bShowDbgInfo=");
		builder.append(bShowDbgInfo);
		builder.append(", btncmd=");
		builder.append(btncmd);
		builder.append(", cxDbgInfo=");
		builder.append(cxDbgInfo);
		builder.append("]");
		return builder.toString();
	}
	
	public String getReport(){
		return toString()+","+DialogHierarchyStateI.i().getHierarchyComp(rzpContextMenu).toString();
	}
}
