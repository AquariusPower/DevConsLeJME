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
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchy;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
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
	private ResizablePanel	rzp;
	private String	strStyle;
	private Container	cntr;
	private Vector3f	v3fHierarchyParentDisplacement;
	private EntityId	entid;
	
	public static class ContextMenu{
		LinkedHashMap<String,Button> hmContextOptions = new LinkedHashMap<String,Button>();
		private Panel owner;
		private ResizablePanel	hrpParent;
		
		/**
		 * to be set only when clicking from the listener here
		 * @param owner
		 */
		private void setContextOwner(Panel owner){
			this.owner=owner;
		}
		
		public Panel getContextOwner(){
			return owner;
		}
		
		@SuppressWarnings("unchecked")
		public void addNewEntry(String strTextKey, Command<Button> cmd){
			Button btn = new Button(strTextKey);
			btn.addClickCommands(cmd);
			hmContextOptions.put(strTextKey, btn);
		}

		public ResizablePanel getOwner() {
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
			
			Button btnOwner = (Button)capture;
			
			ContextMenu cm = UserDataI.i().getUserDataPSH(btnOwner, ContextMenu.class);
			
			if(cm!=null){
				cm.setContextOwner(btnOwner);
				ContextMenuI.i().showContextMenu(event,btnOwner,cm);
				event.setConsumed();
			}
		}
	}

	public void configure(){//Node nodeParent) {
		strStyle = GuiGlobals.getInstance().getStyles().getDefaultStyle();
		
		rzp = new ResizablePanel(strStyle);
		
//		HierarchyComponent comp = hrp.createComponent(HierarchyComponent.class);
//		_HierarchyComponent comp = _HierarchySystemI.i().createComponentAt(hrp);
		entid = DialogHierarchySystemI.i().createEntity(ContextMenuI.class.getSimpleName());
		DialogHierarchyStateI.i().put(entid, rzp);
		
//		hrp.updateComponent(new HierarchyComponent(comp,true,null));
//		_HierarchySystemI.i().workOn(comp).setAsHierarchyTop();
		DialogHierarchySystemI.i().setHierarchyPriority(entid,EHierarchy.Top);
		
		rzp.setAllEdgesEnabled(false); //it is here for the hierarchy (not the resizing)
		
		cntr = new Container(strStyle);
		rzp.setContents(cntr);
		
		CursorEventControl.addListenersToSpatial(rzp, new ContextMenuListenerI());
		
//		this.nodeParent=nodeParent;
		
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				if(rzp.getParent()!=null){
//					ResizablePanel rzpParent = DialogHierarchySystemI.i().getHierarchyParentGuiLinkFor(entid).getResizablePanel();
					EntityId entidParent = DialogHierarchyStateI.i().getHierarchyParentOf(entid);
					ResizablePanel rzpParent = DialogHierarchyStateI.i().getResizablePanelFor(entidParent);
					
					rzp.setLocalTranslation(
						rzpParent.getLocalTranslation().subtract(
							v3fHierarchyParentDisplacement));
				}
				return true;
			}
		}.setName("ContextMenuUpdate").setLoop(true).setDelaySeconds(0.1f));
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
	
	private void showContextMenu(CursorButtonEvent event, Button btnOwner, ContextMenu cm) {
		//TODO populate context menu
		cntr.clearChildren();
		
		int i=0;
		Label lbl = new Label("Context:"+btnOwner.getText());
		DragParentestPanelListenerI.i().applyAt(lbl);
		cntr.addChild(lbl, i++, 0);
		for(Entry<String, Button> entry : (cm.hmContextOptions).entrySet()){
			cntr.addChild(entry.getValue(), i++, 0);
		}
		
//		_HierarchyComponent comp = cm.getOwner().getComponent(_HierarchyComponent.class);
		DialogHierarchyStateI.i().showDialogAsModal(
			DialogHierarchyStateI.i().getEntityIdFor(cm.getOwner()), entid);
//		comp=_HierarchySystemI.i().workOn(comp).showAsHierarchyModal(hrp.getComponent(_HierarchyComponent.class));
//		nodeParent.attachChild(hrp);
		
		rzp.setPreferredSize(new Vector3f(200,30*cm.hmContextOptions.size(),rzp.getPreferredSize().z));
		rzp.setLocalTranslation(event.getX(),event.getY(),0);//btnOwner.getWorldTranslation());
		
		v3fHierarchyParentDisplacement = cm.getOwner().getLocalTranslation().subtract(
			rzp.getLocalTranslation());
	}
	
	public void hideContextMenu() {
		rzp.removeFromParent();
	}


	public boolean isTheContextMenu(ResizablePanel hs) {
		return (hs==rzp);
	}	
	
}
