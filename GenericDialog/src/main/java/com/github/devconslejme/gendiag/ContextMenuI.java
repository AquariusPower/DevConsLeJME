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
import java.util.HashMap;
import java.util.Map.Entry;

import javax.xml.crypto.dsig.spec.HMACParameterSpec;

import com.github.devconslejme.gendiag.HierarchySorterI.IHierarchySorter;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
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
	public static ContextMenuI i(){return GlobalInstanceManagerI.i().get(ContextMenuI.class);}
	
	private Node	nodeParent;
	private HierarchyResizablePanel	hrp;
	private String	strStyle;
	private Container	cntr;
	
	public static class ContextMenu{
		HashMap<String,Button> hmEntry = new HashMap<String,Button>();
		private Panel owner;
		
		public void setContextOwner(Panel owner){
			this.owner=owner;
		}
		
		public Panel getContextOwner(){
			return owner;
		}
		
		@SuppressWarnings("unchecked")
		public void putNewEntry(String strTextKey, Command<Button> cmd){
			Button btn = new Button(strTextKey);
			btn.addClickCommands(cmd);
			hmEntry.put(strTextKey, btn);
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
		public static ContextMenuOwnerListenerI i(){return GlobalInstanceManagerI.i().get(ContextMenuOwnerListenerI.class);}
		
		@Override
		protected void click(CursorButtonEvent event, Spatial target,				Spatial capture) {
			super.click(event, target, capture);
			
			if(event.getButtonIndex()!=1)return; //right mouse button
			if(!Panel.class.isInstance(capture))return;
			
			Panel pnlOwner = (Panel)capture;
			
			ContextMenu cm = UserDataI.i().getUserDataPSH(pnlOwner, ContextMenu.class);
			
			if(cm!=null){
				cm.setContextOwner(pnlOwner);
				ContextMenuI.i().showContextMenu(event,pnlOwner,cm);
				event.setConsumed();
			}
		}
	}

	public void configure(Node nodeParent) {
		strStyle = GuiGlobals.getInstance().getStyles().getDefaultStyle();
		
		hrp = new HierarchyResizablePanel(strStyle);
		
		cntr = new Container(strStyle);
		hrp.setContents(cntr);
		
		CursorEventControl.addListenersToSpatial(hrp, new ContextMenuListenerI());
		
		this.nodeParent=nodeParent;
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
	
	private void showContextMenu(CursorButtonEvent event, Spatial sptOwner, ContextMenu cm) {
		//TODO populate context menu
		cntr.clearChildren();
		
		int i=0;
		for(Entry<String, Button> entry : (cm.hmEntry).entrySet()){
			cntr.addChild(entry.getValue(), i++, 0);
		}
		
		nodeParent.attachChild(hrp);
		
		hrp.setPreferredSize(new Vector3f(200,30*cm.hmEntry.size(),hrp.getPreferredSize().z));
		hrp.setLocalTranslation(sptOwner.getWorldTranslation());
	}
	
	public void hideContextMenu() {
		hrp.removeFromParent();
	}


	public boolean isTheContextMenu(IHierarchySorter hs) {
		return (hs==hrp);
	}	
	
}
