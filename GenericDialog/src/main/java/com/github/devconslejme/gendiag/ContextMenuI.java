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

import java.util.HashMap;
import java.util.Map.Entry;

import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.jme3.app.Application;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.UserData;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
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
		
		@SuppressWarnings("unchecked")
		public void putNewEntry(String strTextKey, Command<Button> cmd){
			Button btn = new Button(strTextKey);
			btn.addClickCommands(cmd);
			hmEntry.put(strTextKey, btn);
		}
	}
	
	public static class ContextMenuListenerI extends DefaultCursorListener{
		public static ContextMenuListenerI i(){return GlobalInstanceManagerI.i().get(ContextMenuListenerI.class);}
		
		@Override
		protected void click(CursorButtonEvent event, Spatial target,				Spatial capture) {
			super.click(event, target, capture);
			
			if(event.getButtonIndex()!=2)return;
			
			ContextMenu cm = UserDataI.i().getUserDataPSH(capture, ContextMenu.class);
			
			if(cm!=null){
				ContextMenuI.i().showContextMenu(event,capture,cm);
				event.setConsumed();
			}
		}
	}

	public void configure(Node nodeParent) {
		strStyle = GuiGlobals.getInstance().getStyles().getDefaultStyle();
		hrp = new HierarchyResizablePanel(strStyle);
		cntr = new Container(strStyle);
		this.nodeParent=nodeParent;
	}
	
	
	public void attachContextMenuAt(Spatial spt, ContextMenu cm){
		UserDataI.i().setUserDataPSH(spt, cm);
		CursorEventControl.addListenersToSpatial(spt,ContextMenuListenerI.i());
	}
	
	private void showContextMenu(CursorButtonEvent event, Spatial capture, ContextMenu cm) {
		//TODO populate context menu
		cntr.clearChildren();
		
		int i=0;
		for(Entry<String, Button> entry:cm.hmEntry.entrySet()){
			cntr.addChild(entry.getValue(), i++, 0);
		}
		
		nodeParent.attachChild(hrp);
	}
	
	/**
	 * TODO call by the hierarchy sorter on focusing anything else
	 */
	public void hideContextMenu() {
		hrp.removeFromParent();
	}	
	
}
