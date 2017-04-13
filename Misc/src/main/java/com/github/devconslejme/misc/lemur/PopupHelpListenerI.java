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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;
import com.simsilica.lemur.style.ElementId;

/**
 * DevSelfNote: Misc lib class should not exist. As soon coehsion is possible, do it!
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PopupHelpListenerI implements CursorListener{
	public static PopupHelpListenerI i(){return GlobalManagerI.i().get(PopupHelpListenerI.class);}
	
	public static enum EPopup{
		strPopupHelpUserData,
		DialogStyleElementIdPopupHelp,
		;
		
		public String s(){return this.toString();}
	}
	private Label	lblPopupHelp;
	private String	strPopupHelp;
	private Node	nodeGui;

	@Override
	public void cursorButtonEvent(CursorButtonEvent event, Spatial target,			Spatial capture) {
	}

	@Override
	public void cursorEntered(CursorMotionEvent event, Spatial target,			Spatial capture) {
		strPopupHelp = getPopupHelp(target);
	}

	@Override
	public void cursorExited(CursorMotionEvent event, Spatial target,			Spatial capture) {
		strPopupHelp=null;
		updatePopupHelp(event.getLocation());
	}

	@Override
	public void cursorMoved(CursorMotionEvent event, Spatial target,			Spatial capture) {
		updatePopupHelp(event.getLocation());
	}
	
	private void updatePopupHelp(Vector2f v2fMousePos){
		if(strPopupHelp!=null){
			lblPopupHelp.setText("["+strPopupHelp+"]");
			
			Vector3f v3fSize = lblPopupHelp.getSize();
			
			float fDistFromCursor=10f;
			float fZAboveAll=1000;
			lblPopupHelp.setLocalTranslation(v2fMousePos.x-v3fSize.x/2, v2fMousePos.y+v3fSize.y+fDistFromCursor, fZAboveAll);
			
			if(lblPopupHelp.getParent()==null)nodeGui.attachChild(lblPopupHelp);
		}else{
//			if(lblPopupHelp!=null){
				if(lblPopupHelp.getParent()!=null){
					lblPopupHelp.removeFromParent();
				}
//			}
		}
	}
	
	public String getPopupHelp(Spatial spt){
		return UserDataI.i().getUserDataPSH(spt, EPopup.strPopupHelpUserData.s());
	}
	public void setPopupHelp(Spatial spt, String strHelp){
		UserDataI.i().setUserDataPSH(spt, EPopup.strPopupHelpUserData.s(), strHelp);
		CursorEventControl.addListenersToSpatial(spt, this);
//		spt.setUserData(EUserDataMiscJme.strPopupHelp.s(), strHelp);
	}

	public void configure(Node nodeParent) {
		this.nodeGui = nodeParent;
		
//		if(lblPopupHelp==null){
			lblPopupHelp = new Label(
				"nothing yet...", 
				new ElementId(EPopup.strPopupHelpUserData.s()),
				GuiGlobals.getInstance().getStyles().getDefaultStyle() //BaseStyles.GLASS
			); 
			lblPopupHelp.setName("Popup Help/Hint Label");
//		}
	}
	
}
