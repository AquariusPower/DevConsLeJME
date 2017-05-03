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

import org.lwjgl.opengl.Display;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Insets3f;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;
import com.simsilica.lemur.style.ElementId;

/**
 * DevSelfNote: Misc lib class should not exist. As soon coehsion is possible, do it!
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PopupHintHelpListenerI implements CursorListener{
	public static PopupHintHelpListenerI i(){return GlobalManagerI.i().get(PopupHintHelpListenerI.class);}
	
	public static enum EPopup{
		strPopupHelpUserData,
		DialogStyleElementIdPopupHelp,
		;
		public String uId(){return EPopup.class.getName()+"/"+toString();}
	}
	private Label	lblPopupHelp;
	private String	strPopupHelp;
	private Node	nodeGui;
	private Container	cntrPopupHelp;
	private int iWrapAt=30;
	private Vector2f	v2fMousePosLast;
	private Spatial	sptLastPopupHelpValidTarget;

	@Override
	public void cursorButtonEvent(CursorButtonEvent event, Spatial target,			Spatial capture) {
	}

	@Override
	public void cursorEntered(CursorMotionEvent event, Spatial target,			Spatial capture) {
		updatePopupHelpText(target);
	}

	private void updatePopupHelpText(Spatial target) {
		if(target==null)target=sptLastPopupHelpValidTarget;
		
		if(target==null)return;
		
		strPopupHelp = getPopupHelp(target);
		if(strPopupHelp!=null){
			this.sptLastPopupHelpValidTarget = target;
		}
	}

	@Override
	public void cursorExited(CursorMotionEvent event, Spatial target,			Spatial capture) {
		strPopupHelp=null;
		sptLastPopupHelpValidTarget=null;
		updatePopupHelp(event.getLocation());
	}

	@Override
	public void cursorMoved(CursorMotionEvent event, Spatial target,			Spatial capture) {
		updatePopupHelp(event.getLocation());
	}
	
	private void updatePopupHelp(Vector2f v2fMousePos){
//		if(v2fMousePos==null){
//			v2fMousePos=this.v2fMousePosLast;
//		}else{
//			this.v2fMousePosLast=v2fMousePos;
//		}
		
		if(strPopupHelp!=null){
			lblPopupHelp.setText(strPopupHelp);
			lblPopupHelp.setInsets(new Insets3f(3, 3, 3, 3));
			
			Vector3f v3fSize = lblPopupHelp.getSize();
			
			float fDistFromCursor=10f;
			float fZAboveAll=1001; //TODO even above lemur mouse cursor's picker raycast?
			
			float fX = v2fMousePos.x-v3fSize.x/2;
			if(fX<0){
				fX=0;
			}else{
				float fDiff = (fX+cntrPopupHelp.getSize().x) - Display.getWidth();
				if(fDiff>0)fX-=fDiff;
			}
//			if(fX+cntrPopupHelp.getSize().x > Display.getWidth()){}
			
			float fY = v2fMousePos.y+v3fSize.y+fDistFromCursor;
			if(fY>Display.getHeight()){
				fY=Display.getHeight();
			}else{
				if( (fY - cntrPopupHelp.getSize().y) < 0 )fY=cntrPopupHelp.getSize().y;
			}
			
			cntrPopupHelp.setLocalTranslation(fX,fY,fZAboveAll);
			
			//TODO position always fully inside app screen limits!!!
			
			if(cntrPopupHelp.getParent()==null)nodeGui.attachChild(cntrPopupHelp);
		}else{
//			if(lblPopupHelp!=null){
				if(cntrPopupHelp.getParent()!=null){
					cntrPopupHelp.removeFromParent();
				}
//			}
		}
	}
	
	public String getPopupHelp(Spatial spt){
		return UserDataI.i().getUserDataPSH(spt, EPopup.strPopupHelpUserData.uId());
	}
	public void resetPopupHelp(Spatial spt){
		UserDataI.i().setUserDataPSH(spt, EPopup.strPopupHelpUserData.uId(), null);
	}
	public void setPopupHintHelp(Spatial spt, String strHelp){
		if(strHelp.length()>iWrapAt){
			Iterable<String> astr = Splitter
					.fixedLength(iWrapAt)
					.split(strHelp);
			strHelp = Joiner.on("\n").join(astr);
		}

		UserDataI.i().setUserDataPSH(spt, EPopup.strPopupHelpUserData.uId(), strHelp);
		CursorEventControl.addListenersToSpatial(spt, this);
//		spt.setUserData(EUserDataMiscJme.strPopupHelp.s(), strHelp);
	}

//	public void configure(Node nodeParent,String strPopupStyle) {
	public void configure(Node nodeParent) {
		this.nodeGui = nodeParent;
		
//		if(lblPopupHelp==null){
			lblPopupHelp = new Label(
				"nothing yet...", 
				new ElementId(EPopup.strPopupHelpUserData.uId()),
				GuiGlobals.getInstance().getStyles().getDefaultStyle() //BaseStyles.GLASS
//				strPopupStyle==null?GuiGlobals.getInstance().getStyles().getDefaultStyle():strPopupStyle //BaseStyles.GLASS
			); 
			lblPopupHelp.setName("Popup Help/Hint Label");
//		}
			lblPopupHelp.setColor(ColorRGBA.Blue);
		
		MiscJmeI.i().addToName(lblPopupHelp, PopupHintHelpListenerI.class.getSimpleName(), true);
		
		cntrPopupHelp = new Container();
		cntrPopupHelp.setBackground(new QuadBackgroundComponent(ColorRGBA.Cyan));
		
		cntrPopupHelp.addChild(lblPopupHelp, 0);
		MiscJmeI.i().addToName(cntrPopupHelp, PopupHintHelpListenerI.class.getSimpleName(), true);
//		((TbtQuadBackgroundComponent)cntrPopupHelp.getBackground()).setColor(ColorRGBA.Cyan);
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				updatePopupHelpText(null);
//				updatePopupHelp(null);
				return true;
			}
		}.setDelaySeconds(1).enableLoop());
	}

	public int getWrapAt() {
		return iWrapAt;
	}

	public void setWrapAt(int iWrapAt) {
		this.iWrapAt = iWrapAt;
	}
	
}
