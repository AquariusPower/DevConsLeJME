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
import java.util.List;

import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.jme.JmeSpatialHierarchyI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.ResizablePanel.ResizerCursorListener;
import com.jme3.app.Application;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.core.GuiComponent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;
import com.simsilica.lemur.event.DefaultMouseListener;
import com.simsilica.lemur.event.FocusMouseListener;
import com.simsilica.lemur.event.MouseEventControl;
import com.simsilica.lemur.focus.FocusManagerState;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DragParentestPanelListenerI implements CursorListener{
	public static DragParentestPanelListenerI i(){return GlobalManagerI.i().get(DragParentestPanelListenerI.class);}
	
	private boolean bDragging = false;
	private boolean bHightlightToo = true;
	private Vector3f	v3fDistToCursor;
	private Panel	pnlParentestBeingDragged;
	private FocusManagerState	focusman;
	private boolean	bDelegateClickCommands=true;
	private Vector3f	v3fInitialDragPos;
	private Vector3f	v3fInitialCurPos;
	private boolean	bIsReallyDragging;
	
	public void configure(){
		focusman = GlobalManagerI.i().get(Application.class).getStateManager().getState(FocusManagerState.class);
	}
	
	@Override
	public void cursorButtonEvent(CursorButtonEvent event, Spatial target, Spatial capture) {
//		if(event.isConsumed())return;
		if(capture==null)return; //TODO rare condition, how to simulate it? click ouside at nothing and release over a panel?
		
		int iButtonClickOk=0;
		if(capture.getControl(CursorEventControl.class).getMouseListener(ResizerCursorListener.class)!=null){
			/**
			 * the resizable will be preferred over the dragging for button 0,
			 * so accept only button 2 to move the parentest (mainly thru the resizable border) 
			 */
			iButtonClickOk=2;
		}
		
		if(event.getButtonIndex()==iButtonClickOk){
			bDragging=event.isPressed();
			if(bDragging){
//				if(focusman.getFocus()==capture){
//					focusman.setFocus(null);
//				}
				
				//find parentest 
				Panel pnlParentest = (Panel)capture.getUserData(getUserDataIdFor(EDrag.ApplyDragAt));
				if(pnlParentest==null)pnlParentest = JmeSpatialHierarchyI.i().getParentest(capture, Panel.class, true);
				
				// base dist calc
				v3fInitialDragPos = pnlParentest.getWorldTranslation().clone();
				v3fInitialCurPos = MiscLemurI.i().getCursorPosCopy(event);
				v3fDistToCursor=v3fInitialDragPos.subtract(v3fInitialCurPos);
				v3fDistToCursor.z=0; //DO NOT MESS WITH Z!!!!
				
				bIsReallyDragging=false;
				
				absorbClickCommands(capture);
			}else{
				pnlParentestBeingDragged=null;
				
				// just click, not dragging
				if(!bIsReallyDragging)delegateClickCommands(capture);
			}
			event.setConsumed();
		}
	}
	
	@Bugfix
	@SuppressWarnings("unchecked")
	private void absorbClickCommands(Spatial capture) {
		if(!isDelegateClickCommands())return;
		
		if (capture instanceof Button) {
			Button btn = (Button) capture;
			List<Command<? super Button>> clickCommands = btn.getClickCommands();
			if(clickCommands==null)return; 
			
			if(clickCommands.size()>0){
				clickCommands = new ArrayList<Command<? super Button>>(clickCommands); //copy b4 clearing
				btn.removeClickCommands(clickCommands.toArray(new Command[0]));
				
				ArrayList<Command<? super Button>> clickCommandsStored = 
					UserDataI.i().getUserDataPSH(btn, EUserData.ClickCommands.s());
				if(clickCommandsStored==null){
					clickCommandsStored=new ArrayList<Command<? super Button>>();
					UserDataI.i().setUserDataPSH(btn, EUserData.ClickCommands.s(), clickCommandsStored);
				}
				clickCommandsStored.addAll(clickCommands);
			}
		}
	}

	private static enum EUserData{
		ClickCommands,
		;
		public String s(){return toString();}
	}
	
	/**
	 * In case Button click commands are not working when the button has focus.
	 * @param capture
	 */
	@Bugfix
	private void delegateClickCommands(Spatial capture) {
		if(!isDelegateClickCommands())return;
		
//		if(focusman.getFocus()!=capture)return; //the ignored click commands bug happens only when the Button has focus
		
		if (capture instanceof Button) {
			Button btn = (Button) capture;
			ArrayList<Command<? super Button>> clickCommandsStored = 
				UserDataI.i().getUserDataPSH(btn, EUserData.ClickCommands.s());
//				List<Command<? super Button>> clickCommands = btn.getClickCommands();
//				if(clickCommands!=null){
//					for(Command<? super Button> a:clickCommands){
			if(clickCommandsStored!=null){
				for(Command<? super Button> a:clickCommandsStored){
					a.execute(btn);
				}
			}
		}
		
	}

	public Panel getParentestBeingDragged(){
		return pnlParentestBeingDragged;
	}
	
//	private void highlightBackground(Spatial spt, boolean bHighLight){
//		if(spt instanceof Panel){
//			Panel pnl = (Panel)spt;
//			GuiComponent gc = pnl.getBackground();
//			if (gc instanceof QuadBackgroundComponent) {
//				QuadBackgroundComponent qbc = (QuadBackgroundComponent) gc;
//				ColorRGBA color = qbc.getColor().clone();
//				
//				String strId = getUserDataIdFor(EDrag.ColorHighlight);
//				ColorRGBA colorStored = (ColorRGBA)spt.getUserData(strId);
//				if(colorStored==null){
//					colorStored=color.clone();
//					spt.setUserData(strId, colorStored);
//				}
//				
//				if(bHighLight){
//					qbc.setColor(ColorI.i().neglightColor(color));
//				}else{
//					qbc.setColor(colorStored);
//				}
//			}
//		}
//	}
	
	@Override
	public void cursorEntered(CursorMotionEvent event, Spatial target,				Spatial capture) {
//		highlightBackground(target,true);
	}
	
	@Override
	public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {
//		highlightBackground(target,false);
	}
	
	@Override
	public void cursorMoved(CursorMotionEvent event, Spatial target,				Spatial capture) {
		if(bDragging){ //((Panel)capture).getPreferredSize() ((Panel)capture).getSize()
			Vector3f v3fCurPos = MiscLemurI.i().getCursorPosCopy(event);
			if(!bIsReallyDragging && v3fInitialCurPos.distance(v3fCurPos)<3f)return;
			
			bIsReallyDragging=true;
			
			// find parentest
			ResizablePanel pnlParentest = (ResizablePanel)capture.getUserData(getUserDataIdFor(EDrag.ApplyDragAt));
			if(pnlParentest==null)pnlParentest = JmeSpatialHierarchyI.i().getParentest(capture, ResizablePanel.class, true);
			
			// position parentest
			Vector3f v3f = v3fCurPos.add(v3fDistToCursor);
//			v3f.z=pnlParentest.getLocalTranslation().z; //DO NOT MESS WITH Z!!!!
			pnlParentest.setLocalTranslationXY(v3f);
			
			pnlParentestBeingDragged = pnlParentest;
			
			event.setConsumed();
		}
	}

	public void applyAt(Panel pnl) {
		applyAt(pnl,null);
		if(isHightlightToo()){
			GuiComponent gc = pnl.getBackground();
			if (gc instanceof QuadBackgroundComponent) {
				QuadBackgroundComponent qbc = (QuadBackgroundComponent) gc;
				HoverHighlightEffectI.i().applyAt(pnl, qbc);
			}else{
				MessagesI.i().debugInfo(this, "unable to apply "+HoverHighlightEffectI.class.getSimpleName(), pnl, gc);
			}
		}
	}
	
	/**
	 * this is a limitation or may be a bug on lemur:
	 * https://github.com/jMonkeyEngine-Contributions/Lemur/issues/50
	 * so as may be fixed in the future, this method may be disabled/removed one day.
	 * @param pnl
	 */
	@Bugfix
	@Workaround
	private void mouseListenerConflictDenier(Panel pnl){
		if(false){
			MouseEventControl mec = pnl.getControl(MouseEventControl.class);
			if(mec!=null){
				if(
						mec.getMouseListener(DefaultMouseListener.class)!=null
						||
						mec.getMouseListener(FocusMouseListener.class)!=null
				){
					if (Button.class.isInstance(pnl) || TextField.class.isInstance(pnl)) {
						throw new DetailedException(
							"do not use with these Panel classes because they use these MouseListener "
							+"that always pre-consume the event and will be problematic with this class "
							+"because your custom click commands would then be ignored in case the button "
							+"has the requestFocus() (as the event is pre-consumed, "
							+"and it only happens if a CursorListener is also active, but why???) ",
							Button.class, TextField.class,
							FocusMouseListener.class, DefaultMouseListener.class, //not accessible: ButtonMouseHandler.class
							DragParentestPanelListenerI.class
						);
					}
				}
			}
		}
	}
	
	/**
	 * 
	 * @param pnl
	 * @param pnlApplyDragAt can be used to move a non attached/linked/parent panel
	 */
	public void applyAt(Panel pnl, Panel pnlApplyDragAt) {
		mouseListenerConflictDenier(pnl);
		
		CursorEventControl.addListenersToSpatial(pnl, this);
		if(pnlApplyDragAt!=null){
			pnl.setUserData(getUserDataIdFor(EDrag.ApplyDragAt), pnlApplyDragAt);
			pnlApplyDragAt.setUserData(getUserDataIdFor(EDrag.ApplyingDragFrom), pnl);
		}
	}
	
	private static enum EDrag{
		ApplyDragAt, 
		ApplyingDragFrom, 
//		ColorHighlight,
		;
	}
	
	private String getUserDataIdFor(EDrag e){
		return DragParentestPanelListenerI.class.getName()+"/"+e;
	}

	public boolean isHightlightToo() {
		return bHightlightToo;
	}

	public void setHightlightToo(boolean bHightlightToo) {
		this.bHightlightToo = bHightlightToo;
	}
	
	@Bugfix
	public boolean isDelegateClickCommands() {
		return bDelegateClickCommands;
	}
	
	@Bugfix
	public void setDelegateClickCommandsDisabled() {
		this.bDelegateClickCommands = false;
	}
}
