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

import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.EffectArrow;
import com.github.devconslejme.misc.jme.EffectManagerStateI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.IndicatorI;
import com.github.devconslejme.misc.jme.IndicatorI.EIndicatorMode;
import com.github.devconslejme.misc.jme.IndicatorI.GeomIndicator;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.ResizablePanel.ResizerCursorListener;
import com.jme3.app.Application;
import com.jme3.input.InputManager;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
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
//	private FocusManagerState	focusman;
	private Vector3f	v3fInitialDragPos;
	private Vector3f	v3fInitialCurPos;
	private boolean	bIsReallyDragging;
	private CursorButtonEvent	eventButtonDown;
	private CursorButtonEvent	eventButtonUp;
	private Node	nodeGui;
	
	private static class MouseDragEffect{
//		private Application	app;
//		private InputManager	inputman;
		private Vector3f	v3fPressedPos;
		private EffectArrow	efDisplaced = new EffectArrow();//new EffectElectricity();
		private GeomIndicator	indicatorFrom;
		private Node	nodeGui;

		public MouseDragEffect(Node nodeGui) {
			this.nodeGui=nodeGui;
			EffectManagerStateI.i().add(efDisplaced);
			efDisplaced.setFollowToMouse(true);
			efDisplaced.setNodeParentest(nodeGui);
			ColorRGBA color = ColorI.i().colorChangeCopy(ColorRGBA.Blue, 0f, 1f);
			efDisplaced.setColor(color);
//			indicatorFrom = IndicatorI.i().createIndicator(ColorI.i().colorChangeCopy(ColorRGBA.Green, 0f, 0.5f));
			indicatorFrom = IndicatorI.i().createIndicator(color)
				.setDenyDestruction()
				.setTargetAndEnable(nodeGui)
				.setEnabled(false)
				.setRotateSpeed(0.5f);
			indicatorFrom.addModes(EIndicatorMode.PulseScaling);
			indicatorFrom.removeModes(EIndicatorMode.MoveBouncing);
			
//			app = GlobalManagerI.i().get(Application.class);
//			inputman = app.getInputManager();
			
			QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
					updateMouseEffect();
					return true;
				}
			}.enableLoopMode().setDelaySeconds(0.1f));
		}
		
		private void updateMouseEffect() {
			if(HWEnvironmentJmeI.i().getMouse().isCursorVisible() && HWEnvironmentJmeI.i().getMouse().isMouseCursorPressedButtons()>0){
				if(v3fPressedPos==null){
//					v3fPressedPos = MiscJmeI.i().toV3fZAA(inputman.getCursorPosition());
					v3fPressedPos = HWEnvironmentJmeI.i().getMouse().getPos3D();
//					v3fPressedPos.z=MiscJmeI.i().getZAboveAllAtGuiNode();
					efDisplaced.setFrom(v3fPressedPos);
					efDisplaced.setPlay(true);
					indicatorFrom.setPositionRelativeToTarget(v3fPressedPos);
					indicatorFrom.setEnabled(true);
//					nodeGui.attachChild(indicatorFrom);
				}
			}else{
				v3fPressedPos=null;
				indicatorFrom.setEnabled(false);
				efDisplaced.setPlay(false);
			}
		}
		
	}
	
	public void configure(Node nodeGui){
		this.nodeGui=nodeGui;
//		focusman = GlobalManagerI.i().get(Application.class).getStateManager().getState(FocusManagerState.class);
		new MouseDragEffect(nodeGui);
	}
	
	@Override
	public void cursorButtonEvent(CursorButtonEvent event, Spatial target, Spatial capture) {
//		if(event.isConsumed())return;
		if(capture==null)return; //TODO rare condition, how to simulate it? click ouside at nothing and release over a panel?
//		DragInfo di = UserDataI.i().getUserDataPSH(capture, DragInfo.class);
//		if(di==null){
//			MessagesI.i().warnMsg(this, "captured has no info?", capture, target, event);
//			return;
//		}
		
		int iButtonClickOk=0;
		if(capture.getControl(CursorEventControl.class).getMouseListener(ResizerCursorListener.class)!=null){
			/**
			 * the resizing will be preferred over the dragging for button 0 at the resizable border,
			 * so accept only button 2 to move the parentest (mainly thru the resizable border) 
			 */
			iButtonClickOk=2;
		}
		
//		DragInfo di = UserDataI.i().getUserDataPSH(capture, DragInfo.class);
//		if(event.getButtonIndex()==iButtonClickOk && di.bEnableDrag){
		if(event.getButtonIndex()==iButtonClickOk){
			if(event.isPressed()){
				eventButtonDown = event;
				DragInfo di = UserDataI.i().getMustExistOrNull(capture, DragInfo.class);
				if(di==null){
					MessagesI.i().warnMsg(this, "captured has no info?", capture, target, event);
					return;
				}
//				DragInfo di = UserDataI.i().getUserDataPSH(capture, DragInfo.class);
				if(!di.bEnableDrag)return; //prevent only starting to have a clean ending
				
				bDragging=true;
//				DragInfo di = UserDataI.i().getUserDataPSH(capture, DragInfo.class);
//				if(di==null || !di.bEnableDrag)return; //prevent only starting to have a clean ending
//				if(!di.bEnableDrag)return; //prevent only starting to have a clean ending
				
//				if(focusman.getFocus()==capture){
//					focusman.setFocus(null);
//				}
				
				//find parentest 
				Panel pnlParentest = di.pnlApplyDragAt; //capture.getUserData(JavaLangI.i().enumUId(EDrag.ApplyDragAt));
				if(pnlParentest==null)pnlParentest = SpatialHierarchyI.i().getParentestOrSelf(capture, Panel.class, true);
				
				// base dist calc
				v3fInitialDragPos = pnlParentest.getWorldTranslation().clone();
				v3fInitialCurPos = MiscLemurI.i().getCursorPosCopy(event);
				v3fDistToCursor=v3fInitialDragPos.subtract(v3fInitialCurPos);
				v3fDistToCursor.z=0; //DO NOT MESS WITH Z!!!!
				
				bIsReallyDragging=false;
				
//				ClickCommandAbsorptionI.i().absorbClickCommands(capture);
			}else{
				eventButtonUp = event;
				
				// just click, not dragging
//				if(!bIsReallyDragging)ClickCommandAbsorptionI.i().delegateClickCommands(capture);
				if(bIsReallyDragging)event.setConsumed();
				
				resetDrag();
			}
			
//			event.setConsumed();
		}else{
			resetDrag();
		}
	}
	
	private void resetDrag(){
		bDragging=false;
		pnlParentestBeingDragged=null;
		bIsReallyDragging=false;
	}
	
	public boolean isDragOverridingButtonUpClickEvent(CursorButtonEvent eventButtonUpExternal){
		return bIsReallyDragging;
	}
	
	@Override
	public void cursorMoved(CursorMotionEvent event, Spatial target,				Spatial capture) {
		if(capture==null)return; //TODO happens when clicking nowhere and dragging to some spatial?
//		DragInfo di = UserDataI.i().getUserDataPSH(capture, DragInfo.class);
//		if(di==null){
//			MessagesI.i().warnMsg(this, "captured has no info?", capture, target, event);
//			return;
//		}
		
		if(!bDragging)return; //((Panel)capture).getPreferredSize() ((Panel)capture).getSize()
//		DragInfo di = UserDataI.i().getUserDataPSH(capture, DragInfo.class);
//		if(!di.bEnableDrag) return;
		
		if(v3fInitialCurPos==null)return;
		
		Vector3f v3fCurPos = MiscLemurI.i().getCursorPosCopy(event);
		if(!bIsReallyDragging && v3fInitialCurPos.distance(v3fCurPos)<3f)return;
		
		bIsReallyDragging=true;
		
		// find parentest
		DragInfo di = UserDataI.i().getMustExistOrNull(capture, DragInfo.class);
		if(di==null){
			MessagesI.i().warnMsg(this, "captured has no info?", capture, target, event);
			return;
		}
//		DragInfo di = UserDataI.i().getUserDataPSH(capture, DragInfo.class);
		ResizablePanel pnlParentest = (ResizablePanel)di.pnlApplyDragAt;//capture.getUserData(JavaLangI.i().enumUId(EDrag.ApplyDragAt));
		if(pnlParentest==null)pnlParentest = SpatialHierarchyI.i().getParentestOrSelf(capture, ResizablePanel.class, true);
		
		// position parentest
		Vector3f v3f = v3fCurPos.add(v3fDistToCursor);
//			v3f.z=pnlParentest.getLocalTranslation().z; //DO NOT MESS WITH Z!!!!
		pnlParentest.setLocalTranslationXY(v3f);
		
		pnlParentestBeingDragged = pnlParentest;
		
		event.setConsumed();
	}

//	@Workaround
//	@Bugfix
//	@SuppressWarnings("unchecked")
//	private void absorbClickCommands(Spatial capture) {
//		if(!isDelegateClickCommands())return;
//		
//		if (capture instanceof Button) {
//			Button btn = (Button) capture;
//			List<Command<? super Button>> clickCommands = btn.getClickCommands();
//			if(clickCommands==null)return; 
//			
//			if(clickCommands.size()>0){
//				clickCommands = new ArrayList<Command<? super Button>>(clickCommands); //copy b4 clearing
//				btn.removeClickCommands(clickCommands.toArray(new Command[0]));
//				
//				ArrayList<Command<? super Button>> clickCommandsStored = 
//					UserDataI.i().getUserDataPSH(btn, EUserData.ClickCommands.s());
//				if(clickCommandsStored==null){
//					clickCommandsStored=new ArrayList<Command<? super Button>>();
//					UserDataI.i().setUserDataPSH(btn, EUserData.ClickCommands.s(), clickCommandsStored);
//				}
//				clickCommandsStored.addAll(clickCommands);
//			}
//		}
//	}
//
//	private static enum EUserData{
//		ClickCommands,
//		;
//		public String s(){return toString();}
//	}
	
//	/**
//	 * In case Button click commands are not working when the button has focus.
//	 * @param capture
//	 */
//	@Workaround
//	@Bugfix
//	private void delegateClickCommands(Spatial capture) {
//		if(!isDelegateClickCommands())return;
//		
////		if(focusman.getFocus()!=capture)return; //the ignored click commands bug happens only when the Button has focus
//		
//		if (capture instanceof Button) {
//			Button btn = (Button) capture;
//			ArrayList<Command<? super Button>> clickCommandsStored = 
//				UserDataI.i().getUserDataPSH(btn, EUserData.ClickCommands.s());
////				List<Command<? super Button>> clickCommands = btn.getClickCommands();
////				if(clickCommands!=null){
////					for(Command<? super Button> a:clickCommands){
//			if(clickCommandsStored!=null){
//				for(Command<? super Button> a:clickCommandsStored){
//					a.execute(btn);
//				}
//			}
//		}
//		
//	}

	public Panel getParentestBeingDragged(){
		return pnlParentestBeingDragged;
	}
	
	@Override
	public void cursorEntered(CursorMotionEvent event, Spatial target,				Spatial capture) {
//		highlightBackground(target,true);
	}
	
	@Override
	public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {
//		highlightBackground(target,false);
	}
	
	/**
	 * Keeping this here as a reference to re-understand and create some test case may be
	 * 
	 * this is a limitation or may be a bug on lemur:
	 * https://github.com/jMonkeyEngine-Contributions/Lemur/issues/50
	 * so as may be fixed in the future, this method may be disabled/removed one day.
	 * @param pnl
	 */
	@Bugfix
	@Workaround
	@Deprecated
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
		
		AbsorbClickCommandsI.i().absorbClickCommands(pnl);
		
		DragInfo di = new DragInfo();
		UserDataI.i().overwriteSafely(pnl,di);
		
		CursorEventControl.addListenersToSpatial(pnl, this);
		
		if(pnlApplyDragAt!=null){
//			pnl.setUserData(JavaLangI.i().enumUId(EDrag.ApplyDragAt), pnlApplyDragAt);
			di.pnlApplyDragAt=pnlApplyDragAt;
			
//			pnlApplyDragAt.setUserData(JavaLangI.i().enumUId(EDrag.ApplyingDragFrom), pnl);
			DragInfo diOther = new DragInfo();
			diOther.pnlApplyingDragFrom=pnl;
			UserDataI.i().overwriteSafely(pnlApplyDragAt,diOther);
		}
	}
	public void applyAt(Panel pnl) {
		applyAt(pnl,null);
		
		if(isHightlightToo() && !HoverHighlightEffectI.i().isAlreadySetFor(pnl)){
			GuiComponent gc = pnl.getBackground();
			if (gc instanceof QuadBackgroundComponent) {
				QuadBackgroundComponent qbc = (QuadBackgroundComponent) gc;
				HoverHighlightEffectI.i().applyAt(pnl, qbc);
			}else{
				MessagesI.i().debugInfo(this, "unable to apply "+HoverHighlightEffectI.class.getSimpleName(), pnl, gc);
			}
		}
	}
	
	private static class DragInfo{
		Panel pnlApplyDragAt;
		Panel pnlApplyingDragFrom;
		boolean bEnableDrag=true;
	}
	
//	private static enum EDrag implements IUDKey{
//		ApplyDragAt(Spatial.class), 
//		ApplyingDragFrom(Spatial.class),
//		bEnableDrag(Boolean.class),
////		ColorHighlight,
//		;
//		
//		EDrag(Class cl){
//			this.cl=cl;
//		}
//		
//		private Class cl;
//		
//		@Override
//		public String getUId(){return JavaLangI.i().enumUId(this);}
//
//		@Override
//		public Class getType() {
//			return cl;
//		}
//	}
	
//	private String getUserDataIdFor(EDrag e){
//		return DragParentestPanelListenerI.class.getName()+"/"+e;
//	}

	public boolean isHightlightToo() {
		return bHightlightToo;
	}

	public void setHightlightToo(boolean bHightlightToo) {
		this.bHightlightToo = bHightlightToo;
	}
	
	public void setEnabledAt(Spatial sptAt, boolean b) {
		DragInfo di = UserDataI.i().getMustExistOrNull(sptAt, DragInfo.class);
		DetailedException.assertNotNull(di,this,sptAt,b);
		di.bEnableDrag=b;
	}
}
