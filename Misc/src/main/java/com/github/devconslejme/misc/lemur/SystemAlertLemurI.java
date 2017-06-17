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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.SystemAlertI;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.EffectElectricity;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.IEffect;
import com.github.devconslejme.misc.jme.SystemAlertJmeI;
import com.github.devconslejme.misc.lemur.EffectsLemurI.EEffChannel;
import com.github.devconslejme.misc.lemur.EffectsLemurI.EEffState;
import com.jme3.bounding.BoundingBox;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.BorderLayout.Position;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;


/**
 * @DevSelfNote ALERT!: Keep independent of dialogs hierarchy! this is above all! 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SystemAlertLemurI extends SystemAlertJmeI {
	public static SystemAlertLemurI i(){return GlobalManagerI.i().retrieveOverridingSupers(SystemAlertLemurI.class, null, SystemAlertJmeI.class, SystemAlertI.class);}
	
	private Container	cntrAlert;
	private Label	lblAlertMsg;
	private Panel	pnlBlocker;
	
	/** the blocker blocks all gui elements to let some special feature be performed */
	private final TimedDelay	tdBlockerGlow = new TimedDelay(3f);
	
	private ColorRGBA	colorBlockerBkg;
	private boolean bAlertStayOnCenter = true;
	private boolean	bStartedShowEffect;
	private boolean	bAlertPanelIsReady;
	private IEffect	ieffAlert;
	@Deprecated
	private BoundingBox	bbGuiNodeWorldBoundPriorToAlert;
	private Node	nodeGui;
	private ColorRGBA colorEdgesDefault=ColorRGBA.Red.clone();
	private ArrayList<Panel> aedgeList = new ArrayList<Panel>();
	
	public void configure(Node nodeGui){
		this.nodeGui=nodeGui;
		
//		if(!GlobalManagerI.i().isSet(SystemAlertI.class)){
//			GlobalManagerI.i().putGlobal(SystemAlertI.class, this); //overrides global
//		}
//		if(!GlobalManagerI.i().isSet(SystemAlertJmeI.class)){
//			GlobalManagerI.i().putGlobal(SystemAlertJmeI.class, this); //overrides global
//		}
		
	}
	
	/**
	 * 
	 * @param asteFrom
	 * @param bKeepGuiBlockerOnce useful when going to retry having subsequent alerts
	 */
	@Override
	public void hideSystemAlert(StackTraceElement[] asteFrom, boolean bKeepGuiBlockerOnce) {
		super.hideSystemAlert(asteFrom,bKeepGuiBlockerOnce);
		cntrAlert.removeFromParent();
		cntrAlert=null;
		
//		lblAlertMsg.removeFromParent();
//		lblAlertMsg=null;
//		if(!bKeepGuiBlockerOnce){
//			pnlBlocker.removeFromParent();
//			pnlBlocker=null;
//		}
		
		bStartedShowEffect=false;
		
		bAlertPanelIsReady=false;
		
		bbGuiNodeWorldBoundPriorToAlert = null;
		
//		bAllowNewEffectCreation=false;
		if(!bKeepGuiBlockerOnce){
			pnlBlocker.removeFromParent();
			pnlBlocker=null;
			
//			ManageEffectsJmeStateI.i().discardEffectsForOwner(this);
			if(ieffAlert!=null)ieffAlert.setPlay(false);
//			ieffAlert=null;
//			bAllowNewEffectCreation=true;
		}
		
		setAlertSpatial(null);
	}
	
	@Override
	public StackTraceElement[] showSystemAlert(String strMsg, Object objActionSourceElement) {
//		bAlertStayOnCenter=false; //follow mouse by default
		
		if(bbGuiNodeWorldBoundPriorToAlert==null){
			//TODO should this be updated as things may change while the alert is running? but it also would require many other things to be updated...
			bbGuiNodeWorldBoundPriorToAlert = (BoundingBox)nodeGui.getWorldBound().clone();
		}
		
		StackTraceElement[] aste = super.showSystemAlert(strMsg,objActionSourceElement);
		
		if(getAlertAsPanel()==null)createAlert();
		
		if(!nodeGui.hasChild(cntrAlert)){
			nodeGui.attachChild(cntrAlert);
//			EEffChannel.ChnGrowShrink.play(EEffState.Show, cntrAlert, getPos(EElement.Alert,null));
		}
		
//		if(!lblAlertMsg.getText().equals(strMsg)){
//			lblAlertMsg.setText(strMsg);
//		}
		
//		updateAlertPosSize();
		
		return aste;
	}
	
	/**
	 * the alert panel is above the lemur picking ray!
	 * so the blocker will receive all clikc events!!!
	 */
	private static class BlockerClickListener extends CursorListenerX{
		@Override
		protected boolean click(CursorButtonEvent event, Spatial target, Spatial capture) {
//			/**
//			 * lemur seems to not let mouse cursor buttons to be forwared to JME other core listeners 
//			 * so it has to be done here.
//			 * TODO confirm that? if this could be avoided by letting the JME action listener at KeyCodeConfigureForJme do all the work? 
//			 */
//			KeyCodeManagerI.i().refreshMouseButtonPressedState(event.getButtonIndex(), event.isPressed());
			
			SystemAlertLemurI.i().tdBlockerGlow.reactivate(); //a simple effect to show it received the click
			
			return true;
		}
	}
	private BlockerClickListener bcl=new BlockerClickListener();
	
	private void createAlert(){
		if(pnlBlocker==null){
			// old trick to prevent access to other gui elements easily! :D
			pnlBlocker = new Button(""); //TODO must be button?
			pnlBlocker.setName("AlertGuiBlocker");
			colorBlockerBkg = ColorRGBA.Red.clone(); //new ColorRGBA(1f,0,0,1);//0.25f);
			pnlBlocker.setBackground(new QuadBackgroundComponent(colorBlockerBkg));
			tdBlockerGlow.setActive(true);
//			GlobalGUINodeI.i().attachChild(pnlBlocker);
			nodeGui.attachChild(pnlBlocker);
			SizeAndLocationI.i().setPreferredSize(pnlBlocker, HWEnvironmentJmeI.i().getDisplay().getAppWindowSize());
			pnlBlocker.setLocalTranslation(getPosForElement(EElement.Blocker, null));
//			MiscLemurI.i().setLocalTranslationXY(pnlBlocker, EnvironmentI.i().getDisplay().getTopLeftCorner());
//			MiscLemurI.i().setLocalTranslationZ(pnlBlocker, MiscJmeI.i().getZAboveAllAtGuiNode()-MiscLemurI.i().getMinSizeZ()); //just below the lemur raycast
			CursorEventControl.addListenersToSpatial(pnlBlocker, bcl);
		}
		
		// the alert container
		QuadBackgroundComponent qbc;
		
		cntrAlert = new Container(new BorderLayout(),getStyle());
		cntrAlert.setName("AlertContainer");
		setAlertSpatial(cntrAlert);
		
		EEffChannel.ChnGrowShrink.applyEffectsAt(cntrAlert);
//		LemurEffectsI.i().addEffectTo(cntrAlert, LemurEffectsI.i().efGrow);
		
		//yellow background with border margin
		lblAlertMsg = new Label("",getStyle());
		lblAlertMsg.setColor(ColorRGBA.Cyan.clone());
		lblAlertMsg.setShadowColor(ColorRGBA.Yellow.clone());
		lblAlertMsg.setShadowOffset(new Vector3f(1,1,0));
		lblAlertMsg.setBackground(null);
		qbc=new QuadBackgroundComponent(ColorI.i().colorChangeCopy(ColorRGBA.Blue, 0f, 0.75f));
		qbc.setMargin(10f, 10f);
		lblAlertMsg.setBorder(qbc);
		lblAlertMsg.setFontSize(18);
		cntrAlert.addChild(lblAlertMsg, BorderLayout.Position.Center);
		
		//edges countour in red
		for(BorderLayout.Position eEdge:new Position[]{Position.East,Position.West,Position.North,Position.South}){
			addAlertEdge(eEdge, ColorRGBA.Red.clone(), new Vector3f(2f,2f,0.1f));
		}
		
	}
	
	@Override
	public boolean isAlertReady(){
		return cntrAlert!=null && cntrAlert.getParent()!=null;
	}
	
	public Panel getAlertAsPanel() {
		return (Panel)super.getAlertSpatial();
	}
	
	@Override
	protected void doCapture(float fTPF, CallableX cxQueuedCall) {
		cxQueuedCall.setDelaySeconds(0.05f); //faster than default
		
		updateMessage();
		
		updateIfPanelIsReady();
		
		if(pnlBlocker!=null){
			ColorI.i().updateColorFading(tdBlockerGlow, colorBlockerBkg, true, 0.25f, 0.35f);
		}
	}
	
	@Override
	protected void endCaptureUserInput() {
		super.endCaptureUserInput();
		//TODO do something here?
	}
	
	private void updateMessage(){
		if(lblAlertMsg!=null)lblAlertMsg.setText(getFullMessage());
	}
	
	private void updateEdgesColor(ColorRGBA c){
		if(c==null)c=colorEdgesDefault;
		for(Panel pnl:aedgeList){
			((QuadBackgroundComponent)pnl.getBackground()).setColor(c);
		}
	}
	
	private void updateIfPanelIsReady(){
		if(getAlertAsPanel()==null)return;
		
		Vector3f v3fLblSize = getAlertAsPanel().getSize();
		if(v3fLblSize.length()==0)return;
		
		bAlertPanelIsReady=true;
		
		if(!bStartedShowEffect){
			EEffChannel.ChnGrowShrink.play(EEffState.Show, cntrAlert, getPosForElement(EElement.Alert,null));
			
			createAlertLinkEffect();
			
			bStartedShowEffect=true;
		}
		
//		if(getAlertUserInteractionIndicator()!=null){
		if(isDynamicInfoSet()){
			updateEdgesColor(ColorRGBA.Cyan.clone());
		}else{
			updateEdgesColor(null);
		}
		
		if(isAlertStayOnCenter()){
			SizeAndLocationI.i().moveToScreenCenterXY(getAlertAsPanel());
		}else{
			SizeAndLocationI.i().setLocalTranslationXY(getAlertAsPanel(), getPosForElement(EElement.Alert,v3fLblSize));
		}
	}

	public String getStyle(){
		return GuiGlobals.getInstance().getStyles().getDefaultStyle();
	}
	
	private void addAlertEdge(BorderLayout.Position eEdge, ColorRGBA color, Vector3f v3fSize){
		QuadBackgroundComponent qbc = new QuadBackgroundComponent(color);
		Label lbl=new Label("",getStyle());
		lbl.setBackground(qbc);
		lbl.setPreferredSize(v3fSize); //TODO queue this?
		
		aedgeList.add(lbl);
		
		cntrAlert.addChild(lbl, eEdge);
	}

	private void createAlertLinkEffect() {
		if(ieffAlert==null){
			ieffAlert = new EffectElectricity().setColor(ColorRGBA.Yellow);
		}
		
		if(getActionSourceElement()!=null){
			ieffAlert.setFollowFromTarget(getActionSourceElement(), new Vector3f(0,0,1));
		}else{
			// use mouse pos
			ieffAlert.setFromTo( getPosForElement(EElement.Effects, HWEnvironmentJmeI.i().getMouse().getPos3D()), 
				getAlertAsPanel().getLocalTranslation());
		}
		
		ieffAlert.setFollowToTarget(cntrAlert, null);
		
		ieffAlert.setPlay(true);
	}
	
	private static enum EElement{
		/**
		 * BEWARE!!! this enums order MATTERS!!!
		 */
		Blocker, //belowest
		Alert, 
		Effects, //toppest
	}
	
	/**
	 * the alert panel is above the lemur picking ray!
	 * 
	 * @param e
	 * @param v3fRef see the code, it may be null or used in different ways
	 * @return
	 */
	private Vector3f getPosForElement(EElement e, Vector3f v3fRef){
		if(v3fRef!=null)v3fRef=v3fRef.clone(); //safety
		Vector3f v3fWdwSize = HWEnvironmentJmeI.i().getDisplay().getAppWindowSize();
		
		/**
		 * TODO the Z displacement should consider each element bounding box Z
		 * The blocker height is quite thin.
		 * The effects may vary... TODO query for the effects bounding box z?
		 * The alert panel is on top, no problem.
		 */
		float fZDispl = 10f;
//		float fZDispl = LemurDiagFocusHelperStateI.i().getDialogZDisplacement();
		
		/**
		 * the alert is ultra special and the Z can be dealt with here!
		 */
//		float fZ=bbGuiNodeWorldBoundPriorToAlert.getMax(null).z;
//		float fZ=MiscJmeI.i().getZAboveAllAtGuiNode()-MiscLemurI.i().getMinSizeZ(); //just below the lemur raycast start point
		float fZ=MiscLemurI.i().getPickingRayCastFromZ()-SizeAndLocationI.i().getMinSizeZ(); //just below the lemur raycast start point
		
		Vector3f v3fPos = null;
//		fZ += fZDispl*(e.ordinal()+1); // attention how the order/index is a multiplier of the displacement!
		fZ += fZDispl*e.ordinal(); // begins in 0. attention how the order/index is a multiplier of the displacement!
		switch(e){ 
			case Blocker:
				v3fPos = new Vector3f(0, v3fWdwSize.y, fZ);
				break;
			case Effects:
				v3fPos = new Vector3f(v3fRef);
				v3fPos.z=fZ;
				break;
			case Alert: // the alert panel is above the lemur picking ray!
				Vector3f v3fAlertSize=v3fRef;
				if(v3fAlertSize==null){
					v3fAlertSize = v3fWdwSize.mult(0.5f);
				}
				
				if(isAlertStayOnCenter()){
					v3fPos = new Vector3f(v3fWdwSize.x/2f,v3fWdwSize.y/2f,0);
					v3fPos.x-=v3fAlertSize.x/2f;
					v3fPos.y+=v3fAlertSize.y/2f;
				}else{
					v3fPos = HWEnvironmentJmeI.i().getMouse().getPosWithMouseOnCenter(v3fAlertSize);
				}
				
				v3fPos.set(new Vector3f(v3fPos.x, v3fPos.y, fZ));
				
				break;
		}
		
		return v3fPos;
	}

	public boolean isAlertStayOnCenter() {
		return bAlertStayOnCenter;
	}

	public SystemAlertLemurI setAlertStayOnCenter(boolean bAlertStayOnCenter) {
		this.bAlertStayOnCenter = bAlertStayOnCenter;
		return this;
	}
	
//	@Override
//	protected void captureUserInput() {
//		/**
//		 * KEEP EMPTY, just to disable super input capture mode
//		 */
//	}

}
