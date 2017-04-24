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

import com.github.devconslejme.es.DialogHierarchySystemI;
import com.github.devconslejme.es.HierarchyComp;
import com.github.devconslejme.es.HierarchyComp.EField;
import com.github.devconslejme.gendiag.ResizablePanel.IResizableListener;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.EffectArrow;
import com.github.devconslejme.misc.jme.EffectElectricity;
import com.github.devconslejme.misc.jme.EffectManagerStateI;
import com.github.devconslejme.misc.jme.IEffect;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.bounding.BoundingBox;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityId;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.DefaultCursorListener;
import com.simsilica.lemur.focus.FocusManagerState;


/**
	* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
	*/
public class DialogHierarchyStateI extends AbstractAppState implements IResizableListener{
	public static DialogHierarchyStateI i(){return GlobalManagerI.i().get(DialogHierarchyStateI.class);}
	
	private Application	app;
	private float	fBeginOrderPosZ;
	private Node	nodeToMonitor;
	private FocusManagerState	focusState;
	private IEffect	ieffParentToChildLink = new EffectArrow();
	private IEffect	ieffLinkedDragEffect = new EffectElectricity().setColor(ColorI.i().colorChangeCopy(ColorRGBA.Blue, 0f, 0.5f));
	private BlockerListener blockerListener = new BlockerListener();
	private ColorRGBA	colorBlocker = ColorI.i().colorChangeCopy(ColorRGBA.Red, 0f, 0.15f);
	private DialogHierarchySystemI sys = DialogHierarchySystemI.i();
	private ResizablePanel	rzpCurrentlyBeingResized;
	/** so the blocker can stay in that gap */
	private float	fInBetweenGapDistZ=1.0f;
	private float	fMinLemurPanelSizeZ = 0.01f; //TODO collect this value dinamically from lemur in some way
	private ColorRGBA	colorInvisible = new ColorRGBA(0,0,0,0);
	protected float	fCurrentOrderPosZ;
	private CallableX cxZOrder = new CallableX() {
		@Override
		public Boolean call() {
			fCurrentOrderPosZ = fBeginOrderPosZ;
			for(Entity ent:sys.getSortedHierarchyDialogs()){
				updateZOrder(ent.getId());
			}
			return true;
		}
	};
	private CallableX	cxAutoFocus = new CallableX() { //TODO this delay still has a chance of typing something at other input field? like when holding for long a key?
		@Override
		public Boolean call() {
			for(Panel pnl:apnlAutoFocus){
				ResizablePanel rzp = MiscJmeI.i().getParentest(pnl, ResizablePanel.class, true);
				HierarchyComp hc = DialogHierarchyStateI.i().getHierarchyComp(rzp);
				if(!hc.isOpened())continue;
				
				if(!hc.isBlocked()){
					GuiGlobals.getInstance().requestFocus(pnl);
					return true; //skip others, they cant fight against each other...
				}
			}
			
			return true;
		}
	}.setName("FocusAtDevConsInput").setDelaySeconds(0.25f).setLoop(true);
	private ArrayList<Panel>	apnlAutoFocus = new ArrayList<Panel>();
	
	public static class BlockerListener extends DefaultCursorListener{
		@Override
		protected void click(CursorButtonEvent event, Spatial target, 	Spatial capture) {
			super.click(event, target, capture);
			
			Visuals vs = DialogHierarchyStateI.i().getVisuals(capture);
			HierarchyComp hc = DialogHierarchySystemI.i().getHierarchyComp(vs.getEntityId());
			if(hc.isBlocked()){
				DialogHierarchyStateI.i().setFocusRecursively(vs.getEntityId());
				event.setConsumed();
			}
		}
	}
	
	/**
	 * keep setters private
	 */
	public static class Visuals{
		private EntityId entid;
		private ResizablePanel rzpDiag;
		private Panel pnlBlocker;
		
		/** only one effect per child, but many per parent */
		private IEffect ieffLinkToParent;
		
		private boolean bAllowPositionRelativeToParent=true;
		private Vector3f	v3fPositionRelativeToParent = new Vector3f(20, -20, 0); //cascade like
//		@Override
//		public boolean equals(Object obj) {
//			if(obj==rzpDiag)return true;
//			if(obj==pnlBlocker)return true;
//			if(obj==ieff)return true;
//			if((Long)obj==entid.getId())return true;
//			return false;
//		}
		public EntityId getEntityId() {
			return entid;
		}
		private void setEntityId(EntityId entid) {
			this.entid = entid;
		}
		
		public ResizablePanel getDialog() {
			return rzpDiag;
		}
		private void setDiag(ResizablePanel rzpDiag) {
			this.rzpDiag = rzpDiag;
		}
		
		public Panel getBlocker() {
			return pnlBlocker;
		}
		private void setBlocker(Panel pnlBlocker) {
			this.pnlBlocker = pnlBlocker;
		}
		
		public Vector3f getPositionRelativeToParent() {
			return v3fPositionRelativeToParent;
		}
		private void setPositionRelativeToParent(Vector3f v3fPositionRelativeToParent) {
//			if(!isAllowPositionRelativeToParent())return;
			this.v3fPositionRelativeToParent = v3fPositionRelativeToParent;
		}
		
		public IEffect getEffLinkToParent() {
			return ieffLinkToParent;
		}
		private void setEffLinkToParent(IEffect ieffLinkToParent) {
			this.ieffLinkToParent = ieffLinkToParent;
		}
		
		public boolean isAllowPositionRelativeToParent() {
			return bAllowPositionRelativeToParent;
		}
		/**
		 * will prevent it from being updated by user input
		 */
		public void ignorePositionRelativeToParent() {
			this.bAllowPositionRelativeToParent = false;
		}
		
	}

	public void configure(Node nodeToMonitor,float fBeginOrderZ){
		this.fBeginOrderPosZ=fBeginOrderZ;
		this.nodeToMonitor=nodeToMonitor;
		
		app=GlobalManagerI.i().get(Application.class);
    app.getStateManager().attach(this);
		focusState=app.getStateManager().getState(FocusManagerState.class);
    
		EffectManagerStateI.i().add(ieffLinkedDragEffect);
		
		DialogHierarchySystemI.i().configure();
//		ed=DialogHierarchySystemI.i().getEntityData();
		
		QueueI.i().enqueue(cxAutoFocus);
	}
	
//	private HashBiMap<Long,Visuals> hmDiag = HashBiMap.create();
	
	public ResizablePanel createDialog(String strName, String strStyle) {
		EntityId entid = DialogHierarchySystemI.i().createEntity(strName);
		
		// main dialog panel
		ResizablePanel rzp = new ResizablePanel(strStyle);
		rzp.addResizableListener(this);
		HoverHighlightEffectI.i().applyAt(rzp, (QuadBackgroundComponent)rzp.getResizableBorder());
		
		// blocker
		Panel pnlBlocker = new Panel(strStyle);
		pnlBlocker.setBackground(new QuadBackgroundComponent(colorBlocker));
		DragParentestPanelListenerI.i().applyAt(pnlBlocker, rzp); // the blocker has not a parent panel! so it will let the dialog be dragged directly!  
		DragParentestPanelListenerI.i().applyAt(rzp); // the resizable border can be used to move/drag the parentest with the middle mouse button!  
		CursorEventControl.addListenersToSpatial(pnlBlocker,blockerListener);
		
		// visual data
		Visuals vs = new Visuals();
		vs.setEntityId(entid);
		vs.setDiag(rzp);
		vs.setBlocker(pnlBlocker);
		UserDataI.i().setUserDataPSH(rzp, vs);
		UserDataI.i().setUserDataPSH(pnlBlocker, vs);
		
		return rzp;
	}
	
	/**
	 * the blocker and the dialog share the same object!
	 * @param spt
	 * @return
	 */
	public Visuals getVisuals(Spatial spt){
		if(spt==null)return null;
		return UserDataI.i().getUserDataPSH(spt,Visuals.class);
	}
	
	public HierarchyComp getHierarchyComp(Spatial spt){
		Visuals vs = getVisuals(spt);
		if(vs==null)return null;
		return sys.getHierarchyComp(vs.getEntityId());
	}
	
	public void showDialog(ResizablePanel rzp) {
		nodeToMonitor.attachChild(rzp);
		
		EntityId entid = getVisuals(rzp).getEntityId();
		sys.setHierarchyComp(entid, 
			EField.bOpened, true,
			EField.lLastFocusTime, app.getTimer().getTime()
		);
		
		setFocusRecursively(entid);
	}

	public void showDialogAsModeless(ResizablePanel rzpParent, ResizablePanel rzpChild) {
		showDialogAs(false, rzpParent, rzpChild);
	}
	public void showDialogAsModal(ResizablePanel rzpParent, ResizablePanel rzpChild) {
		showDialogAs(true, rzpParent, rzpChild);
	}
	private void showDialogAs(boolean bModal, ResizablePanel rzpParent, ResizablePanel rzpChild) {
		if(!rzpParent.isOpened())throw new DetailedException("parent not open",rzpParent,rzpChild);
		
		sys.setHierarchyComp(getVisuals(rzpChild).getEntityId(), 
			EField.eidHierarchyParent, getVisuals(rzpParent).getEntityId(),
			EField.bHierarchyModal, bModal
		);
		
		showDialog(rzpChild);
		
		applyParentChildLinkEffect(rzpParent, rzpChild);
	}
	
	protected void applyParentChildLinkEffect(ResizablePanel rzpParent, ResizablePanel rzpChild) {
		if(!getHierarchyComp(rzpParent).isShowLinksFromChilds())return;
		
		Visuals vsChild = getVisuals(rzpChild);
		if(vsChild.getEffLinkToParent()==null || vsChild.getEffLinkToParent().isDiscarded()){
			IEffect effLink = ieffParentToChildLink.clone();
			
			effLink
				.setFollowFromTarget(rzpParent, null)
				.setFollowToTarget(rzpChild, null)
				.useFollowToPosZ()
				.setPlay(true)
				;
			
			EffectManagerStateI.i().add(effLink);
			
			vsChild.setEffLinkToParent(effLink);
		}
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		sys.update(tpf, app.getTimer().getTime());
		
		updateDragLinkToParentEffect();
		
		updateVolatileModalAutoClose();
	}
	
	private void updateVolatileModalAutoClose() {
		Spatial spt = focusState.getFocus();
		if(spt==null)return;
		
		ResizablePanel rzp = MiscJmeI.i().getParentest(spt, ResizablePanel.class, true);
		HierarchyComp hc = getHierarchyComp(rzp);
		if(hc==null)return;
		
		if(!hc.isVolatileModal())return;
		
		if(hc.isBlocked())return; //it can have an active child
		
		if(!rzp.isUpdateLogicalStateSucces())return; //as 1st time it may not be ready yet
		
		if(!MiscLemurI.i().isMouseCursorOver(rzp)){
			rzp.close();
		}
	}

	private void updateDragLinkToParentEffect(){
		Panel pnl = DragParentestPanelListenerI.i().getParentestBeingDragged();
		HierarchyComp hc = getHierarchyComp(pnl);
		if(hc!=null){
			if(!ieffLinkedDragEffect.isPlaying()){ //this also identifies the start of the dragging!
				EntityId entidParent = hc.getHierarchyParent();
				if(entidParent!=null){
					ResizablePanel rzpParent = getOpenDialog(entidParent);
					ieffLinkedDragEffect
						.setOwner(pnl)
						.setFollowFromTarget(rzpParent, null)
						.setFollowToTarget(pnl, null)
						.useFollowToPosZ()
						.setPlay(true)
						;
				}
				
				setFocusRecursively(getEntityId(pnl));
			}
		}else{
			ieffLinkedDragEffect.setPlay(false);
		}
	}
	
	public ResizablePanel getOpenDialog(EntityId entid){
		for(Spatial spt:nodeToMonitor.getChildren()){
			Visuals vs = getVisuals(spt);
			if(vs==null)continue;
			if(vs.getEntityId().equals(entid))return vs.getDialog();
		}
		return null;
	}
	
	/**
	 * 
	 * @param tpf ignored if null
	 * @param entid
	 * @param rzp
	 */
//	public void updateBlocker(Float tpf,EntityId entid, ResizablePanel rzp){
	public void updateBlocker(Float tpf,Visuals vs){
		// blocker work
		Panel pnlBlocker = vs.getBlocker();
		if(vs.getDialog().isOpened()){
			if(pnlBlocker.getParent()==null){
				nodeToMonitor.attachChild(pnlBlocker);
			}
			
			// how many childs are modal
			int iModalCount=0;
			for(Entity entChild:sys.getAllOpenedDialogs(vs.getEntityId())){
				if(entChild.get(HierarchyComp.class).isHierarchyModal())iModalCount++;
			}
			sys.enableBlockingLayer(vs.getEntityId(),iModalCount>0);
			
			// z order
			Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(vs.getDialog());
			if(v3fSize!=null){
				if(Float.compare(v3fSize.length(),0f)!=0){ //waiting top panel be updated by lemur
					Vector3f v3fPos = vs.getDialog().getLocalTranslation().clone();
					
					QuadBackgroundComponent qbc = ((QuadBackgroundComponent)pnlBlocker.getBackground());
					if(sys.isBlocked(vs.getEntityId())){
						v3fPos.z += v3fSize.z + fInBetweenGapDistZ/2f; //above
						qbc.setColor(colorBlocker);
					}else{
						v3fPos.z -= fInBetweenGapDistZ/2f; //below to not receive mouse/cursor events
						qbc.setColor(colorInvisible);
					}
					
					pnlBlocker.setLocalTranslation(v3fPos);
					
					Vector3f v3fBlockerSize = v3fSize.clone();
					v3fBlockerSize.z=fMinLemurPanelSizeZ;
					pnlBlocker.setPreferredSize(v3fBlockerSize);//rzp.getPreferredSize());
				}
			}
			
		}else{
			if(pnlBlocker.getParent()!=null){
				pnlBlocker.removeFromParent();
			}
		}
	}
	
	/**
	 * see {@link DialogHierarchySystemI#getAllOpenedDialogs(EntityId)}
	 * @return
	 */
	public ResizablePanel[] getAllOpenedDialogs() {
		ArrayList<ResizablePanel> arzp = new ArrayList<ResizablePanel>();
		for(Entity ent:sys.getAllOpenedDialogs(null)){
			arzp.add(getOpenDialog(ent.getId()));
		}
		return arzp.toArray(new ResizablePanel[0]);
	}
	
	@Override
	public void resizerUpdatedLogicalStateEvent(float tpf, ResizablePanel rzpSource) {
		Visuals vs = getVisuals(rzpSource);
//		EntityId entid = vs.getEntityId();
		updateBlocker(tpf, vs);
		
		updateDragResizeRelativeParentPos(tpf, rzpSource, vs);
	}
	
	private void updateDragResizeRelativeParentPos(float tpf, ResizablePanel rzpSource, Visuals vs) {
		if(!vs.isAllowPositionRelativeToParent())return;
		
		HierarchyComp hc = getHierarchyComp(rzpSource);
		EntityId entidParent = hc.getHierarchyParent();
		if(entidParent!=null){
			if(vs.getPositionRelativeToParent()!=null){
				ResizablePanel rzpParent = getOpenDialog(entidParent);
				
				if(
						DragParentestPanelListenerI.i().getParentestBeingDragged()==rzpSource
						||
						getCurrentlyBeingResized()==rzpSource
				){
					/**
					 * update displacement
					 */
					vs.setPositionRelativeToParent(
						rzpSource.getLocalTranslation().subtract(rzpParent.getLocalTranslation())
					);
				}else{
//					if(vs.isAllowPositionRelativeToParent()){
						/**
						 * set position relatively to parent
						 */
						Vector3f v3fNewPos = rzpParent.getLocalTranslation().clone();
						v3fNewPos.addLocal(vs.getPositionRelativeToParent());
						v3fNewPos.z=rzpSource.getLocalTranslation().z;
						rzpSource.setLocalTranslation(v3fNewPos);
//					}
				}
			}
		}
	}

	public ResizablePanel getCurrentlyBeingResized(){
		return rzpCurrentlyBeingResized;
	}

	@Override
	public void resizedEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {
		if(rzpCurrentlyBeingResized==null){ //marks it's start!
			setFocusRecursively(getEntityId(rzpSource));
		}
		
		rzpCurrentlyBeingResized=rzpSource;
	}

	@Override
	public void endedResizingEvent(ResizablePanel rzpSource) {
		rzpCurrentlyBeingResized=null; //user can resize only one at a time 
	}

	@Override
	public void removedFromParentEvent(ResizablePanel rzpSource) {
//		updateBlocker(null, getVisuals(rzpSource).getEntityId(), rzpSource);
		Visuals vs = getVisuals(rzpSource);
		
		updateBlocker(null, vs);
		
		sys.setHierarchyComp(vs.getEntityId(), EField.bOpened, false);
		
		if(vs.getEffLinkToParent()!=null)vs.getEffLinkToParent().setAsDiscarded();
	}
	
	private void updateZOrder(EntityId entid){
		ResizablePanel rzp = getOpenDialog(entid);
		Vector3f v3f = rzp.getLocalTranslation().clone();
		v3f.z=fCurrentOrderPosZ;
		rzp.setLocalTranslation(v3f);
		sys.setHierarchyComp(entid, EField.fDialogZ, fCurrentOrderPosZ);
		
		// prepare next
		BoundingBox bb = (BoundingBox)getOpenDialog(entid).getWorldBound();
		if(bb!=null){ //only if it is ready
			float fHeight = bb.getZExtent()*2f;
			sys.setHierarchyComp(entid, EField.fBoundingHeightZ, fHeight);
			fCurrentOrderPosZ += fHeight +fInBetweenGapDistZ;
			
			MessagesI.i().debugInfo(this, "DiagHierarchyZOrder:"+rzp.getName()+","+entid+","+v3f.z+","+fHeight);
		}
		
	}
	
	/**
	 * for the entire current tree
	 * @param entid
	 */
	public void setFocusRecursively(EntityId entid){
		setFocusRecursively(sys.getParentest(entid), false);
		
		QueueI.i().enqueue(cxZOrder);
	}
	public void setFocusRecursively(EntityId entid, boolean bRecursing){
		ResizablePanel rzp = getOpenDialog(entid);
		GuiGlobals.getInstance().requestFocus(rzp);
		sys.updateLastFocusAppTimeNano(entid, app.getTimer().getTime());
		
		for(Entity ent:sys.getAllOpenedDialogs(entid)){
			setFocusRecursively(ent.getId(),true);
		}
	}

	public EntityId getEntityId(Spatial spt) {
		return getVisuals(spt).getEntityId();
	}

	public float getInBetweenGapDistZ() {
		return fInBetweenGapDistZ;
	}

	public void setInBetweenGapDistZ(float fInBetweenGapDistZ) {
		this.fInBetweenGapDistZ = fInBetweenGapDistZ;
	}
	
	
	public void addRequestAutoFocus(Panel pnl) {
		if(!apnlAutoFocus.contains(pnl))apnlAutoFocus.add(pnl);
	}

//	public String getReport(ResizablePanel rzp) {
//		String str="";
//		return getHierarchyComp(rzp).toString();
//		return str;
//	}

}
