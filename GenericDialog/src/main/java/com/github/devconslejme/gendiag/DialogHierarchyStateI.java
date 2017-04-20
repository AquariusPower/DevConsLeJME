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

import com.github.devconslejme.es.DialogHierarchySystemI;
import com.github.devconslejme.es.HierarchyComp;
import com.github.devconslejme.es.HierarchyComp.EField;
import com.github.devconslejme.gendiag.ResizablePanel.IResizableListener;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.EffectArrow;
import com.github.devconslejme.misc.jme.EffectElectricity;
import com.github.devconslejme.misc.jme.EffectManagerStateI;
import com.github.devconslejme.misc.jme.IEffect;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.es.EntityId;
import com.simsilica.lemur.Panel;
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
	
	public static class BlockerListener extends DefaultCursorListener{
		@Override
		protected void click(CursorButtonEvent event, Spatial target, 	Spatial capture) {
			super.click(event, target, capture);
			
			Visuals vs = DialogHierarchyStateI.i().getVisuals(capture);
			HierarchyComp hc = DialogHierarchySystemI.i().getHierarchyComp(vs.getEntityId());
			if(hc.isBlocking()){
				DialogHierarchyStateI.i().setFocusRecursively(entid);
				event.setConsumed();
			}
		}
		
	}
	private static class Visuals{
		private EntityId entid;
		private ResizablePanel rzpDiag;
		private Panel pnlBlocker;
		private IEffect ieffLinkToParent;
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
		public void setEntityId(EntityId entid) {
			this.entid = entid;
		}
		public ResizablePanel getDialog() {
			return rzpDiag;
		}
		public void setDiag(ResizablePanel rzpDiag) {
			this.rzpDiag = rzpDiag;
		}
		public Panel getBlocker() {
			return pnlBlocker;
		}
		public void setBlocker(Panel pnlBlocker) {
			this.pnlBlocker = pnlBlocker;
		}
		public Vector3f getPositionRelativeToParent() {
			return v3fPositionRelativeToParent;
		}
		public void setPositionRelativeToParent(Vector3f v3fPositionRelativeToParent) {
			this.v3fPositionRelativeToParent = v3fPositionRelativeToParent;
		}
		public IEffect getEffLinkToParent() {
			return ieffLinkToParent;
		}
		public void setEffLinkToParent(IEffect ieffLinkToParent) {
			this.ieffLinkToParent = ieffLinkToParent;
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
	}
	
//	private HashBiMap<Long,Visuals> hmDiag = HashBiMap.create();
	
	public ResizablePanel createDialog(String strName) {
		EntityId entid = DialogHierarchySystemI.i().createEntity(strName);
		
		// main dialog panel
		ResizablePanel rzp = new ResizablePanel(null);
		rzp.addUpdateLogicalStateListener(this);
		HoverHighlightEffectI.i().applyAt(rzp, (QuadBackgroundComponent)rzp.getResizableBorder());
		
		// blocker
		Panel pnlBlocker = new Panel("");
		pnlBlocker.setBackground(new QuadBackgroundComponent(colorBlocker));
		DragParentestPanelListenerI.i().applyAt(pnlBlocker, rzp); // the blocker has not a parent panel! so it will let the dialog be dragged directly!  
		CursorEventControl.addListenersToSpatial(pnlBlocker,blockerListener);
		
		// visual data
		Visuals vs = new Visuals();
		vs.entid=entid;
		vs.rzpDiag=rzp;
		vs.pnlBlocker=pnlBlocker;
		UserDataI.i().setUserDataPSH(rzp, vs);
		UserDataI.i().setUserDataPSH(pnlBlocker, vs);
		
		return rzp;
	}
	
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
		
		sys.setHierarchyComp(getVisuals(rzp).getEntityId(), 
			EField.bOpened, true,
			EField.lLastFocusTime, app.getTimer().getTime()
		);
	}

	public void showDialogAsModal(ResizablePanel rzpParent, ResizablePanel rzpChild) {
		if(!rzpParent.isOpened())throw new DetailedException("parent not open",rzpParent,rzpChild);
		
		sys.setHierarchyComp(getVisuals(rzpChild).getEntityId(), 
			EField.eidHierarchyParent, getVisuals(rzpParent).getEntityId(),
			EField.bHierarchyModal, true
		);
		
		showDialog(rzpChild);
		
		applyParentToChildLinkEffect(rzpParent, rzpChild);
	}
	
	protected void applyParentToChildLinkEffect(ResizablePanel rzpParent, ResizablePanel rzpChild) {
		if(!getHierarchyComp(rzpParent).isShowLinksFromChilds())return;
		
		Visuals vsChild = getVisuals(rzpChild);
		if(vsChild.getEffLinkToParent()==null){
			IEffect effLink = ieffParentToChildLink.clone();
			
			effLink
				.setFollowFromTarget(rzpParent, null)
				.setFollowToTarget(rzpChild, null)
				.setPlay(true)
				;
			
			EffectManagerStateI.i().add(effLink);
			
			vsChild.setEffLinkToParent(effLink);
		}
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		updateDragLinkToParentEffect();
	}
	
	private void updateDragLinkToParentEffect(){
		Panel pnl = DragParentestPanelListenerI.i().getParentestBeingDragged();
		HierarchyComp hc = getHierarchyComp(pnl);
		if(hc!=null){
			EntityId entidParent = hc.getHierarchyParent();
			if(entidParent!=null){
				ResizablePanel rzpParent = getDialog(entidParent);
				if(!ieffLinkedDragEffect.isPlaying()){
					ieffLinkedDragEffect
						.setOwner(pnl)
						.setFollowFromTarget(rzpParent, null)
						.setFollowToTarget(pnl, null)
						.setPlay(true)
						;
				}
			}
		}else{
			ieffLinkedDragEffect.setPlay(false);
		}
	}
	
	public ResizablePanel getDialog(EntityId entid){
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
	public void updateBlocker(Float tpf,EntityId entid, ResizablePanel rzp){
/*
		Entity ent = ed.getEntity(entid, HierarchyComp.class);
		
		// blocker work
		Panel pnlBlocker = hmBlocker.get(ent.getId().getId());
		if(rzp.isOpened()){
			if(pnlBlocker.getParent()==null){
				nodeToMonitor.attachChild(pnlBlocker);
			}
			
			// how many childs are modal
			int iModalCount=0;
			for(Entity entChild:sys.prepareSortedHierarchyDialogs(ent.getId())){
				if(entChild.get(HierarchyComp.class).isHierarchyModal())iModalCount++;
			}
			
			if(iModalCount==0){
				sys.enableBlockingLayer(ent,false);
			}
			
			// z order
			Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(rzp);
			if(v3fSize!=null){
				if(Float.compare(v3fSize.length(),0f)!=0){ //waiting top panel be updated by lemur
					Vector3f v3fPos = rzp.getLocalTranslation().clone();
					
					QuadBackgroundComponent qbc = ((QuadBackgroundComponent)pnlBlocker.getBackground());
					if(sys.isBlocking(ent)){
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
*/		
	}
	
	@Override
	public void resizerUpdatedLogicalStateEvent(float tpf, ResizablePanel rzpSource) {
		Visuals vs = getVisuals(rzpSource);
		EntityId entid = vs.getEntityId();
		updateBlocker(tpf, entid, rzpSource);
		
		HierarchyComp hc = getHierarchyComp(rzpSource);
		EntityId entidParent = hc.getHierarchyParent();
		if(entidParent!=null){
			if(vs.getPositionRelativeToParent()!=null){
				ResizablePanel rzpParent = getDialog(entidParent);
				
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
					/**
					 * set position relatively to parent
					 */
					Vector3f v3fNewPos = rzpParent.getLocalTranslation().clone();
					v3fNewPos.addLocal(vs.getPositionRelativeToParent());
					v3fNewPos.z=rzpSource.getLocalTranslation().z;
					rzpSource.setLocalTranslation(v3fNewPos);
				}
			}
		}
	}
	
	public ResizablePanel getCurrentlyBeingResized(){
		return rzpCurrentlyBeingResized;
	}

	@Override
	public void resizedEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {
		rzpCurrentlyBeingResized=rzpSource;
	}

	@Override
	public void endedResizingEvent(ResizablePanel rzpSource) {
		rzpCurrentlyBeingResized=null; //user can resize only one at a time 
	}

	@Override
	public void removedFromParentEvent(ResizablePanel rzpSource) {
		updateBlocker(null, getVisuals(rzpSource).getEntityId(), rzpSource);
	}
	
}
