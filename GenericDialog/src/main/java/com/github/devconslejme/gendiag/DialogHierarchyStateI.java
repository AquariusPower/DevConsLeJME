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
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.EffectArrow;
import com.github.devconslejme.misc.jme.EffectElectricity;
import com.github.devconslejme.misc.jme.EffectManagerStateI;
import com.github.devconslejme.misc.jme.IEffect;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.google.common.collect.HashBiMap;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityComponent;
import com.simsilica.es.EntityId;
import com.simsilica.es.EntitySet;
import com.simsilica.es.PersistentComponent;
import com.simsilica.es.base.DefaultEntityData;
import com.simsilica.lemur.GuiGlobals;
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
	
	private EntitySet	entset;
	private FocusManagerState	focusState;
	private ResizablePanel	rzpCurrentlyBeingResized;
	private Node nodeToMonitor;
	
	private float	fBeginOrderPosZ = 0f;
	private float	fCurrentOrderPosZ;
	
	/**
	 * so the blocker can stay in that gap
	 */
	private float	fInBetweenGapDistZ=1.0f;
	private float	fMinLemurPanelSizeZ = 0.01f;
	
	private HashBiMap<Long,ResizablePanel> hmDiag = HashBiMap.create();
	private HashBiMap<Long,Panel> hmBlocker = HashBiMap.create();
	private DefaultEntityData	ed;
	private DialogHierarchySystemI	hisys;
	private BlockerListener blockerListener = new BlockerListener();
	private IEffect	ieffParentToChildLink = new EffectArrow();
	private IEffect	ieffLinkedDragEffect = new EffectElectricity().setColor(ColorI.i().colorChangeCopy(ColorRGBA.Blue, 0f, 0.5f));
	private Application	app;
	private ColorRGBA	colorBlocker = ColorI.i().colorChangeCopy(ColorRGBA.Red, 0f, 0.15f);
	private ColorRGBA	colorInvisible = new ColorRGBA(0,0,0,0);
	
	public void configure(Node nodeToMonitor,float fBeginOrderZ){
		this.fBeginOrderPosZ=fBeginOrderZ;
		this.nodeToMonitor=nodeToMonitor;
		
		app=GlobalManagerI.i().get(Application.class);
    app.getStateManager().attach(this);
		focusState=app.getStateManager().getState(FocusManagerState.class);
    
		EffectManagerStateI.i().add(ieffLinkedDragEffect);
		
		DialogHierarchySystemI.i().configure();
		ed=DialogHierarchySystemI.i().getEntityData();
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		super.initialize(stateManager, app);
		
		hisys = DialogHierarchySystemI.i();
		ed = hisys.getEntityData();
		entset = hisys.getEntities();
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		DialogHierarchySystemI.i().update(tpf, app.getTimer().getTime());
		
		for(Entity ent:entset){
			updateEntity(tpf, ent);
			
			if(hisys.isHasAnyDialogOpened()){
	//			if(getHierarchyComponentList(null).size()>0){
				Entity entTop = hisys.getTopMostDialog();
				if(!ContextMenuI.i().isTheContextMenu(hmDiag.get(entTop.getId()))){
					ContextMenuI.i().hideContextMenu();
				}
			}
			
		}
		
		updateDragEffect();
		
		fCurrentOrderPosZ = fBeginOrderPosZ;
		for(Entity ent:hisys.getSortedHierarchyDialogs()){
			updateZOrder(ent);
			
//			updateFocusTime(ent,hc);
		}
		
		hisys.prepareSortedHierarchyDialogs(null); //last thing
	}
	
//	public void updateChangedEntity(Float tpf,EntityId entid){
//	//	Entity ent = ed.getEntity(entid, GuiLink.class,ShownState.class);
//		Entity ent = ed.getEntity(entid, HierarchyComp.class);
	public void updateEntity(Float tpf,Entity ent){
		// close self if parent dialog closed
		ResizablePanel rzp = hmDiag.get(ent.getId().getId());
		if(rzp.isOpened()){
			HierarchyComp hc = ent.get(HierarchyComp.class);
			if(hc.getHierarchyParent()!=null){
				Entity entHierarchyParent = ed.getEntity(
					hc.getHierarchyParent(), HierarchyComp.class);
				if(!hmDiag.get(entHierarchyParent.getId().getId()).isOpened()){
					rzp.close();
					hmBlocker.get(ent.getId().getId()).removeFromParent();
				}
			}
		}
		
		HierarchyComp hc = ent.get(HierarchyComp.class);
		if(!hc.isInitVisuals()){
			initializeNewEntity(tpf,ent);
			ed.setComponent(ent.getId(), hc=new HierarchyComp(hc,EField.bInitVisuals,true));
		}
		
		if(hc.isOpened() && rzp.getParent()==null){
			ent.set(hc=new HierarchyComp(hc, EField.bOpened, false));
		}else
		if(!hc.isOpened() && rzp.getParent()!=null){
			ent.set(hc=new HierarchyComp(hc, EField.bOpened, true));
		}
		
//		ResizablePanel rzp = hmDiag.get(ent.getId().getId());
		Panel pnlBlocker = hmBlocker.get(ent.getId().getId());
		
		Vector3f v3f = rzp.getLocalTranslation();
		v3f.z=hc.getZ();
		rzp.setLocalTranslation(v3f);
		
		Vector3f v3fBlocker = pnlBlocker.getLocalTranslation();
		v3fBlocker.z=hc.getBlockerZ();
		pnlBlocker.setLocalTranslation(v3fBlocker);
		
		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(rzp);
		if(v3fSize!=null){//wait it be ready
			ent.set(hc=new HierarchyComp(hc,
				EField.fBoundingHeightZ, v3fSize.z
			));
		}
		
		updateFocusTime(ent,rzp);
		
		updateShowHierarchyChildAndParent(ent,hc,rzp);
		
	}

	public void put(EntityId entid, ResizablePanel rzp) {
		hmDiag.put(entid.getId(), rzp);
	}
	
	private void updateFocusTime(Entity ent, ResizablePanel rzp) {
		EntityId entidUpdLFTime=null;
		
		// has real input focus
		if(entidUpdLFTime==null && isFocused(rzp)){
			entidUpdLFTime=ent.getId();
		}
		
		// being dragged
		if(entidUpdLFTime==null){
			Panel pnlParentestDragged = DragParentestPanelListenerI.i().getParentestBeingDragged();
			if(pnlParentestDragged!=null && ResizablePanel.class.isInstance(pnlParentestDragged)){
				entidUpdLFTime=getEntityIdFor((ResizablePanel)pnlParentestDragged);
			}
		}
		
		// being resized
		if(entidUpdLFTime==null && getCurrentlyBeingResized()!=null){
			entidUpdLFTime=getEntityIdFor(getCurrentlyBeingResized());
		}
		
		if(entidUpdLFTime!=null){
			setFocusRecursively(entidUpdLFTime);
//			updateLastFocusAppTimeNano(entidUpdLFTime);
		}
	}

	public Spatial getFocused(){
		return focusState.getFocus();
	}
	public boolean isFocused(Spatial spt){
		if(spt.equals(getFocused()))return true; //quick test 1st
		if(getFocused()==null)return false;
		
		Spatial sptParentest = MiscJmeI.i().getParentest(spt, Panel.class, true);
		Spatial sptFocusedParentest = MiscJmeI.i().getParentest(getFocused(), Panel.class, true);
		return sptParentest.equals(sptFocusedParentest);
	}

	public void setFocusRecursively(EntityId entid){
		ArrayList<Entity> aentChild = hisys.prepareSortedHierarchyDialogs(entid);
//		if(ed.getComponent(entid, Blocker.class).isBlocking()){
		if(aentChild.size()==0){
			ResizablePanel rzp = hmDiag.get(entid);
			GuiGlobals.getInstance().requestFocus(rzp);
			hisys.updateLastFocusAppTimeNano(entid);
		}else{
			for(Entity ent:aentChild){
				setFocusRecursively(ent.getId());
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
		updateBlocker(null, getEntityIdFor(rzpSource), rzpSource);
	}

	@Override
	public void resizerUpdatedLogicalStateEvent(float tpf, ResizablePanel rzpSource) {
		EntityId entid = getEntityIdFor(rzpSource);
		updateBlocker(tpf, entid, rzpSource);
		
		EntityId entidParent = ed.getComponent(entid, HierarchyComp.class).getHierarchyParent();
		if(entidParent!=null){
			RelativePos rp = ed.getComponent(entid, RelativePos.class);
			if(rp!=null){
				ResizablePanel rzpParent = hmDiag.get(entidParent.getId());
				
//				Panel panelUserAction = DragParentestPanelListenerI.i().getParentestBeingDragged();
//				if(panelUserAction==null){
//					panelUserAction=getCurrentlyBeingResized();
//				}
				
				if(
						DragParentestPanelListenerI.i().getParentestBeingDragged()==rzpSource
						||
						getCurrentlyBeingResized()==rzpSource
				){
//				if(panelUserAction!=null){
					/**
					 * update displacement
					 */
					ed.setComponent(entid, new RelativePos(
							rzpSource.getLocalTranslation().subtract(rzpParent.getLocalTranslation()) ) );
					
//					if(!ieffLinkedDragEffect.isPlaying()){
//						ieffLinkedDragEffect
//							.setOwner(rzpSource)
//							.setFollowFromTarget(rzpParent, null)
//							.setFollowToTarget(rzpSource, null)
//							.setPlay(true)
//							;
//					}
				}else{
//					if(ieffLinkedDragEffect.isPlaying()){
//						ieffLinkedDragEffect.setPlay(false);
//						ieffLinkedDragEffect.setOwner(null);
//					}
					
					/**
					 * set position relatively to parent
					 */
					Vector3f v3fNewPos = rzpParent.getLocalTranslation().clone();
					v3fNewPos.addLocal(rp.getPositionRelativeToParent());
					v3fNewPos.z=rzpSource.getLocalTranslation().z;
					
	//					v3fNewPos.x=rzpParent.getLocalTranslation().x 
	//						+ (rzpParent.getSize().x/2f - rzpSource.getSize().x/2f + fCascadeDist);
	//					v3fNewPos.y=rzpParent.getLocalTranslation().y 
	//						- (rzpParent.getSize().y/2f - rzpSource.getSize().y/2f + fCascadeDist);
					
					rzpSource.setLocalTranslation(v3fNewPos);
				}
			}
		}
	}


	public static class BlockerListener extends DefaultCursorListener{
		@Override
		protected void click(CursorButtonEvent event, Spatial target, 	Spatial capture) {
			super.click(event, target, capture);
			
			EntityId entid = UserDataI.i().getUserDataPSH(capture,EntityId.class);
			HierarchyComp blk = DialogHierarchyStateI.i().ed.getComponent(entid,HierarchyComp.class);
			if(blk.isBlocking()){
				DialogHierarchyStateI.i().setFocusRecursively(entid);
				event.setConsumed();
			}
		}
		
	}
	
	/**
	 * will prevent access to parent
	 * @param compChild
	 * @return 
	 */
	public void showDialogAsModal(EntityId entidParent, EntityId entidChild){
		applyHierarchyChild(entidParent,entidChild,true);
//		QueueI.i().enqueue(new CallableX() {
//			@Override
//			public Boolean call() {
//				if(!ed.getComponent(entidParent, Initialized.class).isInitialized())return false;
//				if(!ed.getComponent(entidChild, Initialized.class).isInitialized())return false;
//				
//				Entity entParent = ed.getEntity(entidParent, getAllRequiredComponentTypesArray());
//				Entity entChild = ed.getEntity(entidChild, getAllRequiredComponentTypesArray());
//				showAndApplyHierarchyChild(entParent,entChild,true);
//				
//				return true;
//			}
//		});
	}
	
	/**
	 * will close if parent closes
	 * @param compChild
	 * @return 
	 */
	public void showDialogAsModeless(EntityId entidParent, EntityId entidChild){
		applyHierarchyChild(entidParent,entidChild,false);
	}
	
	private void applyHierarchyChild(EntityId entidParent, EntityId entidChild, boolean bModal){
		HierarchyComp hcChild = ed.getComponent(entidChild, HierarchyComp.class);
		ed.setComponent(entidChild,hcChild=new HierarchyComp(hcChild,
			EField.eidHierarchyParent, entidParent,
			EField.bHierarchyModal, bModal
		));
	}
	private void updateShowHierarchyChildAndParent(Entity entChild, HierarchyComp hcChild, ResizablePanel rzpChild){
//		HierarchyComp hcChild = ed.getComponent(entidChild, HierarchyComp.class);
		if(hcChild.isOpened())return; //already opened
		
		EntityId entidParent=hcChild.getHierarchyParent();
		if(entidParent==null)return;
		
		if(!hcChild.isInitVisuals())return;
		
		HierarchyComp hcParent = ed.getComponent(entidParent, HierarchyComp.class);
		if(!hcParent.isInitVisuals())return;
		
		Entity entParent = ed.getEntity(entidParent, hisys.getAllRequiredComponentTypesArray());
//		Entity entChild = ed.getEntity(entidChild, hisys.getAllRequiredComponentTypesArray());
		
		if(hcChild.isHierarchyModal())hisys.enableBlockingLayer(entParent,true);
		
//		ResizablePanel rzpChild = hmDiag.get(entChild.getId().getId());
		showDialog(rzpChild); //show it
		setFocusRecursively(entChild.getId());
		
		if(hcParent.isShowLinkToChild()){
			ResizablePanel rzpParent = hmDiag.get(entParent.getId().getId());
			applyParentToChildLinkEffect(rzpParent,rzpChild);
		}
	}
	
	public void initializeNewEntity(Float tpf,Entity ent) {
		/////////////// blocker
		ResizablePanel rzp = hmDiag.get(ent.getId().getId());
		
	//	if(ed.getComponent(ent.getId(), Blocker.class)==null){
			Panel pnlBlocker = new Panel("");
			hmBlocker.put(ent.getId().getId(), pnlBlocker);
//			ed.setComponent(ent.getId(),new Blocker(pnlBlocker,null)); //ent.set(
			
			pnlBlocker.setBackground(new QuadBackgroundComponent(colorBlocker));
			
			//the blocker has not a parent panel! so it will let the dialog be dragged directly!
			DragParentestPanelListenerI.i().applyAt(pnlBlocker, rzp);  
			
			CursorEventControl.addListenersToSpatial(pnlBlocker,blockerListener);
			
			UserDataI.i().setUserDataPSH(pnlBlocker,ent.getId());
	//	}
		
		// panel
		rzp.addUpdateLogicalStateListener(this);
		HoverHighlightEffectI.i().applyAt(rzp, (QuadBackgroundComponent)rzp.getResizableBorder());
	}
	
	//	ArrayList<ResizablePanel> arzpZOrderList = new ArrayList<ResizablePanel>();
	public void showDialog(ResizablePanel rzp) {
//		if(arzpZOrderList.contains(rzp)){
//			arzpZOrderList.remove(rzp);
//		}
//		
//		arzpZOrderList.add(rzp);
//		
//		recursiveWorkOnChildDiagsOf(rzp,true);
		
		nodeToMonitor.attachChild(rzp);
	}

	public void setParentToChildLinkEffect(IEffect i){
		this.ieffParentToChildLink=i;
	}
	
	/**
	 * when there is a parent and child link
	 * @param i
	 */
	public void setDragEffect(IEffect i){
		this.ieffLinkedDragEffect =i;
	}
	
	protected void applyParentToChildLinkEffect(ResizablePanel rzpParent, ResizablePanel rzpChild) {
		IEffect effLink = ieffParentToChildLink.clone();
		
		effLink
			.setOwner(rzpParent)
			.setFollowFromTarget(rzpParent, null)
			.setFollowToTarget(rzpChild, null)
			.setPlay(true)
			;
		
		EffectManagerStateI.i().add(effLink);
	}
	
	/**
	 * 
	 * @param tpf ignored if null
	 * @param entid
	 * @param rzp
	 */
	public void updateBlocker(Float tpf,EntityId entid, ResizablePanel rzp){
//		Entity ent = ed.getEntity(entid, GuiLink.class,ShownState.class,Blocker.class);
		Entity ent = ed.getEntity(entid, HierarchyComp.class);
		
		// blocker work
		Panel pnlBlocker = hmBlocker.get(ent.getId().getId());
		if(rzp.isOpened()){
			if(pnlBlocker.getParent()==null){
				nodeToMonitor.attachChild(pnlBlocker);
			}
			
			// how many childs are modal
			int iModalCount=0;
			for(Entity entChild:hisys.prepareSortedHierarchyDialogs(ent.getId())){
				if(entChild.get(HierarchyComp.class).isHierarchyModal())iModalCount++;
			}
			
			if(iModalCount==0){
				hisys.enableBlockingLayer(ent,false);
			}
			
//			if(isBlocked(ent)){
//				pnlBlocker.setBackground(
//					new QuadBackgroundComponent(
//						ColorI.i().colorChangeCopy(ColorRGBA.Red, -0.5f, 0.5f)));
//			}else{
//				//transparent
//				pnlBlocker.setBackground(new QuadBackgroundComponent(new ColorRGBA(0,0,0,0)));
//			}
			
			// z order
			Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(rzp);
			if(v3fSize!=null){
				if(Float.compare(v3fSize.length(),0f)!=0){ //waiting top panel be updated by lemur
					Vector3f v3fPos = rzp.getLocalTranslation().clone();
					
					QuadBackgroundComponent qbc = ((QuadBackgroundComponent)pnlBlocker.getBackground());
					if(hisys.isBlocking(ent)){
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
	
	public void setBlockerColor(ColorRGBA color){
		colorBlocker = color;
	}
	
	public ResizablePanel getResizablePanelFor(EntityId entid){
		return hmDiag.get(entid.getId());
	}
	public EntityId getEntityIdFor(ResizablePanel rzp){
		return new EntityId(hmDiag.inverse().get(rzp));
//		for(Entity ent:entsetHierarchyQuery){
//			if(rzp==ent.get(GuiLink.class).getResizablePanel()){
//				return ent.getId();
//			}
//		}
//		return null;
	}

	private void updateDragEffect(){
		Panel pnl = DragParentestPanelListenerI.i().getParentestBeingDragged();
		if(pnl!=null && ResizablePanel.class.isInstance(pnl)){
			EntityId entid = getEntityIdFor((ResizablePanel)pnl);
			EntityId entidParent = ed.getComponent(entid, HierarchyComp.class).getHierarchyParent();
			if(entidParent!=null){
				ResizablePanel rzpParent = hmDiag.get(entidParent.getId());
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

	public EntityId getHierarchyParentOf(EntityId entid) {
		Entity ent = hisys.getEntityFor(entid,HierarchyComp.class);
		EntityId entidParent = ent.get(HierarchyComp.class).getHierarchyParent();
		return entidParent;
//		Entity entParent = hisys.getEntityFor(entidParent,HierarchyComp.class);
//		return entParent.get(HierarchyComp.class);
	}

	private void updateZOrder(Entity ent){
		HierarchyComp hc = ent.get(HierarchyComp.class);
	// main position
//		Vector3f v3fPos = rzp.getLocalTranslation();
		ent.set(new HierarchyComp(hc,EField.fZ,fCurrentOrderPosZ));
//		rzp.setLocalTranslation(v3fPos.x,v3fPos.y,fCurrentOrderPosZ);
//		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(rzp);
//		if(v3fSize!=null){ //only if it is ready
		Float fHeight = hc.getBoundingHeightZ();
		if(fHeight!=null){ //only if it is ready
//			fCurrentOrderPosZ += v3fSize.z +fInBetweenGapDistZ;
			fCurrentOrderPosZ += fHeight +fInBetweenGapDistZ;
		}
	}

	public static class RelativePos implements EntityComponent, PersistentComponent{
		private Vector3f	v3fPositionRelativeToParent = new Vector3f(20, -20, 0); //cascade like
		/**
		 * @param v3fPositionRelativeToParent if null will use default
		 */
		public RelativePos(Vector3f v3fPositionRelativeToParent) {
			if(v3fPositionRelativeToParent!=null)this.v3fPositionRelativeToParent = v3fPositionRelativeToParent;
		}
		public Vector3f getPositionRelativeToParent() {
			return v3fPositionRelativeToParent;
		}
	}

	public void setAutoMoveRelativelyToParent(EntityId entid){
		ed.setComponent(entid, new RelativePos(null));
	}
	
}
