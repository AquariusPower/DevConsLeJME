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

package com.github.devconslejme.es;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Set;

import com.github.devconslejme.es.HierarchyComp.EField;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchy;
import com.github.devconslejme.misc.HierarchySorterI.IHierarchy;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.TimeConvertI;
import com.simsilica.es.ComponentFilter;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityComponent;
import com.simsilica.es.EntityId;
import com.simsilica.es.EntitySet;
import com.simsilica.es.Name;
import com.simsilica.es.PersistentComponent;
import com.simsilica.es.base.DefaultEntityData;


/**
 * DevSelfNote: Components: only getters; unmuttable: do not extend, store things that cant be changed or references; TODO confirm if references is a valid unmuttable... or Ids should be used instead?
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DialogHierarchySystemI {
	public static DialogHierarchySystemI i(){return GlobalManagerI.i().get(DialogHierarchySystemI.class);}
	
//	private EntitySet	entsetBasicQuery; 
	private EntitySet	entsetHierarchyQuery;
	
//	private Application	app;

//	private ResizablePanel	rzpCurrentlyBeingResized;
	
	private DefaultEntityData	ed;
	private ArrayList<Class>	aclAllComponentTypes;

	private ArrayList<Entity>	aentSortedHierarchyDialogs = new ArrayList<Entity>();
	
	public void configure(){
    ed = new DefaultEntityData(); //holds all components
		recreateFullQuery(); //just to create it empty so new entities will be detected
		if(entsetHierarchyQuery.size()>0)throw new DetailedException("must begin empty so news and changes can be properly applied",entsetHierarchyQuery,ed);
    
//		app=GlobalManagerI.i().get(Application.class);
//    app.getStateManager().attach(this);
	}
	
	public Entity getTopMostDialog() {
		ArrayList<Entity> aent = prepareSortedHierarchyDialogs(null);
		return aent.get(aent.size()-1);
	}
	
	
//	public static class Blocker implements EntityComponent, PersistentComponent{
////		private ColorRGBA color;
//		Panel val;
//		private boolean bBlocking=false;
//		
//		public Blocker(Panel val, Boolean bBlocking) {
//			this.val = val;
//			if(bBlocking!=null)this.bBlocking = bBlocking;
//		}
//		
//		public Panel getLayer(){ return val; }
//		public boolean isBlocking() {return bBlocking;}
////		public ColorRGBA getColor() {return color;}
//	}
	
	
//	public static class LastFocusTime implements EntityComponent, PersistentComponent{
//		long val=-1;
//		public LastFocusTime() {}
//		public LastFocusTime(long val) { this.val=val; }
//		public long getLastFocusTime(){ return val; }
//	}
	
//	public static class ShownState implements EntityComponent, PersistentComponent{
//		private EntityId hierarchyParent=null;
//		private EHierarchy	eHierarchyTop=EHierarchy.Normal;
//		private boolean	bHierarchyModal=false;
//		private boolean bShowLinkToChild=true;
//		
//		public ShownState() {}
//		
//		public ShownState(
//				EntityId hierarchyParent, 
//				EHierarchy eHierarchyTop,
//				Boolean bHierarchyModal,
//				Boolean bShowLinkToChild
//		) {
//			this.hierarchyParent = hierarchyParent;
//			if(eHierarchyTop   !=null)this.eHierarchyTop   = eHierarchyTop;
//			if(bHierarchyModal !=null)this.bHierarchyModal = bHierarchyModal;
//			if(bShowLinkToChild!=null)this.bShowLinkToChild=bShowLinkToChild;
//		}
//
//		public EntityId getHierarchyParent() {
//			return hierarchyParent;
//		}
//
//		public EHierarchy getHierarchyPriority() {
//			return eHierarchyTop;
//		}
//
//		public boolean isHierarchyModal() {
//			return bHierarchyModal;
//		}
//
//		public boolean isShowLinkToChild() {
//			return bShowLinkToChild;
//		}
//		
//	}
	
	public boolean isBlocking(Entity ent){
		HierarchyComp hc=ent.get(HierarchyComp.class);
		if(hc==null)return false;
		return hc.isBlocking();
	}

	public void enableBlockingLayer(Entity ent, boolean b){
		HierarchyComp hc = ent.get(HierarchyComp.class); //ed.getComponent(ent.getId(), Blocker.class)
//		ent.set(new Blocker(hc.getLayer(),b));
		ent.set(new HierarchyComp(hc,
			EField.bBlocking,b));
	}
	
	public EntityId updateLastFocusAppTimeNano(EntityId entid) {
		EntityId entidParent = entid;
		EntityId entidParentest = entidParent;
//		System.err.println("cheking");
		while(true){ 
//			System.err.println(entidParent.getId());
//			entidParent = ed.getComponent(entidParent, ShownState.class).getHierarchyParent();
			entidParent = ed.getComponent(entidParent, HierarchyComp.class).getHierarchyParent();
			if(entidParent==null)break;
//			ed.setComponent(entidParent,
//					new LastFocusTime(TimeConvertI.i().getNanosFrom(app.getTimer())));
			entidParentest = entidParent;
		}
		
		Entity entParentest = entsetHierarchyQuery.getEntity(entidParentest);
		entParentest.set(new HierarchyComp(entParentest.get(HierarchyComp.class), 
			EField.lLastFocusTime, lTime //TimeConvertI.i().getNanosFrom(app.getTimer())
		));
//		ed.setComponent(entidParentest, 
//			new LastFocusTime(TimeConvertI.i().getNanosFrom(app.getTimer())));
		
		return entidParentest;
	}
	
	private static class FilterByHierarchyParent implements ComponentFilter<EntityComponent>{
		private Entity	ent;
		public FilterByHierarchyParent(Entity ent){this.ent=ent;}
		@Override
		public Class<EntityComponent> getComponentType() {
			return null;
		}
		@Override
		public boolean evaluate(EntityComponent c) {
			return ((HierarchyComp)c).getHierarchyParent().equals(ent.getId());
		}
	}
	
	/**
	 * 
	 * @param entParentFilter if null will bring all possible
	 * @return
	 */
	public ArrayList<Entity> prepareSortedHierarchyDialogs(EntityId entidParentFilter){
		aentSortedHierarchyDialogs.clear();
		
		/**
		 * TODO how to make this work?
		EntitySet entset = ed.getEntities(new FilterByHierarchyParent(ent), ShownState.class);
		 */
//		EntitySet entset = ed.getEntities(GuiLink.class,ShownState.class,LastFocusTime.class);
		
		for(Entity entChild:entsetHierarchyQuery){
			HierarchyComp hcChild = entChild.get(HierarchyComp.class);
			if(!hcChild.isOpened())continue;
			
			boolean bAdd=false;
			if(entidParentFilter==null){
				bAdd=true;
			}else{
				EntityId entidParent = hcChild.getHierarchyParent();
				if(entidParentFilter.equals(entidParent)){
					bAdd=true;
				}
			}
			
			if(bAdd)aentSortedHierarchyDialogs.add(entChild);
		}		
		
		if(entidParentFilter==null)sortDialogs(aentSortedHierarchyDialogs);
//		Collections.sort(aent,cmpr); // uses LastFocusTime
		
		return aentSortedHierarchyDialogs;
	}
	
	private Comparator<Entity> cmprByLastFocusTime = new Comparator<Entity>() {
		@Override
		public int compare(Entity o1, Entity o2) {
			return Long.compare(
				o1.get(HierarchyComp.class).getLastFocusTime(),
				o2.get(HierarchyComp.class).getLastFocusTime()
			);
		}
	};

	private long	lTime;
	
	private static class DiagHierarchyWrapper implements IHierarchy{
		private Entity	ent;
		private HierarchyComp hc;

		public DiagHierarchyWrapper(Entity ent){
			this.ent=ent;
			hc = ent.get(HierarchyComp.class);
		}
		
		public Entity getEntity(){
			return ent;
		}
		
		@Override
		public DiagHierarchyWrapper getHierarchyParent() {
			if(hc.getHierarchyParent()==null)return null;
			
			return new DiagHierarchyWrapper(
				DialogHierarchySystemI.i().entsetHierarchyQuery.getEntity(
					hc.getHierarchyParent()
				)
			);
		}

		@Override
		public EHierarchy getHierarchyPriority() {
			return hc.getHierarchyPriority();
		}

		@Override
		public long getLastActivationNanoTime() {
			return ent.get(HierarchyComp.class).getLastFocusTime();
		}
		
		@Override
		public boolean equals(Object obj) {
			return ent.getId().equals(((DiagHierarchyWrapper)obj).getEntity().getId());
		}
	}
	
//	Comparator<HierarchySort> cmprEntEquals = new Comparator<HierarchySort>() {
//		@Override
//		public int compare(HierarchySort o1, HierarchySort o2) {
//			if(o1.getEntity().getId().equals(o2.getEntity().getId()))return 0;
//			return -1; //anything not 0
//		}
//	};
	
	private void sortDialogs(ArrayList<Entity> aentMainList) {
		ArrayList<DiagHierarchyWrapper> ahs = new ArrayList<DiagHierarchyWrapper>();
		for(Entity ent:aentMainList){
			ahs.add(new DiagHierarchyWrapper(ent));
		}
		
		HierarchySorterI.i().sort(ahs);//,cmprEntEquals);
		
		aentMainList.clear();
		
		for(DiagHierarchyWrapper hs:ahs){
			aentMainList.add(hs.getEntity());
		}
	}
	
//	/**
//	 * TODO generalize and put at the simple @MiscPackage
//	 * the last items will be the ones shown above all others
//	 * @param aentMainList
//	 */
//	private void _sortDialogs(ArrayList<Entity> aentMainList) {
//		if(aentMainList.size()==0)return;
//		
//		/**
//		 * root dialogs, that have no parent
//		 */
//		ArrayList<Entity> aentRootDialogs = new ArrayList<Entity>(); 
//		ArrayList<Entity> aentRootTopDialogs = new ArrayList<Entity>();
//		for(Entity ent:aentMainList.toArray(new Entity[0])){
//			ShownState ss = ent.get(ShownState.class);
//			if(ss.getHierarchyParent()==null){
//				if(ss.isHierarchyTop()){
//					aentRootTopDialogs.add(ent);
//				}else{
//					aentRootDialogs.add(ent);
//				}
//				
//				aentMainList.remove(ent);
//			}
//		}
//		
//		// all remaining are childs and recursive childs
//		ArrayList<Entity> aentChilds = new ArrayList<Entity>(); //that have no parent
//		aentChilds.addAll(aentMainList);
//		aentMainList.clear();
//		
//		Collections.sort(aentRootDialogs,cmprByLastFocusTime);
//		Collections.sort(aentRootTopDialogs,cmprByLastFocusTime);
//		Collections.sort(aentChilds,cmprByLastFocusTime);
//		
//		//////////////////////////// populate main
//		// add roots 
//		aentMainList.addAll(aentRootDialogs);
//		aentMainList.addAll(aentRootTopDialogs);
//		
//		/**
//		 * add childs just after their parents,
//		 * 
//		 * reversed because: 
//		 * the latest focused topmost child will become the 1st after its parent,
//		 * and just after, the previously focused child of the same parent will be the 1st after its parent
//		 * pushing the topmost to the 2nd place after its parent... and so on...
//		 * 
//		 * 1)
//		 * parent
//		 * |_topmost child
//		 * 
//		 * 2)
//		 * parent
//		 * |_previously focused child
//		 * |_topmost child
//		 */
//		Collections.reverse(aentChilds);
//		labelCheckChildListEmpty:while(aentChilds.size()>0){
//			for(Entity entChild:aentChilds.toArray(new Entity[0])){
//				EntityId entidParent = entChild.get(ShownState.class).getHierarchyParent();
//				
//				// find the current index of the parent on the main list
//				int iIndexOfParentAtMain=-1;
//				for (int i = 0; i < aentMainList.size(); i++) {
//					Entity entChk = aentMainList.get(i);
//					if(entChk.getId().equals(entidParent)){
//						iIndexOfParentAtMain = i;
//						break;
//					}
//				}
//				
//				if(iIndexOfParentAtMain>-1){
//					aentChilds.remove(entChild);
//					aentMainList.add(iIndexOfParentAtMain+1, entChild);
//					continue labelCheckChildListEmpty;
//				}
//			}
//		}
//	}


//	private void updateLoop() {
//		fCurrentOrderPosZ = fBeginOrderPosZ;
//		for(Entity ent:getHierarchyDialogs(null)){
//			ResizablePanel rzp=ent.get(GuiLink.class).getResizablePanel();
//			
//			updateZOrder(rzp);
//			
//			updateFocusTime(ent,rzp);
//		}
//		
//		updateDragEffect();
//	}
	
//	private void updateFocusTime(Entity ent, ResizablePanel rzp) {
//		EntityId entidUpdLFTime=null;
//		
//		// has real input focus
//		if(entidUpdLFTime==null && isFocused(rzp)){
//			entidUpdLFTime=ent.getId();
//		}
//		
//		// being dragged
//		if(entidUpdLFTime==null){
//			Panel pnlParentestDragged = DragParentestPanelListenerI.i().getParentestBeingDragged();
//			if(pnlParentestDragged!=null && ResizablePanel.class.isInstance(pnlParentestDragged)){
//				entidUpdLFTime=getEntityIdFor((ResizablePanel)pnlParentestDragged);
//			}
//		}
//		
//		// being resized
//		if(entidUpdLFTime==null && getCurrentlyBeingResized()!=null){
//			entidUpdLFTime=getEntityIdFor(getCurrentlyBeingResized());
//		}
//		
//		if(entidUpdLFTime!=null){
//			setFocusRecursively(entidUpdLFTime);
////			updateLastFocusAppTimeNano(entidUpdLFTime);
//		}
//	}
	
//	public ResizablePanel getCurrentlyBeingResized(){
//		return rzpCurrentlyBeingResized;
//	}
	
	public boolean isHasAnyDialogOpened() {
		return (prepareSortedHierarchyDialogs(null).size()>0);
	}

	public void cleanupRemovedEntity(Float tpf, Entity ent) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	public ArrayList<String> getListAsReport(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Entity ent:prepareSortedHierarchyDialogs(null)){
			astr.add(getReport(ent,false));
		}
		return astr;
	}

	public String getReport(Entity ent, boolean b) {
		StringBuilder sb = new StringBuilder();
		
		HierarchyComp hc = ent.get(HierarchyComp.class);
//		ResizablePanel rzp = ent.get(GuiLink.class).getResizablePanel();
		
		sb.append("pri="+hc.getHierarchyPriority());
		sb.append("/");
		
//		sb.append("z="+rzp.getSize().z);
//		sb.append(",");
//		sb.append("Pz="+rzp.getPreferredSize().z);
//		sb.append("/");
		
		sb.append("tm="+hc.getLastFocusTime());
		sb.append("/");
		
//		sb.append("dbgnm="+rzp.getName());
//		sb.append("/");
		
		return sb.toString();
	}

	public void setHierarchyPriority(EntityId entid, EHierarchy eHierarchy) {
		Entity ent = ed.getEntity(entid, HierarchyComp.class);
		HierarchyComp ss = ent.get(HierarchyComp.class);
//	ent.set(
		ed.setComponent(entid, 
			new HierarchyComp(ss,
//				ss==null?null:ss.getHierarchyParent(), 
				EField.eHierarchy,eHierarchy 
//				ss==null?null:ss.isHierarchyModal(),
//				null
			)
		);
	}
	
	public Entity getEntityFor(EntityId entid, Class... acl) {
		return ed.getEntity(entid, acl);
	}

//	public GuiLink getHierarchyParentGuiLinkFor(EntityId entid) {
//		Entity ent = getEntityFor(entid,HierarchyComp.class);
//		EntityId entidParent = ent.get(HierarchyComp.class).getHierarchyParent();
//		Entity entParent = getEntityFor(entidParent,GuiLink.class);
//		return entParent.get(GuiLink.class);
//	}
	
//	@Override
//	public void resizerUpdatedLogicalStateEvent(float tpf, ResizablePanel rzpSource) {
//		EntityId entid = getEntityIdFor(rzpSource);
//		updateBlocker(tpf, entid, rzpSource);
//		
//		EntityId entidParent = ed.getComponent(entid, HierarchyComp.class).getHierarchyParent();
//		if(entidParent!=null){
//			RelativePos rp = ed.getComponent(entid, RelativePos.class);
//			if(rp!=null){
//				ResizablePanel rzpParent = ed.getComponent(entidParent, GuiLink.class).getResizablePanel();
//				
////				Panel panelUserAction = DragParentestPanelListenerI.i().getParentestBeingDragged();
////				if(panelUserAction==null){
////					panelUserAction=getCurrentlyBeingResized();
////				}
//				
//				if(
//						DragParentestPanelListenerI.i().getParentestBeingDragged()==rzpSource
//						||
//						getCurrentlyBeingResized()==rzpSource
//				){
////				if(panelUserAction!=null){
//					/**
//					 * update displacement
//					 */
//					ed.setComponent(entid, new RelativePos(
//							rzpSource.getLocalTranslation().subtract(rzpParent.getLocalTranslation()) ) );
//					
////					if(!ieffLinkedDragEffect.isPlaying()){
////						ieffLinkedDragEffect
////							.setOwner(rzpSource)
////							.setFollowFromTarget(rzpParent, null)
////							.setFollowToTarget(rzpSource, null)
////							.setPlay(true)
////							;
////					}
//				}else{
////					if(ieffLinkedDragEffect.isPlaying()){
////						ieffLinkedDragEffect.setPlay(false);
////						ieffLinkedDragEffect.setOwner(null);
////					}
//					
//					/**
//					 * set position relatively to parent
//					 */
//					Vector3f v3fNewPos = rzpParent.getLocalTranslation().clone();
//					v3fNewPos.addLocal(rp.getPositionRelativeToParent());
//					v3fNewPos.z=rzpSource.getLocalTranslation().z;
//					
//	//					v3fNewPos.x=rzpParent.getLocalTranslation().x 
//	//						+ (rzpParent.getSize().x/2f - rzpSource.getSize().x/2f + fCascadeDist);
//	//					v3fNewPos.y=rzpParent.getLocalTranslation().y 
//	//						- (rzpParent.getSize().y/2f - rzpSource.getSize().y/2f + fCascadeDist);
//					
//					rzpSource.setLocalTranslation(v3fNewPos);
//				}
//			}
//		}
//	}

//	@Override
//	public void removedFromParentEvent(ResizablePanel rzpSource) {
//		updateBlocker(null, getEntityIdFor(rzpSource), rzpSource);
//	}

//	@Override
//	public void resizedEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {
//		rzpCurrentlyBeingResized=rzpSource;
//	}
//
//	@Override
//	public void endedResizingEvent(ResizablePanel rzpSource) {
//		rzpCurrentlyBeingResized=null; //user can resize only one at a time 
//	}

	public DefaultEntityData getEntityData(){
		return ed;
	}

//	public static class GuiLink implements EntityComponent, PersistentComponent{
//		private ResizablePanel val;
//		public GuiLink(ResizablePanel val) { this.val=val; }
//		public ResizablePanel getResizablePanel(){ return val; }
//	}
	
//	public static class Initialized implements EntityComponent, PersistentComponent{
//		private boolean	bInitialized;
//		public Initialized(boolean bInitialized) {
//			this.bInitialized=bInitialized;
//		}
//		public boolean isInitialized() {
//			return bInitialized;
//		}
//	}
	
//	public EntityId createEntity(ResizablePanel rzp,String strName){
	public EntityId createEntity(String strName){
//	  if(entsetBasicQuery==null){ // keep here to be easy to sync with the settings below
//		  entsetBasicQuery = ed.getEntities(
//		  	Initialized.class,
//		  	GuiLink.class,
//		  	Name.class
//		  );
//	  }
//		recreateQuery();
//		if(entsetMainQuery.size()>0)throw new DetailedException("must begin empty so news and changes can be properly applied",entsetMainQuery,ed);
	  EntityId entid = ed.createEntity(); //to attach components to
//	  ed.setComponent(entid, new Initialized(false));
	  ed.setComponent(entid, new HierarchyComp(null,
	  	EField.bInitVisuals,false
	  ));
//	  ed.setComponent(entid, new GuiLink(rzp));
	  ed.setComponent(entid, new Name(strName));
	  
	  return entid;
	}
	
	private void recreateFullQuery(){
		if(entsetHierarchyQuery!=null){
			applyFullQueryChanges(null); //last update for it
		}
		
		entsetHierarchyQuery = ed.getEntities(getAllRequiredComponentTypesArray());
	}
	
	private void applyFullQueryChanges(Float tpf){
//		if(entsetBasicQuery!=null && entsetBasicQuery.applyChanges()) { //contains only basic components
//			// newly matching entities
//			initializeNewEntities(tpf,entsetBasicQuery.getAddedEntities()); //after this, will contain all hierarchy components
//		}
		
		if(entsetHierarchyQuery.applyChanges()) { //contains all components required by hierarchy
			// newly matching entities
			initializeNewEntities(tpf,entsetHierarchyQuery.getAddedEntities());
			
			// entities that have merely changed TODO like in have any component changed? 
			updateChangedEntities(tpf,entsetHierarchyQuery.getChangedEntities());
			
			// entities that are no longer matching TODO like in one or more of the required query components went missing
			workOnEntitiesThatAreNotFullyMatchingAnymore(tpf,entsetHierarchyQuery.getRemovedEntities());
		}
	}
	
	private void initializeNewEntities(Float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			HierarchyComp hc = ent.get(HierarchyComp.class);
			if(!hc.isInitHierarchy()){
				initializeNewEntity(tpf,ent);
				ed.setComponent(ent.getId(), new HierarchyComp(hc,EField.bInitHierarchy,true));
			}
		}
	}
	
	private void initializeNewEntity(Float tpf, Entity ent) {
		// keep as place holder
	}

	private void updateChangedEntities(Float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			updateChangedEntity(tpf,ent.getId());
		}
	}

	private void updateChangedEntity(Float tpf, EntityId id) {
		// keep as place holder
	}

	public void update(float tpf, long lTime) {
		this.lTime=lTime;
		applyFullQueryChanges(tpf);
		
//		fCurrentOrderPosZ = fBeginOrderPosZ;
//		for(Entity ent:prepareSortedHierarchyDialogs(null)){
//			updateZOrder(ent);
//			
////			updateFocusTime(ent,hc);
//		}
		
//		updateDragEffect();
		
//		if(isHasAnyDialogOpened()){
////			if(getHierarchyComponentList(null).size()>0){
//			Entity ent = getTopMostDialog();
//			if(!ContextMenuI.i().isTheContextMenu(ent.get(GuiLink.class).getResizablePanel())){
//				ContextMenuI.i().hideContextMenu();
//			}
//		}
	}
	
	private void workOnEntitiesThatAreNotFullyMatchingAnymore(Float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			cleanupRemovedEntity(tpf,ent);
		}
	}
	
	public Class[] getAllRequiredComponentTypesArray() {
		return getAllRequiredComponentTypesList().toArray(new Class[0]);
	}
	public ArrayList<Class> getAllRequiredComponentTypesList() {
		if(aclAllComponentTypes==null){
			aclAllComponentTypes = new ArrayList<Class>();
			addRequiredComponentType(HierarchyComp.class);
//			addRequiredComponentType(Initialized.class);
//			addRequiredComponentType(GuiLink.class);
			addRequiredComponentType(Name.class);
//			addRequiredComponentType(Blocker.class);
//			addRequiredComponentType(LastFocusTime.class);
//			addRequiredComponentType(ShownState.class);
		}
		return aclAllComponentTypes;
	}
	public void addRequiredComponentType(Class cl){
		if(!aclAllComponentTypes.contains(cl)){
			aclAllComponentTypes.add(cl);
			MessagesI.i().debugInfo(this,"added component: "+cl.getName());
			recreateFullQuery();
		}else{
			MessagesI.i().warnMsg(this, "component already added "+cl.getName());
		}
	}

	public EntitySet getEntities() {
		return ed.getEntities(getAllRequiredComponentTypesArray());
	}

	public Entity[] getSortedHierarchyDialogs() {
		return aentSortedHierarchyDialogs.toArray(new Entity[0]);
	}
	
}
