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

package com.github.devconslejme.gendiag.es;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Set;

import com.github.devconslejme.gendiag.ContextMenuI;
import com.github.devconslejme.gendiag.ResizablePanel;
import com.github.devconslejme.gendiag.ResizablePanel.IResizableListener;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchy;
import com.github.devconslejme.misc.HierarchySorterI.IHierarchy;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.TimeConvertI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.EffectArrow;
import com.github.devconslejme.misc.jme.EffectElectricity;
import com.github.devconslejme.misc.jme.EffectManagerStateI;
import com.github.devconslejme.misc.jme.IEffect;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.es.ComponentFilter;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityComponent;
import com.simsilica.es.EntityId;
import com.simsilica.es.EntitySet;
import com.simsilica.es.Name;
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
 * DevSelfNote: Components: only getters; unmuttable: do not extend, store things that cant be changed or references; TODO confirm if references is a valid unmuttable... or Ids should be used instead?
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DialogHierarchyI extends AbstractAppState implements IResizableListener{
	public static DialogHierarchyI i(){return GlobalManagerI.i().get(DialogHierarchyI.class);}
	
	private EntitySet	entsetBasicQuery; 
	private EntitySet	entsetHierarchyQuery;
	
	private Node nodeToMonitor;
	private Application	app;
	private FocusManagerState	focusState;
	private float	fBeginOrderPosZ = 0f;

	private ResizablePanel	rzpCurrentlyBeingResized;
	
	/**
	 * so the blocker can stay in that gap
	 */
	private float	fInBetweenGapDistZ=1.0f;
	
	private DefaultEntityData	ed;
	private ArrayList<Class>	aclAllComponentTypes;
	private BlockerListener blockerListener = new BlockerListener();
	private float	fMinLemurPanelSizeZ = 0.01f;
	private float	fCurrentOrderPosZ;
	private ColorRGBA	colorBlocker = ColorI.i().colorChangeCopy(ColorRGBA.Red, 0f, 0.15f);
	private ColorRGBA	colorInvisible = new ColorRGBA(0,0,0,0);
	private IEffect	ieffParentToChildLink = new EffectArrow();
	private IEffect	ieffLinkedDragEffect = new EffectElectricity().setColor(ColorI.i().colorChangeCopy(ColorRGBA.Blue, 0f, 0.5f));
	
	public void configure(Node nodeToMonitor, float fBeginOrderZ){
		this.fBeginOrderPosZ=fBeginOrderZ;
		this.nodeToMonitor=nodeToMonitor;
		
    ed = new DefaultEntityData(); //holds all components
		recreateFullQuery(); //just to create it empty so new entities will be detected
		if(entsetHierarchyQuery.size()>0)throw new DetailedException("must begin empty so news and changes can be properly applied",entsetHierarchyQuery,ed);
    
		app=GlobalManagerI.i().get(Application.class);
    app.getStateManager().attach(this);
    
	//	app.getStateManager().attach(this);
		focusState=app.getStateManager().getState(FocusManagerState.class);
		
//		QueueI.i().enqueue(new CallableX() 
//			{
//				@Override
//				public Boolean call() {
//					updateLoop();
//					
//					if(isHasAnyDialogOpened()){
//		//			if(getHierarchyComponentList(null).size()>0){
//						Entity ent = getTopMostDialog();
//						if(!ContextMenuI.i().isTheContextMenu(ent.get(GuiLink.class).getResizablePanel())){
//							ContextMenuI.i().hideContextMenu();
//						}
//					}
//					
//					return true;
//				}
//			}
//			.setName(DialogHierarchyI.class.getSimpleName())
//			.setDelaySeconds(0.25f)
//			.setLoop(true)
//			.setUserCanPause(true)
//		);
		
		EffectManagerStateI.i().add(ieffLinkedDragEffect);
	}
	
	protected Entity getTopMostDialog() {
		ArrayList<Entity> aent = getHierarchyDialogs(null);
		return aent.get(aent.size()-1);
		
//		Entity entTopMost = aent.get(0);
//		for(Entity ent:aent){
//			if(
//					entTopMost.get(GuiLink.class).getResizablePanel().getLocalTranslation().z
//					<
//					ent.get(GuiLink.class).getResizablePanel().getLocalTranslation().z
//			){
//				
//			}
//		}
//		return entTopMost;
	}
	
	public static class HierarchyComp implements EntityComponent, PersistentComponent{
		public static enum EField{
			bBlocking, //#syncTo
			bInitialized, //#syncTo
			lLastFocusTime,
			eidParent,
			eHierarchy,
			bHierarchyModal,
			bShowLinkToChild,
			;
		}
		private boolean bBlocking=false; //#syncFrom bBlocking
		private boolean	bInitialized=false; //#syncFrom bInitialized
		private long lLastFocusTime=-1;
		private EntityId eidParent=null;
		private EHierarchy	eHierarchy=EHierarchy.Normal;
		private boolean	bHierarchyModal=false;
		private boolean bShowLinkToChild=true;
		
		public boolean isBlocking() {return bBlocking;}
		public boolean isInitialized() {return bInitialized;}
		public long getLastFocusTime() {return lLastFocusTime;}
		public EntityId getHierarchyParent() {return eidParent;}
		public EHierarchy getHierarchyPriority() {return eHierarchy;}
		public boolean isHierarchyModal() {return bHierarchyModal;}
		public boolean isShowLinkToChild() {return bShowLinkToChild;}
		
		public HierarchyComp(HierarchyComp copyFrom, Object... aobjFieldsAndValues){
			copyFrom(copyFrom); //initialize
			
			EField e = null;
			Object objValue = null;
			for(Object obj:aobjFieldsAndValues){
				if(e!=null){objValue=obj;}
				else
				if (obj instanceof EField){e = (EField) obj; continue;}
				
				switch (e) {
					case bBlocking:				this.bBlocking=(Boolean)objValue;break;
					case bInitialized:		this.bInitialized=(Boolean)objValue;break;
					case bHierarchyModal:	this.bHierarchyModal=(Boolean)objValue;break;
					case bShowLinkToChild:this.bShowLinkToChild=(Boolean)objValue;break;
					case eHierarchy:			this.eHierarchy=(EHierarchy)objValue;break;
					case eidParent:				this.eidParent=(EntityId)objValue;break;
					case lLastFocusTime:	this.lLastFocusTime=(Long)objValue;break;
				}
				
				e = null;
			}
		}
		
		private void copyFrom(HierarchyComp copyFrom) {
			this.bBlocking=copyFrom.bBlocking;
			this.bInitialized=copyFrom.bInitialized;
		}

	}
	
	public static class Blocker implements EntityComponent, PersistentComponent{
//		private ColorRGBA color;
		Panel val;
		private boolean bBlocking=false;
		
		public Blocker(Panel val, Boolean bBlocking) {
			this.val = val;
			if(bBlocking!=null)this.bBlocking = bBlocking;
		}
		
		public Panel getLayer(){ return val; }
		public boolean isBlocking() {return bBlocking;}
//		public ColorRGBA getColor() {return color;}
	}
	
	public static class BlockerListener extends DefaultCursorListener{
		@Override
		protected void click(CursorButtonEvent event, Spatial target, 	Spatial capture) {
			super.click(event, target, capture);
			
			EntityId entid = UserDataI.i().getUserDataPSH(capture,EntityId.class);
			Blocker blk = DialogHierarchyI.i().ed.getComponent(entid,Blocker.class);
			if(blk.isBlocking()){
				DialogHierarchyI.i().setFocusRecursively(entid);
				event.setConsumed();
			}
		}
		
	}
	
	public static class LastFocusTime implements EntityComponent, PersistentComponent{
		long val=-1;
		public LastFocusTime() {}
		public LastFocusTime(long val) { this.val=val; }
		public long getLastFocusTime(){ return val; }
	}
	
	public static class ShownState implements EntityComponent, PersistentComponent{
		private EntityId hierarchyParent=null;
		private EHierarchy	eHierarchyTop=EHierarchy.Normal;
		private boolean	bHierarchyModal=false;
		private boolean bShowLinkToChild=true;
		
		public ShownState() {}
		
		public ShownState(
				EntityId hierarchyParent, 
				EHierarchy eHierarchyTop,
				Boolean bHierarchyModal,
				Boolean bShowLinkToChild
		) {
			this.hierarchyParent = hierarchyParent;
			if(eHierarchyTop   !=null)this.eHierarchyTop   = eHierarchyTop;
			if(bHierarchyModal !=null)this.bHierarchyModal = bHierarchyModal;
			if(bShowLinkToChild!=null)this.bShowLinkToChild=bShowLinkToChild;
		}

		public EntityId getHierarchyParent() {
			return hierarchyParent;
		}

		public EHierarchy getHierarchyPriority() {
			return eHierarchyTop;
		}

		public boolean isHierarchyModal() {
			return bHierarchyModal;
		}

		public boolean isShowLinkToChild() {
			return bShowLinkToChild;
		}
		
	}
	
	public void setBlockerColor(ColorRGBA color){
		colorBlocker = color;
	}
	
	public void initializeNewEntity(Float tpf,Entity ent) {
//		EntityId entid = ent.getId();
		
//	  ed.setComponent(entid, new GuiLink(rzp));
//	  ed.setComponent(entid, new Initialized(false));
//	  ed.setComponent(entid, new Name(strName));
	  
//		if(ed.getComponent(ent.getId(), ShownState.class)==null){
		//if(ent.get(ShownState.class)==null){
			ed.setComponent(ent.getId(), new ShownState());
//		}
		
//		if(ed.getComponent(ent.getId(), LastFocusTime.class)==null){
		//if(ent.get(LastFocusTime.class)==null){
			ed.setComponent(ent.getId(), new LastFocusTime());
//		}
		
		/////////////// blocker
		ResizablePanel rzp = ent.get(GuiLink.class).getResizablePanel();
		
//		if(ed.getComponent(ent.getId(), Blocker.class)==null){
			Panel pnlBlocker = new Panel("");
			ed.setComponent(ent.getId(),new Blocker(pnlBlocker,null)); //ent.set(
			
			pnlBlocker.setBackground(new QuadBackgroundComponent(colorBlocker));
			
			//the blocker has not a parent panel! so it will let the dialog be dragged directly!
			DragParentestPanelListenerI.i().applyAt(pnlBlocker, rzp);  
			
			CursorEventControl.addListenersToSpatial(pnlBlocker,blockerListener);
			
			UserDataI.i().setUserDataPSH(pnlBlocker,ent.getId());
//		}
		
		// panel
		rzp.addUpdateLogicalStateListener(this);
		HoverHighlightEffectI.i().applyAt(rzp, (QuadBackgroundComponent)rzp.getResizableBorder());
	}

	public boolean isBlocking(Entity ent){
		Blocker blocker=ent.get(Blocker.class);
		if(blocker==null)return false;
		return blocker.isBlocking();
	}

	public void enableBlockingLayer(Entity ent, boolean b){
		Blocker blocker = ent.get(Blocker.class); //ed.getComponent(ent.getId(), Blocker.class)
		ent.set(new Blocker(blocker.getLayer(),b));
	}
	
	/**
	 * will prevent access to parent
	 * @param compChild
	 * @return 
	 */
	public void showDialogAsModal(EntityId entidParent, EntityId entidChild){
		showAndApplyHierarchyChild(entidParent,entidChild,true);
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
		showAndApplyHierarchyChild(entidParent,entidChild,false);
	}
	
	private void showAndApplyHierarchyChild(EntityId entidParent, EntityId entidChild, boolean bModal){
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				if(!ed.getComponent(entidParent, Initialized.class).isInitialized())return false;
				if(!ed.getComponent(entidChild, Initialized.class).isInitialized())return false;
				
				Entity entParent = ed.getEntity(entidParent, getAllRequiredComponentTypesArray());
				Entity entChild = ed.getEntity(entidChild, getAllRequiredComponentTypesArray());
				
				ResizablePanel rzpChild = entChild.get(GuiLink.class).getResizablePanel();
				
				ShownState ssChild = entChild.get(ShownState.class);
//				entChild.set(new ShownState(
				ed.setComponent(entChild.getId(),new ShownState(
					entParent.getId(), 
					ssChild==null?null:ssChild.getHierarchyPriority(), 
					bModal,
					null)
				);
//				setHierarchyParentAtChild(entParent,entChild);
				if(bModal)enableBlockingLayer(entParent,true);
				
				showDialog(rzpChild); //show it
				setFocusRecursively(entChild.getId());
//				updateLastFocusAppTimeNano(entChild.getId());
				
				ShownState ssParent = entChild.get(ShownState.class);
				if(ssParent.isShowLinkToChild()){
					ResizablePanel rzpParent = entParent.get(GuiLink.class).getResizablePanel();
					applyParentToChildLinkEffect(rzpParent,rzpChild);
				}
				
				return true;
			}
		});
		
	}
	
//	private void setHierarchyParentAtChild(Entity entParent, Entity entChild) {
//		ShownState ss = entChild.get(ShownState.class);
//		entChild.set(new ShownState(entParent.getId(), ss.isHierarchyTop(), ss.isHierarchyModal()));
//	}
	
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
	
//	private static class DialogHierarchy{
//		
//	}
	
//	/**
//	 * 
//	 * @param rzp
//	 * @param bReAdd if false, will close all childs
//	 */
//	private void recursiveWorkOnChildDiagsOf(ResizablePanel rzp,boolean bReAdd) {
//		EntityId entityIdFor = getEntityIdFor(rzp);
//		for(Entity ent:getHierarchyDialogs(entityIdFor)){
//			ResizablePanel rzpChild = ent.get(GuiLink.class).getResizablePanel();
//			arzpZOrderList.remove(rzpChild);
//			if(bReAdd){
//				arzpZOrderList.add(rzpChild);
//			}else{
//				rzp.close();
//			}
//		}
//	}
//	
//	public void closeDialog(ResizablePanel rzp){
//		recursiveWorkOnChildDiagsOf(rzp,false);
//		rzp.close();
//	}
	
	public void setFocusRecursively(EntityId entid){
		ArrayList<Entity> aentChild = getHierarchyDialogs(entid);
//		if(ed.getComponent(entid, Blocker.class).isBlocking()){
		if(aentChild.size()==0){
			ResizablePanel rzp = ed.getComponent(entid, GuiLink.class).getResizablePanel();
			GuiGlobals.getInstance().requestFocus(rzp);
			updateLastFocusAppTimeNano(entid);
		}else{
			for(Entity ent:aentChild){
				setFocusRecursively(ent.getId());
			}
		}
	}
	
	private EntityId updateLastFocusAppTimeNano(EntityId entid) {
		EntityId entidParent = entid;
		EntityId entidParentest = entidParent;
//		System.err.println("cheking");
		while(true){ 
//			System.err.println(entidParent.getId());
			entidParent = ed.getComponent(entidParent, ShownState.class).getHierarchyParent();
			if(entidParent==null)break;
//			ed.setComponent(entidParent,
//					new LastFocusTime(TimeConvertI.i().getNanosFrom(app.getTimer())));
			entidParentest = entidParent;
		}
				
		ed.setComponent(entidParentest, 
			new LastFocusTime(TimeConvertI.i().getNanosFrom(app.getTimer())));
		
		return entidParentest;
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

	private static class FilterByHierarchyParent implements ComponentFilter<EntityComponent>{
		private Entity	ent;
		public FilterByHierarchyParent(Entity ent){this.ent=ent;}
		@Override
		public Class<EntityComponent> getComponentType() {
			return null;
		}
		@Override
		public boolean evaluate(EntityComponent c) {
			return ((ShownState)c).getHierarchyParent().equals(ent.getId());
		}
	}
	
	/**
	 * 
	 * @param entParentFilter if null will bring all possible
	 * @return
	 */
	public ArrayList<Entity> getHierarchyDialogs(EntityId entidParentFilter){
		ArrayList<Entity> aent = new ArrayList<Entity>();
		
		/**
		 * TODO how to make this work?
		EntitySet entset = ed.getEntities(new FilterByHierarchyParent(ent), ShownState.class);
		 */
//		EntitySet entset = ed.getEntities(GuiLink.class,ShownState.class,LastFocusTime.class);
		
		for(Entity entChild:entsetHierarchyQuery){
			if(!entChild.get(GuiLink.class).getResizablePanel().isOpened())continue;
			
			boolean bAdd=false;
			if(entidParentFilter==null){
				bAdd=true;
			}else{
				EntityId entidParent = entChild.get(ShownState.class).getHierarchyParent();
				if(entidParentFilter.equals(entidParent)){
					bAdd=true;
				}
			}
			
			if(bAdd)aent.add(entChild);
		}		
		
		if(entidParentFilter==null)sortDialogs(aent);
//		Collections.sort(aent,cmpr); // uses LastFocusTime
		
		return aent;
	}
	
//	private Comparator<Entity> cmpr = new Comparator<Entity>() {
//		@Override
//		public int compare(Entity o1, Entity o2) {
//			ShownState ss1 = o1.get(ShownState.class); 
//			ShownState ss2 = o2.get(ShownState.class); 
//			// top only against top
//			if( ss1.isHierarchyTop() && !ss2.isHierarchyTop())return  1;
//			if(!ss1.isHierarchyTop() &&  ss2.isHierarchyTop())return -1;
//			
//			// parent diag below child diag
//			if(ss1.getHierarchyParent()==o2.getId())return  1;
//			if(ss2.getHierarchyParent()==o1.getId())return -1;
//			if(ss1==ss2)return 0;
//			
//			// last focus
//			return Long.compare(
//				o1.get(LastFocusTime.class).getLastFocusTime(),
//				o2.get(LastFocusTime.class).getLastFocusTime()
//			);
//		}
//	};
	private Comparator<Entity> cmprByLastFocusTime = new Comparator<Entity>() {
		@Override
		public int compare(Entity o1, Entity o2) {
			return Long.compare(
				o1.get(LastFocusTime.class).getLastFocusTime(),
				o2.get(LastFocusTime.class).getLastFocusTime()
			);
		}
	};
	
	private static class DiagHierarchyWrapper implements IHierarchy{
		private Entity	ent;
		private ShownState ss;

		public DiagHierarchyWrapper(Entity ent){
			this.ent=ent;
			ss = ent.get(ShownState.class);
		}
		
		public Entity getEntity(){
			return ent;
		}
		
		@Override
		public DiagHierarchyWrapper getHierarchyParent() {
			if(ss.getHierarchyParent()==null)return null;
			
			return new DiagHierarchyWrapper(
				DialogHierarchyI.i().entsetHierarchyQuery.getEntity(
					ss.getHierarchyParent()
				)
			);
		}

		@Override
		public EHierarchy getHierarchyPriority() {
			return ss.getHierarchyPriority();
		}

		@Override
		public long getLastActivationNanoTime() {
			return ent.get(LastFocusTime.class).getLastFocusTime();
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

	public void updateChangedEntity(Float tpf,EntityId entid){
		Entity ent = ed.getEntity(entid, GuiLink.class,ShownState.class);
		
		// close self if parent dialog closed
		ResizablePanel rzp = ent.get(GuiLink.class).getResizablePanel();
		if(rzp.isOpened()){
			ShownState ss = ent.get(ShownState.class);
			if(ss.getHierarchyParent()!=null){
				Entity entHierarchyParent = ed.getEntity(
					ss.getHierarchyParent(), GuiLink.class);
				if(!entHierarchyParent.get(GuiLink.class).getResizablePanel().isOpened()){
					rzp.close();
					ent.get(Blocker.class).getLayer().removeFromParent();
				}
			}
		}
		
		// childs will have a reference to the parent now...
//		// remove closed childs
//		for(_Del_HierarchySystemI child:getEntityListOfHierarchyComponents(c())){
//			if(child.getEntityOwner().isClosed()){
//				child.getEntityOwner().updateComponent(new _Del_HierarchySystemI(child, (_Del_HierarchySystemI)null));
////				arzdHierarchyChildList.remove(compChild);
//			}
//		}
	}
	
	/**
	 * 
	 * @param tpf ignored if null
	 * @param entid
	 * @param rzp
	 */
	private void updateBlocker(Float tpf,EntityId entid, ResizablePanel rzp){
		Entity ent = ed.getEntity(entid, GuiLink.class,ShownState.class,Blocker.class);
		
		// blocker work
		Panel pnlBlocker = ent.get(Blocker.class).getLayer();
		if(rzp.isOpened()){
			if(pnlBlocker.getParent()==null){
				nodeToMonitor.attachChild(pnlBlocker);
			}
			
			// how many childs are modal
			int iModalCount=0;
			for(Entity entChild:getHierarchyDialogs(ent.getId())){
				if(entChild.get(ShownState.class).isHierarchyModal())iModalCount++;
			}
			
			if(iModalCount==0){
				enableBlockingLayer(ent,false);
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
					if(isBlocking(ent)){
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
	
	private void updateDragEffect(){
		Panel pnl = DragParentestPanelListenerI.i().getParentestBeingDragged();
		if(pnl!=null && ResizablePanel.class.isInstance(pnl)){
			EntityId entid = getEntityIdFor((ResizablePanel)pnl);
			EntityId entidParent = ed.getComponent(entid, ShownState.class).getHierarchyParent();
			if(entidParent!=null){
				ResizablePanel rzpParent = ed.getComponent(entidParent, GuiLink.class).getResizablePanel();
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
	
	private void updateZOrder(ResizablePanel rzp){
		// main position
		Vector3f v3fPos = rzp.getLocalTranslation();
		rzp.setLocalTranslation(v3fPos.x,v3fPos.y,fCurrentOrderPosZ);
		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(rzp);
		if(v3fSize!=null){ //only if it is ready
			fCurrentOrderPosZ += v3fSize.z +fInBetweenGapDistZ;
		}
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
	
	public ResizablePanel getCurrentlyBeingResized(){
		return rzpCurrentlyBeingResized;
	}
	
	public boolean isHasAnyDialogOpened() {
		return (getHierarchyDialogs(null).size()>0);
	}

	public void cleanupRemovedEntity(Float tpf, Entity ent) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	public ArrayList<String> getListAsReport(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Entity ent:getHierarchyDialogs(null)){
			astr.add(getReport(ent,false));
		}
		return astr;
	}

	public String getReport(Entity ent, boolean b) {
		StringBuilder sb = new StringBuilder();
		
		ShownState ss = ent.get(ShownState.class);
		ResizablePanel rzp = ent.get(GuiLink.class).getResizablePanel();
		LastFocusTime lft = ent.get(LastFocusTime.class);
		
		sb.append("pri="+ss.getHierarchyPriority());
		sb.append("/");
		
		sb.append("z="+rzp.getSize().z);
		sb.append(",");
		sb.append("Pz="+rzp.getPreferredSize().z);
		sb.append("/");
		
		sb.append("tm="+lft.getLastFocusTime());
		sb.append("/");
		
		sb.append("dbgnm="+rzp.getName());
		sb.append("/");
		
		return sb.toString();
	}

	public void setHierarchyPriority(EntityId entid, EHierarchy eHierarchy) {
		Entity ent = ed.getEntity(entid, ShownState.class);
		ShownState ss = ent.get(ShownState.class);
//	ent.set(
		ed.setComponent(entid, 
			new ShownState(
				ss==null?null:ss.getHierarchyParent(), 
				eHierarchy, 
				ss==null?null:ss.isHierarchyModal(),
				null
			)
		);
	}
	
	public EntityId getEntityIdFor(ResizablePanel rzp){
//		EntitySet entset = ed.getEntities(GuiLink.class);
		for(Entity ent:entsetBasicQuery){
			if(rzp==ent.get(GuiLink.class).getResizablePanel()){
				return ent.getId();
			}
		}
		return null;
	}

	public Entity getEntityFor(EntityId entid, Class... acl) {
		return ed.getEntity(entid, acl);
	}

	public GuiLink getHierarchyParentGuiLinkFor(EntityId entid) {
		Entity ent = getEntityFor(entid,ShownState.class);
		EntityId entidParent = ent.get(ShownState.class).getHierarchyParent();
		Entity entParent = getEntityFor(entidParent,GuiLink.class);
		return entParent.get(GuiLink.class);
	}
	
	public void setAutoMoveRelativelyToParent(EntityId entid){
		ed.setComponent(entid, new RelativePos(null));
	}
	
	@Override
	public void resizerUpdatedLogicalStateEvent(float tpf, ResizablePanel rzpSource) {
		EntityId entid = getEntityIdFor(rzpSource);
		updateBlocker(tpf, entid, rzpSource);
		
		EntityId entidParent = ed.getComponent(entid, ShownState.class).getHierarchyParent();
		if(entidParent!=null){
			RelativePos rp = ed.getComponent(entid, RelativePos.class);
			if(rp!=null){
				ResizablePanel rzpParent = ed.getComponent(entidParent, GuiLink.class).getResizablePanel();
				
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

	@Override
	public void removedFromParentEvent(ResizablePanel rzpSource) {
		updateBlocker(null, getEntityIdFor(rzpSource), rzpSource);
	}

	@Override
	public void resizedEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {
		rzpCurrentlyBeingResized=rzpSource;
	}

	@Override
	public void endedResizingEvent(ResizablePanel rzpSource) {
		rzpCurrentlyBeingResized=null; //user can resize only one at a time 
	}

	public DefaultEntityData getEntityData(){
		return ed;
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
	
	public static class GuiLink implements EntityComponent, PersistentComponent{
		private ResizablePanel val;
		public GuiLink(ResizablePanel val) { this.val=val; }
		public ResizablePanel getResizablePanel(){ return val; }
	}
	
	public static class Initialized implements EntityComponent, PersistentComponent{
		private boolean	bInitialized;
		public Initialized(boolean bInitialized) {
			this.bInitialized=bInitialized;
		}
		public boolean isInitialized() {
			return bInitialized;
		}
	}
	
	public EntityId createEntity(ResizablePanel rzp,String strName){
	  if(entsetBasicQuery==null){ // keep here to be easy to sync with the settings below
		  entsetBasicQuery = ed.getEntities(
		  	Initialized.class,
		  	GuiLink.class,
		  	Name.class
		  );
	  }
//		recreateQuery();
//		if(entsetMainQuery.size()>0)throw new DetailedException("must begin empty so news and changes can be properly applied",entsetMainQuery,ed);
	  EntityId entid = ed.createEntity(); //to attach components to
	  ed.setComponent(entid, new Initialized(false));
	  ed.setComponent(entid, new GuiLink(rzp));
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
		if(entsetBasicQuery!=null && entsetBasicQuery.applyChanges()) { //contains only basic components
			// newly matching entities
			initializeNewEntities(tpf,entsetBasicQuery.getAddedEntities()); //after this, will contain all hierarchy components
		}
		
		if(entsetHierarchyQuery.applyChanges()) { //contains all components required by hierarchy
			// entities that have merely changed TODO like in have any component changed? 
			updateChangedEntities(tpf,entsetHierarchyQuery.getChangedEntities());
			
			// entities that are no longer matching TODO like in one or more of the required query components went missing
			workOnEntitiesThatAreNotFullyMatchingAnymore(tpf,entsetHierarchyQuery.getRemovedEntities());
		}
	}
	
	@Override
	public void update(float tpf) {
		applyFullQueryChanges(tpf);
		
		fCurrentOrderPosZ = fBeginOrderPosZ;
		for(Entity ent:getHierarchyDialogs(null)){
			ResizablePanel rzp=ent.get(GuiLink.class).getResizablePanel();
			
			updateZOrder(rzp);
			
			updateFocusTime(ent,rzp);
		}
		
		updateDragEffect();
		
		if(isHasAnyDialogOpened()){
//			if(getHierarchyComponentList(null).size()>0){
			Entity ent = getTopMostDialog();
			if(!ContextMenuI.i().isTheContextMenu(ent.get(GuiLink.class).getResizablePanel())){
				ContextMenuI.i().hideContextMenu();
			}
		}
	}
	
	private void initializeNewEntities(Float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			if(!ent.get(Initialized.class).isInitialized()){
				initializeNewEntity(tpf,ent);
				ed.setComponent(ent.getId(), new Initialized(true));
			}
		}
	}
	
	private void updateChangedEntities(Float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			updateChangedEntity(tpf,ent.getId());
		}
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
			addRequiredComponentType(Initialized.class);
			addRequiredComponentType(GuiLink.class);
			addRequiredComponentType(Name.class);
			addRequiredComponentType(Blocker.class);
			addRequiredComponentType(LastFocusTime.class);
			addRequiredComponentType(ShownState.class);
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
	
}
