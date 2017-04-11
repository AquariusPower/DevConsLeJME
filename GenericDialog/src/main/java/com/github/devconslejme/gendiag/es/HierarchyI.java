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
import java.util.Collections;
import java.util.Comparator;

import com.github.devconslejme.gendiag.ContextMenuI;
import com.github.devconslejme.gendiag.ResizablePanel;
import com.github.devconslejme.gendiag.es.GenericDialogZayES.GuiLink;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.TimeConvertI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.lemur.DragParentestListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.jme3.app.Application;
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
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.focus.FocusManagerState;


/**
 * only getters
 * unmuttable: do not extend, store things that cant be changed or references
 * TODO confirm if references is a valid unmuttable... or Ids should be used instead?
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HierarchyI {
	public static HierarchyI i(){return GlobalInstanceManagerI.i().get(HierarchyI.class);}
	
	private Node nodeToMonitor;
	private Application	app;
	private FocusManagerState	focusState;
	private float	fBeginOrderZ = 0f;
	private float	fSafeZDist=1.0f;
	private DefaultEntityData	ed;
	private ArrayList<Class>	aclAllComponentTypes;
	
	public void configure(Node nodeToMonitor, float fBeginOrderZ){
		this.ed = GlobalInstanceManagerI.i().get(GenericDialogZayES.class).getEntityData();
		this.app=GlobalInstanceManagerI.i().get(Application.class);
		this.fBeginOrderZ=fBeginOrderZ;
		this.nodeToMonitor=nodeToMonitor;
	//	app.getStateManager().attach(this);
		focusState=app.getStateManager().getState(FocusManagerState.class);
		
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				organizeDialogsStack();
				
				if(isHasAnyDialogOpened()){
	//			if(getHierarchyComponentList(null).size()>0){
					Entity ent = getTopMostDialog();
					if(!ContextMenuI.i().isTheContextMenu(ent.get(GuiLink.class).getResizablePanel())){
						ContextMenuI.i().hideContextMenu();
					}
				}
				
				return true;
			}
	
		}
		.setName(HierarchyI.class.getSimpleName())
		.setDelaySeconds(0.25f)
		.setLoop(true)
		.setUserCanPause(true));
	}
	
	protected Entity getTopMostDialog() {
		ArrayList<Entity> aent = getHierarchyChildrenFor(null);
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

	public static class Blocker implements EntityComponent, PersistentComponent{
		Button val;
		public Blocker(Button val) { this.val=val; }
		public Button getButton(){ return val; }
	}

	public static class LastFocusTime implements EntityComponent, PersistentComponent{
		long val=-1;
		public LastFocusTime() {}
		public LastFocusTime(long val) { this.val=val; }
		public long getLastFocusTime(){ return val; }
	}
	
	public static class ShownState implements EntityComponent, PersistentComponent{
		private EntityId hierarchyParent=null;
		private boolean	bHierarchyTop=false;
		private boolean	bHierarchyModal=false;
		
		public ShownState() {}
		
		public ShownState(EntityId hierarchyParent, Boolean bHierarchyTop,	Boolean bHierarchyModal) {
			this.hierarchyParent = hierarchyParent;
			if(bHierarchyTop!=null)this.bHierarchyTop = bHierarchyTop;
			if(bHierarchyModal!=null)this.bHierarchyModal = bHierarchyModal;
		}

		public EntityId getHierarchyParent() {
			return hierarchyParent;
		}

		public boolean isHierarchyTop() {
			return bHierarchyTop;
		}

		public boolean isHierarchyModal() {
			return bHierarchyModal;
		}
	}
	
	public Class[] getAllComponentTypesArray() {
		return getAllComponentTypesList().toArray(new Class[0]);
	}
	public ArrayList<Class> getAllComponentTypesList() {
		if(aclAllComponentTypes==null){
			aclAllComponentTypes = new ArrayList<Class>();
			aclAllComponentTypes.addAll(GenericDialogZayES.i().getAllComponentTypesList());
			aclAllComponentTypes.add(Blocker.class);
			aclAllComponentTypes.add(LastFocusTime.class);
			aclAllComponentTypes.add(ShownState.class);
		}
		return aclAllComponentTypes;
	}
	
	public void initialize(float tpf,Entity ent) {
		if(ed.getComponent(ent.getId(), ShownState.class)==null){
			ed.setComponent(ent.getId(), new ShownState());
		}
		
		if(ed.getComponent(ent.getId(), LastFocusTime.class)==null){
			ed.setComponent(ent.getId(), new LastFocusTime());
		}
		
		/////////////// blocker
		if(ed.getComponent(ent.getId(), Blocker.class)==null){
			Button btn = new Button("");
			ed.setComponent(ent.getId(),new Blocker(btn)); //ent.set(
			
			btn.setBackground(
				new QuadBackgroundComponent(//ColorRGBA.Red));
					ColorI.i().colorChangeCopy(ColorRGBA.Red, -0.75f, 0.5f)));
			
			DragParentestListenerI.i().applyAt(btn, null);
		}
		
		// panel
		ResizablePanel rzp = ent.get(GuiLink.class).getResizablePanel();
		HoverHighlightEffectI.i().applyAt(rzp, (QuadBackgroundComponent)rzp.getResizableBorder());
	}

	public boolean isBlocked(Entity ent){
		Blocker blocker=ent.get(Blocker.class);
		if(blocker==null)return false;
		return blocker.getButton().getParent()!=null;
	}

	public void setEnabledBlockerLayer(Entity ent, boolean b){
		Button blocker = ent.get(Blocker.class).getButton();
		if(b){
			nodeToMonitor.attachChild(blocker);
		}else{
			blocker.removeFromParent();
		}
	}
	
	/**
	 * will prevent access to parent
	 * @param compChild
	 * @return 
	 */
	public void showAsHierarchyModal(EntityId entidParent, EntityId entidChild){
		applyHierarchyChild(
				ed.getEntity(entidParent, getAllComponentTypesArray()),
				ed.getEntity(entidChild, getAllComponentTypesArray()),
				true);
	}
	
	/**
	 * will close if parent closes
	 * @param compChild
	 * @return 
	 */
	public void showAsHierarchyModeless(Entity entParent, Entity entChild){
		applyHierarchyChild(entParent,entChild,false);
	}
	
	private void applyHierarchyChild(Entity entParent, Entity entChild, boolean bModal){
		ResizablePanel rzpChild = entChild.get(GuiLink.class).getResizablePanel();
		
		ShownState ss = entChild.get(ShownState.class);
//		entChild.set(new ShownState(
		ed.setComponent(entChild.getId(),new ShownState(
			entParent.getId(), 
			ss==null?null:ss.isHierarchyTop(), 
			bModal));
//		setHierarchyParentAtChild(entParent,entChild);
		if(bModal)setEnabledBlockerLayer(entParent,true);
		
		showDialog(rzpChild); //show it
		updateLastFocusAppTimeNano(entChild);
	}
	
//	private void setHierarchyParentAtChild(Entity entParent, Entity entChild) {
//		ShownState ss = entChild.get(ShownState.class);
//		entChild.set(new ShownState(entParent.getId(), ss.isHierarchyTop(), ss.isHierarchyModal()));
//	}

	private void showDialog(ResizablePanel rzp) {
		nodeToMonitor.attachChild(rzp);
	}
	
	private void updateLastFocusAppTimeNano(Entity ent) {
//		ent.set(
		ed.setComponent(ent.getId(),
			new LastFocusTime(TimeConvertI.i().getNanosFrom(app.getTimer())));
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
	 * @param ent if null will bring all possible
	 * @return
	 */
	public ArrayList<Entity> getHierarchyChildrenFor(Entity ent){
		ArrayList<Entity> aent = new ArrayList<Entity>();
		
		/**
		 * TODO how to make this work?
		EntitySet entset = ed.getEntities(new FilterByHierarchyParent(ent), ShownState.class);
		 */
		EntitySet entset = ed.getEntities(GuiLink.class,ShownState.class,LastFocusTime.class);
		
		for(Entity entChild:entset){
			if(ent==null || entChild.get(ShownState.class).getHierarchyParent().equals(ent.getId())){
				aent.add(entChild);
			}
		}		
		
		Collections.sort(aent,cmpr);
		
		return aent;
	}
	
	public void update(float tpf,Entity ent){
		ent = ed.getEntity(ent.getId(), getAllComponentTypesArray());
		
		/********************
		 * beware what you code here!!! 
		 ********************/
		
		// close self if parent dialog closed
		ResizablePanel rzp = ent.get(GuiLink.class).getResizablePanel();
		if(!rzp.isClosed()){
			ShownState ss = ent.get(ShownState.class);
			if(ss.getHierarchyParent()!=null){
				Entity entHierarchyParent = ed.getEntity(
					ss.getHierarchyParent(), GuiLink.class);
				if(entHierarchyParent.get(GuiLink.class).getResizablePanel().isClosed()){
					rzp.close();
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
		
		// blocker work
		if(isBlocked(ent)){
			// how many childs are modal
			int iModalCount=0;
			for(Entity entChild:getHierarchyChildrenFor(ent)){
				if(entChild.get(ShownState.class).isHierarchyModal())iModalCount++;
			}
			
			if(iModalCount==0){
				// remove blocker
				setEnabledBlockerLayer(ent,false);
			}else{
				Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(rzp);
				if(Float.compare(v3fSize.length(),0f)!=0){ //waiting top panel be updated by lemur
					Vector3f v3fPos = rzp.getLocalTranslation().clone();
					v3fPos.z += v3fSize.z;
					ent.get(Blocker.class).getButton().setLocalTranslation(v3fPos);
					
					ent.get(Blocker.class).getButton().setPreferredSize(rzp.getPreferredSize());
				}
			}
		}
	}

	private void organizeDialogsStack() {
		float fOrderZ = fBeginOrderZ;
		for(Entity ent:getHierarchyChildrenFor(null)){
			ResizablePanel pnl=ent.get(GuiLink.class).getResizablePanel();
			pnl.getLocalTranslation().z=fOrderZ;
			Vector3f v3f = MiscJmeI.i().getBoundingBoxSize(pnl);
			if(v3f!=null){ //only if it is ready
				fOrderZ += v3f.z +fSafeZDist;
			}
			
			if(isFocused(pnl)){
				updateLastFocusAppTimeNano(ent);
			}
		}
	}

	private Comparator<Entity> cmpr = new Comparator<Entity>() {
		@Override
		public int compare(Entity o1, Entity o2) {
			ShownState ss1 = o1.get(ShownState.class); 
			ShownState ss2 = o2.get(ShownState.class); 
			// top only against top
			if( ss1.isHierarchyTop() && !ss2.isHierarchyTop())return  1;
			if(!ss1.isHierarchyTop() &&  ss2.isHierarchyTop())return -1;
			
			// parent diag below child diag
			if(ss1.getHierarchyParent()==o2.getId())return  1;
			if(ss2.getHierarchyParent()==o1.getId())return -1;
			if(ss1==ss2)return 0;
			
			// last focus
			return Long.compare(
				o1.get(LastFocusTime.class).getLastFocusTime(),
				o2.get(LastFocusTime.class).getLastFocusTime()
			);
		}
	};

	public boolean isHasAnyDialogOpened() {
		return (getHierarchyChildrenFor(null).size()>0);
	}

	public void cleanup(float tpf, Entity ent) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	public ArrayList<String> getListAsReport(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Entity ent:getHierarchyChildrenFor(null)){
			astr.add(getReport(ent,false));
		}
		return astr;
	}

	public String getReport(Entity ent, boolean b) {
		StringBuilder sb = new StringBuilder();
		
		ShownState ss = ent.get(ShownState.class);
		ResizablePanel rzp = ent.get(GuiLink.class).getResizablePanel();
		LastFocusTime lft = ent.get(LastFocusTime.class);
		
		sb.append("top="+ss.isHierarchyTop());
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

	public void setAsHierarchyTop(EntityId entid) {
		Entity ent = ed.getEntity(entid, ShownState.class);
		ShownState ss = ent.get(ShownState.class);
//	ent.set(
		ed.setComponent(entid, 
			new ShownState(
				ss==null?null:ss.getHierarchyParent(), 
				true, 
				ss==null?null:ss.isHierarchyModal()));
	}
	
	public EntityId getEntityIdFor(ResizablePanel rzp){
		EntitySet entset = ed.getEntities(GuiLink.class);
		for(Entity ent:entset){
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
		Entity ent = HierarchyI.i().getEntityFor(entid,ShownState.class);
		EntityId entidParent = ent.get(ShownState.class).getHierarchyParent();
		Entity entParent = HierarchyI.i().getEntityFor(entidParent,GuiLink.class);
		return entParent.get(GuiLink.class);
	}
}
