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
import java.util.Collections;
import java.util.Comparator;

import com.github.devconslejme.misc.EntitySystem.IComponent;
import com.github.devconslejme.misc.EntitySystem.ISystem;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.ReportI.IReport;
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
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.focus.FocusManagerState;


/**
 * This will detect and organize all dialogs (Panels attached to main JME nodes),
 * based on their hierarchy.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HierarchySystemI implements ISystem {
	public static HierarchySystemI i(){return GlobalInstanceManagerI.i().get(HierarchySystemI.class);}
	
	private ArrayList<Panel> apnl = new ArrayList<Panel>();
	private Application	app;
	private Node nodeToMonitor;
	private float	fBeginOrderZ = 0f;
	private DialogHierarchyWorker worker = new DialogHierarchyWorker();
	
//	public Application getApp(){
//		return app;
//	}
	
	private Comparator<HierarchyComponent> cmpr = new Comparator<HierarchyComponent>() {
		@Override
		public int compare(HierarchyComponent o1, HierarchyComponent o2) {
			// top only against top
			if( o1.isHierarchyTop() && !o2.isHierarchyTop())return  1;
			if(!o1.isHierarchyTop() &&  o2.isHierarchyTop())return -1;
			
			// parent diag below child diag
			if(o1.getHierarchyParent()==o2)return  1;
			if(o2.getHierarchyParent()==o1)return -1;
			if(o1==o2)return 0;
			
			// last focus
			return Long.compare(o1.getLastFocusAppTimeNano(),o2.getLastFocusAppTimeNano());
		}
	};
	private float	fSafeZDist=1.0f;
//	private ArrayList<HierarchyComponent>	ahsList  = new ArrayList<HierarchyComponent>();
	private FocusManagerState	focusState;
	
//	public static interface IHierarchySorter extends IReport {
//		HierarchyIncarnation getHierarchyParent();
//		HierarchyIncarnation[] getHierarchyChildList();
//		Long getLastFocusAppTimeNano();
//		boolean isTopHierarchy();
//		boolean isModal();
//		void updateLastFocusAppTimeNano();
//		ResizablePanel getOwner();
//	}
	
	public void configure(Node nodeToMonitor, float fBeginOrderZ){
		this.app=GlobalInstanceManagerI.i().get(Application.class);
		this.fBeginOrderZ=fBeginOrderZ;
		this.nodeToMonitor=nodeToMonitor;
//		app.getStateManager().attach(this);
		focusState=app.getStateManager().getState(FocusManagerState.class);
		
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				organizeDialogsStack();
				
				if(isHasAnyDialogOpened()){
//				if(getHierarchyComponentList(null).size()>0){
					HierarchyComponent hs = getEntityListOfHierarchyComponents(null).get(getEntityListOfHierarchyComponents(null).size()-1);
					if(!ContextMenuI.i().isTheContextMenu(hs.getEntityOwner())){
						ContextMenuI.i().hideContextMenu();
					}
				}
				
				return true;
			}

		}
		.setName(HierarchySystemI.class.getSimpleName())
		.setDelaySeconds(0.25f)
		.setLoop(true)
		.setUserCanPause(true));
	}
	
	public boolean isHasAnyDialogOpened() {
		return (getEntityListOfHierarchyComponents(null).size()>0);
	}
	
//	@Override
//	public void update(float tpf) {
//		super.update(tpf);
////		nodeToMonitor.getChildren()
//		organizeDialogsStack();
//	}
	
	public ArrayList<HierarchyComponent> getEntityListOfHierarchyComponents(HierarchyComponent parentFilter){
		ArrayList<HierarchyComponent> list = new ArrayList<HierarchyComponent>();
		
		for(Spatial spt:nodeToMonitor.getChildren()){
			if(spt instanceof ResizablePanel){
				ResizablePanel rzp=(ResizablePanel)spt;
				HierarchyComponent comp = rzp.getComponent(HierarchyComponent.class);
				if(comp!=null){
					if(parentFilter==null || comp.getHierarchyParent()==parentFilter){
						list.add(comp);
					}
				}
			}
		}
		
		Collections.sort(list,cmpr);
		
		return list;
	}
	
	private void organizeDialogsStack() {
		float fOrderZ = fBeginOrderZ;
		for(HierarchyComponent comp:getEntityListOfHierarchyComponents(null)){
			ResizablePanel pnl=comp.getEntityOwner();
			pnl.getLocalTranslation().z=fOrderZ;
			fOrderZ += MiscJmeI.i().getBoundingBoxSize(pnl).z +fSafeZDist;
			
			if(isFocused(pnl)){
				updateLastFocusAppTimeNano(comp);
			}
		}
	}
	
	private HierarchyComponent updateLastFocusAppTimeNano(HierarchyComponent comp) {
		return comp.getEntityOwner().updateComponent(
			new HierarchyComponent(
				comp,
				TimeConvertI.i().getNanosFrom(app.getTimer())
			)
		);
	}

	public boolean isFocused(Spatial spt){
		if(spt.equals(getFocused()))return true; //quick test 1st
		if(getFocused()==null)return false;
		
		Spatial sptParentest = MiscJmeI.i().getParentest(spt, Panel.class, true);
		Spatial sptFocusedParentest = MiscJmeI.i().getParentest(getFocused(), Panel.class, true);
		return sptParentest.equals(sptFocusedParentest);
	}
	
	public Spatial getFocused(){
		return focusState.getFocus();
	}
	
//	public ArrayList<IHierarchySorter> getList(){
//		return new ArrayList<IHierarchySorter>(ahsList);
//	}
	
	public ArrayList<String> getListAsReport(){
		ArrayList<String> astr = new ArrayList<String>();
		DialogHierarchyWorker incarn = new DialogHierarchyWorker();
		for(HierarchyComponent comp:getEntityListOfHierarchyComponents(null)){
			astr.add(workOn(comp).getReport(false));
		}
		return astr;
	}
	
	public class DialogHierarchyWorker implements IReport {//implements IComponent,IHierarchySorter{
		private HierarchyComponent comp;
		
		public DialogHierarchyWorker(){}
		
		public DialogHierarchyWorker set(HierarchyComponent comp){
			this.comp=comp;
			
			if(!comp.isInitialized()){
				init();
//				app = GlobalInstanceManagerI.i().get(Application.class);
				HoverHighlightEffectI.i().applyAt(c().getEntityOwner(), (QuadBackgroundComponent)c().getEntityOwner().getResizableBorder());
			}
			
			return this;
		}
		
		public HierarchyComponent c(){
			return comp;
		}
		
		private void init(){
			// blocker
			c().getBlocker().setBackground(
				new QuadBackgroundComponent(//ColorRGBA.Red));
					ColorI.i().colorChangeCopy(ColorRGBA.Red, -0.75f, 0.5f)));
			
			DragParentestListenerI.i().applyAt(c().getBlocker(), c().getEntityOwner());
		}
		
		public void setEnabledBlockerLayer(boolean b){
			Panel blocker = c().getBlocker();
			if(b){
				nodeToMonitor.attachChild(blocker);
			}else{
				blocker.removeFromParent();
			}
		}
		
//		@Override
//		protected void resizedTo(Vector3f v3fNewSize) {
//			super.resizedTo(v3fNewSize);
//		}
		
		/**
		 * will prevent access to parent
		 * @param compChild
		 * @return 
		 */
		public HierarchyComponent showAsHierarchyModal(HierarchyComponent compChild){
			return applyHierarchyChild(compChild,true);
		}
		
		/**
		 * will close if parent closes
		 * @param compChild
		 * @return 
		 */
		public HierarchyComponent showAsHierarchyModeless(HierarchyComponent compChild){
			return applyHierarchyChild(compChild,false);
		}
		
		private HierarchyComponent applyHierarchyChild(HierarchyComponent compChild, boolean bModal){
			ResizablePanel rzpChild = compChild.getEntityOwner();
			
			compChild=rzpChild.updateComponent(new HierarchyComponent(compChild,null,bModal));
			compChild=setMeAsHierarchyParentAt(compChild);
			if(bModal)setEnabledBlockerLayer(true);
			
			showDialog(rzpChild); //show it
			compChild=updateLastFocusAppTimeNano(compChild);
			
			return compChild;
		}
		
		private HierarchyComponent setMeAsHierarchyParentAt(HierarchyComponent compChild) {
			return compChild.getEntityOwner().updateComponent(new HierarchyComponent(compChild, c()));
		}

		private void showDialog(ResizablePanel rzp) {
			nodeToMonitor.attachChild(rzp);
		}

		protected void update(float tpf){
			/********************
			 * beware what you code here!!! 
			 ********************/
			
			// close self if parent dialog closed
			if(c().getHierarchyParent()!=null){
				if(c().getHierarchyParent().getEntityOwner().isClosed()){
					c().getEntityOwner().removeFromParent();
				}
			}
			
			// remove closed childs
			for(HierarchyComponent child:getEntityListOfHierarchyComponents(c())){
				if(child.getEntityOwner().isClosed()){
					child.getEntityOwner().updateComponent(new HierarchyComponent(child, (HierarchyComponent)null));
//					arzdHierarchyChildList.remove(compChild);
				}
			}
			
			// blocker work
			if(isBlocked()){
				// how many childs are modal
				int iModalCount=0;
				for(HierarchyComponent child:getEntityListOfHierarchyComponents(c())){
					if(child.isHierarchyModal())iModalCount++;
				}
				
				if(iModalCount==0){
					// remove blocker
					setEnabledBlockerLayer(false);
				}else{
					Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(c().getEntityOwner());
					if(Float.compare(v3fSize.length(),0f)!=0){ //waiting top panel be updated by lemur
						Vector3f v3fPos = c().getEntityOwner().getLocalTranslation().clone();
						v3fPos.z += v3fSize.z;
						c().getBlocker().setLocalTranslation(v3fPos);
						
						c().getBlocker().setPreferredSize(c().getEntityOwner().getPreferredSize());
					}
				}
			}
		}
		
		public boolean isBlocked(){
			return c().getBlocker().getParent()!=null;
		}
		
		@Override
		public String getReport(boolean b) {
			StringBuilder sb = new StringBuilder();
			
			sb.append("top="+c().isHierarchyTop());
			sb.append("/");
			
			sb.append("z="+c().getEntityOwner().getSize().z);
			sb.append(",");
			sb.append("Pz="+c().getEntityOwner().getPreferredSize().z);
			sb.append("/");
			
			sb.append("tm="+c().getLastFocusAppTimeNano());
			sb.append("/");
			
			sb.append("dbgnm="+c().getEntityOwner().getName());
			sb.append("/");
			
			return sb.toString();
		}

		public HierarchyComponent setAsHierarchyTop() {
			comp=c().getEntityOwner().updateComponent(new HierarchyComponent(comp,true,null));
			return comp;
		}
		
//		@Override
//		public Class<HierarchyIncarnation> getCompositeType() {
//			return HierarchyIncarnation.class;
//		}

	}
	
	public DialogHierarchyWorker workOn(HierarchyComponent comp){
		return worker.set(comp);
	}
	
	@Override
	public <T extends IComponent> void updateComponent(T comp, float tpf) {
		workOn((HierarchyComponent)comp).update(tpf);
	}

	public HierarchyComponent createComponentAt(ResizablePanel hrp) {
		return hrp.createComponent(HierarchyComponent.class);
	}

}
