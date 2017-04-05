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
import java.util.Collections;
import java.util.Comparator;

import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.MiscLibI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.bounding.BoundingBox;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Panel;


/**
 * This will detect and organize all dialogs (Panels attached to main JME nodes),
 * based on their hierarchy.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HierarchySorterI extends AbstractAppState{
	public static HierarchySorterI i(){return GlobalInstanceManagerI.i().get(HierarchySorterI.class);}
	
	private ArrayList<Panel> apnl = new ArrayList<Panel>();
	private Application	app;
	private Node nodeToMonitor;
	private float	fBeginOrderZ = 0f;
	
//	public Application getApp(){
//		return app;
//	}
	
	private Comparator<IHierarchySorter> cmpr = new Comparator<IHierarchySorter>() {
		@Override
		public int compare(IHierarchySorter o1, IHierarchySorter o2) {
			Panel pnl1 = (Panel)o1;
			Panel pnl2 = (Panel)o2;
			
			// top only against top
			if(o1.isTopHierarchy() && !o2.isTopHierarchy())return  1;
			if(o2.isTopHierarchy() && !o1.isTopHierarchy())return -1;
			
			// parent diag below child diag
			if(o1.getHierarchyParent()==o2)return  1;
			if(o2.getHierarchyParent()==o1)return -1;
			if(o1==o2)return 0;
			
			// last focus
			return Long.compare(o1.getLastFocusAppTimeNano(),o2.getLastFocusAppTimeNano());
		}
	};
	private float	fSafeZDist=1.0f;
	
	public static interface IHierarchySorter {
		public Panel getHierarchyParent();
		public Panel[] getHierarchyChildList();
		public Long getLastFocusAppTimeNano();
		public boolean isTopHierarchy();
		public boolean isModal();
	}
	
	public void configure(Node nodeToMonitor, float fBeginOrderZ){
		this.app=GlobalInstanceManagerI.i().get(Application.class);
		this.fBeginOrderZ=fBeginOrderZ;
		this.nodeToMonitor=nodeToMonitor;
		app.getStateManager().attach(this);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
//		nodeToMonitor.getChildren()
		organizeDialogsStack();
	}
	
	private void organizeDialogsStack() {
		ArrayList<IHierarchySorter> aido = new ArrayList<IHierarchySorter>();
		
		for(Spatial spt:nodeToMonitor.getChildren()){
			if(spt instanceof Panel){
				Panel pnl=(Panel)spt;
				if(pnl instanceof IHierarchySorter){
					aido.add((IHierarchySorter)pnl);
				}
			}
		}
		
		Collections.sort(aido,cmpr);
		
		float fOrderZ = fBeginOrderZ;
		for(IHierarchySorter ido:aido){
			Panel pnl=(Panel)ido;
			pnl.getLocalTranslation().z=fOrderZ;
//			fOrderZ += (((BoundingBox)pnl.getWorldBound()).getZExtent()*2) +1.0f; //+1 is safety
			fOrderZ += MiscJmeI.i().getBoundingBoxSize(pnl).z +fSafeZDist;
		}
	}
}