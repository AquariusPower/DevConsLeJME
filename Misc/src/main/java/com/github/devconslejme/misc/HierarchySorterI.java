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

package com.github.devconslejme.misc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
 * turn a hierarchical tree into a list
 * based on priority, activation time and parenting (parent<->child)
 * initially for dialogs
 * 
 * FIXME an activated child of the same parent wont gain priority over the others (of the same parent)
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HierarchySorterI {
	public static HierarchySorterI i(){return GlobalManagerI.i().get(HierarchySorterI.class);}
	
	public static enum EHierarchyType{
		Top, //DevCons, alerts and thin panels
		Normal, //default
		Bottom, //background anything
		;
		public String s(){return toString();}
	}
	
	public static interface IHierarchy{
		public <T extends IHierarchy> T getHierarchyParent();
		public EHierarchyType getHierarchyPriority();
		public long getLastActivationNanoTime();
	}
	
	private Comparator<IHierarchy> cmprByLastActivationTime = new Comparator<IHierarchy>() {
		@Override
		public int compare(IHierarchy o1, IHierarchy o2) {
			return Long.compare(
				o1.getLastActivationNanoTime(),
				o2.getLastActivationNanoTime()
			);
		}
	};
	
	/**
	 * The last items will be the ones to be priorized.
	 * The class extending {@link IHierarchy} must implement equals() to be used to compare with the hierarchy parent.
	 * @param ahMainList it will be modified
	 */
	@SuppressWarnings("unchecked")
	public <T extends IHierarchy> void sort(ArrayList<T> ahMainList){
		if(ahMainList.size()==0)return;
		
		/**
		 * root dialogs, that have no parent
		 */
		ArrayList<T> ahNormal = new ArrayList<T>(); 
		ArrayList<T> ahTop = new ArrayList<T>();
		ArrayList<T> ahBottom = new ArrayList<T>();
		for(IHierarchy ih:ahMainList.toArray(new IHierarchy[0])){
			if(ih.getHierarchyParent()==null){
				switch (ih.getHierarchyPriority()) {
					case Top:
						ahTop.add((T)ih);
						break;
					case Normal:
						ahNormal.add((T)ih);
						break;
					case Bottom:
						ahBottom.add((T)ih);
						break;
				}
				
				ahMainList.remove(ih);
			}
		}
		
		// all remaining are childs and recursive childs
		ArrayList<IHierarchy> ahChilds = new ArrayList<IHierarchy>(); //that have no parent
		ahChilds.addAll(ahMainList);
		ahMainList.clear();
		
		Collections.sort(ahTop,cmprByLastActivationTime);
		Collections.sort(ahNormal,cmprByLastActivationTime);
		Collections.sort(ahBottom,cmprByLastActivationTime);
		
		Collections.sort(ahChilds,cmprByLastActivationTime);
		
		//////////////////////////// populate main
		// add roots (parent-less), THIS ORDER MATTERS!
		ahMainList.addAll(ahBottom);
		ahMainList.addAll(ahNormal);
		ahMainList.addAll(ahTop);
		
		/**
		 * add childs just after their parents,
		 * 
		 * reversed because: 
		 * the latest focused topmost child will become the 1st after its parent,
		 * and just after, the previously focused child of the same parent will be the 1st after its parent
		 * pushing the topmost to the 2nd place after its parent... and so on...
		 * 
		 * 1)
		 * parent
		 * |_topmost child
		 * 
		 * 2)
		 * parent
		 * |_previously focused child
		 * |_topmost child
		 */
		Collections.reverse(ahChilds);
		labelCheckChildListEmpty:while(ahChilds.size()>0){
			for(IHierarchy hChild:ahChilds.toArray(new IHierarchy[0])){
				T hParent = (T) hChild.getHierarchyParent();
				
				// find the current index of the parent on the main list
				int iIndexOfParentAtMain=ahMainList.indexOf(hParent);
				if(iIndexOfParentAtMain>-1){ //found!
					// move the child to just after its parent at main list
					ahChilds.remove(hChild);
					ahMainList.add(iIndexOfParentAtMain+1, (T)hChild);
					continue labelCheckChildListEmpty; 
				}else{
					if(ahChilds.indexOf(hParent)==-1){
						throw new DetailedException("inconsistent hierarchy: "
							+"the parent should be at least still at childs list",
							hParent,ahChilds,ahMainList);
					}
				}
			}
		}
	}
	
//	private <T extends IHierarchy> int indexOf(ArrayList<T> ah, T h){
//		int iIndexOfParentAtMain=-1;
//		for (int i = 0; i < ah.size(); i++) {
//			T hChk = ah.get(i);
//			if(hChk.equals(h)){
//				iIndexOfParentAtMain = i;
//				break;
//			}
//		}
//		return iIndexOfParentAtMain;
//	}
}
