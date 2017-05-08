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
package com.github.devconslejme.misc.jme;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.function.Function;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI.CallableWeak;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SpatialHierarchyI {
	public static SpatialHierarchyI i(){return GlobalManagerI.i().get(SpatialHierarchyI.class);}
	
	@SuppressWarnings({ "unchecked" })
	public <T extends Node> T getParentest(Spatial sptStartFrom, Class<T> clTypeParentest, boolean bIncludeStartFrom){
		T parentest = null;
		if(bIncludeStartFrom && clTypeParentest.isInstance(sptStartFrom))parentest=(T)sptStartFrom;
		
		Node nodeParent = sptStartFrom.getParent();
		while(nodeParent!=null){
			if(clTypeParentest.isInstance(nodeParent)){
				parentest=(T)nodeParent;
			}
			nodeParent=nodeParent.getParent();
		}
		
		return parentest;
	}

	public ArrayList<Node> getAllParents(Spatial spt) {
		ArrayList<Node> anode = new ArrayList<>();
		Node nodeParent = spt.getParent();
		while(nodeParent!=null){
			anode.add(nodeParent);
			nodeParent=nodeParent.getParent();
		}
		return anode;
	}
	
	public static class FuncSptConcreteClassMatcher implements Function<Spatial,Boolean>{
		private Class clFilter;
		public FuncSptConcreteClassMatcher(Class clFilter) {
			this.clFilter=clFilter;
		}
		public Class getClassFilter() {
			return clFilter;
		}
		@Override
		public Boolean apply(Spatial spt) {
			return clFilter.isInstance(spt);
		}
	}
	
	public <T extends Spatial> T getChildRecursiveExactMatch(Spatial sptParentestToChk, Class<T> clFilter){
		return getChildRecursiveExactMatch(sptParentestToChk, 
			new FuncSptConcreteClassMatcher(clFilter));
//			new SpatialMatcherCallableX().putKeyValue(Class.class.getName(), clFilter));
	}
	
	public <T extends Spatial> T getChildRecursiveExactMatch(Spatial sptParentestToChk, Function<Spatial,Boolean> funcMatcher){
		ArrayList<T> asptList = getAllChildrenRecursiveFrom(sptParentestToChk, null, funcMatcher);
		if(asptList.isEmpty() || asptList.size()>1)throw new DetailedException("not exact match",sptParentestToChk,funcMatcher,asptList);
		return asptList.get(0);
	}

	/**
	 * @param sptParentestToChk
	 * @param clFilter if Spatial, will bring all
	 * @param iMaxDepth max recursion depth, can be null (unlimited) 
	 * @return
	 */
	public <T extends Spatial> ArrayList<T> getAllChildrenRecursiveFrom(Spatial sptParentestToChk, Class<T> clFilter, Integer iMaxDepth) {
		return getAllChildrenRecursiveFrom(sptParentestToChk, iMaxDepth, 
			new FuncSptConcreteClassMatcher(clFilter));
//			new SpatialMatcherCallableX().putKeyValue(Class.class.getName(), clFilter));
	}
	/**
	 * 
	 * @param sptParentestToChk
	 * @param iMaxDepth max recursion depth, can be null (unlimited) 
	 * @param callMatcher can be null, or can retrieve key: Spatial.class.getName()
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T extends Spatial> ArrayList<T> getAllChildrenRecursiveFrom(Spatial sptParentestToChk, Integer iMaxDepth, Function<Spatial,Boolean> funcMatcher) {
		if(sptParentestToChk==null)throw new DetailedException("null spatial");
		
		ArrayList<T> asptList = new ArrayList<T>();
		
		if(iMaxDepth!=null){
			if(iMaxDepth==0)return asptList; 
			iMaxDepth-=1;
		}
		
		Node nodeParent = null;
		if (sptParentestToChk instanceof Node) {
			nodeParent = (Node) sptParentestToChk;
		}else{
			return asptList;
		}
		
		// add direct children
		for(Spatial sptChild:nodeParent.getChildren()){
//			if(funcMatcher!=null)funcMatcher.putKeyValue(Spatial.class.getName(),sptChild);
			if(funcMatcher==null || funcMatcher.apply(sptChild)){
				asptList.add((T)sptChild);
			}
		}
		
		// deep search
		for(Spatial sptChild:nodeParent.getChildren()){
			if(sptChild instanceof Node){
				asptList.addAll(getAllChildrenRecursiveFrom(sptChild, iMaxDepth, funcMatcher));
			}
		}
		
		return asptList;
	}
	
//	public static interface DoSomething{
//		public boolean doIt(Spatial spt);
//	}
	
	public static class SpatialInfo{
		private Spatial spatial;
		private int iDepth;
		private Object	objCustom;
		
		public Spatial getSpatial() {
			return spatial;
		}
		public SpatialInfo setSpatial(Spatial spatial) {
			this.spatial = spatial;
			return this;
		}
		public int getDepth() {
			return iDepth;
		}
		public SpatialInfo setDepth(int iDepth) {
			this.iDepth = iDepth;
			return this;
		}
		
		public SpatialInfo setCustomValue(Object obj) {
			this.objCustom=obj;
			return this;
		}
		
		@SuppressWarnings("unchecked")
		public <T> T getCustomValue(){
			return (T)objCustom;
		}
		
		@Override
		public String toString() {
			return ""+getCustomValue();
		}
	}
	
	/**
	 * 
	 * @param node
	 * @param funcDo
	 * @param iCurrentDepth can be 0
	 * @param hmStore will be created if null
	 * @return
	 */
	public LinkedHashMap<Spatial,SpatialInfo> doSomethingRecursively(
		Node node, 
		Function<SpatialInfo,Boolean> funcDo, 
		int iCurrentDepth, 
		LinkedHashMap<Spatial,SpatialInfo> hmStore
	){
		if(hmStore==null)hmStore = new LinkedHashMap<Spatial,SpatialInfo>();
		
		iCurrentDepth++; //starts on 1, the parentest is 0
		
		for(Spatial sptChild:node.getChildren()){
			SpatialInfo spti = new SpatialInfo();
			spti.setSpatial(sptChild);
			spti.setDepth(iCurrentDepth);
			
			if(funcDo.apply(spti))hmStore.put(sptChild,spti);
			
			if(sptChild instanceof Node){
				doSomethingRecursively((Node)sptChild, funcDo, iCurrentDepth, hmStore);
			}
		}
		
		return hmStore;
	}
	
}
