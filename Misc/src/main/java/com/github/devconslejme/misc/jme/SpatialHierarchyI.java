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
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SpatialHierarchyI {
	public static SpatialHierarchyI i(){return GlobalManagerI.i().get(SpatialHierarchyI.class);}
	
	/**
	 * this can bring even the parentest nodes like rootNode and guiNode
	 * @param sptStartFrom
	 * @param clTypeParentest
	 * @param bIncludeStartFrom
	 * @return
	 */
	public <T extends Spatial> T getParentestOrSelf(Spatial sptStartFrom, Class<T> clTypeParentest, boolean bIncludeStartFrom){
		return getParentestOrSelf(sptStartFrom, clTypeParentest, bIncludeStartFrom, true);
	}
	
	/**
	 * 
	 * @param sptStartFrom
	 * @param clTypeParentest
	 * @param bIncludeStartFrom
	 * @param bIncludeLast if false will skip any kind of last parent, not necessarily the maching type required
	 * @return 
	 */
	@SuppressWarnings({ "unchecked" })
	public <T extends Spatial> T getParentestOrSelf(Spatial sptStartFrom, Class<T> clTypeParentest, boolean bIncludeStartFrom, boolean bIncludeLast){
//		Spatial sptRet = bIncludeStartFrom ? sptStartFrom : null;
//		
//		if(!bIncludeLast){ //skip top nodes like root and gui
//			if(sptStartFrom.getParent().getParent()==null){ //YES double parent check!!!
//				return (T)sptRet;
//			}
//		}
//		
//		sptStartFrom.des
		
		
		T ret = null;
		if(bIncludeStartFrom && clTypeParentest.isInstance(sptStartFrom)){
			ret=(T)sptStartFrom;
		}
		
		Node nodeParent = sptStartFrom.getParent();
		boolean bBreakNow=false;
		boolean bIsLast=false;
		while(nodeParent!=null){
			if(nodeParent.getParent()==null){ //it is already the top node.
				bBreakNow=true;
				bIsLast=true;
			}
//			boolean bAtTopNode = false;
//			if(nodeParent.getParent().getParent()==null){ //YES double parent check!!!
//				break; //skips the top/root/gui nodes 
//			}
			
			if(clTypeParentest.isInstance(nodeParent)){
				if(bIncludeLast || !bIsLast){
					ret=(T)nodeParent;
				}
			}
			
			if(bBreakNow)break;
			
//			if(!bIncludeLast && nodeParent.getParent()!=null){
//				// ex.:parent->rootNode  ->null
//				if(nodeParent.getParent().getParent()==null){ //YES double parent check!!!
//					break; //skips the top/root/gui nodes 
//				}
//			}
			
			nodeParent=nodeParent.getParent();
		}
		
		return ret;
	}
	
	/**
	 * 
	 * @param spt
	 * @param bIncludeLast usually the root virtual world or gui node
	 * @return last one is the parentest
	 */
	@SuppressWarnings("unchecked")
	public <T extends Node> ArrayList<T> getAllParents(Spatial spt, boolean bIncludeLast) {
		ArrayList<T> anode = new ArrayList<>();
		
		T nodeParent = (T)spt.getParent();
		
		while(nodeParent!=null){
			anode.add(nodeParent);
			nodeParent=(T)nodeParent.getParent();
		}
		
		if(!bIncludeLast && anode.size()>0)anode.remove(anode.size()-1);
		
		return anode;
	}
	
	public static class FuncSptConcreteClassMatcher <TYPE extends Spatial> implements Function<Spatial,Boolean>{
		private Class<TYPE>	clFilter;
		
		public FuncSptConcreteClassMatcher(Class<TYPE> clFilter) {
			this.clFilter=clFilter;
		}
		public Class<TYPE> getClassFilter() {
			return clFilter;
		}
		@Override
		public Boolean apply(Spatial spt) {
			return clFilter.isInstance(spt);
		}
	}
	
	/**
	 * TODO see {@link Node#descendantMatches(Class,String)}, may be this can deprecate..
	 * @param nodeParentestToChk
	 * @param clFilter
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T extends Spatial> T getChildRecursiveExactMatch(Node nodeParentestToChk, Class<T> clFilter){
		return (T)getChildRecursiveExactMatch(
			nodeParentestToChk, 
			new FuncSptConcreteClassMatcher(clFilter)
		);
	}
	
	/**
	 * see {@link #getAllChildrenRecursiveFrom(Node, Integer, Function)}
	 * @param nodeParentestToChk
	 * @param funcMatcher
	 * @return
	 */
	public <T extends Spatial> T getChildRecursiveExactMatch(Node nodeParentestToChk, Function<Spatial,Boolean> funcMatcher){
		ArrayList<T> asptList = getAllChildrenRecursiveFrom(nodeParentestToChk, null, funcMatcher);
		if(asptList.isEmpty() || asptList.size()>1)throw new DetailedException("not exact match",nodeParentestToChk,funcMatcher,asptList);
		return asptList.get(0);
	}

	/**
	 * @param nodeParentestToChk
	 * @param clFilter if Spatial, will bring all
	 * @param iMaxDepth max recursion depth, can be null (unlimited) 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T extends Spatial> ArrayList<T> getAllChildrenOfTypeRecursiveFrom(Node nodeParentestToChk, Class<T> clTypeFilter, Integer iMaxDepth) {
		return getAllChildrenRecursiveFrom(
			nodeParentestToChk, 
			iMaxDepth, 
			new FuncSptConcreteClassMatcher(clTypeFilter)
		);
	}
	/**
	 * 
	 * @param nodeParentestToChk
	 * @param iDepth initial/max recursion depth, can be null (unlimited) 
	 * @param funcMatcher can be null, or can retrieve key: Spatial.class.getName(), matches if return true
	 * @return the matched spatials
	 */
	@SuppressWarnings("unchecked")
	public <T extends Spatial> ArrayList<T> getAllChildrenRecursiveFrom(Node nodeParentestToChk, Integer iDepth, Function<Spatial,Boolean> funcMatcher) {
		if(nodeParentestToChk==null)throw new DetailedException("null spatial");
		
		ArrayList<T> asptList = new ArrayList<T>();
		
		if(iDepth!=null){
			if(iDepth==0)return asptList; 
			iDepth-=1;
		}
		
		Node nodeParent = null;
		if (nodeParentestToChk instanceof Node) {
			nodeParent = (Node) nodeParentestToChk;
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
				asptList.addAll(getAllChildrenRecursiveFrom((Node)sptChild, iDepth, funcMatcher));
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
	 * @param funcDo if return true, will add the spatial to the store var to be returned
	 * @param iCurrentDepth can be 0
	 * @param hmStore will be created if null
	 * @return the list of successful doings
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

	public boolean isChildRecursive(Node nodeParent, Node... anodeChild) {
		for(Node nodeChild:anodeChild){
			if(nodeChild==null)continue;
			for(Node nodeP:getAllParents(nodeChild, true)){
				if(nodeP==nodeParent)return true;
			}
		}
		
		return false;
	}
	
	public void debugHelperAutoGiveNames(Spatial... aspt){
		Function<SpatialInfo, Boolean> func = new Function<SpatialInfo, Boolean>() {
			@Override
			public Boolean apply(SpatialInfo si) {
				if(si.getSpatial().getName()==null){
					Spatial spt=null;
					String str=null;
					for(Node node:getAllParents(si.getSpatial(), false)){
						if(node.getName()!=null){
							str="ChldOf:";
							spt=node;
							break;
//							si.getSpatial().setName("ChldOf:"+node.getName());
//							return true;
						}
					}
					
					if(spt==null && si.getSpatial() instanceof Node){
						ArrayList<Spatial> aspt = getAllChildrenRecursiveFrom((Node)si.getSpatial(), 0, new Function<Spatial,Boolean>(){
							@Override
							public Boolean apply(Spatial spt) {
								if(spt.getName()!=null)return true;
								return false;
							}
						});
						
						if(aspt.size()>0){
							str="PrntOf(depth?):";
							spt=aspt.get(0);
						}
					}
					
					if(spt!=null){
						si.getSpatial().setName(str+spt.getName());
						return true;
					}
				}
				
				return false;
			}
		};
		
		if(aspt.length==0){
			doSomethingRecursively(AppI.i().getRootNode(), func, 0, null);
			doSomethingRecursively(AppI.i().getGuiNode(), func, 0, null);
		}else{
			for(Spatial spt:aspt){
				func.apply(new SpatialInfo().setSpatial(spt));
			}
		}
	}

	public boolean isRelated(Spatial sptA, Spatial sptB) {
		if(sptA==sptB)return true;
		if(sptA.getParent()==sptB.getParent())return true;
		if(getAllParents(sptA,false).contains(sptB))return true;
		if(getAllParents(sptB,false).contains(sptA))return true;
		return false;
	}
}
