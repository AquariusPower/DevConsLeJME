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

import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI.CallableWeak;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.jme3.app.Application;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingVolume;
import com.jme3.font.BitmapText;
import com.jme3.font.LineWrapMode;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.scene.Mesh.Mode;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.VertexBuffer.Type;
import com.jme3.util.BufferUtils;

/**
 * DevSelfNote: Misc lib class should not exist. As soon coehsion is possible, do it!
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MiscJmeI {
	public static MiscJmeI i(){return GlobalManagerI.i().get(MiscJmeI.class);}
	
	@SuppressWarnings({ "unchecked" })
	public <T extends Node> T getParentest(Spatial spt, Class<T> clTypeParentest, boolean bIncludeFirst){
		T parentest = null;
		if(bIncludeFirst && clTypeParentest.isInstance(spt))parentest=(T)spt;
		
		Node nodeParent = spt.getParent();
		while(nodeParent!=null){
			if(clTypeParentest.isInstance(nodeParent)){
				parentest=(T)nodeParent;
			}
			nodeParent=nodeParent.getParent();
		}
		
		return parentest;
	}
	
	public Vector3f getBoundingBoxSize(Spatial spt){
		BoundingVolume bv = spt.getWorldBound();
		if(bv==null)return null; //it is not ready yet
		return ((BoundingBox)bv).getExtent(null).mult(2f);
	}
	
	public void recursivelyApplyTextNoWrap(Node nodeParent) {
		for(Spatial spt:nodeParent.getChildren()){
			if(spt instanceof BitmapText){
				((BitmapText)spt).setLineWrapMode(LineWrapMode.NoWrap);
			}
			if(spt instanceof Node){
				recursivelyApplyTextNoWrap((Node)spt);
			}
		}
	}

//	public BitmapText getBitmapTextFrom(Node node){
//		for(Spatial c : node.getChildren()){
//			if(c instanceof BitmapText){
//				return (BitmapText)c;
//			}
//		}
//		return null;
//	}
	
	public static class SimpleClassMatcherCallableX extends CallableX{
		@Override
		public Boolean call() {
			Class clFilter = getValue(Class.class.getName());
			Spatial spt = getValue(Spatial.class.getName());
			return (clFilter.isInstance(spt));
		}
	}
	
	public <T extends Spatial> T getChildRecursiveExactMatch(Spatial sptParentestToChk, Class<T> clFilter){
		return getChildRecursiveExactMatch(sptParentestToChk, 
			new SimpleClassMatcherCallableX().putKeyValue(Class.class.getName(), clFilter));
	}
	
	public <T extends Spatial> T getChildRecursiveExactMatch(Spatial sptParentestToChk, CallableX callMatcher){
		ArrayList<T> asptList = getAllChildrenRecursiveFrom(sptParentestToChk, null, callMatcher);
		if(asptList.isEmpty() || asptList.size()>1)throw new DetailedException("not exact match",sptParentestToChk,callMatcher,asptList);
		return asptList.get(0);
	}
	
	/**
	 * 
	 * @param sptParentestToChk
	 * @param clTypeFilter if Spatial, will bring all
	 * @return
	 */
	/**
	 * 
	 * @param sptParentestToChk
	 * @param callMatcher can be null, or can retrieve key: Spatial.class.getName()
	 * @param iMaxDepth max recursion depth, can be null (unlimited) 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T extends Spatial> ArrayList<T> getAllChildrenRecursiveFrom(Spatial sptParentestToChk, Class<T> clFilter, Integer iMaxDepth) {
		return getAllChildrenRecursiveFrom(sptParentestToChk, iMaxDepth, 
			new SimpleClassMatcherCallableX().putKeyValue(Class.class.getName(), clFilter));
	}
	public <T extends Spatial> ArrayList<T> getAllChildrenRecursiveFrom(Spatial sptParentestToChk, Integer iMaxDepth, CallableX callMatcher) {
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
			if(callMatcher!=null)callMatcher.putKeyValue(Spatial.class.getName(),sptChild);
			if(callMatcher==null || callMatcher.call()){
				asptList.add((T)sptChild);
			}
		}
		
		// deep search
		for(Spatial sptChild:nodeParent.getChildren()){
			if(sptChild instanceof Node){
				asptList.addAll(getAllChildrenRecursiveFrom(sptChild, iMaxDepth, callMatcher));
			}
		}
		
		return asptList;
	}
	
	public Vector3f getMouseCursorPosition(){
		Vector2f v2f = GlobalManagerI.i().get(Application.class).getInputManager().getCursorPosition();
		return new Vector3f(v2f.x,v2f.y,0);
	}

	/**
	 * 
	 * @param av3f each dot from the multi-line
	 * @return
	 */
	public Mesh updateMultiLineMesh(Mesh mesh, Vector3f[] av3f){
		if(mesh==null)mesh=new Mesh();
//		mesh.setStreamed();
		mesh.setMode(Mode.LineStrip);
		
		FloatBuffer fbuf = BufferUtils.createFloatBuffer(av3f);
		mesh.setBuffer(Type.Position,3,fbuf);
		
		ShortBuffer sbuf = BufferUtils.createShortBuffer(av3f.length);
		for(Short si=0;si<sbuf.capacity();si++){sbuf.put(si);}
		
		mesh.setBuffer(Type.Index,1,sbuf);
		
		mesh.updateBound();
		mesh.updateCounts();
		
		return mesh;
	}
}
