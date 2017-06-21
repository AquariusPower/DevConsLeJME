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
import java.util.HashMap;
import java.util.Map.Entry;

import com.github.devconslejme.misc.Annotations.Bean;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bounding.BoundingVolume;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Box;

/**
 * These visuals are to not be integrated as child into other spatials as they would most probably 
 * modify their world bound (the extra axes scaling), possibly causing other troubles on calculations depending 
 * on correct bounds.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DebugVisualsI {
	public static DebugVisualsI i(){return GlobalManagerI.i().get(DebugVisualsI.class);}
	
	private boolean bVisualsEnabled=false;
	private boolean bShowWorldBound=false;
	private float fUpdateDelay=1f;
	private HashMap<Spatial,NodeDbg> ahmShowWorldBound = new HashMap<Spatial,NodeDbg>();
	private Node	axesRef;
	private Vector3f	v3fAxesDisplaceCam;
	private boolean bEnableDbgVis;
	
	public void configure(){ //just to let the global be promptly instantiated
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}.enableLoopMode());
		
		KeyBindCommandManagerI.i().putBindCommandsLater("Ctrl+D",new CallBoundKeyCmd(){
			@Override
			public Boolean callOnKeyReleased(int iClickCountIndex) {
				ArrayList<RayCastResultX> acr = WorldPickingI.i().raycastPiercingFromCenter(null);
				if(acr.size()>0)toggleWorldBoundAndRotAxes(acr.get(0).getGeom());
				return true;
			}
		}.setName("ToggleDebugVisualsAt"));
		
		axesRef = NodeI.i().createRotationAxes(null);
		float fScale=0.1f;
		axesRef.setLocalScale(fScale);
		v3fAxesDisplaceCam=new Vector3f(0f,0f,1f+fScale);
		
		Runnable r = new Runnable() {
//			private long lPrevious=0;
//			private long	lNew;
			@Override
			public void run() {
				while(true){
					if(HWEnvironmentJmeI.i().getMouse()!=null){
						if(System.currentTimeMillis()-HWEnvironmentJmeI.i().getLastFrameMilis() > 500){ //wait half a second
							HWEnvironmentJmeI.i().getMouse().forceUngrab();
						}
					}
					
					//TODO? if(AppI.i().isExiting())break;
					
					try {
						Thread.sleep(100);
					} catch (InterruptedException e) {
						e.printStackTrace();
					} 
				}
			}
		};
		Thread t = new Thread(r);
		t.setName("UngrabMouseOnFreezeOrDebugBreakPoint");
		t.start();
	} 
	
	protected void update(float tpf) {
//		if(bShowWorldBound)
		updateShowWorldBound(tpf);
		
		if(isEnableDbgVis()) {
			if(axesRef.getParent()==null)AppI.i().getRootNode().attachChild(axesRef);
			AppI.i().placeAtCamWPos(axesRef, v3fAxesDisplaceCam, false); //being at world, must be in the center TODO put at gui node?
		}else {
			if(axesRef.getParent()!=null)(axesRef).removeFromParent();
		}
	}
	
	public static class NodeAxesDbg extends Node implements IDbg{
		private Spatial	sptTarget;

		public NodeAxesDbg(Spatial spt) {
			this.sptTarget=spt;
		}
		@Override
		public void setTarget(Spatial spt) {
			this.sptTarget=spt;
		}

		@Override
		public Spatial getTarget() {
			return sptTarget;
		}
	}
	
	public static class NodeDbg extends Node implements IDbg{
		private GeometryBVolDbg geombv;
		private NodeAxesDbg	axes;
//		private boolean	bShowAxes;
		private Spatial	sptTarget;
		
		private boolean	bShow;
		private Node nodeParentBkp;
		public boolean isShow() {
			return bShow;
		}
		
		public NodeDbg(Spatial spt) {
			setName(DebugVisualsI.class.getSimpleName()+":Node");
			this.sptTarget = spt;
		}
		
		@Override
		public boolean removeFromParent() {
			if(getParent()!=null)nodeParentBkp=getParent();
			return super.removeFromParent();
		}
		
		public void reAttachToPreviousParent() {
			if(getParent()==null && nodeParentBkp!=null)nodeParentBkp.attachChild(this);
		}
		
//		@Override
//		public void setLocalTranslation(float x, float y, float z) {
//			super.setLocalTranslation(x, y, z);
//			if(getLocalTranslation().length()==0){
//				System.out.println("brkpt");
//			}
//		}
//		@Override
//		public void setLocalTranslation(Vector3f localTranslation) {
//			super.setLocalTranslation(localTranslation);
//			if(getLocalTranslation().length()==0){
//				System.out.println("brkpt");
//			}
//		}
//		@Override
//		public void updateLogicalState(float tpf) {
//			super.updateLogicalState(tpf);
//			if(getLocalTranslation().length()==0){
//				System.out.println("brkpt");
//			}
//		}
//		
//		@Override
//		public void setLocalTranslation(Vector3f localTranslation) {
//			super.setLocalTranslation(localTranslation);
//		}
		
		public void updateAxes() {
//			float fMult=2f*1.1f;//10% beyond limits to be surely visible
			float fMult=1.5f;//10% beyond limits to be surely visible
			if(geombv.bb!=null){
				Vector3f v3fE = geombv.bb.getExtent(null);
				axes.setLocalScale(Math.max(v3fE.x,Math.max(v3fE.y,v3fE.z))*fMult);
//				axes.setLocalScale(geombv.bb.getExtent(null).mult(fMult));
			}else
			if(geombv.bs!=null){
				axes.setLocalScale(geombv.bs.getRadius()*fMult);
			}
			
			axes.setLocalRotation(sptTarget.getWorldRotation());
		}
		
		public NodeDbg setShow(boolean b) {
			this.bShow=b;
			
			if(b){
				sptTarget.hasAncestor(AppI.i().getRootNode());
				AppI.i().getRootNode().attachChild(this);
//				Node parent = sptTarget.getParent();
//				if(parent!=null){
//					parent.attachChild(this);
//				}
			}else{
				removeFromParent();
			}
			
//			geombv.setTarget(getTarget());
//			geombv.setShow(b);
			
//			axes.setTarget(getTarget());
//			axes.setShow(b);
			
			return this;
		}

		@Override
		public void setTarget(Spatial spt) {
			this.sptTarget=spt;
		}

		@Override
		public Spatial getTarget() {
			return sptTarget;
		}
	}
	
	public static class GeometryBVolDbg extends Geometry implements IDbg{
		private BoundingBox	bb;
		private BoundingSphere	bs;
		private Spatial	sptTarget;
//		private boolean	bShowWorldBound;
////	public boolean isShow() {
////	return bShowWorldBound;
////}

		public GeometryBVolDbg(Spatial sptTargetToShowBoundingVolume) {
			this.sptTarget = sptTargetToShowBoundingVolume;
			this.setMesh(new Box(0.01f,0.01f,0.01f)); //placeholder (that will be modified) to avoid raycast collision missing mesh problem, this is just a debug thing anyaway...
		}

		public BoundingBox getTargetBB() {
			return bb;
		}

		public void setTargetBB(BoundingBox bb) {
			this.bb = bb;
		}

		public void setTargetBS(BoundingSphere bs) {
			this.bs = bs;
		}

		public BoundingSphere getTargetBS() {
			return bs;
		}

//		public void setShow(boolean b) {
//			DebugVisualsI.i().setShow(this,b);
//		}
//		public GeometryBVolDbg setShow(boolean bShowWorldBound) {
//			this.bShowWorldBound=bShowWorldBound;
//			
//			if(!bShowWorldBound){
//				removeFromParent();
//			}else{
//				if(sptTarget.getParent()!=null){
//					sptTarget.getParent().attachChild(this);
//				}
//			}
//			
//			return this;
//		}

//		public boolean isShow() {
//			return bShowWorldBound;
//		}

		@Override
		public void setTarget(Spatial spt) {
			this.sptTarget=spt;
		}

		@Override
		public Spatial getTarget() {
			return sptTarget;
		}

	}
	
	public static interface IDbg{
		void setTarget(Spatial spt);
		Spatial getTarget();
	}
	
	protected void updateShowWorldBound(float tpf) {
		for(Entry<Spatial, NodeDbg> entry:ahmShowWorldBound.entrySet()){
			Spatial spt = entry.getKey();
			NodeDbg nd = entry.getValue();
			if(bEnableDbgVis) {
				if(nd.getParent()==null)nd.reAttachToPreviousParent();
				
				if(spt.getParent()==null && nd.getParent()!=null){
					nd.removeFromParent();
					continue;
				}
				
				if(spt.getParent()!=null && nd.getParent()==null && nd.isShow()){
					nd.setShow(true);
				}
				
				updateWorldBoundAndAxes(spt,nd);
	//			System.out.println("B:"+nd.getLocalTranslation());
	//			nd.setLocalTranslation(spt.getLocalTranslation().clone());
				nd.setLocalTranslation(spt.getWorldTranslation().clone());
	//			System.out.println("A:"+nd.getLocalTranslation());
			}else {
				if(nd.getParent()!=null)nd.removeFromParent();
			}
		}
	}
	
	protected NodeDbg createWorldBoundGeomAndAxes(Spatial spt){
		NodeDbg nd = ahmShowWorldBound.get(spt);
		if(nd==null)nd=new NodeDbg(spt);
		return updateWorldBoundAndAxes(spt,nd);
	}
	protected NodeDbg updateWorldBoundAndAxes(Spatial sptTarget,NodeDbg nd){
//		GeometryBVolDbg geomBound = nd.geombv;
		BoundingVolume bv = sptTarget.getWorldBound();
		GeometryBVolDbg geomBoundNew=null;
		ColorRGBA color=ColorRGBA.Blue;
//		boolean bCreating=false;
		//TODO if it was already created, could just modify it?
		if (bv instanceof BoundingBox) {
			BoundingBox bb = (BoundingBox) bv;
//			if(nd.geombv==null || !bb.getExtent(null).equals(nd.geombv.getTargetBB().getExtent(null))){
			if(nd.geombv==null || !bb.getExtent(null).equals(((BoundingBox)nd.geombv.getWorldBound()).getExtent(null))){
				geomBoundNew = GeometryI.i().create(new Box(bb.getXExtent(),bb.getYExtent(),bb.getZExtent()), 
					color, false,	createGeomBVolDbg(sptTarget) );
//				bCreating=true;
				geomBoundNew.setTargetBB(bb);
			}
		}else
		if (bv instanceof BoundingSphere) {
			BoundingSphere bs = (BoundingSphere) bv;
//			if(nd.geombv==null || bs.getRadius()!=nd.geombv.getTargetBS().getRadius()){
			if(nd.geombv==null || bs.getRadius()!=((BoundingSphere)nd.geombv.getWorldBound()).getRadius()){
				geomBoundNew = GeometryI.i().create(MeshI.i().sphere(bs.getRadius()), 
					color, false, createGeomBVolDbg(sptTarget));
//				bCreating=true;
				geomBoundNew.setTargetBS(bs);
			}
		}else{
			throw new UnsupportedOperationException("unsupported "+bv.getClass());
		}
		
		if(geomBoundNew!=null){
			geomBoundNew.getMaterial().getAdditionalRenderState().setWireframe(true);
			if(nd.geombv!=null)nd.geombv.removeFromParent();
			nd.attachChild(geomBoundNew);
			
//			//compensate the rotation
//			Quaternion qua = nd.getWorldRotation().clone();
//			qua.negate();
//			geomBoundNew.setLocalRotation(qua);
			
			nd.geombv=geomBoundNew;
		}
		
		if(nd.axes==null){
	//		nd = new NodeDbg(spt);
			nd.axes = NodeI.i().createRotationAxes(new NodeAxesDbg(sptTarget));
			nd.attachChild(nd.axes);
		}
		
		nd.updateAxes();
		
		return nd;
	}

	protected GeometryBVolDbg createGeomBVolDbg(Spatial spt) {
		GeometryBVolDbg geom = new GeometryBVolDbg(spt);
		WorldPickingI.i().addSkip(geom);
		return geom;
	}
	
//	/**
//	 * TODO couldnt this just be the existing arrow effect? duplicated concepts?
//	 * @param nodeBase
//	 * @param sptFrom
//	 * @param sptTo
//	 * @param color
//	 * @return
//	 */
//	public ArrowGeometry createArrowFollowing(Node nodeBase, Spatial sptFrom, Spatial sptTo, ColorRGBA color){
//		ArrowGeometry ga = createArrow(color);
////		MiscJmeI.i().addToName(ga, DebugVisualsI.class.getSimpleName(), true);
//		
//		ga.setFromToCenterMode(EFollowMode.Edge, EFollowMode.Edge);
//		
//		ga.setControllingQueue(
//			QueueI.i().enqueue(new CallableXAnon() {
////				private Spatial	sptBeingFollowed=sptTarget;
//				@Override	public Boolean call() {
//					if(ga.isDestroy()){
//						ga.removeFromParent();
//						endLoopMode();
//					}else{
//						if(isVisualsEnabled()){
//							if(ga.getParent()==null)nodeBase.attachChild(ga);
//							ga.setFromTo(sptFrom, sptTo);
//						}else{
//							if(ga.getParent()!=null)ga.removeFromParent();
//						}
//					}
//					
//					return true;
//				}}.enableLoopMode().setDelaySeconds(getUpdateDelay()))
//		);
//	
//		return ga;
//	}
	
	@Bean
	public float getUpdateDelay() {
		return fUpdateDelay;
	}
	
	/**
	 * 
	 * @param fUpdateDelay >= 0
	 * @return
	 */
	@Bean
	public DebugVisualsI setUpdateDelay(float fUpdateDelay) {
		this.fUpdateDelay = fUpdateDelay>=0 ? fUpdateDelay : 0;
		return this;
	}

	public boolean isVisualsEnabled() {
		return bVisualsEnabled;
	}

	public void setVisualsEnabled(boolean bVisualsEnabled) {
		this.bVisualsEnabled = bVisualsEnabled;
	}
	
//	public ArrowGeometry createArrow(ColorRGBA color){
//		ArrowGeometry geom = new ArrowGeometry();
//		MiscJmeI.i().addToName(geom, DebugVisualsI.class.getSimpleName(), true);
//		geom.setMesh(new Arrow(new Vector3f(0,0,1f))); //its size will be controled by z scale
//		geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
//		return geom;
//	}
	
	public void showWorldBoundAndRotAxes(Spatial spt) {
		ahmShowWorldBound.put(spt, createWorldBoundGeomAndAxes(spt).setShow(true));
//		if(!ahmShowWorldBound.get(spt)==null)asptShowWorldBound.add(spt);
//		setShowWorldBound(true);
	}
	
	public void hideWorldBoundAndRotAxes(Spatial spt) {
		NodeDbg nd = ahmShowWorldBound.remove(spt);
		if(nd!=null)nd.removeFromParent();
	}
	
	public void toggleWorldBoundAndRotAxes(Spatial spt){
		if(ahmShowWorldBound.get(spt)==null){
			showWorldBoundAndRotAxes(spt);
		}else{
			hideWorldBoundAndRotAxes(spt);
		}
	}
	
	public boolean isShowSpatialsWorldBoundsEnabled() {
		return bShowWorldBound;
	}

	public DebugVisualsI setShowSpatialsWorldBoundsEnabled(boolean bShowWorldBound) {
		this.bShowWorldBound = bShowWorldBound;
		if(!this.bShowWorldBound){
			for(NodeDbg noded:ahmShowWorldBound.values()){
				noded.setShow(bShowWorldBound);
			}
		}
		return this; //for beans setter
	}

	public boolean isEnableDbgVis() {
		return bEnableDbgVis;
	}

	public DebugVisualsI setEnableDbgVis(boolean bEnableDbgVis) {
		this.bEnableDbgVis = bEnableDbgVis;
		return this; 
	}

	
//	public void showRotationAxis(Spatial spt){
//		ahmShowWorldBound.put(spt, createWorldBoundGeom(spt).setShow(true));
//	}
}
