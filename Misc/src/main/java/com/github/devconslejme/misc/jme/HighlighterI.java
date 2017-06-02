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

import java.util.HashMap;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ColorI.ColorGlow;
import com.jme3.app.SimpleApplication;
import com.jme3.collision.CollisionResult;
import com.jme3.input.FlyByCamera;
import com.jme3.math.ColorRGBA;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Sphere;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HighlighterI {
	public static HighlighterI i(){return GlobalManagerI.i().get(HighlighterI.class);}
	
	public static class NodeHighLigh extends Node{
		public NodeHighLigh(Geometry geom) {
			this.geomTarget=geom;
			//	colorHighlight.a=0.5f; 
			/**
			 * alpha = 0.5f, just to let it be auto-configured as transparent bucket below
			 */
//			geomHighlight = GeometryI.i().create(new Sphere(),new ColorRGBA(1,1,1,0.5f));
			geomHighlight = GeometryI.i().create(new Sphere(),new ColorRGBA(1,1,1,0.5f)); 
			geomHighlight.getMaterial().setColor("Color",HighlighterI.i().colorHighlight); //only a copy of that color will be there, we need to apply the glowing one
//			colorHighlight=(ColorRGBA)geomHighlight.getMaterial().getParam("Color").getValue();
			geomHighlight.setName("HighLight");
			
			// this way more things can be added to the node
			attachChild(geomHighlight);
			
			WorldPickingI.i().addSkip(geomHighlight);
		}
		
		@Override
		public void updateLogicalState(float tpf) {
			/**
			 * Also, for a few geometries regions (not all hit spots just a few), 
			 * it was necessary to update this at every frame :/ TODO my fault?
			 */
			setLocalTransform(geomTarget.getLocalTransform());
			
			super.updateLogicalState(tpf);
		}
		
		private Geometry	geomTarget;
		private Geometry	geomHighlight;
	}
	private HashMap<Geometry,NodeHighLigh> ahlnList = new HashMap<Geometry,NodeHighLigh>();
	
	private ColorRGBA	colorHighlight=new ColorRGBA(1,1,1,0.5f);
	private boolean bDebug=false;
	private boolean bEnabled=true;
	private FlyByCamera	flycam;
//	private Node nodeHightlight = new Node();
//	private ColorRGBA	colorHighlight=ColorRGBA.Yellow.clone();
//	private TimedDelay tdColorGlow = new TimedDelay(15f, "").setActive(true);
	private ColorGlow	cg;

	private NodeHighLigh	nhMouseCursorOver;

	private SimpleApplication	sappOpt;
	
	public void configure(FlyByCamera flycam){
		this.flycam = flycam;
		this.sappOpt=G.i(SimpleApplication.class);
		if(sappOpt!=null){
			if(this.flycam==null)this.flycam=G.i(SimpleApplication.class).getFlyByCamera();
		}
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}).enableLoopMode();
		
//		nhMouseCursorOver = createNodeHighlight();
		nhMouseCursorOver = new NodeHighLigh(null);
		
		cg = new ColorGlow(colorHighlight,true).setStartHigh(true);
		cg.enable();
	}
	
	
//	private NodeHighLigh createNodeHighlight(Geometry geom) {
//		NodeHighLigh nh = new NodeHighLigh(geom);
//		return nh;
//	}


	protected void reset(NodeHighLigh nh){
		nh.geomTarget=null;
		nh.removeFromParent();
//		cg.disable();
		DebugVisualsI.i().hideWorldBoundAndRotAxes(nh);
	}
	
	protected void update(float tpf) {
		/**
		 * mouse cursor visible hovering over things
		 */
		if(flycam.isEnabled()){
			reset(nhMouseCursorOver);
		}else{
			Geometry geomTargetNew=null;
			for(CollisionResult cr:WorldPickingI.i().raycastPiercingAtCursor(null)){
				geomTargetNew=cr.getGeometry();
				break; //1st only
			}
			if(geomTargetNew!=null && ahlnList.get(geomTargetNew)==null){
				if(nhMouseCursorOver.geomTarget!=geomTargetNew){
					nhMouseCursorOver.geomTarget = geomTargetNew;
					update(nhMouseCursorOver,tpf);
				}
			}else{
				reset(nhMouseCursorOver);
			}
		}
		
		// other selecteds
		for(NodeHighLigh nh:ahlnList.values()){
			update(nh,tpf);
		}
		
		HWEnvironmentJmeI.i().putCustomInfo("Highlighting", nhMouseCursorOver.geomTarget!=null ? nhMouseCursorOver.geomTarget.toString() : null);
	}
	
	protected void update(NodeHighLigh nh,  float tpf) {
		if(isDebug())System.out.println(this+":"+nh.geomTarget.getName());
		
//		cg.enable();
		
		nh.geomHighlight.setMesh(nh.geomTarget.getMesh());
		
		/**
		 * This makes the placement be precise even with moving spatials, but may be destructive if 
		 * this child is not expected to be there (like if the childs there are scanned for some reason).
		 */
		nh.geomTarget.getParent().attachChild(nh); //
		
//		/**
//		 * Also, for a few geometries regions (not all hit spots just a few), 
//		 * it was necessary to update this at every frame :/ TODO my fault?
//		 */
//		nh.setLocalTransform(nh.geomTarget.getLocalTransform());
		
		if(isDebug())DebugVisualsI.i().showWorldBoundAndRotAxes(nh);
		
//		HWEnvironmentJmeI.i().putCustomInfo("Highlighting", nh.geomTarget!=null ? nh.geomTarget.toString() : "(nothing)");
	}

	public boolean isDebug() {
		return bDebug;
	}

	public HighlighterI setDebug(boolean bDebug) {
		this.bDebug = bDebug;
		return this; 
	}

	public ColorRGBA getColorHighlight() {
		return colorHighlight;
	}

	public HighlighterI setColorHighlight(ColorRGBA colorHighlight) {
		this.colorHighlight.set(colorHighlight);
		return this; 
	}
	
	public void applyAt(Geometry geom) {
		assert geom!=null;
		if(ahlnList.get(geom)==null)ahlnList.put(geom, new NodeHighLigh(geom));
	}
	
	public void removeFrom(Geometry geom) {
		reset(ahlnList.remove(geom));
	}
	
}
