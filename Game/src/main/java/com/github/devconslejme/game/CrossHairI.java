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
package com.github.devconslejme.game;

import com.github.devconslejme.game.TargetI.TargetGeom;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.jme3.app.SimpleApplication;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.control.AreaUtils;
import com.jme3.scene.shape.Curve;
import com.jme3.scene.shape.Line;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class CrossHairI {
	public static CrossHairI i(){return GlobalManagerI.i().get(CrossHairI.class);}

	private SimpleApplication	sappOpt;
	private Node	nodeTarget;
	private Node	nodeToAttach;
	private Node	nodeDot;
	private boolean	bKeepTargetCrhOnCenterIfNoTarget=true;
	private float	fScale=10;
	private ColorRGBA	colorTarget;
	private float	fTgtColorAlpha=0.25f;
	private Material	matTarget;
	
	public void configure(Node nodeToAttach){
//		sappOpt=G.i(SimpleApplication.class);
		
//		create();
		
		this.nodeToAttach = nodeToAttach;
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}).enableLoopMode().setDelaySeconds(0.1f);
	} //keep even if emtpy
	
	protected void update(float tpf) {
		boolean bCursorVisible = HWEnvironmentJmeI.i().getMouse().isCursorVisible();
		
		if(nodeTarget!=null){
			TargetGeom tgt = TargetI.i().getLastTarget();
			
			if(tgt==null){
				if(bKeepTargetCrhOnCenterIfNoTarget && !bCursorVisible){
					if(nodeTarget.getParent()==null)nodeToAttach.attachChild(nodeTarget);
					nodeTarget.setLocalTranslation(HWEnvironmentJmeI.i().getDisplay().getCenter(0f));
					colorTarget.a=fTgtColorAlpha;
					matTarget.getAdditionalRenderState().setLineWidth(1);
					nodeTarget.setLocalScale(fScale);
				}else{
					if(nodeTarget.getParent()!=null)nodeTarget.removeFromParent();
				}
			}else{
				if(nodeTarget.getParent()==null)nodeToAttach.attachChild(nodeTarget);
				
				Vector3f v3f = AppI.i().getScreenCoordinates(tgt.getGeometryHit().getWorldTranslation());
				nodeTarget.setLocalTranslation(v3f);
				colorTarget.a=1f;
				matTarget.getAdditionalRenderState().setLineWidth(3);
				
				float fAreaAtScreen = AreaUtils.calcScreenArea(tgt.getGeometryHit().getWorldBound(), tgt.getDistance(), HWEnvironmentJmeI.i().getDisplay().getWidth());
				float fPseudoRadiusAtScreen = FastMath.sqrt(fAreaAtScreen);
//				nodeTarget.setLocalScale(fPseudoRadiusAtScreen/fScale);
				nodeTarget.setLocalScale(fPseudoRadiusAtScreen/(fScale/2f));
			}
		}
		
		if(nodeDot!=null){
			if(bCursorVisible){
				if(nodeDot.getParent()!=null)nodeDot.removeFromParent();
			}else{
				if(nodeDot.getParent()==null)nodeToAttach.attachChild(nodeDot);
			}
		}
	}

	public Spatial reinitialize(){
		if(nodeTarget!=null && nodeTarget.getParent()!=null)nodeTarget.removeFromParent();
		if(nodeDot!=null && nodeDot.getParent()!=null)nodeDot.removeFromParent();
		
		nodeTarget=new Node("CrossHair:Target");
		nodeDot=new Node("CrossHair:Dot");
		
		//////////////////// "dot"
		float fDotRadius=0.25f;
		Geometry geomDotV = GeometryI.i().create(new Line(new Vector3f(0,-fDotRadius,0),new Vector3f(0,fDotRadius,0)), 
			ColorI.i().colorChangeCopy(ColorRGBA.Cyan, 0f, 0.75f));
		geomDotV.setQueueBucket(Bucket.Gui);
		nodeDot.attachChild(geomDotV);
		
		Geometry geomDotH = (Geometry)geomDotV.deepClone();
		geomDotH.rotate(0,0,FastMath.DEG_TO_RAD*90);
		nodeDot.attachChild(geomDotH);
		
		geomDotH.move(-fDotRadius/4f,0,0); //after cloning!
		
		centralize(nodeDot);
		
		/////////////////// target TODO fire spread helper indicator based on distance and target area
		Vector3f[] av3f={
				new Vector3f(0,-1,0),
				new Vector3f(1,0,0),
				new Vector3f(0,1,0)
		};
		
		Geometry geomA = GeometryI.i().create(new Curve(av3f, 1), 
			ColorI.i().colorChangeCopy(ColorRGBA.Cyan, 0f, fTgtColorAlpha));
		matTarget = geomA.getMaterial();//.getAdditionalRenderState().setLineWidth(3);
		colorTarget = (ColorRGBA)geomA.getMaterial().getParam("Color").getValue();
		geomA.setQueueBucket(Bucket.Gui);
		nodeTarget.attachChild(geomA);
		
		Geometry geomB = (Geometry)geomA.clone(false);//deepClone();
		geomB.lookAt(Vector3f.UNIT_Z.negate(), Vector3f.UNIT_Y);
		nodeTarget.attachChild(geomB);
		
		geomA.move(1,0,0); //after cloning!
		geomB.move(-1,0,0);
		
		centralize(nodeTarget);
		
//		if(sappOpt!=null){
//			sappOpt.getGuiNode().attachChild(node);
//		}
		
		return nodeTarget;
	}
	
	private void centralize(Node node) {
		node.scale(fScale);
		node.setLocalTranslation(HWEnvironmentJmeI.i().getDisplay().getCenter(0f));
	}

	public Object debugTest(Object... aobj){
		return null;
	}
}
