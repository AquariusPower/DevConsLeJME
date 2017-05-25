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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.TimedDelay;
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
	
	private boolean bDebug=false;
	private boolean bEnabled=true;
	private FlyByCamera	flycam;
	private Node nodeHightlight = new Node();
	private Geometry	geomTarget;
	private Geometry	geomHighlight;
	private ColorRGBA	colorHighlight=ColorRGBA.Yellow.clone();
	private TimedDelay tdColorGlow = new TimedDelay(1f, "").setActive(true);
	
	public void configure(FlyByCamera flycam){
		this.flycam = flycam;
		if(G.i(SimpleApplication.class)!=null){
			if(this.flycam==null)this.flycam=G.i(SimpleApplication.class).getFlyByCamera();
		}
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}).enableLoopMode();
		
		colorHighlight.a=0.5f; //just to let it be auto-configured as transparent bucket below
		geomHighlight = GeometryI.i().create(new Sphere(),colorHighlight); //.getMaterial().setColor("GlowColor", 	ColorRGBA.Yellow);
		colorHighlight=(ColorRGBA) geomHighlight.getMaterial().getParam("Color").getValue();
		geomHighlight.setName("HighLight");
		
		nodeHightlight.attachChild(geomHighlight);
		
		WorldPickingI.i().addSkip(geomHighlight);
		
		
	}
	
	
	protected void reset(){
		nodeHightlight.removeFromParent();
		DebugVisualsI.i().hideWorldBoundAndRotAxes(nodeHightlight);
	}
	
	protected void update(float tpf) {
		if(flycam.isEnabled()){
			reset();
			return;
		}
		
		geomTarget=null;
		for(CollisionResult cr:WorldPickingI.i().raycastPiercingAtCursor(null)){
			geomTarget=cr.getGeometry();
			break; //1st only
		}
			
		if(geomTarget!=null){
			// this way more things can be added to the node
//			nodeHightlight.setLocalTransform(geomTarget.getWorldTransform());
//			nodeHightlight.scale(1.01f);
			
			float fVal = tdColorGlow.getCurrentDelayCalcDynamic(7f);
//			tdColorGlow.resetAndChangeDelayTo(7f).setActive(true);
			colorHighlight.set(0,0,0,1f);
			switch((int)fVal){
				case 0:colorHighlight.r=fVal%1f;break;
				case 1:colorHighlight.g=fVal%1f;break;
				case 2:colorHighlight.b=fVal%1f;break;
				case 3:colorHighlight.r=colorHighlight.g=fVal%1f;break;
				case 4:colorHighlight.r=colorHighlight.b=fVal%1f;break;
				case 5:colorHighlight.g=colorHighlight.b=fVal%1f;break;
				case 6:colorHighlight.r=colorHighlight.g=colorHighlight.b=fVal%1f;break;
			}
//			colorHighlight.set(1,0,0,1);
			
			if(geomTarget!=geomHighlight){
				geomHighlight.setMesh(geomTarget.getMesh());
//				if(nodeHightlight.getParent()==null)MiscJmeI.i().getNodeVirtualWorld().attachChild(nodeHightlight);
				if(nodeHightlight.getParent()==null){
					geomTarget.getParent().attachChild(nodeHightlight);
					nodeHightlight.setLocalTransform(geomTarget.getLocalTransform());
				}
				if(isDebug())DebugVisualsI.i().showWorldBoundAndRotAxes(nodeHightlight);
			}
		}else{
			reset();
		}
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
	
}