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
//	private ColorRGBA	colorHighlight=ColorRGBA.Yellow.clone();
	private ColorRGBA	colorHighlight;
	private TimedDelay tdColorGlow = new TimedDelay(15f, "").setActive(true);
	
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
		
//		colorHighlight.a=0.5f; 
		/**
		 * alpha = 0.5f, just to let it be auto-configured as transparent bucket below
		 */
		geomHighlight = GeometryI.i().create(new Sphere(),new ColorRGBA(1,1,1,0.5f)); //.getMaterial().setColor("GlowColor", 	ColorRGBA.Yellow);
		colorHighlight=(ColorRGBA)geomHighlight.getMaterial().getParam("Color").getValue();
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
			float fValOriginal = tdColorGlow.getCurrentDelayCalcDynamic(7f); //MUST BE 7: r g b rg rb gb rgb!!!
			float fPercGlow=fValOriginal%1f;
			if(fPercGlow<0.5f){
				fPercGlow*=2f; //0.0 to 0.5 will become 0.0 to 1.0
			}else{
				fPercGlow=1f-fPercGlow;
				fPercGlow*=2f; //0.5 to 1.0 will become 1.0 to 0.0
			}
			float fColorCompMin=0.75f;
			float fColorCompMax=1f;
			float fColorCompDiff=fColorCompMax-fColorCompMin;
			float fColorComp=fColorCompMin+(fColorCompDiff*fPercGlow);
			
			float fAlphaMin=0f;
			float fAlphaMax=0.50f;
			float fAlphaDiff=fAlphaMax-fAlphaMin;
			float fAlpha=fAlphaMin+(fAlphaDiff*fPercGlow);
			colorHighlight.set(0,0,0,fAlpha);
			
			switch((int)fValOriginal){
				case 0:colorHighlight.r=fColorComp;break;
				case 1:colorHighlight.g=fColorComp;break;
				case 2:colorHighlight.b=fColorComp;break;
				case 3:colorHighlight.r=colorHighlight.g=fColorComp;break;
				case 4:colorHighlight.r=colorHighlight.b=fColorComp;break;
				case 5:colorHighlight.g=colorHighlight.b=fColorComp;break;
				case 6:colorHighlight.r=colorHighlight.g=colorHighlight.b=fColorComp;break;
			}
			
			if(geomTarget!=geomHighlight){
				geomHighlight.setMesh(geomTarget.getMesh());
				
				/**
				 * This makes the placement be precise even with moving spatials, but may be destructive if 
				 * this child is not expected to be there (like if the childs there are scanned for some reason).
				 */
				geomTarget.getParent().attachChild(nodeHightlight); //
				
				/**
				 * Also, for a few geometries regions (not all hit spots just a few), 
				 * it was necessary to update this at every frame :/ TODO my fault?
				 */
				nodeHightlight.setLocalTransform(geomTarget.getLocalTransform());
				
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
