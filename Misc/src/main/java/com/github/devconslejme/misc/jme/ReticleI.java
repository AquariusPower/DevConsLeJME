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
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.github.devconslejme.misc.lemur.SizeAndLocationI;
import com.jme3.app.SimpleApplication;
import com.jme3.input.FlyByCamera;
import com.jme3.math.ColorRGBA;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Torus;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ReticleI {
	public static ReticleI i(){return GlobalManagerI.i().get(ReticleI.class);}
	
	public void configure(){} //keep even if empty to help init global
	
	public Node createReticle(){
		Node node = new Node("Reticle");
		
		Geometry torus = GeometryI.i().create(
			new Torus(100, 10, 20, 200), 
			ColorI.i().colorChangeCopy(ColorRGBA.White,0f,0.85f));
		
		node.attachChild(torus);
		node.attachChild(GeometryI.i().create(MeshI.i().box(10), ColorRGBA.Yellow));
		
		return node;
	}
	
	public void initAutoSetup(){
		SimpleApplication sapp = G.i(SimpleApplication.class);
		if(sapp!=null){
			FlyByCamera flycam = sapp.getFlyByCamera();
			if(flycam instanceof FlyByCameraX){
				FlyByCameraX flycamx = (FlyByCameraX)flycam;
				Node node = createReticle();
				flycamx.setReticle(node, sapp.getGuiNode()); //TODO shouldnt be another layer, below the gui node?
				sapp.getGuiNode().attachChild(node);
				node.setLocalTranslation(EnvironmentJmeI.i().getDisplay().getCenter());
//				node.move(0, -1000, -10);
				
//				GeometryI.i().createArrowFollowing(sapp.getGuiNode(), null, node, ColorRGBA.Yellow)
//					.setFromTo(EnvironmentJmeI.i().getDisplay().getCenter(), node.getLocalTranslation());
				
				EffectElectricity el = new EffectElectricity();
				el.setFollowFromTarget(node, null);
				el.setFollowToMouse(true);
				el.setPlay(true);
				EffectManagerStateI.i().add(el);
			}
		}
	}
}
