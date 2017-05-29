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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.jme3.app.SimpleApplication;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Curve;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class CrossHairI {
	public static CrossHairI i(){return GlobalManagerI.i().get(CrossHairI.class);}

	private SimpleApplication	sappOpt;
	private Node	node;
	
	public void configure(){
		sappOpt=G.i(SimpleApplication.class);
		
//		create();
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}).enableLoopMode().setDelaySeconds(0.25f);
	} //keep even if emtpy
	
	protected void update(float tpf) {
		if(node==null)return;
		if(HWEnvironmentJmeI.i().getMouse().isCursorVisible()){
			node.removeFromParent();
		}else{
			sappOpt.getGuiNode().attachChild(node);
		}
	}

	public Spatial reinitialize(){
		if(node!=null && node.getParent()!=null)node.removeFromParent();
		
		Vector3f[] av3f={
				new Vector3f(0,-1,0),
				new Vector3f(1,0,0),
				new Vector3f(0,1,0)
		};
		
		node=new Node("CrossHair");
		
		Geometry geomA = GeometryI.i().create(new Curve(av3f, 1), 
			ColorI.i().colorChangeCopy(ColorRGBA.Cyan, 0f, 0.25f));
		geomA.setQueueBucket(Bucket.Gui);
		node.attachChild(geomA);
		
		Geometry geomB = (Geometry)geomA.deepClone();
		geomB.lookAt(Vector3f.UNIT_Z.negate(), Vector3f.UNIT_Y);
		node.attachChild(geomB);
		
		geomA.move(1,0,0);
		geomB.move(-1,0,0);
		
		node.scale(10);
		node.setLocalTranslation(HWEnvironmentJmeI.i().getDisplay().getCenter(0f));
		
//		if(sappOpt!=null){
//			sappOpt.getGuiNode().attachChild(node);
//		}
		
		return node;
	}
	
	public Object debugTest(Object... aobj){
		return null;
	}
}
