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
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.BatchNode;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.SimpleBatchNode;
import com.jme3.scene.shape.Sphere;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DecalI {
	public static DecalI i(){return GlobalManagerI.i().get(DecalI.class);}
	
	public static enum EDecal{
		Exploded,
		Burn,
		Hole,
		;
		public String s() {return this.toString();}
	}
	
	private SimpleBatchNode	sbnProjectilesAtWorld = new SimpleBatchNode("Decals");
	private Geometry geomFactoryBurn;
	private Geometry geomFactoryHole;
	private Geometry geomFactoryExploded;
	
	public void configure() {
//		geomFactory = GeometryI.i().create(MeshI.i().sphere(0.05f), ColorRGBA.Black);
		geomFactoryBurn = GeometryI.i().create(new Sphere(4,5,0.025f), ColorRGBA.Black);
		geomFactoryHole = GeometryI.i().create(new Sphere(4,5,0.025f), ColorRGBA.Yellow);
		geomFactoryExploded = GeometryI.i().create(new Sphere(4,5,0.05f), ColorRGBA.Cyan);
	}
	
	public void createAtMainThread(Node nodeParent, Vector3f v3fWHitPos, EDecal e) {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				Geometry geom = null;
				switch(e) {
					case Burn:
						geom = geomFactoryBurn.clone();
						break;
					case Exploded:
						geom = geomFactoryExploded.clone();
						break;
					case Hole:
						geom = geomFactoryHole.clone();
						break;
				}
				
				if(sbnProjectilesAtWorld.getParent()==null)AppI.i().getRootNode().attachChild(sbnProjectilesAtWorld);
//				(nodeParent!=null ? nodeParent : AppI.i().getRootNode()).attachChild(geomFactory.clone());
				geom.setLocalTranslation(v3fWHitPos);
				(nodeParent!=null ? nodeParent : sbnProjectilesAtWorld).attachChild(geom);
				if(nodeParent instanceof BatchNode)((BatchNode)nodeParent).batch();
				return true;
			}
		});
	}
	
}

