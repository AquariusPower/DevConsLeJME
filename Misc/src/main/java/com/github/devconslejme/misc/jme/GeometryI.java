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
import com.github.devconslejme.misc.jme.ArrowGeometry.EFollowMode;
import com.jme3.bounding.BoundingSphere;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Sphere;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GeometryI {
	public static GeometryI i(){return GlobalManagerI.i().get(GeometryI.class);}

	public Geometry create(Mesh mesh, ColorRGBA color) {
		return create(mesh,color,null,null);
	}
	
	/**
	 * 
	 * @param mesh
	 * @param color
	 * @param bTransparent null will be auto if alpha<1f
	 * @param geomStore custom subclass to setup
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T extends Geometry> T create(Mesh mesh, ColorRGBA color, Boolean bTransparent, T geomStore) {
		if(bTransparent==null)bTransparent = color.a<1f;
//		GeometryX geom = new GeometryX(mesh.getClass().getSimpleName());
		if(geomStore==null)geomStore = (T)new Geometry();
		geomStore.setName(mesh.getClass().getSimpleName());
		geomStore.setMesh(mesh);
		if(mesh instanceof Sphere){
			Sphere s = ((Sphere)mesh);
			geomStore.setModelBound(new BoundingSphere(s.getRadius(),Vector3f.ZERO));
		}
		if(color!=null)geomStore.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
		if(bTransparent)geomStore.setQueueBucket(Bucket.Transparent);
		return geomStore;
	}
	
	public ArrowGeometry createArrow(ColorRGBA color){
		ArrowGeometry geom = new ArrowGeometry();
		MiscJmeI.i().addToName(geom, DebugVisualsI.class.getSimpleName(), true);
//		geom.setMesh(new Arrow(new Vector3f(0,0,1f))); //its length will be controled by z scale
		geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
		return geom;
	}
	/**
	 * TODO couldnt this just be the existing arrow effect? duplicated concepts?
	 * @param nodeBase
	 * @param sptFrom
	 * @param sptTo
	 * @param color
	 * @return
	 */
	public ArrowGeometry createArrowFollowing(Node nodeBase, Spatial sptFrom, Spatial sptTo, ColorRGBA color){
		ArrowGeometry ga = createArrow(color);
//		MiscJmeI.i().addToName(ga, DebugVisualsI.class.getSimpleName(), true);
		
		ga.setFromToCenterMode(EFollowMode.Edge, EFollowMode.Edge);
		
		ga.setControllingQueue(
			QueueI.i().enqueue(new CallableXAnon() {
//				private Spatial	sptBeingFollowed=sptTarget;
				@Override	public Boolean call() {
					if(ga.isDestroy()){
						ga.removeFromParent();
						QueueI.i().removeLoopFromQueue(this); //endLoopMode();
					}else{
						if(ga.isEnabled()){
							if(ga.getParent()==null)nodeBase.attachChild(ga);
							ga.setFromTo(sptFrom, sptTo);
						}else{
							if(ga.getParent()!=null)ga.removeFromParent();
						}
					}
					
					setDelaySeconds(ga.getUpdateDelay());
					
					return true;
				}}.enableLoopMode())//.setDelaySeconds(getUpdateDelay()))
		);
	
		return ga;
	}
	
}
