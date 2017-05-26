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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.app.SimpleApplication;
import com.jme3.input.FlyByCamera;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Torus;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ReticleI {
	public static ReticleI i(){return GlobalManagerI.i().get(ReticleI.class);}

	private Node	nodeBorder;
	private Node	node;
	private int	fBorderIR;
	private float	fBorderOR;
	private Quaternion	quaBorderStartAt;
	
	public void configure(){} //keep even if empty to help init global
	
	public Node createReticle(){
		node = new Node("Reticle");
		
		int iDMin=EnvironmentJmeI.i().getDisplay().getMinSize();
		int iDMax=EnvironmentJmeI.i().getDisplay().getMaxSize();
		
		// scope border
		nodeBorder=new Node("Border");
		fBorderIR=5;
		fBorderOR=(iDMin/2f)-fBorderIR;
		Geometry border = GeometryI.i().create(new Torus(40, 5, fBorderIR, fBorderOR),ColorRGBA.Gray);
		nodeBorder.attachChild(border);
		Geometry zoomMarkerCurrent = createZoomLevelMarker(ColorRGBA.Red);
		zoomMarkerCurrent.scale(1.25f);
		nodeBorder.attachChild(zoomMarkerCurrent);
		quaBorderStartAt = nodeBorder.getLocalRotation().clone();
		node.attachChild(nodeBorder);
		
//		DebugVisualsI.i().showWorldBoundAndRotAxes(nodeBorder);
		
		// blocker
//		{
		float fDiagonal = FastMath.sqrt(FastMath.pow(iDMin,2)+FastMath.pow(iDMax,2));
		float fBlockerIR=(fDiagonal-fBorderOR)/2f;
		float fBlockerOR=fBorderOR+fBlockerIR-100; //TODO why had to -100?
		Geometry blocker = GeometryI.i().create(new Torus(25, 5, fBlockerIR, fBlockerOR),
			ColorI.i().colorChangeCopy(ColorRGBA.Black,0f,0.97f));
//			ColorRGBA.Blue);
		blocker.setQueueBucket(Bucket.Inherit); //restore gui one
//		blocker.move(new Vector3f(0,0,-(fBlockerIR*10)));
		blocker.move(new Vector3f(0,0,-fBlockerIR));
		node.attachChild(blocker);
//		}
		
//		(EnvironmentJmeI.i().getDisplay().getMaxSize()-EnvironmentJmeI.i().getDisplay().getMinSize())/2f;

		
//		torus.getMaterial().getAdditionalRenderState().setWireframe(true);
		
//		node.attachChild(torus);
////		node.attachChild(GeometryI.i().create(MeshI.i().box(10), ColorRGBA.Yellow));
//		node.attachChild(GeometryI.i().create(MeshI.i().box(10),
//			ColorI.i().colorChangeCopy(ColorRGBA.Yellow,0f,0.85f)));
		
		return node;
	}
	
	private ArrayList<Geometry> ageomZoomMarkersList = new ArrayList<>();
	private int	iZoomIndexLast=-1;
	
	public void updateZoomMarkers(int iTot){
		if(iTot==ageomZoomMarkersList.size())return;
		
		for(Geometry geom:ageomZoomMarkersList){
			geom.removeFromParent();
		}
		ageomZoomMarkersList.clear();
		
		for(int i=0;i<iTot;i++){
			Geometry geom=createZoomLevelMarker(ColorRGBA.Black);
			nodeBorder.getParent().attachChild(geom);
			Node nodePivot = new Node();
			nodePivot.rotateUpTo(Vector3f.UNIT_Z);
			nodeBorder.getParent().attachChild(nodePivot);
			RotateI.i().rotateAroundPivot(geom, nodePivot, fDegAnglePerZoomStep*i*FastMath.DEG_TO_RAD, false);
			ageomZoomMarkersList.add(geom);
		}
	}
	
	float fDegAnglePerZoomStep=10f;
	public void updateZoomLevel(int iZoomIndex){
		if(iZoomIndexLast == iZoomIndex)return;
//		nodeBorder.lookAt(Vector3f.UNIT_X, Vector3f.UNIT_Y);
////		nodeBorder.setLocalRotation(quaBorderStartAt);
		RotateI.i().rotateSpinning(nodeBorder, Vector3f.UNIT_X, Vector3f.UNIT_Z, 
//			(-90+(fDegAnglePerZoomStep*(ageomZoomMarkersList.size()-iZoomIndex)))*FastMath.DEG_TO_RAD);
			(90+(fDegAnglePerZoomStep*((ageomZoomMarkersList.size()-1)-iZoomIndex)))*FastMath.DEG_TO_RAD);
	}
	
	private Geometry createZoomLevelMarker(ColorRGBA color) {
		Geometry zoommarker = GeometryI.i().create(MeshI.i().cone(fBorderIR*2f),color);
		zoommarker.setLocalTranslation(fBorderOR,0,fBorderIR);
		zoommarker.lookAt(new Vector3f(), Vector3f.UNIT_Y);
		return zoommarker;
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
				
				QueueI.i().enqueue(new CallableXAnon() {
					@Override
					public Boolean call() {
						updateZoomMarkers(flycamx.getTotalZoomSteps());
						updateZoomLevel(flycamx.getCurrentZoomLevelIndex());
						return true;
					}
				}).setName("ReticleUpdateZoom").enableLoopMode().setDelaySeconds(0.5f);
				
//				EffectElectricity el = new EffectElectricity();
//				el.setFollowFromTarget(node, null);
//				el.setFollowToMouse(true);
//				el.setPlay(true);
//				EffectManagerStateI.i().add(el);
			}
		}
	}
}
