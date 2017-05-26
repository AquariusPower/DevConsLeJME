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
import java.util.Arrays;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.app.SimpleApplication;
import com.jme3.bounding.BoundingBox;
import com.jme3.font.BitmapText;
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

	private ReticleNode	rn;
	protected Vector3f	v3fAppWSize = new Vector3f();
	
	public void configure(){} //keep even if empty to help init global
	
	public static class ReticleNode extends Node{
		private Node	nodeBorder;
	//	private Node	node;
		private int	fBorderIR=5;
		private float	fBorderOR;
		private Quaternion	quaBorderStartAt;
		private BitmapText[]	abtDistMarks;
		private boolean	bBinoculars;
		private ArrayList<Geometry> ageomZoomMarkersList = new ArrayList<>();
		private int	iZoomIndexLast=-1;
		private float fDegAnglePerZoomStep=10f;
		
		public Node getNodeBorder() {
			return nodeBorder;
		}

		public int getBorderIR() {
			return fBorderIR;
		}

		public float getBorderOR() {
			return fBorderOR;
		}

		public Quaternion getQuaBorderStartAt() {
			return quaBorderStartAt;
		}

		public ArrayList<BitmapText> getDistMarksList() {
			return new ArrayList(Arrays.asList(abtDistMarks));
		}

		public boolean isBinoculars() {
			return bBinoculars;
		}

		public ArrayList<Geometry> getZoomMarkersList() {
			return ageomZoomMarkersList;
		}

		public int getZoomIndexLast() {
			return iZoomIndexLast;
		}

		public float getDegAnglePerZoomStep() {
			return fDegAnglePerZoomStep;
		}

		public float getDistMarkY(float fDivBorderOR, int iMarkIndex) {
			if(!bBinoculars){
				float fStep = fBorderOR/fDivBorderOR;
				float fTopStart=20;
				switch(iMarkIndex){
					case 0: return fTopStart;
					case 1: return -(fStep*1);
					case 2: return -(fStep*3);
					case 3: return -(fStep*5);
					case 4: return -(fStep*7);
				}
			}
			throw new DetailedException("invalid index",iMarkIndex);
		}
		
		public void updateDistanceMarkersValues(int... ai){
			if(!bBinoculars){
				for(int i=0;i<5;i++){
					abtDistMarks[i].setText((i==0?"    ":"=== ")+ai[i]);
				}
			}
		}
		public void updateZoomMarkers(int iTot){
			if(iTot==ageomZoomMarkersList.size())return;
			
			for(Geometry geom:ageomZoomMarkersList){
				geom.removeFromParent();
			}
			ageomZoomMarkersList.clear();
			
			for(int i=0;i<iTot;i++){
				Geometry geom=createZoomLevelMarker(ColorRGBA.DarkGray);
				nodeBorder.getParent().attachChild(geom);
				Node nodePivot = new Node();
				nodePivot.rotateUpTo(Vector3f.UNIT_Z);
				nodeBorder.getParent().attachChild(nodePivot);
				RotateI.i().rotateAroundPivot(geom, nodePivot, fDegAnglePerZoomStep*i*FastMath.DEG_TO_RAD, false);
				ageomZoomMarkersList.add(geom);
			}
		}
		
		public void updateZoomLevel(int iZoomIndex){
			if(iZoomIndexLast == iZoomIndex)return;
			RotateI.i().rotateSpinning(nodeBorder, Vector3f.UNIT_X, Vector3f.UNIT_Z, 
				(90+(fDegAnglePerZoomStep*((ageomZoomMarkersList.size()-1)-iZoomIndex)))*FastMath.DEG_TO_RAD);
		}
		
		private Geometry createZoomLevelMarker(ColorRGBA color) {
			Geometry zoommarker = GeometryI.i().create(MeshI.i().cone(fBorderIR*2f),color);
			zoommarker.setLocalTranslation(fBorderOR,0,fBorderIR);
			zoommarker.lookAt(new Vector3f(), Vector3f.UNIT_Y);
			return zoommarker;
		}

		public ReticleNode setBinocularsMode(boolean bBinoculars) {
			this.bBinoculars = bBinoculars;
			return this; 
		}

		public ReticleNode setDegAnglePerZoomStep(float fDegAnglePerZoomStep) {
			this.fDegAnglePerZoomStep = fDegAnglePerZoomStep;
			return this; 
		}

		public ReticleNode setBorderIR(int fBorderIR) {
			this.fBorderIR = fBorderIR;
			return this; 
		}
		
	}
	
	public ReticleNode createReticle(ReticleNode rnStore){
		if(rnStore==null)rnStore=new ReticleNode();
		
//		node = new Node("Reticle");
		rnStore.setName("Reticle"+(rnStore.isBinoculars()?".binoc":""));
		
		int iDMin=EnvironmentJmeI.i().getDisplay().getMinSize();
		int iDMax=EnvironmentJmeI.i().getDisplay().getMaxSize();
		
		// scope border
		rnStore.nodeBorder=new Node("Border");
		rnStore.fBorderOR=(iDMin/2f)-rnStore.getBorderIR();
		Geometry border = GeometryI.i().create(new Torus(40, 5, rnStore.getBorderIR(), rnStore.fBorderOR),ColorRGBA.Gray);
		rnStore.nodeBorder.attachChild(border);
		Geometry zoomMarkerCurrent = rnStore.createZoomLevelMarker(ColorI.i().colorChangeCopy(ColorRGBA.Red,-0.5f));
		zoomMarkerCurrent.scale(2f);
		zoomMarkerCurrent.move(0,0,rnStore.getBorderIR());
		rnStore.nodeBorder.attachChild(zoomMarkerCurrent);
		rnStore.quaBorderStartAt = rnStore.nodeBorder.getLocalRotation().clone();
		rnStore.attachChild(rnStore.nodeBorder);
		
		// distance marks
		if(rnStore.isBinoculars()){
			//TODO
		}else{
			rnStore.abtDistMarks=new BitmapText[5];
			for(int i=0;i<5;i++){
				rnStore.abtDistMarks[i]=new BitmapText(TextI.i().loadDefaultMonoFont());
				rnStore.attachChild(rnStore.abtDistMarks[i]);
				rnStore.abtDistMarks[i].setLocalTranslation(-10,rnStore.getDistMarkY(8,i),0);
				rnStore.abtDistMarks[i].setColor(ColorRGBA.DarkGray);
			}
			rnStore.updateDistanceMarkersValues(100,200,300,400,500);
			
			// arrows
			float fArrowsZ = -10f;
			float fCenterMargin = 20f;
			rnStore.attachChild(createLine(rnStore, fArrowsZ, fCenterMargin));
			ArrowGeometry arrowRight = GeometryI.i().createArrow(ColorRGBA.DarkGray);
			arrowRight.setFromTo(new Vector3f(rnStore.fBorderOR,0,fArrowsZ), new Vector3f(fCenterMargin,0,fArrowsZ));
			rnStore.attachChild(arrowRight);
			ArrowGeometry arrowBottom = GeometryI.i().createArrow(ColorRGBA.DarkGray);
			arrowBottom.setFromTo(new Vector3f(0,-rnStore.fBorderOR,fArrowsZ), new Vector3f(0,-fCenterMargin,fArrowsZ));
			rnStore.attachChild(arrowBottom);
		}
		
		// blocker
		float fProportion = iDMin/(float)iDMax;
		float fDiagonal = FastMath.sqrt(FastMath.pow(iDMin,2)+FastMath.pow(iDMax,2));
		float fBlockerIR=((fDiagonal/2f)-rnStore.fBorderOR)/2f;
//		float fBlockerIR=((iDMax/2)-(iDMin/2))/2;if(fBlockerIR<1f)fBlockerIR=1f;
		float fBlockerOR=rnStore.fBorderOR+fBlockerIR; //TODO why had to -100?
		fBlockerIR*=1.25f; //after outer radius calc TODO why 1.25f worked? torus creation inner radius innacuracy? or my fault?
		Geometry blocker = GeometryI.i().create(new Torus(25, 5, fBlockerIR, fBlockerOR),
			new ColorRGBA(0.0125f,0.025f,0.05f,0.97f)); //glass feeling, TODO use a texture?
		blocker.setQueueBucket(Bucket.Inherit); //restore gui one
		blocker.move(new Vector3f(0,0,-fBlockerIR));
		rnStore.attachChild(blocker);
		
		return rnStore;
	}
	
	private Node createLine(ReticleNode rnStore, float fArrowsZ, float fCenterMargin ) {
		Vector3f v3fFrom = new Vector3f(-rnStore.fBorderOR,0,fArrowsZ);
		Vector3f v3fTo=new Vector3f(-fCenterMargin,0,fArrowsZ);
		
		ArrowGeometry ag = GeometryI.i().createArrow(ColorRGBA.DarkGray);
		ag.setFromTo(v3fFrom,v3fTo);
		Node node = new Node();
		node.attachChild(ag);
		
		float fTipScale = rnStore.fBorderOR/10f;
//		node.setLocalScale(fTipScale,fTipScale,1f);
		
		return node;
	}

	/**
	 * see {@link #autoRecreate(ReticleNode)}
	 * @param rnStore
	 */
	public void initAutoSetup(boolean bBinoculars){
		autoRecreate(new ReticleNode().setBinocularsMode(bBinoculars));
	}
	
	/**
	 * 
	 * @param rnStore can come with some things pre-setup
	 */
	public void autoRecreate(ReticleNode rnStore){
		if(rn!=null)rn.removeFromParent();
		
		SimpleApplication sapp = G.i(SimpleApplication.class);
		if(sapp!=null){
			FlyByCamera flycam = sapp.getFlyByCamera();
			FlyByCameraX flycamx=(flycam instanceof FlyByCameraX) ? (FlyByCameraX)flycam : null;
			
			rn = createReticle(rnStore);
			if(flycamx!=null)flycamx.setReticle(rn, sapp.getGuiNode()); //TODO shouldnt be another layer, below the gui node?
			sapp.getGuiNode().attachChild(rn);
			rn.setLocalTranslation(EnvironmentJmeI.i().getDisplay().getCenter(
				-((BoundingBox)rn.getWorldBound()).getExtent(null).length()
			));
			
			if(flycamx!=null){
				QueueI.i().enqueue(new CallableXAnon() {
					@Override
					public Boolean call() {
						Vector3f v3fAppWSizeCurrent = EnvironmentJmeI.i().getDisplay().getAppWindowSize();
						if(!v3fAppWSizeCurrent.equals(v3fAppWSize)){
							autoRecreate(null);
							v3fAppWSize.set(v3fAppWSizeCurrent);
//							QueueI.i().removeLoopFromQueue(this); //a new one will be created
							return true; 
						}
						
						rn.updateZoomMarkers(flycamx.getTotalZoomSteps());
						rn.updateZoomLevel(flycamx.getCurrentZoomLevelIndex());
						return true;
					}
				}).setName("ReticleUpdateZoom").enableLoopMode().setDelaySeconds(0.5f);
			}
				
//				EffectElectricity el = new EffectElectricity();
//				el.setFollowFromTarget(node, null);
//				el.setFollowToMouse(true);
//				el.setPlay(true);
//				EffectManagerStateI.i().add(el);
			
		}
		
	}

	public ReticleNode getLastReticuleNode() {
		return rn;
	}

}
