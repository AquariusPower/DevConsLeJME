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
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.bounding.BoundingBox;
import com.jme3.collision.CollisionResult;
import com.jme3.font.BitmapText;
import com.jme3.input.FlyByCamera;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Torus;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ReticleI {
	public static ReticleI i(){return GlobalManagerI.i().get(ReticleI.class);}

	private ReticleNode	rnLastConfigured;
	protected Vector3f	v3fAppWSize = new Vector3f();
	private CallableXAnon	cxUpdate;
	private Float	fRangeDist;
	private Spatial	sptTarget;
	private SimpleApplication	sapp;
	private Application	app;
	
	public void configure(){//keep even if empty to help init global
    KeyBindCommandManagerI.i().putBindCommandsLater("T",
    	new CallBoundKeyCmd(){
    		@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
					ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
					if(acr.size()>0){
						sptTarget=SpatialHierarchyI.i().getParentest(acr.get(0).getGeometry(), Node.class, true, false);
					}else{
						sptTarget=null;
					}
					return true;
				}
			}.setName("AcquireTarget").holdKeyPressedForContinuousCmd()
		);
	} 
	
	public static class ReticleNode extends Node{
		// user configurable
		private boolean	bBinoculars=false; //false=scope
		private int	iZoomIndexLast=-1;
		private float fDegAnglePerZoomStep=10f;
		private int	fBorderIR=5;
		
		// auto setup
		private Node	nodeBorder;
		private float	fBorderOR;
		private Quaternion	quaBorderStartAt;
		private BitmapText[]	abtDistMarksV;
		private BitmapText[]	abtDistMarksH;
		private ArrayList<Geometry> ageomZoomMarkersList = new ArrayList<>();
		private BitmapText	btRangeDist;
		
		public ReticleNode(){
			setName(ReticleNode.class.getSimpleName());
		}
		
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

//		public ArrayList<BitmapText> getDistMarksList() {
//			return new ArrayList(Arrays.asList(abtDistMarksV));
//		}

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

		public float getDistMark(boolean bVertical,int iMarkIndex) {
			float fStepBinocV = (fBorderOR*2f)/abtDistMarksV.length; //Vertical is the reference to keep
			if(bVertical){
				if(bBinoculars){
					float fTopStart=fBorderOR;
					return fTopStart+(fStepBinocV*iMarkIndex);
				}else{
					float fDivBorderOR=8f;
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
			}else{
				if(bBinoculars){
					float fLeftStart=-fBorderOR;
					return fLeftStart+(fStepBinocV*iMarkIndex);
				}else{
					//nothing
				}
			}
			throw new DetailedException("invalid index",iMarkIndex);
		}
		
		public void updateDistanceMarkersValues(boolean bVertical, int... ai){
			if(!bBinoculars){
				for(int i=0;i<5;i++){
					if(bVertical){
						abtDistMarksV[i].setText((i==0?"    ":"=== ")+ai[i]);
					}else{
						abtDistMarksH[i].setText(ai[i]+"\n|");
					}
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

		public void updateRangeFinderValue(Float fTargetDist,Float fRangeDist) {
//			if(btRangeDist.getLocalTranslation().length()>0)System.out.println(btRangeDist.getWorldTranslation());
			btRangeDist.setText(""
				+(fRangeDist==null?"...":StringI.i().fmtFloat(fRangeDist,1)+"m")
				+"\n"
				+(fTargetDist==null?"":"Tgt:"+StringI.i().fmtFloat(fTargetDist,1)+"m")
			);
		}
		
	}
	
	public ReticleI setRangeFinderTarget(Spatial spt){
		this.sptTarget = spt;
		return this;
	}
	public ReticleI setRangeFinderDistanceValue(Float fRangeDist){
		this.fRangeDist = fRangeDist;
		return this;
	}
	
	/**
	 * all children will be removed!
	 * @param rnStore can come with some things pre-setup
	 * @return
	 */
	public ReticleNode createReticle(ReticleNode rnStore){
		if(rnStore==null)rnStore=new ReticleNode();
		rnStore.detachAllChildren();
		
//		node = new Node("Reticle");
		rnStore.setName("Reticle"+(rnStore.isBinoculars()?".binoc":""));
		
		int iDMin=HWEnvironmentJmeI.i().getDisplay().getMinSize();
		int iDMax=HWEnvironmentJmeI.i().getDisplay().getMaxSize();
		
		// scope border
		rnStore.nodeBorder=new Node("Border");
		rnStore.fBorderOR=(iDMin/2f)-rnStore.getBorderIR();
		Geometry border = GeometryI.i().create(new Torus(40, 5, rnStore.getBorderIR(), rnStore.fBorderOR),ColorRGBA.Gray);
		rnStore.nodeBorder.attachChild(border);
		rnStore.quaBorderStartAt = rnStore.nodeBorder.getLocalRotation().clone();
		rnStore.attachChild(rnStore.nodeBorder);
		
		Geometry zoomMarkerCurrent = rnStore.createZoomLevelMarker(ColorI.i().colorChangeCopy(ColorRGBA.Red,-0.5f));
		zoomMarkerCurrent.scale(2f);
		zoomMarkerCurrent.move(0,0,rnStore.getBorderIR());
		rnStore.nodeBorder.attachChild(zoomMarkerCurrent);
		
		rnStore.btRangeDist=createBText(rnStore,ColorRGBA.Green);
		rnStore.btRangeDist.setText("+99999.9m"); //TODO maximum displayable distance, create a formatter and get from there considering the world limits
//		rnStore.btRangeDist = new BitmapText(TextI.i().loadDefaultMonoFont());
//		rnStore.btRangeDist.setColor(ColorRGBA.Green);
		int iHalfMin=iDMin/2;
//		rnStore.btRangeDist.setLocalTranslation(iDMin/2f-50f,iDMin/2f-50f,0);
		rnStore.btRangeDist.setLocalTranslation(
			iHalfMin-rnStore.btRangeDist.getLineWidth(),
			iHalfMin-rnStore.btRangeDist.getHeight(),
			0);
//		rnStore.btRangeDist.setLocalTranslation(300f,300f,300);
//		rnStore.attachChild(rnStore.btRangeDist);
		
		// distance marks
		float fArrowsZ = -10f;
		if(rnStore.isBinoculars()){ //mortar recalculations markers
			createTextMarkers(rnStore,true,11);
			createTextMarkers(rnStore,false,11);
			rnStore.updateDistanceMarkersValues(true,7,6,5,4,3,2,1,0,1,2,3); //vertical
			rnStore.updateDistanceMarkersValues(false,5,4,3,2,1,0,1,2,3,4,5); //horizontal
			
			// arrows
			float fCenterMargin = 10f;
			rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Left));
			rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Right));
			rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Top));
			rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Bottom));
		}else{ //bullet drop dists
			createTextMarkers(rnStore,true,5);
			rnStore.updateDistanceMarkersValues(true,100,200,300,400,500); //vertical
			
			// arrows
			float fCenterMargin = 20f;
			rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Left));
			rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Right));
			rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Bottom));
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
	
	private BitmapText createBText(ReticleNode rnStore, ColorRGBA color) {
		BitmapText bt = new BitmapText(TextI.i().loadDefaultMonoFont());
		bt.setColor(color);
		rnStore.attachChild(bt);
		return bt;
	}
	private void createTextMarkers(ReticleNode rnStore, boolean bVertical, int iTotal) {
		if(bVertical){
			rnStore.abtDistMarksV=new BitmapText[iTotal];
		}else{
			rnStore.abtDistMarksH=new BitmapText[iTotal];
		}
		
		for(int i=0;i<iTotal;i++){
			BitmapText bt=createBText(rnStore, ColorRGBA.DarkGray);
//			BitmapText bt=new BitmapText(TextI.i().loadDefaultMonoFont());
			float fH=bt.getLineHeight();
//			bt.setColor(ColorRGBA.DarkGray);
//			rnStore.attachChild(bt);
			
			if(bVertical){
				bt.setLocalTranslation(-10,rnStore.getDistMark(bVertical,i),0);
				rnStore.abtDistMarksV[i]=bt;
			}else{
				bt.setLocalTranslation(rnStore.getDistMark(bVertical,i),fH,0);
				rnStore.abtDistMarksH[i]=bt;
			}
		}
	}

	public static enum EEdge{
		Left,
		Right,
		Bottom,
		Top,
	}
	
	private Node createArrowLine(ReticleNode rnStore, float fArrowsZ, float fCenterMargin, EEdge ee) {
		Vector3f v3fFrom = new Vector3f(0,0,fArrowsZ);
		Vector3f v3fTo = new Vector3f(0,0,fArrowsZ);
		switch(ee){
			case Left:
				v3fFrom.x=-rnStore.fBorderOR;
				v3fTo.x=-fCenterMargin;
				break;
			case Right:
				v3fFrom.x=rnStore.fBorderOR;
				v3fTo.x=fCenterMargin;
				break;
			case Top:
				v3fFrom.y=rnStore.fBorderOR;
				v3fTo.y=fCenterMargin;
				break;
			case Bottom:
				v3fFrom.y=-rnStore.fBorderOR;
				v3fTo.y=-fCenterMargin;
				break;
		}
		
		ArrowGeometry ag = GeometryI.i().createArrow(ColorRGBA.DarkGray);
		ag.setScaleArrowTip(rnStore.fBorderOR/20f);
//	node.setLocalScale(fTipScale,fTipScale,1f);
		ag.setFromTo(v3fFrom,v3fTo);
		Node node = new Node();
		node.attachChild(ag);
		
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
	 * see {@link #createReticle(ReticleNode)}
	 * @param rnStore 
	 */
	public void autoRecreate(ReticleNode rnStore){
		if(rnLastConfigured!=null)rnLastConfigured.removeFromParent();
		
		app = G.i(Application.class);
		sapp = G.i(SimpleApplication.class);
		if(sapp!=null){ //TODO it cannot really independ of flycamX and sapp yet...
			FlyByCamera flycam = sapp.getFlyByCamera();
			FlyByCameraX flycamx=(flycam instanceof FlyByCameraX) ? (FlyByCameraX)flycam : null;
			
			rnLastConfigured = createReticle(rnStore);
			if(flycamx!=null)flycamx.setReticle(rnLastConfigured, sapp.getGuiNode()); //TODO shouldnt be another layer, below the gui node?
			sapp.getGuiNode().attachChild(rnLastConfigured);
			rnLastConfigured.setLocalTranslation(HWEnvironmentJmeI.i().getDisplay().getCenter(
				-((BoundingBox)rnLastConfigured.getWorldBound()).getExtent(null).length()
			));
			
			if(flycamx!=null){
				if(cxUpdate==null)cxUpdate=(new CallableXAnon() {
					@Override
					public Boolean call() {
						Vector3f v3fAppWSizeCurrent = HWEnvironmentJmeI.i().getDisplay().getAppWindowSize();
						if(v3fAppWSize.length()==0 || !v3fAppWSizeCurrent.equals(v3fAppWSize)){ //1st time is length=0
							autoRecreate(rnLastConfigured);
							v3fAppWSize.set(v3fAppWSizeCurrent);
//							QueueI.i().removeLoopFromQueue(this); //a new one will be created
							return true; 
						}
						
						ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
						Float fDist=null;
						Float fTargetDist=null;
						if(acr.size()>0)fDist=(acr.get(0).getDistance());
						
						if(sptTarget!=null && sptTarget.hasAncestor(sapp.getRootNode())){
							fTargetDist=(app.getCamera().getLocation().distance(sptTarget.getWorldTranslation()));
							HighlighterI.i().applyAt(sptTarget);
						}else{
							HighlighterI.i().removeFrom(sptTarget);
							sptTarget=null;
							fTargetDist=null;
						}
//						if(sptTarget==null){
//							ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
//							if(acr.size()>0){
//								setRangeFinderDistanceValue(acr.get(0).getDistance());
//							}else{
//								setRangeFinderDistanceValue(null);
//							}
//						}else{
//							if(sptTarget.hasAncestor(sapp.getRootNode())){
////							sapp.getRootNode().hasAncestor(ancestor)
////							if(SpatialHierarchyI.i().getParentest(sptTarget, Node.class, false)!=null)
//								setRangeFinderDistanceValue(app.getCamera().getLocation().distance(sptTarget.getWorldTranslation()));
//							}else{
//								sptTarget=null;
//								setRangeFinderDistanceValue(null);
//							}
//						}
						
						rnLastConfigured.updateRangeFinderValue(fTargetDist,fDist);
						rnLastConfigured.updateZoomMarkers(flycamx.getTotalZoomSteps());
						rnLastConfigured.updateZoomLevel(flycamx.getCurrentZoomLevelIndex());
						return true;
					}
				}).setName("ReticleUpdateZoom").enableLoopMode().setDelaySeconds(0.5f);
				QueueI.i().enqueue(cxUpdate);
			}
				
//				EffectElectricity el = new EffectElectricity();
//				el.setFollowFromTarget(node, null);
//				el.setFollowToMouse(true);
//				el.setPlay(true);
//				EffectManagerStateI.i().add(el);
			
		}
		
	}

	public ReticleNode getLastReticuleNode() {
		return rnLastConfigured;
	}

}
