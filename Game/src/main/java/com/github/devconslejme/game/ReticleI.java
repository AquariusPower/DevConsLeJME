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

import java.util.ArrayList;

import com.github.devconslejme.game.TargetI.TargetGeom;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.jme.ArrowGeometry;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.FlyByCameraX;
import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.MeshI;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.github.devconslejme.misc.jme.RotateI;
import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.github.devconslejme.misc.jme.WorldPickingI;
import com.jme3.app.Application;
import com.jme3.bounding.BoundingBox;
import com.jme3.font.BitmapFont.Align;
import com.jme3.font.BitmapText;
import com.jme3.font.Rectangle;
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

	private ReticleNode	rnLastConfigured;
	private Vector3f	v3fAppWSize = new Vector3f();
	private CallableXAnon	cxUpdate;
	private Float	fRangeDist;
//	private Spatial	sptTarget;
//	private SimpleApplication	sappOpt;
//	private Application	app;
//	private Geometry	geomTarget;
	private float	fBorderRadiusMarginPerc=0.1f;
	protected TargetGeom	tgtLast;
	private TargetGeom	tgt;
	private Node	nodeGui;
	private FlyByCameraX	flycamx;
	private float fDefaultDegAnglePerZoomStep=90f;
	private boolean bShowRangeTgtInfo;
	private boolean bCreateMarkers;
	
	public void configure(Node nodeGui, FlyByCameraX flycamx){
		this.nodeGui = nodeGui;//keep even if empty to help init global
		this.flycamx = flycamx;
	} 
	
	public static class ReticleNode extends Node{
		// user configurable
		private boolean	bBinoculars=false; //false=scope
		private int	iZoomIndexLast=-1;
		private float fDegAnglePerZoomStep=ReticleI.i().getDefaultDegAnglePerZoomStep();
		private int	fBorderIR=5;
		
		// auto setup
		private Node	nodeBorder;
		private float	fBorderOR;
		private Quaternion	quaBorderStartAt;
		private BitmapText[]	abtDistMarksV;
		private BitmapText[]	abtDistMarksH;
		private ArrayList<Geometry> ageomZoomMarkersList = new ArrayList<>();
		private BitmapText	btInfo;
		public Vector3f	v3fMarkersCenter = new Vector3f();
		public Node	nodeBorderAndZoomMarkers;
		public Geometry	zoomMarkerCurrent;
		public Node	nodeZoomMarkerCurrent;
		public Node	nodeZoomMarkerCurrentRot;
		private int	iZoomIndex;
		
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
			float fEmptyMarginBinoc=(fBorderOR*2f)*0.05f;
			float fStepBinocV = (fBorderOR*2f)/abtDistMarksV.length; //Vertical is the reference to keep
			if(bVertical){
				if(bBinoculars){
					float fTopStart=fBorderOR-fEmptyMarginBinoc;
					return fTopStart-(fStepBinocV*iMarkIndex);
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
					float fLeftStart=-((abtDistMarksH.length/2)*fStepBinocV);
					return fLeftStart+(fStepBinocV*iMarkIndex);
				}else{
					//nothing
				}
			}
			throw new DetailedException("invalid index",iMarkIndex);
		}
		
		public void updateDistanceMarkersValues(boolean bVertical, int... ai){
//			if(!bBinoculars){
				for(int i=0;i<ai.length;i++){
					if(bVertical){
						String str="";
						if(bBinoculars){
							switch(ai[i]){
								case 0:str="";break;
								case  2:str=">x< "+ai[i];break;
								default:str="--- "+ai[i];break;
							}
//							str=(ai[i]==0 ? "" : "--- "+ai[i]);
						}else{
							str=(i==0?"    ":"=== ")+ai[i];
						}
						abtDistMarksV[i].setText(str);
					}else{
						if(bBinoculars){
							abtDistMarksH[i].setText(ai[i]!=0 ? ai[i]+"\n|" : "");
						}
					}
				}
//			}
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
			if(iZoomIndex>=ageomZoomMarkersList.size())return;//not ready
			
			this.iZoomIndex = ageomZoomMarkersList.size()-1-iZoomIndex;
			Geometry geom = ageomZoomMarkersList.get(this.iZoomIndex);
			zoomMarkerCurrent.setLocalTransform(geom.getLocalTransform());
			zoomMarkerCurrent.scale(2f);
			geom.getParent().attachChild(zoomMarkerCurrent);
		}
		
		/**
		 * keep for the calcs
		 * @param iZoomIndex
		 */
		@Deprecated
		public void _updateZoomLevel(int iZoomIndex){
			if(iZoomIndexLast == iZoomIndex)return;
			/**
			 * TODO is not working for scaled binoc...
			 */
//		RotateI.i().rotateSpinning(nodeBorder, Vector3f.UNIT_X, Vector3f.UNIT_Z, 
			RotateI.i().rotateSpinning(nodeZoomMarkerCurrentRot, Vector3f.UNIT_X, Vector3f.UNIT_Z, 
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

		public void updateInfo(TargetGeom tgt,Float fRangeDist,Float fFoV) {
//			if(btRangeDist.getLocalTranslation().length()>0)System.out.println(btRangeDist.getWorldTranslation());
			btInfo.setText(""
				+(iZoomIndex+1)+"x"
				+"\n"
				+(fFoV==null?"":"FoV "+StringI.i().fmtFloat(fFoV,0)+""+Character.toString((char)176)+"")//"°")
				+"\n"
//				+"RF "+(fRangeDist==null?"...":StringI.i().fmtFloat(fRangeDist,1)+"m")
				+"RF "+DistancesI.i().fmtDist(fRangeDist)
				+"\n"
				+(tgt==null?"":"Tg "+tgt.getDistanceStr()) //last (may not be set)
			);
		}
		
	}
	
	public ReticleI setRangeFinderTarget(TargetGeom tgt){
		this.tgt = tgt;
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
		
//		float fBinocXScale=1f; //TODO 1.5f; updateZoomLevel() may not be working well...
		float fBinocXScale=1.5f; //TODO 1.5f; updateZoomLevel() may not be working well...
		/////////////////////////////// reticle border
		rnStore.nodeBorderAndZoomMarkers = new Node();
		rnStore.nodeBorder=new Node("Border");
		rnStore.fBorderOR=(iDMin/2f)-rnStore.getBorderIR();
		rnStore.fBorderOR*=(1f-fBorderRadiusMarginPerc);
		Geometry border = GeometryI.i().create(new Torus(40, 5, rnStore.getBorderIR(), rnStore.fBorderOR),ColorRGBA.Gray);
		rnStore.nodeBorder.attachChild(border);
		rnStore.quaBorderStartAt = rnStore.nodeBorder.getLocalRotation().clone();
//		if(rnStore.bBinoculars)rnStore.nodeBorder.scale(fBinocXScale,1,1);
		if(rnStore.bBinoculars)rnStore.nodeBorderAndZoomMarkers.scale(fBinocXScale,1,1);
		rnStore.nodeBorderAndZoomMarkers.attachChild(rnStore.nodeBorder);
		rnStore.attachChild(rnStore.nodeBorderAndZoomMarkers);
		
		//////////////////////////////// current zoom level
		rnStore.ageomZoomMarkersList.clear(); //reset the zoom level markers
		rnStore.zoomMarkerCurrent = rnStore.createZoomLevelMarker(ColorI.i().colorChangeCopy(ColorRGBA.Red,-0.5f));
		rnStore.zoomMarkerCurrent.scale(2f);
		rnStore.zoomMarkerCurrent.move(0,0,rnStore.getBorderIR());
//		rnStore.nodeBorder.attachChild(zoomMarkerCurrent);
//		rnStore.nodeBorderAndZoomMarkers.attachChild(rnStore.zoomMarkerCurrent);
		
		rnStore.nodeZoomMarkerCurrent=new Node();
		if(rnStore.bBinoculars)rnStore.nodeZoomMarkerCurrent.scale(fBinocXScale,1,1);
		rnStore.attachChild(rnStore.nodeZoomMarkerCurrent);
		
		rnStore.nodeZoomMarkerCurrentRot=new Node();
		rnStore.nodeZoomMarkerCurrentRot.attachChild(rnStore.zoomMarkerCurrent);
		rnStore.nodeZoomMarkerCurrent.attachChild(rnStore.nodeZoomMarkerCurrentRot);
		
		///////////////////////////// auto target/dist range info
		BitmapText bt = StringTextJmeI.i().createBitmapTextMono("Tgt:+99999.9m\nDummy",ColorRGBA.Green); //TODO maximum displayable distance, create a formatter and get from there considering the world limits
		bt.setBox(new Rectangle(0, 0, bt.getLineWidth(), bt.getHeight()));
		bt.setAlignment(Align.Right);
//		bt = new BitmapText(TextI.i().loadDefaultMonoFont());
//		bt.setColor(ColorRGBA.Green);
		int iHalfMin=iDMin/2;
//		bt.setLocalTranslation(iDMin/2f-50f,iDMin/2f-50f,0);
		bt.setLocalTranslation(
			iHalfMin-bt.getLineWidth(),
			iHalfMin-bt.getHeight(),
			0);
//		bt.setLocalTranslation(300f,300f,300);
//		rnStore.attachChild(bt);
		rnStore.btInfo=bt;
		if(isShowRangeTgtInfo())rnStore.attachChild(bt);
		
		//////////////////////////////// blocker
		float fProportion = iDMin/(float)iDMax;
		float fDiagonal = FastMath.sqrt(FastMath.pow(iDMin,2)+FastMath.pow(iDMax,2));
		float fBlockerIR=((fDiagonal/2f)-rnStore.fBorderOR)/2f;
//		float fBlockerIR=((iDMax/2)-(iDMin/2))/2;if(fBlockerIR<1f)fBlockerIR=1f;
		float fBlockerOR=rnStore.fBorderOR+fBlockerIR; //TODO why had to -100?
		fBlockerIR*=1.25f; //after outer radius calc TODO why 1.25f worked? torus creation inner radius innacuracy? or my fault?
		Geometry blocker = GeometryI.i().create(new Torus(25, 5, fBlockerIR, fBlockerOR),
			new ColorRGBA(0.0125f,0.025f,0.05f,0.98f)); //glass feeling, TODO use a texture?
		blocker.setQueueBucket(Bucket.Inherit); //restore gui one
		blocker.move(new Vector3f(0,0,-fBlockerIR));
		if(rnStore.bBinoculars)blocker.scale(fBinocXScale,1,1);
		rnStore.attachChild(blocker);
		
		///////////////////////////////// text markers for distance info
		if(isCreateMarkers()) {
	//		float fArrowsZ = -10f;
			float fArrowsZ = -fBlockerIR*2f;
			if(rnStore.isBinoculars()){ //mortar recalculations markers
				rnStore.v3fMarkersCenter = new Vector3f(0,-(rnStore.fBorderOR/5)*2,0);
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
				rnStore.v3fMarkersCenter = new Vector3f();
				createTextMarkers(rnStore,true,5);
				rnStore.updateDistanceMarkersValues(true,100,200,300,400,500); //vertical
				
				// arrows
				float fCenterMargin = 20f;
				rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Left));
				rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Right));
				rnStore.attachChild(createArrowLine(rnStore, fArrowsZ, fCenterMargin, EEdge.Bottom));
			}
		}
		
		return rnStore;
	}
	
//	private BitmapText createBText(ReticleNode rnStore, ColorRGBA color) {
//		BitmapText bt = new BitmapText(TextStringI.i().loadDefaultMonoFont());
//		bt.getSize();
//		bt.setText("(not set)");
//		bt.setColor(color);
//		rnStore.attachChild(bt);
//		return bt;
//	}
	private void createTextMarkers(ReticleNode rnStore, boolean bVertical, int iTotal) {
		if(bVertical){
			rnStore.abtDistMarksV=new BitmapText[iTotal];
		}else{
			rnStore.abtDistMarksH=new BitmapText[iTotal];
		}
		
		for(int i=0;i<iTotal;i++){
			BitmapText bt=StringTextJmeI.i().createBitmapTextMono("",ColorRGBA.DarkGray);
			rnStore.attachChild(bt);
//			BitmapText bt=new BitmapText(TextI.i().loadDefaultMonoFont());
//			bt.setColor(ColorRGBA.DarkGray);
//			rnStore.attachChild(bt);
			
			if(bVertical){
				bt.setLocalTranslation(-10,rnStore.getDistMark(bVertical,i),0);
				rnStore.abtDistMarksV[i]=bt;
			}else{
				float fH=bt.getLineHeight()*1.5f;
				bt.setLocalTranslation(rnStore.getDistMark(bVertical,i),fH+rnStore.v3fMarkersCenter.y,0);
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
		float fLength=rnStore.fBorderOR*2f;
		switch(ee){
			case Left:
				v3fFrom.x=-fLength;
				v3fTo.x=-fCenterMargin;
				break;
			case Right:
				v3fFrom.x=fLength;
				v3fTo.x=fCenterMargin;
				break;
			case Top:
				v3fFrom.y=fLength;
				v3fTo.y=fCenterMargin;
				break;
			case Bottom:
				v3fFrom.y=-fLength;
				v3fTo.y=-fCenterMargin;
				break;
		}
		
		ArrowGeometry ag = GeometryI.i().createArrow(ColorRGBA.DarkGray);
		ag.setScaleArrowTip(rnStore.fBorderOR/20f);
//	node.setLocalScale(fTipScale,fTipScale,1f);
		ag.setFromTo(v3fFrom,v3fTo);
		ag.move(rnStore.v3fMarkersCenter);
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
		
//		app = G.i(Application.class);
//		sappOpt = G.i(SimpleApplication.class);
//		if(sappOpt!=null){ //TODO it cannot really independ of flycamX and sapp yet...
//			FlyByCamera flycam = sappOpt.getFlyByCamera();
//			FlyByCameraX flycamx=(flycam instanceof FlyByCameraX) ? (FlyByCameraX)flycam : null;
			
			rnLastConfigured = createReticle(rnStore);
			if(flycamx!=null)flycamx.setReticle(rnLastConfigured, nodeGui); //TODO shouldnt be another layer, below the gui node?
			nodeGui.attachChild(rnLastConfigured);
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
						}else{
							ArrayList<RayCastResultX> acr = WorldPickingI.i().raycastPiercingDisplFromCenter(null, rnLastConfigured.v3fMarkersCenter);
							Float fDist=null;
//							Float fTargetDist=null;
							if(acr.size()>0)fDist=(acr.get(0).getDistance());
							
							TargetI.i().setRayCastFromXY(rnLastConfigured.v3fMarkersCenter);
							TargetGeom tgt = TargetI.i().getLastSingleTarget();
//							if(tgt!=null)fTargetDist=tgt.getDistance();
							
//							public ReticleI setRangeFinderTarget(Target tgt){
//								this.tgt = tgt;
//								return this;
//							}
//							public ReticleI setRangeFinderDistanceValue(Float fRangeDist){
//								this.fRangeDist = fRangeDist;
//								return this;
//							}

//							tgtLast = TargetI.i().getLastTarget();
//							if(tgtLast!=null && tgtLast.getRootSpatial().hasAncestor(sappOpt.getRootNode())){
//								fTargetDist=(app.getCamera().getLocation().distance(
//									tgtLast.getRootSpatial().getWorldTranslation()));
//								HighlighterI.i().applyAt(tgtLast.getGeometryHit());
//							}else{
//								resetLastTarget();
//								fTargetDist=null;
//							}
							
							rnLastConfigured.updateInfo(tgt,fDist,flycamx.getFOV());
							rnLastConfigured.updateZoomLevel(flycamx.getCurrentZoomLevelIndex());
							rnLastConfigured.updateZoomMarkers(flycamx.getTotalZoomSteps());
						}
						
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
			
//		}
		
	}

//	protected void resetTarget() {
//		if(geomTarget!=null)HighlighterI.i().removeFrom(geomTarget);
//		sptTarget=null;
//		geomTarget=null;
//	}
	public ReticleNode getLastReticuleNode() {
		return rnLastConfigured;
	}
	public float getBorderRadiusMarginPerc() {
		return fBorderRadiusMarginPerc;
	}
	public ReticleI setBorderRadiusMarginPerc(float fBorderRadiusMarginPerc) {
		this.fBorderRadiusMarginPerc = fBorderRadiusMarginPerc;
		return this; 
	}
	public float getDefaultDegAnglePerZoomStep() {
		return fDefaultDegAnglePerZoomStep;
	}
	public ReticleI setDefaultDegAnglePerZoomStep(float fDefaultDegAnglePerZoomStep) {
		this.fDefaultDegAnglePerZoomStep = fDefaultDegAnglePerZoomStep;
		return this; 
	}
	public boolean isShowRangeTgtInfo() {
		return bShowRangeTgtInfo;
	}
	public ReticleI setShowRangeTgtInfo(boolean bShowRangeTgtInfo) {
		this.bShowRangeTgtInfo = bShowRangeTgtInfo;
		return this; 
	}
	public boolean isCreateMarkers() {
		return bCreateMarkers;
	}
	public ReticleI setCreateMarkers(boolean bCreateMarkers) {
		this.bCreateMarkers = bCreateMarkers;
		return this; 
	}

}
