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

import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.ColorI.ColorRGBAx;
import com.github.devconslejme.misc.jme.MeshI.Cone;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bounding.BoundingVolume;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Sphere;
import com.jme3.scene.shape.Torus;

/**
 * improve with fancyness (shaders, lighting, shadows, sfx and voices hehe)
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class OriginDevice extends Node{
	private NodeAxis	nodeTorX;
	private NodeAxis	nodeTorY;
	private NodeAxis	nodeTorZ;
	private EffectElectricity	ef;
	private TimedDelay	tdEffectRetarget;
	private float	fRetargetDefaultDelay=3;
	private ArrayList<NodeAxis> anodeMainShapes=new ArrayList<NodeAxis>();
	private ArrayList<NodeAxis> anodeElectricShapesList=new ArrayList<NodeAxis>();
//	private Node	nodeBase = new Node(OriginDevice.class.getSimpleName());
	private Integer	iMaxHoldMilisBkp = null;
//	private Integer	iThicknessBkp = null;
	private float fDisplacement;
	private int	iCS;
	private int	iRS;
	private Vector3f	v3fBaseSpeed=new Vector3f(1,1,1).mult(0.0025f);
	private NodeAxis	nodeSelfElectrocute;
	private float	fIR=0.1f;
	private float	fRotTorOpac=0.15f;
	private Vector3f v3fSpeed=new Vector3f();
	private NodeAxis	nodeElectricA;
	private NodeAxis	nodeElectricB;
//	private NodeAxis	nodeElectricSrc;
	private boolean	bUnstable;
	private ERotMode erm = ERotMode.Disaligned;
	private Spatial	sptElectricSrc;
	private NodeAxis	nodeElectricSrc;
	private boolean	bSameAxis;
	private ESourceMode esm = ESourceMode.Follow;
	private float	fInitialDistToSrc;
	private float	fMoveBaseSpeed=0.01f;
	private boolean	bUpdateElectricalEffectForNewSourceOnce;
	
	public static class NodeAxis extends Node{
		public NodeAxis(String str) {
			super(str);
		}

		EAxis ea;
	}
	
	public OriginDevice(){
		setName(OriginDevice.class.getSimpleName());
		
		fDisplacement=5;
		iCS=50;
		iRS=15;
//		fBaseSpeed=0.0025f;
//		v3fSpeed.set(fBaseSpeed,fBaseSpeed,fBaseSpeed);
		v3fSpeed.set(v3fBaseSpeed);
		
		init();
	}
	
	public void update(float fTPF){
		updateTorusRotations();
		updateElectricalEffects();
		updateAxisMainShapes();
		updateSourceMode();
	}
	
	/**
	 * TODO deplet energy sources
	 */
	protected void updateSourceMode() {
		if(sptElectricSrc==null)return;
		if(nodeElectricSrc!=null)return; //dont mess with self
		
		Vector3f v3fSrcWPos = sptElectricSrc.getWorldTranslation();
		
		Vector3f v3fDist=v3fSrcWPos.subtract(getWorldTranslation());
		float fDist = v3fDist.length();
		
		float fPseudoDiameter = (float) Math.cbrt(getWorldBound().getVolume());  
		float fTractionForceBasedOnDiameterMult = 3f;
		float fMaxTractionDist=fPseudoDiameter*fTractionForceBasedOnDiameterMult;
		if(fDist > fMaxTractionDist){
			sptElectricSrc=null;
			return; //disconnected
		}
		
		float fMoveSpeed=fMoveBaseSpeed;
		float fFollowDist = fMaxTractionDist/2f;
		float fFollowBoostMult=10f;
		if(esm==ESourceMode.Follow || esm==ESourceMode.MoveTo){
			if(esm==ESourceMode.Follow){
				fMoveSpeed*=fDist/fFollowDist;
				fMoveSpeed*=fFollowBoostMult;
			}else
			if(esm==ESourceMode.MoveTo){
				fMoveSpeed*=fDist/fMaxTractionDist;
			}
		}else{
			fMoveSpeed*=fMaxTractionDist/fDist;
			
			if(fDist < fPseudoDiameter/2f){
				fMoveSpeed=fMoveBaseSpeed*fDist;
			}
		}
		
		if(fMoveSpeed<fMoveBaseSpeed)fMoveSpeed=fMoveBaseSpeed;
		
		Vector3f v3fDir = v3fDist.normalize();
		Vector3f v3fSpeed = v3fDir.mult(fMoveSpeed);
		float fSafeMinDist=0.01f;
		switch(esm){
			case JustStay:
				//TODO do nothing? could deplet the energy source at least
				break;
				
			case Attract:
				if(fDist>fSafeMinDist)sptElectricSrc.move(v3fSpeed.negate());
				break;
			case Repel:
				sptElectricSrc.move(v3fSpeed);
				break;
				
			case Follow:
//				if(fInitialDistToSrc < fDist){
				if(fDist > fFollowDist*1.1f){
					move(v3fSpeed);
					lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
				}else
				if(fDist < fFollowDist*0.9f){
					move(v3fSpeed.negate());
				}
				break;
				
			case MoveAway:
				move(v3fSpeed.negate());
				lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
				getLocalRotation().negate(); //TODO this looks bad... will be upside down?
				break;
			case MoveTo:
				if(fDist>fSafeMinDist)move(v3fSpeed);
				lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
				break;
		}
	}

	protected void init(){
		// origin
		attachChild(DebugVisualsI.i()
			.createArrow(ColorRGBA.Red).setFromTo(Vector3f.ZERO, Vector3f.UNIT_X));
		attachChild(DebugVisualsI.i()
			.createArrow(ColorRGBA.Green).setFromTo(Vector3f.ZERO, Vector3f.UNIT_Y));
		attachChild(DebugVisualsI.i()
			.createArrow(ColorRGBA.Blue).setFromTo(Vector3f.ZERO, Vector3f.UNIT_Z));
		
		// some basic shapes
		nodeTorX=createAxis(Vector3f.UNIT_X, MeshI.i().box(0.5f));
		nodeTorY=createAxis(Vector3f.UNIT_Y, new Sphere(10,10,0.5f));
//		torZ=createAxisThing(Vector3f.UNIT_Z, new Cylinder(5,10,0.5f,0.001f,1f,true,false));
		nodeTorZ=createAxis(Vector3f.UNIT_Z, new Cone());
		
		// electricity
		tdEffectRetarget = new TimedDelay(fRetargetDefaultDelay,"").setActive(true);
		ef=new EffectElectricity();
//		ef.setColor(ColorRGBA.Cyan); //opaque
		float f=0.5f;ef.setColor(new ColorRGBA(f,f,1f,1));
		ef.setAmplitudePerc(0.05f);
		ef.getElectricalPath().setMinMaxPerc(0.05f, 0.1f);
		ef.setFromTo(new Vector3f(),new Vector3f()).setPlay(true); //just to allow started
		EffectManagerStateI.i().add(ef);
	}
	
	protected void updateAxisMainShapes() {
		for(NodeAxis node:anodeMainShapes){
			Vector3f v3f = getRotSpeedCopy();
			if(node==nodeElectricA || node==nodeElectricB){
				if(nodeElectricA==nodeElectricB){
					v3f = getRotSpeedCopy(ERotMode.Chaotic);
				}
				
				v3f.multLocal(10f);
			}
			
			node.rotate(v3f.x,v3f.y,v3f.z);
		}
	}

	protected void updateElectricalEffects() {
		ef.setPlay(bUnstable);
		if(!bUnstable)return;
		
		ef.setNodeParent(this.getParent());
		
		if(tdEffectRetarget.isReady(true) || bUpdateElectricalEffectForNewSourceOnce){
			bUpdateElectricalEffectForNewSourceOnce=false;
			nodeSelfElectrocute=null;
			if(iMaxHoldMilisBkp!=null){
				ef.getElectricalPath().setMaxHoldMilis(iMaxHoldMilisBkp);
				ef.setOverrideThickness(null);
				iMaxHoldMilisBkp=null;
			}
			
			tdEffectRetarget.resetAndChangeDelayTo(fRetargetDefaultDelay*FastMath.nextRandomFloat()).setActive(true);
			
			int iA=-1;
			nodeElectricA = null;
			Vector3f v3fWorldA = null;
			if(sptElectricSrc!=null){
				nodeElectricA=nodeElectricSrc;
				iA=anodeElectricShapesList.indexOf(nodeElectricA); //external spatial will be -1
//				iA=anodeElectricShapesList.indexOf(sptElectricSrc); //external spatial will be -1
//				if(iA>-1)nodeElectricA=(NodeAxis) sptElectricSrc;
				v3fWorldA = sptElectricSrc.getWorldTranslation();
			}else{
				iA=FastMath.nextRandomInt(0, anodeElectricShapesList.size()-1);
				nodeElectricA = anodeElectricShapesList.get(iA);
				v3fWorldA = nodeElectricA.getWorldTranslation();
			}
			
//			bSameAxis = (nodeElectricA!=null && nodeElectricB!=null && nodeElectricA.ea==nodeElectricB.ea);
			
			int iB=FastMath.nextRandomInt(0, anodeElectricShapesList.size()-1);
			nodeElectricB = anodeElectricShapesList.get(iB);
			if(iA!=iB){
				ef.setFromTo(
					v3fWorldA,
					nodeElectricB.getWorldTranslation()
				);
			}else{
				//TODO create new effect to self electr
				nodeSelfElectrocute=nodeElectricA;
			}
		}
		
		if(nodeSelfElectrocute!=null){
			if(iMaxHoldMilisBkp==null){
				iMaxHoldMilisBkp = ef.getElectricalPath().getMaxHoldMilis();
				ef.getElectricalPath().setMaxHoldMilis(100); //frenetic
				ef.setOverrideThickness(2);
			}
			
			BoundingVolume bv = nodeSelfElectrocute.getWorldBound();
			float fScale=1f;
			if (bv instanceof BoundingBox)fScale = ((BoundingBox)bv).getExtent(null).length();
			if (bv instanceof BoundingSphere)fScale = ((BoundingSphere)bv).getRadius();
			ef.setFromTo(
				MiscJmeI.i().getRandomSpot(bv.getCenter(), fScale),
				MiscJmeI.i().getRandomSpot(bv.getCenter(), fScale)
			);
		}
		
		bSameAxis = (nodeElectricA!=null && nodeElectricB!=null && nodeElectricA.ea==nodeElectricB.ea);
	}
	
	private NodeAxis electricNodeFor(Spatial spt) {
		ArrayList<Node> anode = SpatialHierarchyI.i().getAllParents(spt, false);
		for(Node node:anode){
			if(anodeElectricShapesList.contains(node)){
				return (NodeAxis)node; //must cast as nodes may not be NodeAxis
			}
		}
		return null;
	}

	public static enum EAxis{
		X,
		Y,
		Z
	}
	
	protected void updateTorusRotations() {
//		nodeTorX.rotate(0,getRotSpeedCopy().x,0);
//		nodeTorY.rotate(0,getRotSpeedCopy().y,0);
//		nodeTorZ.rotate(0,getRotSpeedCopy().z,0);
		Vector3f v3fSpeed = getRotSpeedCopy();
		rotateTor(nodeTorX,v3fSpeed);//,EAxis.X);
		rotateTor(nodeTorY,v3fSpeed);//,EAxis.Y);
		rotateTor(nodeTorZ,v3fSpeed);//,EAxis.Z);
	}
	
	protected void rotateTor(NodeAxis nodeTor,Vector3f v3fSpeed){//, EAxis ea) {
		boolean bTorChildA = SpatialHierarchyI.i().isChildRecursive(nodeTor,nodeElectricA);
		boolean bTorChildB = SpatialHierarchyI.i().isChildRecursive(nodeTor,nodeElectricB);
		
//		if(bTorChildB && nodeTor.ea==EAxis.X){
//			System.out.println("rbeakpoint");
//		}
		
		float fBoost=10f;
		float fMult=1f;
		if(bUnstable){
			if(nodeSelfElectrocute!=null && nodeTor.ea==nodeSelfElectrocute.ea){
				if(anodeMainShapes.contains(nodeSelfElectrocute)){
					/**
					 * main shape electrocuting self, will boost related torus spin
					 */
					fMult=(fBoost*2f);
				}else{
					if(bTorChildA || bTorChildB){
						/**
						 * if it is an intersection electrifying self, the torus will stop moving (just to differ from non equal axis behavior)
						 */
						fMult=0; //hold
					}
				}
			}else{
				if(anodeMainShapes.contains(nodeElectricA) && anodeMainShapes.contains(nodeElectricB)){
					if(nodeTor.ea==nodeElectricA.ea || nodeTor.ea==nodeElectricB.ea){
						/**
						 * both are main shapes (always mismatching axis), spin fast reversed, only related torus
						 */
						fMult=(-fBoost);
					}
				}else{
					if(bTorChildA || bTorChildB){
						/**
						 * any is a torus intersection, spin fast
						 */
						fMult=(fBoost);
						
						if(bTorChildA && bTorChildB){
							/**
							 * both are intersections of the same torus? spin reversed
							 */
							fMult*=-1f;
						}
						
						if(!bSameAxis){
							/**
							 * non matching axis? chaotic mode
							 */
							v3fSpeed=getRotSpeedCopy(ERotMode.Chaotic);
						}
					}
				}
			}
		}
		
		float f=0;
		switch(nodeTor.ea){
			case X:
				f=v3fSpeed.x;
				break;
			case Y:
				f=v3fSpeed.y;
				break;
			case Z:
				f=v3fSpeed.z;
				break;
		}
		
		nodeTor.rotate(0,f*fMult,0);
	}

//	protected void rotate(NodeAxis node, Vector3f v3fUp, boolean bZOnly){
	protected void rotate(NodeAxis node, boolean bZOnly){
		float fRot=FastMath.DEG_TO_RAD*90;
//		if(!bZOnly && v3fUp.x==1)node.rotate(0, fRot, 0);
//		if(!bZOnly && v3fUp.y==1)node.rotate(0, fRot, 0);
//		if(v3fUp.z==1)node.rotate(-fRot, fRot, 0);
		if(!bZOnly && node.ea==EAxis.X)node.rotate(0, fRot, 0);
		if(!bZOnly && node.ea==EAxis.Y)node.rotate(0, fRot, 0);
		if(node.ea==EAxis.Z)node.rotate(-fRot, fRot, 0);
	}
	
	protected NodeAxis createAxis(Vector3f v3fUp, Mesh mesh) {
		ColorRGBA color = new ColorRGBA(v3fUp.x,v3fUp.y,v3fUp.z,1f);
		
		// small shape
		NodeAxis nodeThing = createAxisShape(mesh, color, v3fUp.mult(fDisplacement), 0.5f, v3fUp, true, null);
		anodeElectricShapesList.add(nodeThing);
		anodeMainShapes.add(nodeThing);
		
		float fDisplacementTorus = fDisplacement+1;
		
		// static rotation track
		NodeAxis nodeTrack=createAxisShape(new Torus(iCS,iRS,0.01f,fDisplacementTorus), 
			color, new Vector3f(0,0,0), 0.15f, v3fUp);
		MiscJmeI.i().addToName(nodeTrack, "Track", false);
		nodeTrack.lookAt(v3fUp, v3fUp);
		
		// rotating torus
		NodeAxis nodeRotating=createAxisShape(new Torus(iCS,iRS,fIR,fDisplacementTorus), 
			color, new Vector3f(0,0,0), fRotTorOpac, v3fUp);
		//TODO this may break if the track contents is changed...
		NodeAxis nodeCore=createAxisShape(new Torus(iCS,iRS,fIR*0.35f,fDisplacementTorus), 
			color, new Vector3f(0,0,0), fRotTorOpac+0.5f, v3fUp);
		rotate(nodeCore,true);
		nodeRotating.attachChild(nodeCore);
		
		createTorusIntersections(nodeRotating,color,fDisplacementTorus,v3fUp);
		
		return nodeRotating;
	}
	
	protected void createTorusIntersections(Node nodeTor, ColorRGBA color, float fDisplacementTorus, Vector3f v3fUp) {
		float fIRa=fIR*1.5f;
		float fAlpha=fRotTorOpac+0.25f;
		
		NodeAxis nodePosit = createAxisShape(new Cone(fIRa*2f),
				color, new Vector3f( fDisplacementTorus,0,0), fAlpha, v3fUp, false, new Vector3f(1,1,2));
		MiscJmeI.i().addToName(nodePosit, "Intersection", false, true);
		nodePosit.lookAt(v3fUp, v3fUp);
		rotate(nodePosit,false);
		nodeTor.attachChild(nodePosit);
		anodeElectricShapesList.add(nodePosit);
		
		NodeAxis nodeNegat=createAxisShape(new Sphere(10,10,fIRa),
			color, new Vector3f(-fDisplacementTorus,0,0), fAlpha, v3fUp);
		MiscJmeI.i().addToName(nodeNegat, "Intersection", false, true);
		nodeTor.attachChild(nodeNegat);
		anodeElectricShapesList.add(nodeNegat);
	}

	protected NodeAxis createAxisShape(Mesh mesh, ColorRGBA color, Vector3f v3f, float fAlpha, Vector3f v3fUp) {
		return createAxisShape( mesh,  color,  v3f,  fAlpha,  v3fUp, false, null);
	}
	protected NodeAxis createAxisShape(Mesh mesh, ColorRGBA color, Vector3f v3f, float fAlpha, Vector3f v3fUp, boolean bAddWireFrame, Vector3f v3fScale) {
		if(v3fScale==null)v3fScale=new Vector3f(1,1,1);
		
		NodeAxis node = new NodeAxis("Node");
		
//		Geometry geom = new Geometry(mesh.getClass().getSimpleName(),mesh);
		Geometry geom = GeometryI.i().create(mesh,ColorI.i().colorChangeCopy(color,0,fAlpha),true);
		
		// name
		MiscJmeI.i().addToName(geom, OriginDevice.class.getSimpleName(), true);
		if(v3fUp.x==1){MiscJmeI.i().addToName(geom, "X", false);node.ea=EAxis.X;}
		if(v3fUp.y==1){MiscJmeI.i().addToName(geom, "Y", false);node.ea=EAxis.Y;}
		if(v3fUp.z==1){MiscJmeI.i().addToName(geom, "Z", false);node.ea=EAxis.Z;}
		
		MiscJmeI.i().addToName(node, geom.getName(), false);
		
//		// color/transparency
//		color=color.clone();
//		color.a=fOpacity;
//		geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
//		if(fOpacity!=1f)geom.setQueueBucket(Bucket.Transparent);
		
		geom.setLocalScale(v3fScale);
		
		Geometry geomWireFrame=null;
		if(bAddWireFrame){
			geomWireFrame = geom.clone();
			geomWireFrame.setMaterial(geomWireFrame.getMaterial().clone());
			geomWireFrame.getMaterial().getAdditionalRenderState().setWireframe(true);
			geomWireFrame.setLocalScale(v3fScale);
		}
		
		// hierarchy/pos
		node.attachChild(geom);
		if(geomWireFrame!=null)node.attachChild(geomWireFrame);
		
		node.setLocalTranslation(v3f);
		node.rotateUpTo(v3fUp);
//		node.lookAt(v3fUp, v3fUp);
		
		attachChild(node);
		
		return node;
	}
	
	public static enum ERotMode{
		Chaotic,
		Disaligned,
		Aligned,
		;
	}
	
	public Vector3f getRotSpeedCopy() {
		return getRotSpeedCopy(null);
	}
	public Vector3f getRotSpeedCopy(ERotMode ermOverride) {
		switch(ermOverride!=null ? ermOverride : this.erm){
			case Chaotic:
				return v3fSpeed.mult(new Vector3f(
						2f*FastMath.nextRandomFloat()-1f,
						2f*FastMath.nextRandomFloat()-1f,
						2f*FastMath.nextRandomFloat()-1f
					));
			case Disaligned:
				return v3fSpeed.mult(new Vector3f(
						FastMath.nextRandomFloat(),
						FastMath.nextRandomFloat()*1.5f,
						FastMath.nextRandomFloat()*2.0f
					));
			default: //just to let it compile...
			case Aligned:
				return v3fSpeed.clone();
		}
	}

	public OriginDevice setRotSpeed(Vector3f v3f) {
//		this.v3fSpeed.set(v3f.x*fBaseSpeed,v3f.y*fBaseSpeed,v3f.z*fBaseSpeed);
		this.v3fSpeed.set(v3fBaseSpeed.mult(v3f));
		return this; //for beans setter
	}

	public ERotMode getRotMode() {
		return erm;
	}

	public OriginDevice setRotMode(ERotMode erm) {
		this.erm = erm;
		return this; //for beans setter
	}

	public boolean isUnstable() {
		return bUnstable;
	}

	public OriginDevice setUnstable(boolean bUnstable) {
		this.bUnstable = bUnstable;
		return this; //for beans setter
	}

	public Spatial getNodeElectricSrc() {
		return sptElectricSrc;
	}
	
	public static enum ESourceMode{
		Attract,
		Repel,
		MoveTo,
		MoveAway,
		Follow,
		JustStay,
		;
	}
	
	public OriginDevice setElectricitySource(Spatial sptElectricSrc) {
		this.sptElectricSrc = sptElectricSrc;
		if(sptElectricSrc!=null){
			fInitialDistToSrc=sptElectricSrc.getWorldTranslation().subtract(getWorldTranslation()).length();
			nodeElectricSrc = electricNodeFor(sptElectricSrc);
//			tdEffectRetarget.reactivate();
			bUpdateElectricalEffectForNewSourceOnce=true;
		}
		return this; //for beans setter
	}

	public ESourceMode getSourceMode() {
		return esm;
	}

	public OriginDevice setSourceMode(ESourceMode esmSourceMode) {
		this.esm = esmSourceMode;
		return this; //for beans setter
	}

	public float getMoveBaseSpeed() {
		return fMoveBaseSpeed;
	}

	public OriginDevice setMoveBaseSpeed(float fMoveBaseSpeed) {
		this.fMoveBaseSpeed = fMoveBaseSpeed;
		return this; //for beans setter
	}
}
