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

import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.ToDo;
import com.github.devconslejme.misc.CalcI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.MeshI.Cone;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bounding.BoundingVolume;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Matrix3f;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Sphere;
import com.jme3.scene.shape.Torus;

/**
 * Improve with fancyness (shaders, lighting, shadows, sfx and voices hehe).
 * 
 * OrDe starts enabled in auto feed mode.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class OriginDevice extends Node{
	public class TargetToken {
	//	long lEnergyWattsPerMilis;
		private EnergyJme elecj;
		private OriginDevice ordeApplier;
	
		public TargetToken(OriginDevice ordeApplier, long lEnergyWattsPerMilis) {
			this.ordeApplier=ordeApplier;
			this.elecj = new EnergyJme(ordeApplier.energy);
			this.elecj.addEnergy(lEnergyWattsPerMilis);
//			this.lEnergyWattsPerMilis=lEnergyWattsPerMilis;
			assert(elecj.getEnergyStored()>=0);
		}
		
	}
	
	/**
	 * each 0.01^3 wold volume = 1 watt/miliseconds
	 * so 1^3 = 100*100*100 w/ms
	 */
	private EnergyJme energy = new EnergyJme(1000000,100000,10000,0);
	
	private NodeAxis	nodeTorX;
	private NodeAxis	nodeTorY;
	private NodeAxis	nodeTorZ;
	private EffectElectricity	efElec;
	private TimedDelay	tdEffectRetarget;
	private float	fRetargetDefaultDelay=3;
	private ArrayList<NodeAxis> anodeMainShapes=new ArrayList<NodeAxis>();
	private ArrayList<NodeAxis> anodeElectricShapesList=new ArrayList<NodeAxis>();
//	private Node	nodeBase = new Node(OriginDevice.class.getSimpleName());
	private Integer	iMaxHoldMilisBkp = null;
//	private Integer	iThicknessBkp = null;
	private float fRadius;
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
//	private boolean	bUnstable;
	private ERotMode erm = ERotMode.Disaligned;
	private Spatial	sptTarget;
	private NodeAxis	nodeaxisUserChosen;
	private boolean	bSameAxis;
//	private ETargetMode esm = ETargetMode.JustStay;
	private ETargetMode esm = ETargetMode.MoveOver;
	private float	fInitialDistToSrc;
	private float	fMoveBaseSpeed=0.01f;
	private boolean	bUpdateElectricalEffectForNewSourceOnce;
	private boolean	bDestroySpatials;
	private boolean	bAutoTargetNearestSpatials;
	private boolean	bRequireTargetsToken=true;
	private float	fTPF;
	private NodeAxis	nodeEnergyCore;
//	private float	fPseudoDiameter;
	private float	fTractionForceBasedOnDiameterMult = 3f;
	private float	fMaxTractionDist;
	private float	fSafeMinDist=0.01f;
	private float	fEnergyCoreRadius;
//	private float	fPseudoEnergyCoreRadius;
//	private float	fPseudoDeviceRadius;
//	private boolean	bTmpAttract;
	private EffectArrow	efHook;
//	private Geometry	geomEnergyCore;
	private boolean	bForceAbsorptionOnce;

	private float	fPetMaxDist;
	
//	/**
//	 * each 0.01^3 wold volume = 1 watt/miliseconds
//	 */
//	private long lVolumeToWattPerMilis = 1000000;
	
	public static class NodeAxis extends Node{
		public NodeAxis(String str) {
			super(str);
		}

		EAxis ea;
		Geometry	geom;
		Geometry	geomWireFrame;
		Node nodeGeometries;
		public EffectElectricity	ef;
	}
	
	public OriginDevice(){
		setName(OriginDevice.class.getSimpleName());
		
		fRadius=5;
		iCS=50;
		iRS=15;
//		fBaseSpeed=0.0025f;
//		v3fSpeed.set(fBaseSpeed,fBaseSpeed,fBaseSpeed);
		v3fSpeed.set(v3fBaseSpeed);
		
		init();
	}
	
	public void update(float fTPF){
		this.fTPF=fTPF;
		
		// self stats
//		float fPseudoDiameter = (float) Math.cbrt(getWorldBound().getVolume());
//		fPseudoDeviceRadius=fDiameter/2f;
//		bUnstable=(energy.isOvercharged());
//		fTractionForceBasedOnDiameterMult = 3f;
		fMaxTractionDist=(fRadius*2f)*fTractionForceBasedOnDiameterMult;
		
		// moves back to world origin
		if(sptTarget==null){
			if(getWorldTranslation().length()>0){
				move(getWorldTranslation()
					.normalize()
					.mult(fMoveBaseSpeed*getWorldTranslation().length())
					.negate()
				);
			}
		}
		
		updateCheckGrow(); //evolve
		updateEnergyCore();
		updateAutoTarget();
		updateTorusRotations();
		updateElectricalEffects();
		updateAxisMainShapes();
		updateEnergySourceInteraction(esm);
	}
	
	private void updateEnergyCore() {
		float fECScale=0.01f;
		if(energy.isHasEnergy()){
//			fECScale=((float)Math.cbrt(lEnergyWattsPerMilis/1000L));
			double dV=energy.energyToVolume();
			fEnergyCoreRadius = (float) CalcI.i().radiusFromVolume(dV);
			fECScale = fEnergyCoreRadius*2f;
		}
		nodeEnergyCore.setLocalScale(fECScale);
		
		float fRotSpeed = isUnstable() ? 0.1f*FastMath.nextRandomFloat() : 0.001f;
		nodeEnergyCore.rotate(fRotSpeed,fRotSpeed,fRotSpeed);
		
//		fPseudoEnergyCoreRadius = (float) (Math.cbrt(nodeEnergyCore.getWorldBound().getVolume())/2f);
		
//		bUnstable=(elecj.isOvercharged());
	}
	
	public String energyInfo(){
		StringBuilder sb = new StringBuilder(energy.energyInfo());
//		sb.append("("+lEnergyWattsPerMilis+">"+lLowEnergy+")w/ms, ");
		sb.append("r=("+StringI.i().fmtFloat(fEnergyCoreRadius)+"/"
			+StringI.i().fmtFloat(fRadius/2f)+")");
		if(sptTarget!=null){
			sb.append("tgt="+sptTarget.getName()+getTargetToken(sptTarget).elecj.energyInfo());
		}
		return sb.toString();
	}
	
	private void updateAutoTarget() {
		if(bAutoTargetNearestSpatials){
			if(sptTarget==null){
				Spatial sptNearest=null;
				Float fDistNearest=null;
				for(Spatial spt:getParent().getChildren()){
					if(spt==this)continue;
					
//					if(fPseudoEnergCoreRadius>=fPseudoRadius/2f)continue;
					if(energy.isOvercharged())continue;
					if(!energy.isLowEnergy())continue;
					
					float fDist = getWorldTranslation().distance(spt.getWorldTranslation());
					if(fDist > fMaxTractionDist)continue;
					
					if(!isRequireTargetsToken() || !hasTargetToken(spt))continue; //TODO "heavier?" is after
					if(
							fDist<=fSafeMinDist && //destruction area 
							energy.getEnergyStored() >= calcEnergyToDisintegrate(spt) //&&
//							getTargetToken(spt).lEnergyWattsPerMilis >=
//							isLowEnergy()
					)continue; //needs more energy
					
					//TODO use pseudoRadius=(bounding.volume^3)/2f to lower the distance based on its limits/edges/extents
					if(sptNearest==null || fDist<fDistNearest){
						sptNearest=spt;
						fDistNearest = getWorldTranslation().distance(sptNearest.getWorldTranslation());
					}
				}
				sptTarget=sptNearest;
			}
			
			if(sptTarget!=null){
				efHook.setFromTo(nodeEnergyCore.getWorldTranslation(), sptTarget.getWorldTranslation());
			}
			
			efHook.setPlay(sptTarget!=null);
		}
	}
	
	/**
	 * TODO deplet energy sources
	 */
	protected void updateEnergySourceInteraction(ETargetMode etmToUse) {
		if(sptTarget==null)return;
		if(nodeaxisUserChosen!=null)return; //dont mess with self
		
		Vector3f v3fSrcWPos = sptTarget.getWorldTranslation();
		
		Vector3f v3fDist=v3fSrcWPos.subtract(getWorldTranslation());
		float fDist = v3fDist.length();
		
		if(fDist > fMaxTractionDist && sptUserChosenTarget==null){
			sptTarget=null;
			return; //disconnected
		}
		
		float fMoveSpeed=fMoveBaseSpeed;
		float fFollowDist = fMaxTractionDist/2f;
		float fFollowBoostMult=10f;
		if(etmToUse==ETargetMode.Follow || etmToUse==ETargetMode.MoveOver){
			if(etmToUse==ETargetMode.Follow){
				fMoveSpeed*=fDist/fFollowDist;
				fMoveSpeed*=fFollowBoostMult;
			}else
			if(etmToUse==ETargetMode.MoveOver){
				fMoveSpeed*=fDist/fMaxTractionDist;
			}
		}else{
			fMoveSpeed*=fMaxTractionDist/fDist;
			
			if(fDist < fRadius){
				fMoveSpeed=fMoveBaseSpeed*fDist;
			}
		}
		
		if(fMoveSpeed<fMoveBaseSpeed)fMoveSpeed=fMoveBaseSpeed;
		
		Vector3f v3fDir = v3fDist.normalize();
		Vector3f v3fSpeed = v3fDir.mult(fMoveSpeed);
//		ETargetMode esmChosen=esm;
//		if(esm==ETargetMode.MoveOver && bTmpAttract)esmChosen=ETargetMode.Attract;
//		switch(esmChosen){
		switch(etmToUse){
			case JustStay:
				//TODO do nothing? could deplet the energy source at least
				break;
				
			case Attract:
				if(fDist>fSafeMinDist){
					if(consumeEnergyPF(EEnergyConsumpWpM.Tractor, fMoveSpeed)>0){
						sptTarget.move(v3fSpeed.negate());
					}
				}else{
					absorbEnergy(sptTarget);
//					bTmpAttract=false;
				}
				break;
			case Repel:
				if(consumeEnergyPF(EEnergyConsumpWpM.Tractor, fMoveSpeed)>0){
					sptTarget.move(v3fSpeed);
				}
				break;
				
			case Follow:
				if(consumeEnergyToMovePF(fMoveSpeed)>0){
					if(fDist > fFollowDist*1.1f){
						move(v3fSpeed);
//						lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
					}else
					if(fDist < fFollowDist*0.9f){
						move(v3fSpeed.negate());
					}
				}
				break;
				
			case MoveAway:
				if(consumeEnergyToMovePF(fMoveSpeed)>0){
					move(v3fSpeed.negate());
//					lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
					getLocalRotation().negate(); //TODO this looks bad... will be upside down?
				}
				break;
			case MoveOver:
				if(fDist>fSafeMinDist){
					if(consumeEnergyToMovePF(fMoveSpeed)>0){
						move(v3fSpeed);
//						lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
//						bTmpAttract=false;
//					}else{
//						updateEnergySourceInteraction(ETargetMode.Attract);
//						bTmpAttract=true;
					}
					updateEnergySourceInteraction(ETargetMode.Attract);
				}else{
					absorbEnergy(sptTarget);
				}
				break;
		}
	}
	
	
	String strMsgError="The origin rotation must not be modified as it is a world reference.";

	private EffectElectricity	setFollowToTarget;

	private Spatial	sptUserChosenTarget;
	@Deprecated	@Override	public void lookAt(Vector3f position, Vector3f upVector) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated	@Override	public void setLocalRotation(Matrix3f rotation) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated @Override	public void setLocalRotation(Quaternion quaternion) {		throw new UnsupportedOperationException("method not implemented");	}
	@Deprecated	@Override	public Spatial rotate(float xAngle, float yAngle, float zAngle) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated @Override	public Spatial rotate(Quaternion rot) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated @Override	public void rotateUpTo(Vector3f newUp) {		throw new UnsupportedOperationException(strMsgError);	}
	
	public long calcEnergyToDisintegrate(Spatial spt){
		return (long) (calcEnergyPF(EEnergyConsumpWpM.Disintegrate)*spt.getWorldBound().getVolume());
	}
	
//	protected long consumeEnergyToDisintegratePF(Spatial spt) {
//		return consumeEnergyPF(EEnergyConsumpWpM.Disintegrate,spt.getWorldBound().getVolume());///1000);
//	}

	protected long disintegrate(Spatial spt) {
		long l = energy.consumeEnergy(calcEnergyToDisintegrate(spt));
		if(l>0){
			spt.removeFromParent();
			if(spt==sptTarget)sptTarget=null;
			if(spt==sptUserChosenTarget)sptUserChosenTarget=null;
//			if(nodeaxisUserChosen==electricNodeFor(spt))nodeaxisUserChosen=null;
		}
		return l;
	}

	protected long absorbEnergy(Spatial spt) {
		return absorbEnergy(spt,false);
	}
	protected long absorbEnergy(Spatial spt,boolean bRemotely) {
		TargetToken tt = getTargetToken(spt);
		long lAbso=0;
		if(tt!=null){
			if(!bRemotely){ //it all
//				lAbso=tt.elecj.getEnergyWattsPerMilis();
				lAbso=energy.absorb(tt.elecj,tt.elecj.getEnergyStored());
				
				bForceAbsorptionOnce=false;
				
				if(canDestroy(spt)){
					disintegrate(spt);
				}
			}else{
				lAbso=energy.absorb(tt.elecj,calcEnergyPF(EEnergyConsumpWpM.RemoteAbsorption));
			}
			
		}
		
		return lAbso;
	}
	
	public static enum EEnergyConsumpWpM{
		Tractor(10), 
		RotateMin(3), 
		SmoothActive(1), 
		Move(5),
		Disintegrate(7), 
		RemoteAbsorption(2), //inverse of consumtion 
		PetFlyExpelsEnergy(8), 
		;
		
		protected long	l;
		
		EEnergyConsumpWpM(long l){
			this.l=l;
		}

		public long getEnergy() {
			return l;
		}

	}
	
	protected TargetToken getTargetToken(Spatial spt) {
		return UserDataI.i().retrieve(spt, TargetToken.class, false);
	}

	protected void init(){
		// origin
		attachChild(NodeI.i().createRotationAxes(null));
//		attachChild(GeometryI.i()
//			.createArrow(ColorRGBA.Red).setFromTo(Vector3f.ZERO, Vector3f.UNIT_X));
//		attachChild(GeometryI.i()
//			.createArrow(ColorRGBA.Green).setFromTo(Vector3f.ZERO, Vector3f.UNIT_Y));
//		attachChild(GeometryI.i()
//			.createArrow(ColorRGBA.Blue).setFromTo(Vector3f.ZERO, Vector3f.UNIT_Z));
		
		// energy core
		nodeEnergyCore=createEnergyCore();
//		geomEnergyCore=SpatialHierarchyI.i().getChildRecursiveExactMatch(nodeEnergyCore, Geometry.class);
		attachChild(nodeEnergyCore);
		anodeElectricShapesList.add(nodeEnergyCore);
//		anodeMainShapes.add(nodeEnerCore);
		
		// toruses
		nodeTorX=createAxis(Vector3f.UNIT_X, MeshI.i().box(0.5f));
		nodeTorY=createAxis(Vector3f.UNIT_Y, new Sphere(10,10,0.5f));
		rotate(nodeTorY, 90, false);
//		torZ=createAxisThing(Vector3f.UNIT_Z, new Cylinder(5,10,0.5f,0.001f,1f,true,false));
		nodeTorZ=createAxis(Vector3f.UNIT_Z, new Cone());
		
		// electricity
		tdEffectRetarget = new TimedDelay(fRetargetDefaultDelay,"").setActive(true);
		efHook=new EffectArrow();
		efHook.setColor(ColorRGBA.Gray);
		EffectManagerStateI.i().add(efHook);
		efElec=prepareEffElec(new ColorRGBA(0.5f,0.5f,1f,1));
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(OriginDevice.this.getParent()==null)return false;
				efElec.setNodeParent(OriginDevice.this.getParent());
				efHook.setNodeParent(OriginDevice.this.getParent());
				return true;
			}
		});
		
//		createPet(EAxis.X);
//		createPet(EAxis.Y);
//		createPet(EAxis.Z);
	}
	
	private EffectElectricity prepareEffElec(ColorRGBA color) {
		EffectElectricity ef = new EffectElectricity();
		ef.setColor(color);
		ef.setAmplitudePerc(0.025f);
		ef.getElectricalPath().setMinMaxPerc(0.05f, 0.1f);
		ef.setFromTo(new Vector3f(),new Vector3f()).setPlay(true); //just to allow started
		EffectManagerStateI.i().add(ef);
		return ef;
	}

	private void preparePet(NodeAxis nodePet) {
		MiscJmeI.i().addToName(nodePet, "Pet", false);
		
		TimedDelay td = new TimedDelay(1f, "").setActive(true);
		
		// Orde's pet
		anodeElectricShapesList.add(nodePet);
		nodePet.geomWireFrame.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(ColorRGBA.Cyan));
		nodePet.rotateUpTo(Vector3f.UNIT_Y); //undo the axis default
		//TODO as the material changed, shouldnt this be required????	node.geomWireFrame.getMaterial().getAdditionalRenderState().setWireframe(true);
		fixGeomToNotBeWireFrame(nodePet.geom);
			
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(getParent()==null)return false;
				
				if(isUnstable()){
					if(nodePet.getParent()==null)getParent().attachChild(nodePet);
					updatePet(getTPF(),nodePet,td);
				}else{
					nodePet.removeFromParent();
				}
				
				return true;
			}
		}).enableLoopMode();//.setDelaySeconds(0.1f);//.setInitialDelay(10));
	}
	
	/**
	 * TODO my fault? the main geometry was not set as it but is becoming wireframe...
	 * @param geom
	 */
	@Bugfix
	@ToDo //because can be my fault
	private void fixGeomToNotBeWireFrame(Geometry geom) {
		geom.getMaterial().getAdditionalRenderState().setWireframe(false);	
	}
	
	protected void updatePet(float fTPF,NodeAxis nodePet, TimedDelay td) {
		if(energy.getPerc()<=1f)return;
		
		///////////// pull/push the pet
		float fDist = nodePet.getLocalTranslation().length(); //is child of Orde, the dist is relative
		float fDistPerc = fDist/fPetMaxDist;
		
		consumeEnergyPF(EEnergyConsumpWpM.PetFlyExpelsEnergy,energy.getUnstablePerc()/3f); //3f is just to last more, then ending lasts 5 full loops this way, no "real" energy reason tho..
		
		float fEnerUnstablePercLimited=energy.getUnstablePerc();
		if(fEnerUnstablePercLimited>1f)fEnerUnstablePercLimited=1f; //so the max dist will be at max for a 200% energy
		
		// the new perc position will be relative to the current perc distance
		float fNewRelativePerc = fEnerUnstablePercLimited/fDistPerc;
		
		nodePet.setLocalTranslation(
			new Vector3f().interpolateLocal( //from world origin
				nodePet.getLocalTranslation(), 
				fNewRelativePerc
			)
		);
		
		nodePet.setLocalScale(fEnerUnstablePercLimited);
		
		///////////////// rotate around Orde
		Vector3f v3fNodeUp = nodePet.getLocalRotation().getRotationColumn(1);//y
		if(td.isReady(true))v3fNodeUp = RotateI.i().randomDirection();
		float fRotSpeed=250f;
		RotateI.i().rotateAroundPivot(nodePet, this, -(fRotSpeed*fTPF)*FastMath.DEG_TO_RAD,	v3fNodeUp, false);
		
		////////////////// spin 
		Quaternion qua = nodePet.nodeGeometries.getLocalRotation().clone();
		Vector3f v3fGeomUp = qua.getRotationColumn(1); //y
		float fSpinSpeed=500f;
		RotateI.i().rotateSpinning(
			nodePet.nodeGeometries,
			v3fGeomUp,
			qua.getRotationColumn(2),
			fSpinSpeed*fTPF*FastMath.DEG_TO_RAD
		);
	}
	
	private void updateCheckGrow() {
		long lConsumeToGrow=energy.getEnergyCapacity()*10;
		if(energy.getEnergyStored()<(lConsumeToGrow))return;
		
		if(energy.consumeEnergy(lConsumeToGrow)>0){
			float fGrowPerc=1.1f;//slow/step grow
			scale(fGrowPerc);
			energy.setEnergyCapacity((long) (energy.getEnergyCapacity()*((double)fGrowPerc)));
		}
		
//		float fGrowPerc = energy.getUnstablePerc()/10f;
//		if(fGrowPerc<1f)return;
//		if(fGrowPerc>1.1f)fGrowPerc=1.1f;//slow/step grow
//		scale(fGrowPerc);
//		energy.setEnergyCapacity((long) (energy.getEnergyCapacity()*((double)fGrowPerc)));
//		energy.consumeEnergy(energy.getEnergyStored());
	}

	private NodeAxis createEnergyCore() {
		NodeAxis node=createAxisShape(new Sphere(10,10,0.5f), //diameter 1f to be scaled
			ColorRGBA.Cyan, new Vector3f(), 0.05f, Vector3f.UNIT_Y, true, null);
		MiscJmeI.i().addToName(node, "EnergyCore", false, true);
		return node;
	}

	protected void updateAxisMainShapes() {
		for(NodeAxis node:anodeMainShapes){
			Vector3f v3f = getRotSpeedCopy();
			float fEnergySpentMultExtra=1f;
			
			if(isUnstable()){
				if(node==nodeElectricA || node==nodeElectricB){
					if(nodeElectricA==nodeElectricB){
						v3f = getRotSpeedCopy(ERotMode.Chaotic);
						fEnergySpentMultExtra=chaoticEnergySpentMult();
					}
					
					v3f.multLocal(10f);
				}
			}
			
			if(consumeEnergyPF(EEnergyConsumpWpM.RotateMin, v3f.length()*fEnergySpentMultExtra)>0){
				node.rotate(v3f.x,v3f.y,v3f.z);
			}
			
			node.ef.setPlay(!energy.isLowEnergy());
		}
		
	}

	private float chaoticEnergySpentMult() {
		return FastMath.nextRandomFloat()*100f;
	}

	protected void updateElectricalEffects() {
		efElec.setPlay(isUnstable());
		consumeEnergyPF(EEnergyConsumpWpM.SmoothActive, isUnstable() ? FastMath.nextRandomFloat()*13f : 1f); //13 is arbitrary luck :)
//		if(!bUnstable){return;}
		
//		efElec.setNodeParent(this.getParent());
//		efHook.setNodeParent(this.getParent());
//		efHook.setFromTo(v3fWorldA,nodeElectricB.getWorldTranslation());
		
		if(tdEffectRetarget.isReady(true) || bUpdateElectricalEffectForNewSourceOnce){
			bUpdateElectricalEffectForNewSourceOnce=false;
			nodeSelfElectrocute=null;
			if(iMaxHoldMilisBkp!=null){
				efElec.getElectricalPath().setMaxHoldMilis(iMaxHoldMilisBkp);
				efElec.setOverrideThickness(null);
				iMaxHoldMilisBkp=null;
			}
			
			tdEffectRetarget.resetAndChangeDelayTo(fRetargetDefaultDelay*FastMath.nextRandomFloat()).setActive(true);
			
			int iA=-1;
			nodeElectricA = null;
			Vector3f v3fWorldA = null;
			if(sptTarget!=null){
				nodeElectricA=nodeaxisUserChosen;
				iA=anodeElectricShapesList.indexOf(nodeElectricA); //external spatial will be -1
//				iA=anodeElectricShapesList.indexOf(sptElectricSrc); //external spatial will be -1
//				if(iA>-1)nodeElectricA=(NodeAxis) sptElectricSrc;
				v3fWorldA = sptTarget.getWorldTranslation();
			}else{
				iA=FastMath.nextRandomInt(0, anodeElectricShapesList.size()-1);
				nodeElectricA = anodeElectricShapesList.get(iA);
				v3fWorldA = nodeElectricA.getWorldTranslation();
			}
			
//			bSameAxis = (nodeElectricA!=null && nodeElectricB!=null && nodeElectricA.ea==nodeElectricB.ea);
			
			int iB=FastMath.nextRandomInt(0, anodeElectricShapesList.size()-1);
			nodeElectricB = anodeElectricShapesList.get(iB);
			if(iA!=iB){
				efElec.setFromTo(v3fWorldA,nodeElectricB.getWorldTranslation());
//				efHook.setFromTo(v3fWorldA,nodeElectricB.getWorldTranslation());
			}else{
				//TODO create new effect to self electr
				nodeSelfElectrocute=nodeElectricA;
			}
		}
		
		if(sptTarget!=null)absorbEnergy(sptTarget,true);
		
		if(nodeSelfElectrocute!=null){
			if(iMaxHoldMilisBkp==null){
				iMaxHoldMilisBkp = efElec.getElectricalPath().getMaxHoldMilis();
				efElec.getElectricalPath().setMaxHoldMilis(100); //frenetic
				efElec.setOverrideThickness(2);
			}
			
			BoundingVolume bv = nodeSelfElectrocute.getWorldBound();
			float fScale=1f;
			if (bv instanceof BoundingBox)fScale = ((BoundingBox)bv).getExtent(null).length();
			if (bv instanceof BoundingSphere)fScale = ((BoundingSphere)bv).getRadius();
			efElec.setFromTo(
				RotateI.i().getRandomSpotAround(bv.getCenter(), fScale),
				RotateI.i().getRandomSpotAround(bv.getCenter(), fScale)
			);
		}
		
		bSameAxis = (nodeElectricA!=null && nodeElectricB!=null && nodeElectricA.ea==nodeElectricB.ea);
	}
	
	protected NodeAxis electricNodeFor(Spatial spt) {
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
		float fEnergySpentMultExtra=1f;
		if(isUnstable()){
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
							fEnergySpentMultExtra=chaoticEnergySpentMult();
						}
					}
				}
			}
		}
		
		float fSpeed=0;
		switch(nodeTor.ea){
			case X:
				fSpeed=v3fSpeed.x;
				break;
			case Y:
				fSpeed=v3fSpeed.y;
				break;
			case Z:
				fSpeed=v3fSpeed.z;
				break;
		}
		
		fSpeed*=fMult;
		if(consumeEnergyPF(EEnergyConsumpWpM.RotateMin,FastMath.abs(fSpeed*fEnergySpentMultExtra))>0){
			nodeTor.rotate(0,fSpeed,0);
		}
	}
	
	public long consumeEnergyToMovePF(float fMult){
		return consumeEnergyPF(EEnergyConsumpWpM.Move, fMult*getWorldBound().getVolume()/100);
	}
	public long consumeEnergyPF(EEnergyConsumpWpM ee, float fMult){
		assert(fMult>=0f);
		
		if(energy.isLowEnergy() && (ee!=EEnergyConsumpWpM.Tractor)){
			return 0;
		}
		
		long lConsume = (long)(calcEnergyPF(ee)*fMult);
		if(lConsume==0)lConsume=1; //TODO imprecision... could accumulate to give at least 1.0?
		
		return energy.consumeEnergy(lConsume);
	}
	
	/**
	 * per frame
	 * @param ee
	 * @return
	 */
	public long calcEnergyPF(EEnergyConsumpWpM ee){
		return getTpfMilis()*ee.getEnergy();
	}
	
	public int getTpfMilis(){
		return (int) (fTPF*1000);
	}
	
//	protected void rotate(NodeAxis node, Vector3f v3fUp, boolean bZOnly){
	protected void rotate(NodeAxis node, float fAngleDegrees, boolean bZOnly){
		float fRotRad=FastMath.DEG_TO_RAD*fAngleDegrees;
//		if(!bZOnly && v3fUp.x==1)node.rotate(0, fRot, 0);
//		if(!bZOnly && v3fUp.y==1)node.rotate(0, fRot, 0);
//		if(v3fUp.z==1)node.rotate(-fRot, fRot, 0);
		if(!bZOnly && node.ea==EAxis.X)node.rotate(0, fRotRad, 0);
		if(!bZOnly && node.ea==EAxis.Y)node.rotate(0, fRotRad, 0);
		if(node.ea==EAxis.Z)node.rotate(-fRotRad, fRotRad, 0);
	}
	
	protected NodeAxis createAxis(Vector3f v3fUp, Mesh mesh) {
		ColorRGBA color = new ColorRGBA(v3fUp.x,v3fUp.y,v3fUp.z,1f);
		
		// axis representation shape
		NodeAxis nodeSimpleShape = createAxisShape(mesh, color, v3fUp.mult(fRadius), 0.5f, v3fUp, true, null);
		anodeElectricShapesList.add(nodeSimpleShape);
		anodeMainShapes.add(nodeSimpleShape);
		
		// effect to world origin
		nodeSimpleShape.ef = prepareEffElec(color)
//				new EffectElectricity()
//			.setColor(color)
			.setFrom(new Vector3f())
			.setFollowToTarget(nodeSimpleShape,null);
//			.setPlay(true);
		EffectManagerStateI.i().add(nodeSimpleShape.ef);
		
		float fDisplacementTorus = fRadius+1;
		
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
		rotate(nodeCore,90,true);
		nodeRotating.attachChild(nodeCore);
		
		createTorusIntersections(nodeRotating,color,fDisplacementTorus,v3fUp);
		
		// pets
		fPetMaxDist=fRadius*1.5f;
		NodeAxis nodePet = createAxisShape(
			MeshI.i().cone(1f), color, new Vector3f(fPetMaxDist,0,0), 1f, v3fUp, true, 
			new Vector3f(0.05f, 0.15f, 1));
		preparePet(nodePet);
		
		return nodeRotating;
	}
	
	protected void createTorusIntersections(Node nodeTor, ColorRGBA color, float fDisplacementTorus, Vector3f v3fUp) {
		float fIRa=fIR*1.5f;
		float fAlpha=fRotTorOpac+0.25f;
		
		// arrow tip
		NodeAxis nodePosit = createAxisShape(new Cone(fIRa*2f),
				color, new Vector3f(fDisplacementTorus,0,0), fAlpha, v3fUp, false, new Vector3f(1,1,2));
		MiscJmeI.i().addToName(nodePosit, "Intersection", false, true);
		nodePosit.lookAt(v3fUp, v3fUp);
		rotate(nodePosit,-90,false);
		nodeTor.attachChild(nodePosit);
		anodeElectricShapesList.add(nodePosit);
		
		// other end
		NodeAxis nodeNegat=createAxisShape(new Sphere(10,10,fIRa),
			color, new Vector3f(-fDisplacementTorus,0,0), fAlpha, v3fUp);
		MiscJmeI.i().addToName(nodeNegat, "Intersection", false, true);
		nodeTor.attachChild(nodeNegat);
		anodeElectricShapesList.add(nodeNegat);
	}

	protected NodeAxis createAxisShape(Mesh mesh, ColorRGBA color, Vector3f v3f, float fAlpha, Vector3f v3fUp) {
		return createAxisShape( mesh,  color,  v3f,  fAlpha,  v3fUp, false, null);
	}
	protected NodeAxis createAxisShape(Mesh mesh, ColorRGBA color, Vector3f v3fPos, float fAlpha, Vector3f v3fUp, boolean bAddWireFrame, Vector3f v3fScale) {
		if(v3fScale==null)v3fScale=new Vector3f(1,1,1);
		NodeAxis node = new NodeAxis("Node");
		Geometry geom = GeometryI.i().create(mesh, ColorI.i().colorChangeCopy(color,0,fAlpha), true,null);
		node.geom=geom;
		
		node.nodeGeometries=new Node();
		
		Geometry geomWireFrame=null;
		if(bAddWireFrame){
			geomWireFrame = new Geometry("WireFrame",mesh);
			ColorRGBA colorW = color.clone();
			colorW.a=1;
			geomWireFrame.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(colorW));
			geomWireFrame.getMaterial().getAdditionalRenderState().setWireframe(true);
			
			node.geomWireFrame=geomWireFrame;
		}
		
		// name
		MiscJmeI.i().addToName(geom, OriginDevice.class.getSimpleName(), true);
		if(v3fUp.x==1){node.ea=EAxis.X;}
		if(v3fUp.y==1){node.ea=EAxis.Y;}
		if(v3fUp.z==1){node.ea=EAxis.Z;}
		MiscJmeI.i().addToName(geom, node.ea.toString(), false);
		if(geomWireFrame!=null){
			MiscJmeI.i().addToName(geomWireFrame, node.ea.toString(), false);
		}
		
		MiscJmeI.i().addToName(node, geom.getName(), false);
		
		// hierarchy/pos/scale/rotation
		node.nodeGeometries.setLocalScale(v3fScale);
		
		node.nodeGeometries.attachChild(geom);
		if(geomWireFrame!=null)node.nodeGeometries.attachChild(geomWireFrame);
		node.attachChild(node.nodeGeometries);
		
		node.setLocalTranslation(v3fPos);
		node.rotateUpTo(v3fUp);
		
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
		return energy.isOvercharged();
	}

//	public OriginDevice setUnstable(boolean bUnstable) {
//		this.bUnstable = bUnstable;
//		return this; //for beans setter
//	}

	public Spatial getNodeElectricSrc() {
		return sptTarget;
	}
	
	public static enum ETargetMode{
		Attract,
		Repel,
		MoveOver,
		MoveAway,
		Follow,
		JustStay,
		;
	}
	
	public OriginDevice setElectricitySource(Spatial sptElectricSrc) {
		return setElectricitySource(sptElectricSrc,true);
	}
	public OriginDevice setElectricitySource(Spatial sptElectricSrc, boolean bForceAbsorption) {
		if(sptElectricSrc!=null && getTargetToken(sptElectricSrc)==null){
			MessagesI.i().warnMsg(this, "skipping", sptElectricSrc, bForceAbsorption);
			return this; //skip
		}
		
		this.sptTarget = sptElectricSrc;
		this.sptUserChosenTarget = sptElectricSrc;
		if(sptElectricSrc!=null){
			fInitialDistToSrc=sptElectricSrc.getWorldTranslation().subtract(getWorldTranslation()).length();
			nodeaxisUserChosen = electricNodeFor(sptElectricSrc);
//			tdEffectRetarget.reactivate();
			bUpdateElectricalEffectForNewSourceOnce=true;
			this.bForceAbsorptionOnce=bForceAbsorption;
		}
		return this; //for beans setter
	}

	public ETargetMode getSourceMode() {
		return esm;
	}

	public OriginDevice setSourceMode(ETargetMode esmSourceMode) {
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

	public boolean isDestroySpatials() {
		return bDestroySpatials;
	}
	
	public boolean canDestroy(Spatial spt){
		if(!bDestroySpatials)return false;
		
		if(isRequireTargetsToken()){
			if(!hasTargetToken(spt))return false;
		}
		
		return true;
	}
	
	public boolean hasTargetToken(Spatial spt){
		if(UserDataI.i().retrieve(spt, TargetToken.class, false)!=null){
			return true;
		}
		return false;
	}
	
	public boolean canTarget(Spatial spt){
		if(!isRequireTargetsToken())return true;
		if(hasTargetToken(spt))return true;
		return false;
	}
	
	/**
	 * 
	 * @param spt
	 * @return target's energy
	 */
	public long applyTargetTokenLater(Spatial spt){
		long lEnergy = energy.calcVolumeToEnergy(spt);
		if(lEnergy==0)return 0;
//			throw new DetailedException("target has no energy",spt);
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(spt.getWorldBound().getVolume()==0)return false; //not ready
				UserDataI.i().put(spt, new TargetToken(OriginDevice.this, lEnergy));
				
				return true;
			}
		});
		
		return lEnergy;
	}
	
	public OriginDevice setDestroySpatials(boolean bDestroySpatials) {
		this.bDestroySpatials = bDestroySpatials;
		return this; //for beans setter
	}

	public boolean isAutoTargetNearestSpatials() {
		return bAutoTargetNearestSpatials;
	}

	public OriginDevice setAutoTargetNearestSpatials(boolean bAutoTargetNearestSpatials) {
		this.bAutoTargetNearestSpatials = bAutoTargetNearestSpatials;
		return this; //for beans setter
	}

	public boolean isRequireTargetsToken() {
		return bRequireTargetsToken;
	}

	public OriginDevice setRequireTargetsToken(boolean bRequireTargetsToken) {
		this.bRequireTargetsToken = bRequireTargetsToken;
		return this; //for beans setter
	}

//	/**
//	 * changes on the original (mesh/material) will afect all copies TODO confirm
//	 * @return
//	 */
//	public Geometry getEnergyCoreGeomCopy() {
//		return geomEnergyCore.clone();
//	}
	
	/**
	 * keep even if empty!
	 * @param aobj
	 * @return
	 */
	public Object debugTest(Object... aobj){
		energy.setEnergy(900000);
		setLocalTranslation(1000,0,0);
		return null;
	}
}
