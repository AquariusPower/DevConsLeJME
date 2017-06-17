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
package com.github.devconslejme.tests;

import java.util.ArrayList;

import com.github.devconslejme.devcons.LoggingI;
import com.github.devconslejme.misc.CalcI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.DebugVisualsI;
import com.github.devconslejme.misc.jme.EffectArrow;
import com.github.devconslejme.misc.jme.EffectElectricity;
import com.github.devconslejme.misc.jme.EffectManagerStateI;
import com.github.devconslejme.misc.jme.EnergyJme;
import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.MeshI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.OriginDevice;
import com.github.devconslejme.misc.jme.OriginDevice.NodeAxis;
import com.github.devconslejme.misc.jme.PhysicsData;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.github.devconslejme.misc.jme.RotateI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.jme.WorldPickingI;
import com.github.devconslejme.projman.SimpleApplicationAndStateAbs;
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

/**
 * Improve with fancyness (shaders, lighting, shadows, sfx and voices hehe).
 * 
 * OrDe starts enabled in auto feed mode.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestOriginDeviceGame extends SimpleApplicationAndStateAbs {
	public static void main(String[] args) {
		TestDevConsFull.main(args); //TODO rm
		if(false){TestOriginDeviceGame test = new TestOriginDeviceGame();test.start();}//TODO re-enable
	}

	private OriginDeviceMonster	orde;

	@Override
	public void simpleInitApp() {
		com.github.devconslejme.misc.jme.PkgCfgI.i().configure(this,getGuiNode(),getRootNode());
		initTest();
	}
	
	public OriginDevice getOriginDevice() {
		return orde;
	}
	
	@Override
	public void update(float tpf) {
		orde.update(tpf);
		HWEnvironmentJmeI.i().putCustomInfo("OrdeEnergy", ""+orde.energyInfo());
	}
	
	public class GeometryVolDbg extends Geometry{}
	
	/**
	 * public so can be called from devcons user cmds
	 */
	@Override
	public void initTest() {
		super.initTest();
		
		// good position related to these objects
		getCamera().setLocation(new Vector3f(9.787677f, 6.957723f, 11.003839f)); //taken from devcons
		getCamera().setRotation(new Quaternion(-0.068618454f, 0.91919893f, -0.18511744f, -0.34072912f)); //taken from devcons
		
		// Orde
		orde = new OriginDeviceMonster().setEnabled(true);
//		DebugVisualsI.i().showWorldBoundAndRotAxes(orde);//TODO rm
		orde
//			.setUnstable(true)
			.setDestroySpatials(true)
//			.setSourceMode(ETargetMode.Attract)
			.setAutoTargetNearestSpatials(true);
		GlobalManagerI.i().putGlobal(OriginDeviceMonster.class,orde);
//		JavaScriptI.i().setJSBindingForEnumsOf(OriginDevice.class);
		getRootNode().attachChild(orde);
		
		// Orde's food
		Node nodeRef=new Node();
		String strOrdeFood="OrdeFood";
		for(int i=1;i<=20;i++){
			nodeRef.rotate(i*30*FastMath.DEG_TO_RAD, 0, 0);
			
			float fExtent=0.1f*i;
			Geometry geom = GeometryI.i().create(MeshI.i().box(fExtent), ColorRGBA.randomColor(), false,null);
			MiscJmeI.i().addToName(geom, strOrdeFood+i, false);
			MiscJmeI.i().addToName(geom, "Extent="+StringI.i().fmtFloat(fExtent), false);
			
			geom.setLocalTranslation(
				new Vector3f(i, (i*i)/3f, i) //move them around spreading
					.mult(1.2f) //spread a bit more
			); 
			
			float fRot=i*15*FastMath.DEG_TO_RAD;
			geom.rotate(fRot,fRot,fRot);
			
			nodeRef.attachChild(geom);
			Vector3f v3fWorld = geom.getWorldTranslation(); //rotated position
			geom.setLocalTranslation(v3fWorld);
			getRootNode().attachChild(geom);
			
			// wont move, just for debug 
			if(false){
				GeometryVolDbg geomVolume = GeometryI.i().create(MeshI.i().sphereFromVolumeOf(geom), ColorRGBA.Red,false,new GeometryVolDbg());
				geomVolume.getMaterial().getAdditionalRenderState().setWireframe(true);
				geomVolume.setLocalTranslation(v3fWorld);
				getRootNode().attachChild(geomVolume);
		    WorldPickingI.i().addSkip(geomVolume);
			}
		    
			DebugVisualsI.i().showWorldBoundAndRotAxes(geom);
			
			orde.applyTargetTokenLater(geom);
		}
		
//		// picking 
//    WorldPickingI.i().addListener(this);
	}
	
//	@Override
//	public boolean updatePickingEvent(ArrayList<CollisionResult> acrList, Geometry geom, Spatial sptParentest) {
//		if(geom!=null){
//			LoggingI.i().logMarker(""+geom);
//			LoggingI.i().logEntry(""+geom.getWorldBound());
//			LoggingI.i().logEntry("Volume="+geom.getWorldBound().getVolume());
//			orde.setElectricitySource(geom);
//			return true;
//		}
//		
//		orde.setElectricitySource(null);
//		
//		return false;
//	}

	public static class NodeAxisGm extends NodeAxis{
		public NodeAxisGm(String str) {
			super(str);
		}
		EffectElectricity	ef;
		EnergyJme en;
		public Node	nodeOrdeRefLike = new Node();
	}
	public static class TargetToken {
	//	long lEnergyWattsPerMilis;
		private EnergyJme en;
		private OriginDeviceMonster ordeApplier;
	
		public TargetToken(OriginDeviceMonster ordeApplier, long lEnergyWattsPerMilis) {
			this.ordeApplier=ordeApplier;
			this.en = new EnergyJme(ordeApplier.energy);
			this.en.addEnergy(lEnergyWattsPerMilis);
//			this.lEnergyWattsPerMilis=lEnergyWattsPerMilis;
			assert(en.getEnergyStored()>=0);
		}
		
	}
	public static class OriginDeviceMonster extends OriginDevice<OriginDeviceMonster,NodeAxisGm>{
		/**
		 * each 0.01^3 wold volume = 1 watt/miliseconds
		 * so 1^3 = 100*100*100 w/ms
		 */
		private EnergyJme energy;
		private EffectElectricity	efElec;
		private TimedDelay	tdEffectRetarget;
		private ArrayList<NodeAxis> anodeMainShapes;
		private ArrayList<NodeAxis> anodeElectricShapesList;
		private Integer	iMaxHoldMilisBkp;
		private NodeAxis	nodeSelfElectrocute;
		private NodeAxis	nodeElectricA;
		private NodeAxis	nodeElectricB;
		private Spatial	sptTarget;
		private boolean	bSameAxis;
		private boolean	bDestroySpatials;
		private boolean	bAutoTargetNearestSpatials;
		private NodeAxis	nodeEnergyCore;
		private float	fMaxTractionDist;
		private float	fEnergyCoreRadius;
		private EffectArrow	efHook;
		private Spatial	sptUserChosenTarget;
		private Spatial	sptForceAbsorption;
		private float	fPetMaxRadiusPercDist;
		private float	fSafeMinDist;
		private float	fTractionForceBasedOnDiameterMult;
		private ETargetMode esm;
		private float	fRetargetDefaultDelay;
		private ERotMode erm;
		private float	fMoveBaseSpeed;
		private String strMsgError;
		private boolean	bPetSpin;
		private boolean	bPetOrbit;
		private boolean	bPetUnstableDist;
		private boolean	bRequireTargetsToken;
		private boolean	bPetScale;

		@Deprecated	@Override	public void lookAt(Vector3f position, Vector3f upVector) {		throw new UnsupportedOperationException(strMsgError);	}
		@Deprecated	@Override	public void setLocalRotation(Matrix3f rotation) {		throw new UnsupportedOperationException(strMsgError);	}
		@Deprecated @Override	public void setLocalRotation(Quaternion quaternion) {		throw new UnsupportedOperationException("method not implemented");	}
		@Deprecated	@Override	public Spatial rotate(float xAngle, float yAngle, float zAngle) {		throw new UnsupportedOperationException(strMsgError);	}
		@Deprecated @Override	public Spatial rotate(Quaternion rot) {		throw new UnsupportedOperationException(strMsgError);	}
		@Deprecated @Override	public void rotateUpTo(Vector3f newUp) {		throw new UnsupportedOperationException(strMsgError);	}
		
		@Override
		public boolean updatePickingEvent(int iButtonIndex, ArrayList<RayCastResultX> acrList, PhysicsData pd, Geometry geom, Spatial sptParentest) {
			if(geom!=null){
				LoggingI.i().logMarker(""+geom);
				LoggingI.i().logEntry(""+geom.getWorldBound());
				LoggingI.i().logEntry("Volume="+geom.getWorldBound().getVolume());
				switch(iButtonIndex){
					case 0:
						setElectricitySource(geom);
						break;
				}
				return true;
			}
			
			setElectricitySource(null);
			
			return false;
		}
		
		@Override
		public NodeAxisGm createNodeAxis(String strName) {
			return new NodeAxisGm(strName);
		}
		
		@Override
		protected void constructorPreInitFields() {
			super.constructorPreInitFields();
			
			bPetSpin=true;
			bPetOrbit =true;
			bPetUnstableDist=true;
			bPetScale=true;
			
			bRequireTargetsToken=true;
			fPetMaxRadiusPercDist=1.1f;
			fSafeMinDist=0.1f;
			fTractionForceBasedOnDiameterMult = 3f;
			esm = ETargetMode.MoveOver;
			fRetargetDefaultDelay=3;
			erm = ERotMode.Disaligned;
			fMoveBaseSpeed=0.01f;
			strMsgError="The origin rotation must not be modified as it is a world reference.";
		}
		public OriginDeviceMonster(){
			super();
		}
		
		public Vector3f moveVelocity(){
			Vector3f v3f=getWorldTranslation()
				.normalize()
				.mult(fMoveBaseSpeed*getWorldTranslation().length());
			if(v3f.length()<fMoveBaseSpeed)v3f.set(fMoveBaseSpeed,0,0);
			return v3f;
		}
		
		@Override
		public void update(float fTPF){
			super.update(fTPF);
			
			// self stats
			fMaxTractionDist=(getRadius()*2f)*fTractionForceBasedOnDiameterMult;
			
			// moves back to world origin
			if(sptTarget==null){
				if(getWorldTranslation().length()>0){
					move(moveVelocity().negate());
				}
			}
			
			updateCheckGrowShrink(); //evolve
			updateEnergyCore();
			updateAutoTarget();
			updateElectricalEffects();
			updateEnergySourceInteraction(esm);
			
			if(isUnstable()){
				boolean bCreate=false;
				if(!tdUnstableCreatePets.isActive()){
					tdUnstableCreatePets.setActive(true);
					bCreate=true;
				}else{
					if(tdUnstableCreatePets.isReady(true))bCreate=true;
				}
				
				if(bCreate){
					createPet(EAxis.values()[FastMath.nextRandomInt(0,2)]);
				}
			}else{
				tdUnstableCreatePets.setActive(false);
			}
		}
		
		TimedDelay tdUnstableCreatePets = new TimedDelay(1f);
		
		private void updateEnergyCore() {
			float fECScale=0.01f;
			if(energy.isHasEnergy()){
//				fECScale=((float)Math.cbrt(lEnergyWattsPerMilis/1000L));
				double dV=energy.energyToVolume();
				fEnergyCoreRadius = (float) CalcI.i().radiusFromVolume(dV);
				fECScale = fEnergyCoreRadius*2f;
			}
			nodeEnergyCore.setLocalScale(fECScale);
			
			float fRotSpeed = isUnstable() ? 0.1f*FastMath.nextRandomFloat() : 0.001f;
			nodeEnergyCore.rotate(fRotSpeed,fRotSpeed,fRotSpeed);
			
//			fPseudoEnergyCoreRadius = (float) (Math.cbrt(nodeEnergyCore.getWorldBound().getVolume())/2f);
//			bUnstable=(elecj.isOvercharged());
		}
		
		public String energyInfo(){
			StringBuilder sb = new StringBuilder(energy.energyInfo());
//			sb.append("("+lEnergyWattsPerMilis+">"+lLowEnergy+")w/ms, ");
			sb.append("r=("+StringI.i().fmtFloat(fEnergyCoreRadius)+"/"
				+StringI.i().fmtFloat(getRadius()/2f)+")");
			sb.append("v3f="+StringTextJmeI.i().fmtVector3f(getWorldTranslation(),2)+", ");
			
			if(sptTarget!=null){
				TargetToken tt = getTargetToken(sptTarget);
				if(tt!=null)sb.append("tgt="+sptTarget.getName()+tt.en.energyInfo());
			}
			
			return sb.toString();
		}
		
		private void updateAutoTarget() {
			if(getParent()==null)return;
			if(!bAutoTargetNearestSpatials)return;
			
			if(sptTarget==null){
				Spatial sptNearest=null;
				Float fDistNearest=null;
				for(Spatial spt:getParent().getChildren()){
					if(spt==this)continue;
					
//						if(fPseudoEnergCoreRadius>=fPseudoRadius/2f)continue;
					if(energy.isOvercharged())continue;
					if(!energy.isLowEnergy())continue;
					
					float fDist = getWorldTranslation().distance(spt.getWorldTranslation());
					if(fDist > fMaxTractionDist)continue;
					
					if(!isRequireTargetsToken() || !hasTargetTokenParentRecursive(spt))continue; //TODO "heavier?" is after
					if(
							fDist<=fSafeMinDist && //destruction area 
							energy.getEnergyStored() >= calcEnergyToDisintegrate(spt) //&&
//								getTargetToken(spt).lEnergyWattsPerMilis >=
//								isLowEnergy()
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
		
		public NodeAxisGm getNodeaxisUserChosen(){
			if(sptTarget==null)return null;
			return electricNodeFor(sptTarget);
		}
		
		/**
		 * TODO deplet energy sources
		 */
		protected void updateEnergySourceInteraction(ETargetMode etmToUse) {
			if(sptTarget==null)return;
			if(getNodeaxisUserChosen()!=null)return; //dont mess with self
			
			Vector3f v3fWTargetPos = sptTarget.getWorldTranslation();
			
			Vector3f v3fDistToTarget=v3fWTargetPos.subtract(getWorldTranslation());
			float fDist = v3fDistToTarget.length();
			
			if(fDist > fMaxTractionDist && sptUserChosenTarget==null){
				setElectricitySource(null);
//				sptTarget=null;
				return; //disconnected
			}
			
//			float fMoveSpeed=fMoveBaseSpeed;//moveVelocity().length();
			float fMoveSpeed=moveVelocity().length();
			float fFollowDist = fMaxTractionDist/2f;
//			float fFollowBoostMult=10f;
//			if(etmToUse==ETargetMode.Follow || etmToUse==ETargetMode.MoveOver){
//				if(etmToUse==ETargetMode.Follow){
//					fMoveSpeed*=fDist/fFollowDist;
//					fMoveSpeed*=fFollowBoostMult;
//				}else
//				if(etmToUse==ETargetMode.MoveOver){
//					fMoveSpeed*=fDist/fMaxTractionDist;
//				}
//			}else{
//				fMoveSpeed*=fMaxTractionDist/fDist;
//				
//				if(fDist < fRadius){
//					fMoveSpeed=fMoveBaseSpeed*fDist;
//				}
//			}
//			
//			if(fMoveSpeed<fMoveBaseSpeed)fMoveSpeed=fMoveBaseSpeed;
			if(fDist < getRadius()){
				fMoveSpeed=fMoveBaseSpeed*fDist;
			}
			
			Vector3f v3fDir = v3fDistToTarget.normalize();
			Vector3f v3fSpeed = v3fDir.mult(fMoveSpeed);
//			ETargetMode esmChosen=esm;
//			if(esm==ETargetMode.MoveOver && bTmpAttract)esmChosen=ETargetMode.Attract;
//			switch(esmChosen){
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
//						bTmpAttract=false;
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
//							lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
						}else
						if(fDist < fFollowDist*0.9f){
							move(v3fSpeed.negate());
						}
					}
					break;
					
				case MoveAway:
					if(consumeEnergyToMovePF(fMoveSpeed)>0){
						move(v3fSpeed.negate());
//						lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
						getLocalRotation().negate(); //TODO this looks bad... will be upside down?
					}
					break;
				case MoveOver:
					if(fDist>fSafeMinDist){
						if(consumeEnergyToMovePF(fMoveSpeed)>0){
							move(v3fSpeed);
//							lookAt(v3fSrcWPos,Vector3f.UNIT_Y);
//							bTmpAttract=false;
//						}else{
//							updateEnergySourceInteraction(ETargetMode.Attract);
//							bTmpAttract=true;
						}
						updateEnergySourceInteraction(ETargetMode.Attract);
					}else{
						absorbEnergy(sptTarget);
					}
					break;
			}
		}
		
		public long calcEnergyToDisintegrate(Spatial spt){
			return (long) (calcEnergyPF(EEnergyConsumpWpM.Disintegrate)*spt.getWorldBound().getVolume());
		}
		
//		protected long consumeEnergyToDisintegratePF(Spatial spt) {
//			return consumeEnergyPF(EEnergyConsumpWpM.Disintegrate,spt.getWorldBound().getVolume());///1000);
//		}

		protected long disintegrate(Spatial spt) {
			long l = energy.consumeEnergy(calcEnergyToDisintegrate(spt));
			if(l>0){
				spt.removeFromParent();
				
				setElectricitySource(null);
//				if(spt==sptTarget)sptTarget=null;
//				if(spt==sptUserChosenTarget)sptUserChosenTarget=null;
//				if(nodeaxisUserChosen==electricNodeFor(spt))nodeaxisUserChosen=null;
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
//					lAbso=tt.elecj.getEnergyWattsPerMilis();
					lAbso=energy.absorb(tt.en,tt.en.getEnergyStored());
					
//					bForceAbsorptionOnce=false;
					
					if(canDestroy(spt)){
						disintegrate(spt);
					}
				}else{
					lAbso=energy.absorb(tt.en,calcEnergyPF(EEnergyConsumpWpM.RemoteAbsorption));
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
			return UserDataI.i().getMustExistOrNull(spt, TargetToken.class);
		}

		@Override
		protected void init(){
			anodeElectricShapesList=new ArrayList<NodeAxis>();
			anodeMainShapes=new ArrayList<NodeAxis>();
			energy = new EnergyJme(1000000,100000,10000,0);
			
			super.init();
			
			for(EAxis ea:EAxis.values()){
				anodeMainShapes.add(getAxisInfo(ea).getRepresentationShape());
			}
			
			// energy core
			nodeEnergyCore=createEnergyCore();
			attachChild(nodeEnergyCore);
			anodeElectricShapesList.add(nodeEnergyCore);
			
			// electricity
			tdEffectRetarget = new TimedDelay(fRetargetDefaultDelay).setActive(true);
			efHook=new EffectArrow();
			efHook.setColor(ColorRGBA.Gray);
			EffectManagerStateI.i().add(efHook);
			efElec=prepareEffElec(new ColorRGBA(0.5f,0.5f,1f,1));
			
			QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
					if(OriginDeviceMonster.this.getParent()==null)return false;
					efElec.setNodeParentest(OriginDeviceMonster.this.getParent());
					efHook.setNodeParentest(OriginDeviceMonster.this.getParent());
					return true;
				}
			});
			
		}
		
		private EffectElectricity prepareEffElec(ColorRGBA color) {
			EffectElectricity ef = new EffectElectricity();
			ef.setColor(color);
			ef.setAmplitudePerc(0.25f);
//			ef.setAmplitudePerc(0.025f);
			ef.getElectricalPath().setMinMaxPerc(0.05f, 0.1f);
			ef.setFromTo(new Vector3f(),new Vector3f()).setPlay(true); //just to allow started
			EffectManagerStateI.i().add(ef);
			return ef;
		}

//		private void preparePet(NodeAxisGm nodePet) {
//			MiscJmeI.i().addToName(nodePet, "Pet", false);
//			
////			TimedDelay tdChangeDir = new TimedDelay(10f).setActive(true).setAsReadyOnce(true);
//			
//			// Orde's pet
//			anodeElectricShapesList.add(nodePet);
//			nodePet.getGeomWireFrame().setMaterial(ColorI.i().retrieveMaterialUnshadedColor(ColorRGBA.Cyan));
//			nodePet.getGeomWireFrame().getMaterial().getAdditionalRenderState().setWireframe(true);
//			nodePet.rotateUpTo(Vector3f.UNIT_Y); //undo the axis default
//			nodePet.removeFromParent(); //independent
//				
////			new Orbiter(this,nodePet,nodePet.getGeom());
////			
////			QueueI.i().enqueue(new CallableXAnon() {
////				@Override
////				public Boolean call() {
////					if(getParent()==null)return false;
////					updatePet(getTPF(),nodePet,tdChangeDir);
////					return true;
////				}
////			}).enableLoopMode();//.setDelaySeconds(0.1f);//.setInitialDelay(10));
//		}
		
//		/**
//		 * TODO use class Orbiter
//		 * @param fTPF
//		 * @param nodePet
//		 * @param td
//		 */
//		protected void updatePet(float fTPF,NodeAxisGm nodePet, TimedDelay td) {
//			if(isUnstable()){
//				if(nodePet.getParent()==null)this.getParent().attachChild(nodePet);
//			}else{
//				if(nodePet.getParent()!=null)nodePet.removeFromParent();
//				return;
//			}
//			
//			/////////////  pet distance
//			Vector3f v3fDir = nodePet.getLocalTranslation().subtract(this.getLocalTranslation()).normalize();
//			if(v3fDir.length()==0){ // may happen after reaching Orde origin
//				v3fDir=RotateI.i().randomDirection();
//				//TODO fix the initial rotation too?
//			}
//			
//			float fPercDist = bPetUnstableDist ? energy.getUnstablePerc() : 1f;
//			if(fPercDist>1f)fPercDist=1f; //so the max dist will be at max for a 200% energy
//			
//			Vector3f v3fDist = v3fDir.mult(fPercDist*getPetMaxDist()); //getLocalScale()
//			
//			nodePet.setLocalTranslation(getLocalTranslation().add(v3fDist));
//			
//			///////////////// rotate around Orde
//			if(bPetOrbit){
//				// irregularity
//				Vector3f v3fNodeUp = nodePet.nodeOrdeRefLike.getLocalRotation().getRotationColumn(1);//y
//				boolean bIrregular=true; //TODO true
//				if(bIrregular){
//					if(td.isReady(true)){
//						nodePet.getV3fAdd().set(
//								FastMath.nextRandomInt(0,1),
//								FastMath.nextRandomInt(0,1),
//								FastMath.nextRandomInt(0,1));
//					}
//					
//					float f=0.25f;
//					v3fNodeUp.addLocal(nodePet.getV3fAdd().mult(f)).normalizeLocal();
//				}
//				nodePet.nodeOrdeRefLike.setLocalTranslation(getLocalTranslation());
////				nodePet.nodeOrdeRefLike.setLocalRotation(getLocalRotation());
//				nodePet.nodeOrdeRefLike.rotateUpTo(v3fNodeUp);
//				if(nodePet.nodeOrdeRefLike.getParent()==null)getParent().attachChild(nodePet.nodeOrdeRefLike);
//				
//				// orbit
//				float fRotSpeed=300f;
//				RotateI.i().rotateAroundPivot(
//						nodePet, 
//						nodePet.nodeOrdeRefLike, 
//						-(fRotSpeed*fTPF)*FastMath.DEG_TO_RAD,	
//						true);
//			}
//			
//			////////////////// spin around self
//			if(bPetSpin){
//				Quaternion qua = nodePet.getNodeGeometries().getLocalRotation().clone();
//				Vector3f v3fGeomUp = qua.getRotationColumn(1); //y
//				float fSpinSpeed=500f;
//				RotateI.i().rotateSpinning(
//					nodePet.getNodeGeometries(),
//					v3fGeomUp,
//					qua.getRotationColumn(2),
//					(fSpinSpeed*fTPF)*FastMath.DEG_TO_RAD
//				);
//			}
//			
//			//////////// scaled based on unstability
//			if(bPetScale){
//				float fPercScale = energy.getUnstablePerc();
//				if(fPercScale>1f)fPercScale=1f; //so the max dist will be at max for a 200% energy
//				nodePet.setLocalScale(getLocalScale().length()*fPercScale);
//			}
//			
//			/////////// energy
//			consumeEnergyPF(EEnergyConsumpWpM.PetFlyExpelsEnergy,energy.getUnstablePerc()/3f); //3f is just to last more, then ending lasts 5 full loops this way, no "real" energy reason tho..
//
//		}
		
		private void updateCheckGrowShrink() {
			Float fPerc=null;
			long lConsumeToGrow=energy.getEnergyCapacity()*10;
			
//			if(energy.isLowEnergy() && getNodeaxisUserChosen()!=null){
			if(getNodeaxisUserChosen()!=null){
				fPerc=0.9f;//slow/step
				energy.addEnergy((long)Math.round(lConsumeToGrow*fPerc));
				setTargetRaw(null);
			}else{
				if(energy.getEnergyStored()>(lConsumeToGrow)){
					if(energy.consumeEnergy(lConsumeToGrow)>0){
						fPerc=1.1f;//slow/step grow
					}
				}
			}
			
			if(fPerc!=null){
				scale(fPerc);
				energy.setEnergyCapacity((long)Math.round(energy.getEnergyCapacity()*((double)fPerc)));
			}
		}

		private NodeAxisGm createEnergyCore() {
			String str="EnergyCore";
			NodeAxisGm node=new NodeAxisGm(str);
			Geometry geom = new Geometry(str,new Sphere(10,10,0.5f)); //diameter 1f to be scaled
			geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(ColorRGBA.Cyan));
			geom.getMaterial().getAdditionalRenderState().setWireframe(true);
			node.attachChild(geom); 
			
			attachChild(node);
			
//			NodeAxisGm node=createAxisShape(new Sphere(10,10,0.5f), //diameter 1f to be scaled
//				ColorRGBA.Cyan, new Vector3f(), 0.05f, Vector3f.UNIT_Y, true, null);
			MiscJmeI.i().addToName(node, str, false, true);
			return node;
		}

		@Override
		protected void rotateMainShape(NodeAxisGm node, Vector3f v3f) {
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
				super.rotateMainShape(node, v3f);
			}
			
			((NodeAxisGm)node).ef.setPlay(!energy.isLowEnergy());
		}
		
		private float chaoticEnergySpentMult() {
			return FastMath.nextRandomFloat()*100f;
		}

		protected void updateElectricalEffects() {
			efElec.setPlay(isUnstable() || sptTarget!=null);
			consumeEnergyPF(EEnergyConsumpWpM.SmoothActive, isUnstable() ? FastMath.nextRandomFloat()*13f : 1f); //13 is arbitrary luck :)
//			if(!bUnstable){return;}
			
//			efElec.setNodeParent(this.getParent());
//			efHook.setNodeParent(this.getParent());
//			efHook.setFromTo(v3fWorldA,nodeElectricB.getWorldTranslation());
			
//			if(tdEffectRetarget.isReady(true) || bUpdateElectricalEffectForNewSourceOnce){
			if(tdEffectRetarget.isReady(true)){
//				bUpdateElectricalEffectForNewSourceOnce=false;
				nodeSelfElectrocute=null;
				if(iMaxHoldMilisBkp!=null){
					efElec.getElectricalPath().setMaxHoldMilis(iMaxHoldMilisBkp);
					efElec.setOverrideThickness(null);
					iMaxHoldMilisBkp=null;
				}
				
				tdEffectRetarget.resetAndChangeDelayTo(fRetargetDefaultDelay*FastMath.nextRandomFloat()).setActive(true);
				
				ArrayList<NodeAxis> aelecVis = getVisibleElectricShapes();
				
				int iA=-1;
				nodeElectricA = null;
				Vector3f v3fWorldA = null;
				if(sptTarget!=null){ // if target is set, it will always be A
					nodeElectricA=getNodeaxisUserChosen();
					iA=anodeElectricShapesList.indexOf(nodeElectricA); //external spatial will be -1
//					iA=anodeElectricShapesList.indexOf(sptElectricSrc); //external spatial will be -1
//					if(iA>-1)nodeElectricA=(NodeAxis) sptElectricSrc;
					v3fWorldA = sptTarget.getWorldTranslation();
				}else{
					iA=FastMath.nextRandomInt(0, aelecVis.size()-1);
					nodeElectricA = aelecVis.get(iA);
					v3fWorldA = nodeElectricA.getWorldTranslation();
				}
				
//				bSameAxis = (nodeElectricA!=null && nodeElectricB!=null && nodeElectricA.ea==nodeElectricB.ea);
				
				int iB=FastMath.nextRandomInt(0, aelecVis.size()-1);
				nodeElectricB = aelecVis.get(iB);
				if(iA!=iB){
					efElec.setFromTo(v3fWorldA,nodeElectricB.getWorldTranslation());
//					efHook.setFromTo(v3fWorldA,nodeElectricB.getWorldTranslation());
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
				if(bv!=null){
					float fScale=1f;
					if (bv instanceof BoundingBox)fScale = ((BoundingBox)bv).getExtent(null).length();
					if (bv instanceof BoundingSphere)fScale = ((BoundingSphere)bv).getRadius();
					efElec.setFromTo(
						RotateI.i().getRandomSpotAround(bv.getCenter(), fScale),
						RotateI.i().getRandomSpotAround(bv.getCenter(), fScale)
					);
				}else{
					efElec.setPlay(false);
				}
			}
			
			bSameAxis = (nodeElectricA!=null && nodeElectricB!=null && nodeElectricA.getEAxis()==nodeElectricB.getEAxis());
		}
		
		protected ArrayList<NodeAxis> getVisibleElectricShapes(){
			ArrayList<NodeAxis> anode = new ArrayList<NodeAxis>();
			for(NodeAxis node:anodeElectricShapesList){
				if(node.getParent()!=null)anode.add(node);
			}
			return anode;
		}
		
		protected NodeAxisGm electricNodeFor(Spatial spt) {
			ArrayList<Node> anode = SpatialHierarchyI.i().getAllParents(spt, false);
			for(Node node:anode){ //nodes may not be NodeAxis
				if(anodeElectricShapesList.contains(node)){
					return (NodeAxisGm)node; //must cast 
				}
			}
			return null;
		}

		@Override
		protected void rotateTor(NodeAxisGm nodeTor,Vector3f v3fSpeed){//, EAxis ea) {
			boolean bTorChildA = SpatialHierarchyI.i().isChildRecursive(nodeTor,nodeElectricA);
			boolean bTorChildB = SpatialHierarchyI.i().isChildRecursive(nodeTor,nodeElectricB);
			
			float fBoost=10f;
			float fMult=1f;
			float fEnergySpentMultExtra=1f;
			if(isUnstable()){
				if(nodeSelfElectrocute!=null && nodeTor.getEAxis()==nodeSelfElectrocute.getEAxis()){
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
						if(nodeTor.getEAxis()==nodeElectricA.getEAxis() || nodeTor.getEAxis()==nodeElectricB.getEAxis()){
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
			
			v3fSpeed.multLocal(fMult);
			if(consumeEnergyPF(EEnergyConsumpWpM.RotateMin,FastMath.abs(rotTorSpeed(nodeTor,v3fSpeed)*fEnergySpentMultExtra))>0){
				super.rotateTor(nodeTor, v3fSpeed);
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
		
		@Override
		protected EAxis createAxis(Vector3f v3fUp, Mesh mesh) {
			EAxis ea = super.createAxis(v3fUp, mesh);
			
			// effect to world origin
			NodeAxisGm nodeRepresentationShape = getAxisInfo(ea).getRepresentationShape();
			nodeRepresentationShape.ef = prepareEffElec(getAxisInfo(ea).getColor())
				.setFrom(new Vector3f())
				.setFollowToTarget(nodeRepresentationShape,null);
			EffectManagerStateI.i().add(nodeRepresentationShape.ef);
			
//			// pets
//			NodeAxisGm nodePet = createAxisShape(ea,
//				MeshI.i().cone(1f), new Vector3f(getPetMaxDist(),0,0), 1f, v3fUp, true, 
//				new Vector3f(0.05f, 0.15f, 1));
//			preparePet(nodePet);
			
			return ea;
		}
		
		private float getPetMaxDist() {
			return fPetMaxRadiusPercDist*getRadius();
		}
		
		@Override
		protected NodeAxisGm createAxisShape(EAxis ea, Mesh mesh,	Vector3f v3fPos, float fAlpha, 
				Vector3f v3fUp, boolean bAddWireFrame,	Vector3f v3fScale
		) {
			NodeAxisGm nd = super.createAxisShape(ea, mesh, v3fPos, fAlpha, v3fUp, bAddWireFrame,	v3fScale);
			applyTargetTokenLater(nd);
			return nd;
		}
		
		public static enum ERotMode{
			Chaotic,
			Disaligned,
			Aligned,
			;
		}
		
		@Override
		public Vector3f getRotSpeedCopy() {
			return getRotSpeedCopy(null);
		}
		public Vector3f getRotSpeedCopy(ERotMode ermOverride) {
			switch(ermOverride!=null ? ermOverride : this.erm){
				case Chaotic:
					return super.getRotSpeedCopy().mult(new Vector3f(
							2f*FastMath.nextRandomFloat()-1f,
							2f*FastMath.nextRandomFloat()-1f,
							2f*FastMath.nextRandomFloat()-1f
						));
				case Disaligned:
					return super.getRotSpeedCopy().mult(new Vector3f(
							FastMath.nextRandomFloat(),
							FastMath.nextRandomFloat()*1.5f,
							FastMath.nextRandomFloat()*2.0f
						));
				default: //just to let it compile...
				case Aligned:
					return super.getRotSpeedCopy().clone();
			}
		}

		public ERotMode getRotMode() {
			return erm;
		}

		public OriginDeviceMonster setRotMode(ERotMode erm) {
			this.erm = erm;
			return this; //for beans setter
		}

		public boolean isUnstable() {
			return energy.isOvercharged();
		}

//		public OriginDevice setUnstable(boolean bUnstable) {
//			this.bUnstable = bUnstable;
//			return this; //for beans setter
//		}

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
		
		public OriginDeviceMonster setElectricitySource(Spatial sptElectricSrc) {
			return setElectricitySource(sptElectricSrc,true);
		}
		public OriginDeviceMonster setElectricitySource(Spatial sptElectricSrc, boolean bForceAbsorptionOnce) {
			if(sptElectricSrc!=null && !hasTargetTokenParentRecursive(sptElectricSrc)){
				MessagesI.i().warnMsg(this, "has no energy, skipping", sptElectricSrc, bForceAbsorptionOnce);
				return this; //skip
			}
			
			setTargetRaw(sptElectricSrc);
//			this.sptTarget = sptElectricSrc;
			this.sptUserChosenTarget = sptElectricSrc;
			if(bForceAbsorptionOnce)sptForceAbsorption=sptElectricSrc;
//			if(sptElectricSrc!=null)nodeaxisUserChosen = electricNodeFor(sptElectricSrc);
			
//			if(sptElectricSrc!=null){
////				fInitialDistToSrc=sptElectricSrc.getWorldTranslation().subtract(getWorldTranslation()).length();
////				tdEffectRetarget.reactivate();
//				bUpdateElectricalEffectForNewSourceOnce=true;
////				this.bForceAbsorptionOnce=bForceAbsorptionOnce;
//			}
			return this; //for beans setter
		}
		
		private void setTargetRaw(Spatial sptElectricSrc) {
			this.sptTarget = sptElectricSrc;
			tdEffectRetarget.setAsReadyOnce(true); //will promptly update the effect
		}

		public ETargetMode getSourceMode() {
			return esm;
		}

		public OriginDeviceMonster setSourceMode(ETargetMode esmSourceMode) {
			this.esm = esmSourceMode;
			return this; //for beans setter
		}

		public float getMoveBaseSpeed() {
			return fMoveBaseSpeed;
		}

		public OriginDeviceMonster setMoveBaseSpeed(float fMoveBaseSpeed) {
			this.fMoveBaseSpeed = fMoveBaseSpeed;
			return this; //for beans setter
		}

		public boolean isDestroySpatials() {
			return bDestroySpatials;
		}
		
		public boolean canDestroy(Spatial spt){
			if(!bDestroySpatials)return false;
			
			if(isRequireTargetsToken()){
				if(!hasTargetTokenParentRecursive(spt))return false;
			}
			
			return true;
		}
		
		protected boolean hasTargetToken(Spatial spt){
			return UserDataI.i().contains(spt, TargetToken.class);
		}
		
		public boolean hasTargetTokenParentRecursive(Spatial spt){
			if(hasTargetToken(spt))return true;
			
			for(Node node:SpatialHierarchyI.i().getAllParents(spt, false)){
				if(hasTargetToken(spt))return true;
			}
			
			return false;
		}
		
		public boolean canTarget(Spatial spt){
			if(!isRequireTargetsToken())return true;
			if(hasTargetTokenParentRecursive(spt))return true;
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
//				throw new DetailedException("target has no energy",spt);
			
			QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
					if(spt.getWorldBound().getVolume()==0)return false; //not ready
					UserDataI.i().overwriteSafely(spt, new TargetToken(OriginDeviceMonster.this, lEnergy));
					
					return true;
				}
			});
			
			return lEnergy;
		}
		
		public OriginDeviceMonster setDestroySpatials(boolean bDestroySpatials) {
			this.bDestroySpatials = bDestroySpatials;
			return this; //for beans setter
		}

		public boolean isAutoTargetNearestSpatials() {
			return bAutoTargetNearestSpatials;
		}

		public OriginDeviceMonster setAutoTargetNearestSpatials(boolean bAutoTargetNearestSpatials) {
			this.bAutoTargetNearestSpatials = bAutoTargetNearestSpatials;
			return this; //for beans setter
		}

		public boolean isRequireTargetsToken() {
			return bRequireTargetsToken;
		}

		public OriginDeviceMonster setRequireTargetsToken(boolean bRequireTargetsToken) {
			this.bRequireTargetsToken = bRequireTargetsToken;
			return this; //for beans setter
		}

//		/**
//		 * changes on the original (mesh/material) will afect all copies TODO confirm
//		 * @return
//		 */
//		public Geometry getEnergyCoreGeomCopy() {
//			return geomEnergyCore.clone();
//		}
		
		/**
		 * keep even if empty!
		 * @param aobj
		 * @return
		 */
		public Object debugTest(Object... aobj){
			energy.setEnergyStored(900000);
			setLocalTranslation((int)aobj[0],(int)aobj[1],(int)aobj[2]);
			return null;
		}
	}
}
