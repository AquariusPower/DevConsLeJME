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
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.DebugVisualsI.ArrowGeometry;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Box;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class IndicatorI {
	public static IndicatorI i(){return GlobalManagerI.i().get(IndicatorI.class);}
	
	boolean bCenterIfOutside = false;
	private boolean	bInstantMove;
	private Node	nodeGui;
	
	public void configure(Node nodeGui){
		this.nodeGui=nodeGui;
	}
	
	public static class GeomIndicatorsData{
		ArrayList<GeomIndicator> agi = new ArrayList<GeomIndicator>();
	}
	
	public static class GeomIndicator extends Geometry{
		private Node	sptTarget;
		private boolean	bQueueLoop = true;
		private boolean	bEnabled;
		private Vector3f	v3fBouncingMoveDirection;
		private Vector3f	v3fLastBouncingValidPos;
		private boolean bCanBeDestroyed=true;
		private ArrayList<EIndicatorMode>	aeModeList = new ArrayList<>();
		private Vector3f	v3fRelative = new Vector3f();
		private Vector3f	v3fRotateSpeed = Vector3f.UNIT_XYZ.clone().mult(0.1f);
		private long	lStartNano;
		private boolean bScalingUp = true;
		private boolean bEnableTractorEffect = false;
		private ArrowGeometry	garrowDbg;
		private EffectElectricity	efTractor = new EffectElectricity();
		

		public GeomIndicator() {
			super(GeomIndicator.class.getSimpleName(),new Box(1,1,3));
			aeModeList.add(EIndicatorMode.Rotating);
			aeModeList.add(EIndicatorMode.MoveBouncing);
			lStartNano = System.nanoTime();
			
			EffectManagerStateI.i().add(efTractor);
//			efTractor.setFollowToMouse(true);
			efTractor.setNodeParent(IndicatorI.i().nodeGui);
			efTractor.setColor(ColorI.i().colorChangeCopy(ColorRGBA.Cyan,0f,0.25f));
//			efTractor.setFollowFromTarget(this,null);
			efTractor.setZOverride(MiscJmeI.i().getZAboveAllAtGuiNode());
			efTractor.setOverrideThickness(1);
			
			setLocalTranslation(MouseCUrsorI.i().getPos()); //initial pos
		}
		
		@Override
		public boolean removeFromParent() {
			efTractor.setPlay(false);
			return super.removeFromParent();
		}
		
		public void setPositionRelativeToTarget(Vector3f v3f){
			this.v3fRelative=v3f.clone();
		}
		
		public GeomIndicator setTargetAndEnable(Node spt){
			this.sptTarget = spt;
			
			GeomIndicatorsData data = IndicatorI.i().getGeomIndicatorsDataFrom(spt);
			
			if(data==null){
				data = IndicatorI.i().createNewGeomIndicatorsDataAt(spt);
//				UserDataI.i().setUserDataPSH(spt, new GeomIndicatorsData());
			}
			
			if(!data.agi.contains(this)){
				data.agi.add(this);
			}
			
			if(aeModeList.contains(EIndicatorMode.OnlyMe)){
				//remove others
				for(GeomIndicator giOther:data.agi){
					if(giOther==this)continue;
					IndicatorI.i().destroyIndicator(giOther);
				}
			}else
			if(aeModeList.contains(EIndicatorMode.OnlyUs)){
				//remove others
				for(GeomIndicator giOther:data.agi){
					if(giOther==this)continue;
					if(giOther.aeModeList.contains(EIndicatorMode.OnlyUs))continue;
					IndicatorI.i().destroyIndicator(giOther);
				}
			}
			
			setEnabled(true);
			
			return this;
		}
		
		public Node getTarget(){
			return sptTarget;
		}
		
		public void endMyQueueLoop(){
			this.bQueueLoop=false;
		}
		
		public boolean isQueueLoop(){
			return bQueueLoop;
		}
		
		public GeomIndicator setEnabled(boolean b){
			this.bEnabled =b ;
			return this;
		}
		
		public boolean isEnabled(){
			return bEnabled;
		}

		public GeomIndicator  setBouncingMoveDirection(Vector3f v3fBouncingMoveDirection) {
			this.v3fBouncingMoveDirection = v3fBouncingMoveDirection;
			return this;
		}

		public Vector3f getBouncingMoveDirection() {
			return v3fBouncingMoveDirection;
		}

		public GeomIndicator setLastBouncingValidPos(Vector3f v3f) {
			this.v3fLastBouncingValidPos=v3f.clone();
			return this;
		}

		public Vector3f getLastBouncingValidPos() {
			return v3fLastBouncingValidPos;
		}
		
		/**
		 * useful for unique indicators that will be moved around, enabled, disabled and re-targeted
		 * @return
		 */
		public GeomIndicator setDenyDestruction(){
			bCanBeDestroyed=false;
			return this;
		}
		
		public boolean isCanBeDestroyed(){
			return bCanBeDestroyed;
		}
		
		public void removeModes(EIndicatorMode... ae){
			for(EIndicatorMode e:ae)aeModeList.remove(e);
		}
		public GeomIndicator addModes(EIndicatorMode... ae){
			for(EIndicatorMode e:ae){
				if(!aeModeList.contains(e)){
					aeModeList.add(e);
				}
			}
			
			return this;
		}

		public Vector3f getRotateSpeed() {
			return v3fRotateSpeed;
		}

		public GeomIndicator setRotateSpeed(float fXYZ) {
			this.v3fRotateSpeed.set(fXYZ,fXYZ,fXYZ);
			return this;
		}
		public GeomIndicator setRotateSpeed(Vector3f v3fRotateSpeed) {
			this.v3fRotateSpeed.set(v3fRotateSpeed);
			return this;
		}

		public void updatePulseScale(float fTPF) {
//			long lDiff = System.nanoTime()-lStartNano;
			float fMinScale = 0.75f;
			float fMaxScale = 2.5f;
			float fStep=10f*fTPF;
			float fScale = getLocalScale().x; //all 3 are the same
			if(fScale<1f && !bScalingUp)fStep/=2f; //slower shrinking to look good
			fScale += bScalingUp ? fStep : -fStep;
			if(fScale>fMaxScale){fScale=fMaxScale;bScalingUp=false;}
			if(fScale<fMinScale){fScale=fMinScale;bScalingUp=true;}
			setLocalScale(fScale);
		}
		
	}
	
	private GeomIndicatorsData createNewGeomIndicatorsDataAt(Spatial spt){
		GeomIndicatorsData data=new GeomIndicatorsData();
		UserDataI.i().setUserDataPSHSafely(spt, data);
		return data;
	}
	private GeomIndicatorsData getGeomIndicatorsDataFrom(Spatial spt){
		return UserDataI.i().getUserDataPSH(spt, GeomIndicatorsData.class);
	}
	
	public static enum EIndicatorMode{
		PulseScaling,
		
		MoveBouncing,
		
		Rotating,
		
		/** will remove other indicators from the target and leave only the ones with this mode */
		OnlyUs,
		
		/** will remove all other indicators from the target and leave only the last one using this mode */
		OnlyMe,
		;
	}
	
//	private HashMap<Spatial,GeomIndicator> hmTargetIndicators = new HashMap<Spatial,GeomIndicator>();
	
//	private boolean bBouncing=false;
	
	
	public GeomIndicator createIndicator(ColorRGBA color){
		GeomIndicator gi = new GeomIndicator();
		gi.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
		
		QueueI.i().enqueue(new CallableXAnon() {
					@Override
					public Boolean call() {
						if(!update(getTPF(),gi))disableLoop();
						return true;
					}
				}
				.setName("IndicatorFollowTarget")
				.setUserCanPause(true)
				.setDelaySeconds(1f/20f)
				.enableLoop()
			);
		
		return gi;
	}

	private boolean update(float fTPF,GeomIndicator gi) {
		if(gi.isEnabled()){
			if(gi.getParent()==null){
				SpatialHierarchyI.i().getParentest(gi.getTarget(), Node.class, true)
					.attachChild(gi);
			}
			
			if(gi.aeModeList.contains(EIndicatorMode.MoveBouncing)){
				bouncingUpdateMove(fTPF,gi);
			}else{
				gi.setLocalTranslation(gi.getTarget().getWorldTranslation().add(gi.v3fRelative));
			}
			
			if(gi.aeModeList.contains(EIndicatorMode.Rotating)){
				gi.rotate(gi.getRotateSpeed().x,gi.getRotateSpeed().y,gi.getRotateSpeed().z);
			}
			
			if(gi.aeModeList.contains(EIndicatorMode.PulseScaling)){
				gi.updatePulseScale(fTPF);
			}
		}else{
			gi.removeFromParent();
		}
		
		if(!gi.isQueueLoop()){
			gi.removeFromParent();
			return false;
		}
		
		return true;
	}
	
	protected void bouncingUpdateMove(float fTPF,GeomIndicator gi) {
		Node sptTarget = gi.getTarget();
		Vector3f v3fPosNew = gi.getLocalTranslation().clone();
//		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(sptTarget);
		
		if(gi.getBouncingMoveDirection()==null){
			gi.setBouncingMoveDirection(MiscJmeI.i().randomDirection());
		}
		
		Vector3f v3fDir = gi.getBouncingMoveDirection();
		v3fPosNew.x+=v3fDir.x;
		v3fPosNew.y+=v3fDir.y;
		
		if(MiscJmeI.i().isInside(sptTarget, v3fPosNew, true)){
			gi.setLocalTranslation(v3fPosNew);
			gi.setLastBouncingValidPos(gi.getLocalTranslation());
			gi.efTractor.setPlay(false);
		}else{
			if(gi.getLastBouncingValidPos()!=null && MiscJmeI.i().isInside(sptTarget, gi.getLastBouncingValidPos(), true)){
				// restore last valid pos
				gi.setLocalTranslation(gi.getLastBouncingValidPos());
			}else{ //reset to initial valid pos
				if(bCenterIfOutside){
					gi.setLocalTranslation(MiscJmeI.i().getWorldCenterPosCopy(sptTarget));
				}else{
					if(isInstantMove()){
						//teleports it
						gi.setLocalTranslation(MiscJmeI.i().getNearestSpotInside(sptTarget,gi.getLocalTranslation()));
//						if(gi.garrowDbg==null){
//							gi.garrowDbg = DebugVisualsI.i().createArrowFollowing(nodeGui, gi, sptTarget, ColorRGBA.Yellow)
//								.setOverrideZ(MiscJmeI.i().getZAboveAllAtGuiNode());
//						}
					}else{
						//move towards target
						gi.setLocalTranslation(
							gi.getLocalTranslation().clone().interpolateLocal(
								sptTarget.getWorldBound().getCenter(), 20f*fTPF));
//						gi.efTractor.setFollowToTarget(sptTarget, null);
//						gi.efTractor.setV3fTo(sptTarget.getWorldBound().getCenter());
						if(gi.bEnableTractorEffect){
							gi.efTractor.setFromTo(gi.getLocalTranslation(), sptTarget.getWorldBound().getCenter());
							gi.efTractor.setPlay(true);
						}
					}
				}
				gi.setLastBouncingValidPos(gi.getLocalTranslation()); 
			}
			
			// new direction
			gi.setBouncingMoveDirection(MiscJmeI.i().randomDirection());
		}
	}
	
	public void destroyIndicator(GeomIndicator gi){
		gi.endMyQueueLoop();
//		gi.efTractor.setAsDiscarded();
//		gi.efTractor.setPlay(false);
//		gi.efTractor=null;
	}
	public void destroyAllIndicatorsAt(Spatial spt){
		GeomIndicatorsData data = getGeomIndicatorsDataFrom(spt);
		if(data==null){
			MessagesI.i().warnMsg(this, "no indicator data at", spt);
			return;
		}
		
		for(GeomIndicator gi:data.agi){
			if(gi.isCanBeDestroyed()){
				destroyIndicator(gi);
			}
		}
	}
	
	public void destroyAllIndicatorsRecursively(Spatial spt) {
		if(spt instanceof Node){
			for(Spatial sptChild:SpatialHierarchyI.i().getAllChildrenRecursiveFrom(spt, Spatial.class, null)){
				destroyAllIndicatorsAt(sptChild);
			}
		}else{
			destroyAllIndicatorsAt(spt);
		}
	}
	public boolean isInstantMove() {
		return bInstantMove;
	}
	public void setInstantMove(boolean bInstantMove) {
		this.bInstantMove = bInstantMove;
	}

//	public boolean isBouncing() {
//		return bBouncing;
//	}
//
//	public void setBouncing(boolean bBouncing) {
//		this.bBouncing = bBouncing;
//	}
	
}
