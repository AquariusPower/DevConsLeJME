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

import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.TimedDelay;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class Orbiter {
	private boolean	bAttached=true;
	
	// features
	private boolean	bPetScale=true;
	private boolean	bPetOrbit=true;
	private boolean	bIrregularity=true;
	private boolean	bGetCloser = true;
	private Spatial	sptToSpin;
		
	// other stuff
	private float	fMaxDist;
	private TimedDelay tdIrregularity;
	private Spatial	nodePet;
	private Spatial	sptPivot;
	private Node nodeHelperAtPivotLocation=new Node();
	private Vector3f getV3fAdd = new Vector3f();
	private float	fPercDist=1f;
	private float	fPercScale=1f;
	private long	lRealTimeStartMilis;
	private long	lSimulationTimeStartMilis;
	private Float fMaxDelayToGetCloser=null;
	private Float fMaxDelayToFullyShrink=null;
	private boolean	bUseRealTime;
	private Node	nodeParent;
	private float	fOrbitSpeed=300f;
	private float	fSpinSpeed=500f;
	
	/**
	 * 
	 * @param sptPivot
	 * @param sptPet
	 * @param sptToSpinInPet can be null
	 */
	public Orbiter(Spatial sptPivot, Spatial sptPet, Spatial sptToSpinInPet){
		assert sptPet.getParent()==null || sptPet.getParent()==sptPivot.getParent();
		
		this.nodeParent = sptPivot.getParent();
		if(sptPet.getParent()==null)nodeParent.attachChild(sptPet);
		
		this.sptPivot = sptPivot;
		
		this.nodePet = sptPet;
		this.sptToSpin = sptToSpinInPet;
		if(this.sptToSpin!=null && Node.class.isInstance(sptPet)){
			assert ((Node)sptPet).hasChild(sptToSpinInPet); //this is recursive
		}
		
		this.fMaxDist = sptPet.getLocalTranslation().distance(sptPivot.getLocalTranslation());
		this.tdIrregularity = new TimedDelay(10f).setActive(true);
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				updatePet(getTPF());
				return true;
			}
		}).enableLoopMode().setName(Orbiter.class.getSimpleName());
		
		lRealTimeStartMilis=System.currentTimeMillis();
		lSimulationTimeStartMilis=SimulationTimeI.i().getMillis();
	}
	
	protected void updatePet(float fTPF) {
		if(getPercDist()==0f || getPercScale()==0f){
			setAttached(false);
		}
		
		if(isAttached()){
			if(nodePet.getParent()==null)nodeParent.attachChild(nodePet);
		}else{
			if(nodePet.getParent()!=null)nodePet.removeFromParent();
			return;
		}
		
		/////////////  pet distance
		Vector3f v3fDir = nodePet.getLocalTranslation().subtract(sptPivot.getLocalTranslation()).normalize();
		if(v3fDir.length()==0){ // may happen after reaching Orde origin
			v3fDir=RotateI.i().randomDirection();
			//TODO fix the initial rotation too?
		}
		
//		float fPercDist = bPetUnstableDist ? energy.getUnstablePerc() : 1f;
//		if(getPercDist()>1f)setPercDist(1f); //so the max dist will be at max for a 200% energy
		
		Vector3f v3fDist = v3fDir.mult(getPercDist()*fMaxDist); //getLocalScale()
		
		nodePet.setLocalTranslation(sptPivot.getLocalTranslation().add(v3fDist));
		
		///////////////// rotate around Orde
		if(isOrbiting()){
			// irregularity
			Vector3f v3fNodeUp = nodeHelperAtPivotLocation.getLocalRotation().getRotationColumn(1);//y
			if(isIrregularity()){
				if(tdIrregularity.isReady(true)){
					getV3fAdd.set(
						FastMath.nextRandomInt(0,1),
						FastMath.nextRandomInt(0,1),
						FastMath.nextRandomInt(0,1));
				}
				
				float f=0.25f;
				v3fNodeUp.addLocal(getV3fAdd.mult(f)).normalizeLocal();
			}
			nodeHelperAtPivotLocation.setLocalTranslation(sptPivot.getLocalTranslation());
//			nodeHelperAtPivotLocation.setLocalRotation(getLocalRotation());
			nodeHelperAtPivotLocation.rotateUpTo(v3fNodeUp);
			if(nodeHelperAtPivotLocation.getParent()==null)nodeParent.attachChild(nodeHelperAtPivotLocation);
			
			// orbit
			RotateI.i().rotateAroundPivot(
					nodePet, 
					nodeHelperAtPivotLocation, 
					-(getOrbitSpeed()*fTPF)*FastMath.DEG_TO_RAD,	
					true);
		}
		
		////////////////// spin around self
		if(sptToSpin!=null){
			Quaternion qua = sptToSpin.getLocalRotation().clone();
			Vector3f v3fGeomUp = qua.getRotationColumn(1); //y
			RotateI.i().rotateSpinning(
				sptToSpin,
				v3fGeomUp,
				qua.getRotationColumn(2),
				(getSpinSpeed()*fTPF)*FastMath.DEG_TO_RAD
			);
		}
		
		//////////// scaled based on unstability
		if(isScaling()){
//			float fPercScale = getPercScale();
//			if(fPercScale>1f)setPercScale(1f); //so the max dist will be at max for a 200% energy
			nodePet.setLocalScale(sptPivot.getLocalScale().length()*getPercScale());
		}
		
	}
	
	public long getStartTimeMilis(){
		if(bUseRealTime){
			return lRealTimeStartMilis;
		}else{
			return lSimulationTimeStartMilis;
		}
	}
	
	public long getLifeTimeMilis(){
		if(bUseRealTime){
			return System.currentTimeMillis()-lRealTimeStartMilis;
		}else{
			return SimulationTimeI.i().getMillis()-lSimulationTimeStartMilis;
		}
	}
	
	public float getPercDist() {
		if(!isGetCloser())return 1f;
		
		if(fMaxDelayToGetCloser!=null){
			return FastMath.clamp( 1f-(getLifeTimeMilis()/(fMaxDelayToGetCloser*1000f)), 0f, 1f );
		}
		
		return fPercDist;
	}

	public Orbiter setPercDist(float fPercDist) {
		assert fMaxDelayToGetCloser==null;
		this.fPercDist = fPercDist;
		return this; 
	}

	public float getPercScale() {
		if(!bPetScale)return 1f;
		
		if(fMaxDelayToFullyShrink!=null){
			return FastMath.clamp( 1f-(getLifeTimeMilis()/(fMaxDelayToFullyShrink*1000f)), 0f, 1f );
		}
		
		return fPercScale;
	}

	public Orbiter setPercScale(float fPercScale) {
		assert fMaxDelayToFullyShrink==null;
		this.fPercScale = fPercScale;
		return this; 
	}

	public Float getMaxDelayToGetCloser() {
		return fMaxDelayToGetCloser;
	}
	
	public void updateIrregularityDelay(Float f){
		float fMin = f==null ? Float.MAX_VALUE : f;
		float fDiv=10f;
		if(f==null){
			if(fMaxDelayToFullyShrink!=null)fMin=Math.min(fMin,fMaxDelayToFullyShrink);
			if(fMaxDelayToGetCloser!=null)fMin=Math.min(fMin,fMaxDelayToGetCloser);
			if(fMin==Float.MAX_VALUE)fMin=fDiv;
		}
		tdIrregularity.resetAndChangeDelayTo(fMin/fDiv).setActive(true);
	}
	
	/** seconds */
	public Orbiter setMaxDelayToGetCloser(Float fMaxDelayToGetCloser) {
		this.fMaxDelayToGetCloser = fMaxDelayToGetCloser;
		updateIrregularityDelay(null);
		return this; 
	}

	public Float getMaxDelayToFullyShrink() {
		return fMaxDelayToFullyShrink;
	}

	/** seconds */
	public Orbiter setMaxDelayToFullyShrink(Float fMaxDelayToFullyShrink) {
		this.fMaxDelayToFullyShrink = fMaxDelayToFullyShrink;
		updateIrregularityDelay(null);
		return this; 
	}

	public boolean isAttached() {
		return bAttached;
	}

	public Orbiter setAttached(boolean bAttached) {
		this.bAttached = bAttached;
		return this; 
	}

	public boolean isIrregularity() {
		return bIrregularity;
	}

	public Orbiter setIrregularity(boolean bIrregularity) {
		this.bIrregularity = bIrregularity;
		return this; 
	}

	public boolean isScaling() {
		return bPetScale;
	}

	public Orbiter setScaling(boolean bPetScale) {
		this.bPetScale = bPetScale;
		return this; 
	}

	public boolean isOrbiting() {
		return bPetOrbit;
	}

	public Orbiter setOrbiting(boolean bPetOrbit) {
		this.bPetOrbit = bPetOrbit;
		return this; 
	}

	public boolean isGetCloser() {
		return bGetCloser;
	}

	public Orbiter setGetCloser(boolean bGetCloser) {
		this.bGetCloser = bGetCloser;
		return this; 
	}

	public float getOrbitSpeed() {
		return fOrbitSpeed;
	}

	public Orbiter setOrbitSpeed(float fOrbitSpeed) {
		this.fOrbitSpeed = fOrbitSpeed;
		return this; 
	}

	public float getSpinSpeed() {
		return fSpinSpeed;
	}

	public Orbiter setSpinSpeed(float fSpinSpeed) {
		this.fSpinSpeed = fSpinSpeed;
		return this; 
	}
	
}
