/* 
Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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
package com.github.devconslejme.misc.lemur;

import java.util.ArrayList;

import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;

/**
 * TODO improve with aura particles shader, mesh, texture? 
 * TODO create like 10 patterns and randomize thru them to be less CPU intensive? also rotate them in teh "direction axis" to look more randomized.
 * TODO the path creation could be a simple/raw/generic at @MiscPackage!!!
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class EffectElectricity extends EffectBaseAbs<EffectElectricity>{
//	private DebugData dbg;
//	private String	strUId = ManageEffectsJmeStateI.i().strLastUId = MiscI.i().getNextUniqueId(ManageEffectsJmeStateI.i().strLastUId);
//	private Vector3f	v3fFrom;
//	private Vector3f	v3fTo;
//	private Vector3f	v3fDirectionNormalized;
//	private float	fDist;
	private float	fPartMinPerc=0.20f;
	private float	fPartMaxPerc=1.00f;
	private float fDeltaPerc = fPartMaxPerc-fPartMinPerc;
	private float fAmplitudePerc = 0.15f;
	private int	iParts;
	private int	iPartMaxDots = 100;
	private ArrayList<Vector3f>	av3fList  = new ArrayList<Vector3f>() {};
	
	@Override
	public EffectElectricity getThis(){return this;}
	
//	@Override
//	public String getUId(){
//		assertNotDiscarded();
//		return strUId;
//	}
	
	
	@Override
	protected void playWork(){
		MiscJmeI.i().updateMultiLineMesh(getGeom().getMesh(), recreatePath().toArray(new Vector3f[0]));
		getGeom().getMaterial().getAdditionalRenderState().setLineWidth(getThickNess());
	}
	
	public long getThickNess(){
		long lRemainMilis = getiHoldUntilMilis() - SimulationTimeI.i().getMillis();
		long lMaxThickness = 8;
		long lThicknessStepMilis = getiMaxHoldMilis()/lMaxThickness;
		long lCurrentThickness = lRemainMilis/lThicknessStepMilis;
//		return FastMath.nextRandomFloat()*4+1;
		return lCurrentThickness>=1 ? lCurrentThickness : 1;
	}
	
	public ArrayList<Vector3f> recreatePath() {
		assertNotDiscarded();
		Vector3f v3fTargetSpot=getLocationTo();
		
		int iPartMaxDotsCurrent = iPartMaxDots;
		int iDotsMaxDist = (int) getLocationFrom().distance(v3fTargetSpot);
		if(iDotsMaxDist<iPartMaxDots)iPartMaxDotsCurrent=iDotsMaxDist;
		
		Vector3f v3fDirectionNormalized = v3fTargetSpot.subtract(getLocationFrom()).normalize();
		Vector3f v3fRelativePartStepMaxPos = v3fDirectionNormalized.mult(iPartMaxDotsCurrent);
		int iMinDotsLength = (int) (iPartMaxDotsCurrent*fPartMinPerc);
		int iMaxAllowedParts = (iDotsMaxDist/iMinDotsLength);
		
		boolean bUpdate=false;
		float fMaxMoveDetectDist=0.01f;
		if(getV3fHoldPreviousFrom().distance(getLocationFrom()) > fMaxMoveDetectDist)bUpdate=true;
		if(getV3fHoldPreviousTo().distance(getLocationTo()) > fMaxMoveDetectDist)bUpdate=true;
		if(getiHoldUntilMilis() < SimulationTimeI.i().getMillis()){
			setiHoldUntilMilis(SimulationTimeI.i().getMillis() + FastMath.nextRandomInt(250, getiMaxHoldMilis() ));
			bUpdate=true;
		}
		
		if(bUpdate){
			av3fList.clear();
			// updating
			getV3fHoldPreviousFrom().set(getLocationFrom());
			getV3fHoldPreviousTo().set(getLocationTo());
		}else{
			// holding
			spreadInnersABit(av3fList);
			return av3fList;
		}
		
		Vector3f v3fPartStart = getLocationFrom();
		av3fList.add(v3fPartStart.clone());
		while(true){
			// move a bit towards the end
			Vector3f v3fPartEnd = new Vector3f(v3fPartStart);
			float fPerc = fPartMinPerc + (FastMath.nextRandomFloat() * fDeltaPerc);
			v3fPartEnd.interpolateLocal(v3fPartEnd.add(v3fRelativePartStepMaxPos), fPerc);
			
			// random coolness missplacement
			float fDots=iPartMaxDotsCurrent*fAmplitudePerc;
			v3fPartEnd.x+=((FastMath.nextRandomFloat()*2f)-1f)*fDots;
			v3fPartEnd.y+=((FastMath.nextRandomFloat()*2f)-1f)*fDots;
			v3fPartEnd.z+=((FastMath.nextRandomFloat()*2f)-1f)*fDots;
			
			int iDotsCurrentDist = (int) getLocationFrom().distance(v3fPartEnd);
			boolean bBreak = false;
			if(!bBreak && av3fList.size()==iMaxAllowedParts-1){
				MessagesI.i().debugInfo(this,"max parts reached, is the code well implemented?");//,getUId());
				bBreak=true; //max parts reached 
			}
			if(!bBreak && (iDotsCurrentDist>=iDotsMaxDist))bBreak=true; //max parts reached 
			if(bBreak){
				v3fPartEnd=v3fTargetSpot.clone();
			}
			
			av3fList.add(v3fPartEnd.clone());
			
			if(bBreak)break;
			
			v3fPartStart = v3fPartEnd.clone();
		}
		
		for(Vector3f v3f:av3fList){
			v3f.subtractLocal(getLocationFrom()); //the mesh is relative to the geometry
		}
		
		return av3fList;
	}
	
	private void spreadInnersABit(ArrayList<Vector3f> av3fList) {
		Vector3f v3fFromTmp=av3fList.get(0);
		Vector3f v3fToTmp=av3fList.get(av3fList.size()-1);
		float fDistMax=v3fFromTmp.distance(v3fToTmp);
		for(int i=1;i<av3fList.size()-1;i++){
			Vector3f v3f=av3fList.get(i);
//		for(Vector3f v3f:av3fList){
			/**
			 * throw a line from <---> to
			 * get the nearest point at it relative to the inner part vertex
			 * not need to be precise tho :(
			 */
			float fDist=v3fFromTmp.distance(v3f);
			Vector3f v3fNearest = v3fFromTmp.clone().interpolateLocal(v3fToTmp, fDist/fDistMax);
			Vector3f v3fNew = v3fNearest.clone().interpolateLocal(v3f, 1.02f);
			v3f.set(v3fNew);
		}
	}
	
	
	public EffectElectricity setAmplitudePerc(float f) {
		this.fAmplitudePerc=f;
		return getThis();
	}
	
	@Override
	public EffectElectricity clone() {
		EffectElectricity eff = (EffectElectricity)super.clone();//new EffectElectricity();
		eff.fAmplitudePerc=this.fAmplitudePerc;
		return eff;
	}
	
}
