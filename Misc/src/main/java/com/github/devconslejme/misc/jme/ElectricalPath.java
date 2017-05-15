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

import com.github.devconslejme.misc.MessagesI;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;

/**
 * This can be rendered as a simple line to feel like an electricity effect.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ElectricalPath {
	private ArrayList<Vector3f>	av3fList  = new ArrayList<Vector3f>() {};
	private long	lHoldUntilMilis;
	private int iPartMaxDots=100;
	private float	fPartMinPerc=0.20f; // max of 5 parts
	private float	fPartMaxPerc=1.00f;
//	private float fDeltaPerc = fPartMaxPerc-fPartMinPerc;
	private int	iMaxHoldMilis = 1000;
	private Vector3f	v3fHoldPreviousFrom=new Vector3f();
	private Vector3f	v3fHoldPreviousTo=new Vector3f();
	private long	lTimeMilis;
	private boolean	bUseRealTime = false;
//	private int	iMinPathParts=3;
//	private int	iMaxPathParts=-1;
	
	public Vector3f getHoldPreviousFrom() {
		return v3fHoldPreviousFrom;
	}

	public void setHoldPreviousFrom(Vector3f v3fHoldPreviousFrom) {
		this.v3fHoldPreviousFrom = v3fHoldPreviousFrom;
	}

	public Vector3f getHoldPreviousTo() {
		return v3fHoldPreviousTo;
	}

	public void setHoldPreviousTo(Vector3f v3fHoldPreviousTo) {
		this.v3fHoldPreviousTo = v3fHoldPreviousTo;
	}

	public int getMaxHoldMilis() {
		return iMaxHoldMilis;
	}

	public void setMaxHoldMilis(int iMaxHoldMilis) {
		this.iMaxHoldMilis = iMaxHoldMilis;
	}
	public void setHoldUntilMilis(long lHoldUntilMilis) {
		this.lHoldUntilMilis = lHoldUntilMilis;
	}
	public long getHoldUntilMilis() {
		return lHoldUntilMilis;
	}
	
	public ArrayList<Vector3f> updateElectricalPath(
			Float fTPF,
			Vector3f v3fFrom,
			Vector3f v3fTo,
			float fAmplitudePerc
	) {
		updateTime(fTPF);
		Vector3f v3fTargetSpot=v3fTo.clone();
		
		int iPartMaxDotsCurrent = iPartMaxDots;
		int iDotsMaxDist = (int) v3fFrom.distance(v3fTargetSpot);
		if(iDotsMaxDist<iPartMaxDots)iPartMaxDotsCurrent=iDotsMaxDist;
		
		Vector3f v3fDirectionNormalized = v3fTargetSpot.subtract(v3fFrom).normalize();
		Vector3f v3fRelativePartStepMaxPos = v3fDirectionNormalized.mult(iPartMaxDotsCurrent);
		float fMinDotsLength = (int) (iPartMaxDotsCurrent*fPartMinPerc);
//		if(iMinDotsLength==0)iMinDotsLength=1;
		int iMaxAllowedParts = (int) (iDotsMaxDist/fMinDotsLength);
		
		boolean bUpdate=false;
		float fMaxMoveDetectDist=0.01f;
		if(v3fHoldPreviousFrom.distance(v3fFrom) > fMaxMoveDetectDist)bUpdate=true;
		if(v3fHoldPreviousTo.distance(v3fTo) > fMaxMoveDetectDist)bUpdate=true;
		if(getHoldUntilMilis() < lTimeMilis){
			setHoldUntilMilis(lTimeMilis + FastMath.nextRandomInt(250, iMaxHoldMilis ));
			bUpdate=true;
		}
		
		if(bUpdate){
			av3fList.clear();
			// updating
			v3fHoldPreviousFrom.set(v3fFrom);
			v3fHoldPreviousTo.set(v3fTo);
		}else{
			// holding
			spreadInnersABit(av3fList);
			return av3fList;
		}
		
		Vector3f v3fPartStart = v3fFrom;
		av3fList.add(v3fPartStart.clone());
		while(true){
			// move a bit towards the end
			Vector3f v3fPartEnd = new Vector3f(v3fPartStart);
			float fPerc = fPartMinPerc + (FastMath.nextRandomFloat() * getDeltaPerc());
			v3fPartEnd.interpolateLocal(v3fPartEnd.add(v3fRelativePartStepMaxPos), fPerc);
			
			// random coolness missplacement
			float fDots=iPartMaxDotsCurrent*fAmplitudePerc;
			v3fPartEnd.x+=((FastMath.nextRandomFloat()*2f)-1f)*fDots;
			v3fPartEnd.y+=((FastMath.nextRandomFloat()*2f)-1f)*fDots;
			v3fPartEnd.z+=((FastMath.nextRandomFloat()*2f)-1f)*fDots;
			
			int iDotsCurrentDist = (int) v3fFrom.distance(v3fPartEnd);
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
			v3f.subtractLocal(v3fFrom); //the mesh is relative to the geometry
		}
		
		return av3fList;
	}
	
	private float getDeltaPerc() {
		return fPartMaxPerc-fPartMinPerc;
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

	public long getRemainingMilis() {
		return lHoldUntilMilis - getTime();
	}
	
//	long lTestTime=System.currentTimeMillis();
	private void updateTime(float fTPF){
		if(isUseRealTime()){
			lTimeMilis = System.currentTimeMillis();
		}else{
			lTimeMilis += fTPF*1000;
		}
//		lTestTime+=fTPF*1000;
//		System.err.println("Time="+lTimeMilis+", TstTime="+lTestTime+", Diff="+(lTimeMilis-lTestTime)+", TPF="+((long)fTPF*1000)+", fTPF="+fTPF);
	}
	
	private long getTime() {
		return lTimeMilis;
	}

	public boolean isUseRealTime() {
		return bUseRealTime;
	}

	public ElectricalPath setUseRealTime(boolean bUseRealTime) {
		this.bUseRealTime = bUseRealTime;
		return this;
	}

//	public void setPathParts(int iMin, int iMax) {
//		this.iMinPathParts=iMin;
//		this.iMaxPathParts=iMax;
//	}

	public int getPartMaxDots() {
		return iPartMaxDots;
	}

	public ElectricalPath setPartMaxDots(int iPartMaxDots) {
		this.iPartMaxDots = iPartMaxDots;
		return this; //for beans setter
	}

	public float getPartMinPerc() {
		return fPartMinPerc;
	}

	public ElectricalPath setPartMinPerc(float fPartMinPerc) {
		this.fPartMinPerc = fPartMinPerc;
		return this; //for beans setter
	}

	public void setMinMaxPerc(float fMin, float fMax) {
		this.fPartMinPerc=fMin;
		this.fPartMaxPerc=fMax;
	}
	
}
