/* 
	Copyright (c) 2016-2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
	
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

package com.github.devconslejme.misc;


/**
 * You can use this also to avoid running code on every loop.
 * Or to have any kind of delayed execution.
 * Even just retrieve the current delay percentual for gradual variations.
 *	
 * It is not active initially because of the start time it will need to be active from.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TimedDelay {
	
	/**
	 * it must be initialized as inactive (null) so the begin time can be set properly/precisely.
	 */
	private Long	lLastUpdateReferenceTimeNano = null;

	private boolean	bOscilate;

	private float	fDelay;

	private String	strHelp;

	private boolean	bInstaReadyOnce;

	private boolean	bUseRealTime=false; //defaults to simulation time

	private boolean	bLockTimeMode;

	/**
	 * This constructor is exclusively for methods local variables.
	 * Such variables will not be stored neither easily accessible at console.
	 * 
	 * @param rfcfgOwnerUseThis
	 * @param fDelay
	 */
	public TimedDelay(float fDelay, String strHelp) {
		this.fDelay=fDelay;
		this.strHelp=strHelp;
	}
	
	public long getCurrentDelayNano() {
		return getCurrentDelayNano(false,false);
	}
	
	/**
	 * 
	 * @param bOverlapLimit if false, update is required to get a value withing the limit. If true,
	 * will use {@link #lLastUpdateReferenceTimeNano} to precisely determine the delay based on 
	 * the remainder of a the division by {@link #getDelayLimitNano()} 
	 * @param bOverlapModeAlsoUpdateReferenceTime
	 * @return
	 */
	public long getCurrentDelayNano(boolean bOverlapLimit, boolean bOverlapModeAlsoUpdateReferenceTime) {
		if(!isActive())throw new NullPointerException("inactive"); //this, of course, affects all others using this method
		
		long lCurrentDelay = 0;
		
		if(bOverlapLimit){
			long lCurrentTimeNano = getTimeNano();
			
			long lTotalDelayNano = lCurrentTimeNano - lLastUpdateReferenceTimeNano;
			
			lCurrentDelay = lTotalDelayNano%getDelayLimitNano();
			
			if(bOverlapModeAlsoUpdateReferenceTime){
				lLastUpdateReferenceTimeNano = lCurrentTimeNano;
			}
		}else{
			lCurrentDelay = getTimeNano() -lLastUpdateReferenceTimeNano;
		}
		
		return lCurrentDelay;
	}
	
	public long getTimeNano(){
		if(isUseRealTime()){
			return System.nanoTime();
		}else{
			return SimulationTimeI.i().getNanoTime();
		}
	}
	
	public void updateTime() {
		lLastUpdateReferenceTimeNano = getTimeNano();
	}
	public boolean isReady() {
		return isReady(false);
	}
	public boolean isReady(boolean bIfReadyWillAlsoUpdate) {
		boolean bReady = getCurrentDelayNano() >= getDelayLimitNano();
		
		if(bInstaReadyOnce){
			bReady=true;
			bInstaReadyOnce=false;
		}
		
		if(bIfReadyWillAlsoUpdate && bReady)updateTime();
		
		return bReady;
	}
	public long getDelayLimitMilis(){
		return getDelayLimitNano()/1000000;
	}
	public long getDelayLimitNano(){
		return TimeConvertI.i().secondsToNano(getDelayLimitSeconds());
	}
	public float getDelayLimitSeconds(){
		if(bOscilate){
			return fDelay*2f;
		}else{
			return fDelay;
		}
	}
	
	/**
	 * will start from now
	 */
	public void reactivate(){
		updateTime();
	}
	
	public void resetTime() {
		lLastUpdateReferenceTimeNano=null;
	}
	
	public boolean isActive() {
		return lLastUpdateReferenceTimeNano!=null;
	}
	
	/**
	 * can be called many subsequent times without updating the reference time
	 * @param b
	 * @return 
	 */
	public TimedDelay setActive(boolean b){
		if(b){
			if(!isActive())updateTime();
			bLockTimeMode=true;
		}else{
			resetTime();
		}
		
		return this;
	}
	
	public TimedDelay setOscilateMode(boolean b){
		this.bOscilate=b;
		return this;
	}
	
	public float getCurrentDelayCalc(float fMaxValue,boolean bIfReadyWillAlsoUpdate) {
		return getCurrentDelayCalc(fMaxValue,bOscilate,false,bIfReadyWillAlsoUpdate);
	}
	public float getCurrentDelayCalcDynamic(float fMaxValue) {
		return getCurrentDelayCalc(fMaxValue,bOscilate,true,null);
	}
	/**
	 * 
	 * @param fMaxValue
	 * @param bOscilate
	 * @param bDynamic
	 * @param bIfReadyWillAlsoUpdate
	 * @return
	 */
	private float getCurrentDelayCalc(float fMaxValue, boolean bOscilate, boolean bDynamic, Boolean bIfReadyWillAlsoUpdate) {
		float fHalf=(fMaxValue/2f);
		
		Float fPerc = null;
		if(bDynamic){
			fPerc = getCurrentDelayPercentualDynamic();
		}else{
			fPerc = getCurrentDelayPercentual(bIfReadyWillAlsoUpdate);
		}
		float fCurrent = (fPerc * fMaxValue);
		
		if(!bOscilate)return fCurrent;
		
		float fOscilatedCurrent=0f;
		
		//ex.: max is 10
		if(fCurrent<fHalf){
			fOscilatedCurrent=fCurrent*2f; //ex.: from 0 to 10: 1 -> 2; 4 -> 8;
		}else{
			fOscilatedCurrent=fMaxValue-((fCurrent-fHalf)*2f); //ex.: from 10 to 0: 6 -> 8; 9 -> 2;
		}
		
		return fOscilatedCurrent;
	}	
	/**
	 * Will overlap {@link #getDelayLimitNano()} not requiring {@link #updateTime()} 
	 * to return precise values.
	 * 
	 * @return
	 */
	public float getCurrentDelayPercentualDynamic() {
		long lCurrentDelay = getCurrentDelayNano(true,false);
		
		double dPerc = 1.0 - ((double)lCurrentDelay)/((double)getDelayLimitNano());
		
		return (float)dPerc;
	}
	
	/**
	 * Will not overlap {@link #getDelayLimitNano()}, so {@link #updateTime()} is required.
	 */
	public float getCurrentDelayPercentual(boolean bIfReadyWillAlsoUpdate) {
		long lCurrentDelayNano = getCurrentDelayNano();
		
		long lDiff = getDelayLimitNano()-lCurrentDelayNano;
		if(lDiff<0)lDiff=0; //if it took too much time, this constraint will fix the negative value
		
		double dPerc = 1.0 - ((double)lDiff)/((double)getDelayLimitNano());
		
		if(isReady(bIfReadyWillAlsoUpdate)){
			return 1f; //if from getting the current to now it gets ready, will return 100%
		}else{
			return (float)dPerc;
		}
	}

	public String getHelp() {
		return strHelp;
	}

	public TimedDelay resetAndChangeDelayTo(float fDelaySeconds){
		resetTime();
		fDelay=(fDelaySeconds);
		return this;
	}

	public TimedDelay setAsReadyOnce(boolean bIfReadyWillAlsoUpdate) {
		bInstaReadyOnce=true;
		if(bIfReadyWillAlsoUpdate)updateTime();
		return this;
	}

	public boolean isUseRealTime() {
		return bUseRealTime;
	}

	public TimedDelay setUseRealTime(boolean bUseRealTime) {
		if(bLockTimeMode && this.bUseRealTime!=bUseRealTime){
			throw new DetailedException("after the first activation, the time mode cannot be changed", this.bUseRealTime);
		}
		this.bUseRealTime = bUseRealTime;
		return this; 
	}
}
