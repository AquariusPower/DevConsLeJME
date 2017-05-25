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
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TimedDelay {
	private boolean	bUseRealTime;
	private boolean	bLockTimeMode;
	private Long	lLastUpdateReferenceTimeNano;
	private long	lDelayNano;
	private boolean	bInstaReadyOnce;
	private boolean	bOscilate;

	public TimedDelay(float fDelay) {
		setDelay(fDelay);
	}
	
	private void setDelay(float fSeconds){
		lDelayNano = TimeConvertI.i().secondsToNano(fSeconds);
	}
	
	public TimedDelay setAsReadyOnce(boolean bIfReadyWillAlsoUpdate) {
		bInstaReadyOnce=true;
		if(bIfReadyWillAlsoUpdate)updateTime();
		return this;
	}
	public TimedDelay resetAndChangeDelayTo(float fDelaySeconds){
		resetTime();
		setDelay(fDelaySeconds);
		return this;
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

	public long getDelayLimitNano(){
		return lDelayNano;
	}

	public long getCurrentDelayNano() {
		return getTimeNano()-lLastUpdateReferenceTimeNano;
	}

	public TimedDelay setActive(boolean b){
		if(b){
			if(!isActive())updateTime();
			bLockTimeMode=true;
		}else{
			resetTime();
		}
		
		return this;
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
	/**
	 * will start from now
	 * @return 
	 */
	public TimedDelay reactivate(){
		updateTime();
		return this;
	}
	
	public TimedDelay resetTime() {
		lLastUpdateReferenceTimeNano=null;
		return this;
	}

	public boolean isActive() {
		return lLastUpdateReferenceTimeNano!=null;
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

	public float calcRemainderAsPercentualMultBy(float fMaxValue) {
		assert isActive();
		double dPerc=0f;
		if(bOscilate){ 
			/**
			 * Remainder of x2 limit is important to keep the variation speed.
			 */
			long lRemainderX2 = getCurrentDelayNano()%(getDelayLimitNano()*2);
			double dPercX2 = lRemainderX2/(double)getDelayLimitNano(); //from 0.0 to 2.0
			if(dPercX2<=1f){ // from 0.0 to 1.0
				dPerc=dPercX2;
			}else{ //from 1.0 to 2.0 will become from 1.0 to 0.0 
				dPerc=2f-dPercX2;
			}
		}else{
			long lRemainder = getCurrentDelayNano()%getDelayLimitNano();
			dPerc = lRemainder/(double)getDelayLimitNano();
		}
		return (float) (fMaxValue*dPerc);
	}

	public TimedDelay setOscilateMode(boolean b) {
		this.bOscilate=b;
		return this;
	}

	public double getCurrentDelay() {
		return TimeConvertI.i().nanoToSeconds(getCurrentDelayNano());
	}

}
