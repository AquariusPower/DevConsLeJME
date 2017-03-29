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

package com.github.devconslejme.misc;

import com.github.devconslejme.DCGlobal;
import com.github.devconslejme.JavaScriptI;
import com.jme3.app.state.AbstractAppState;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DynamicFPSLimiterStateI extends AbstractAppState{
	private static DynamicFPSLimiterStateI instance = new DynamicFPSLimiterStateI();
	/**instance*/public static DynamicFPSLimiterStateI i(){return instance;}
	
	private long	lNanoFrameDelayByCpuUsage;
	private long	lNanoDelayLimit;
	private long	lNanoTimePrevious;
	private long	lNanoThreadSleep;
	private int	iMaxFPS;
	
	public void configure(){
		DCGlobal.app().getStateManager().attach(this);
		JavaScriptI.i().setJSBinding(this);
	}
	
	public void setMaxFps(int iMaxFPS){
		this.iMaxFPS=iMaxFPS;
		if(this.iMaxFPS<1)this.iMaxFPS=1;
		lNanoDelayLimit = (long) TimeConvertI.i().secondsToNano(((1.0f/this.iMaxFPS)));
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		try {
			/**
			 * //MUST BE BEFORE THE SLEEP!!!!!!
			 */
			lNanoFrameDelayByCpuUsage = System.nanoTime() - lNanoTimePrevious; //must be real time as must be independent of the simulation time
			lNanoThreadSleep = lNanoDelayLimit -lNanoFrameDelayByCpuUsage;
			if(lNanoThreadSleep<0L)lNanoThreadSleep=0L; //only useful for reports
			
			if(lNanoThreadSleep>0L)Thread.sleep(getThreadSleepTimeMilis());
			
			/**
			 * MUST BE AFTER THE SLEEP!!!!!!!
			 */
			lNanoTimePrevious = System.nanoTime(); //must be real time as must be independent of the simulation time
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
	}
	
	public long getThreadSleepTimeMilis(){
		return lNanoThreadSleep/1000000L;
	}
	
}
