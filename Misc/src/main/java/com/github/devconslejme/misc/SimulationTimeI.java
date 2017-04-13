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
public class SimulationTimeI {
	public static SimulationTimeI i(){return GlobalManagerI.i().get(SimulationTimeI.class);}
	
//	/**
//	 * {@link Request} uses 0 as reference to a resetted request.
//	 * 1L makes no difference at all, so lets init with it.
//	 * The simulation could be actually initialized with any valid time too.
//	 */
//	public SimulationTime(long lStartNano){
//		if(lStartNano<1L)throw new DetailedException("must be >= 1L",this);
//		this.lSimulationNano=lStartNano;
//	}
	
//	public SimulationTime(){
//		this(1L);
//	}	
	
	private long lSimulationNano;
	private boolean	bPaused;
	private float fSpeed=1f;
	
	public long getNanoTime() {
		return lSimulationNano;
	}
	
	public long getMillis(){
//		return lSimulationNano/1000000L;
		return TimeConvertI.i().nanoToMilis(lSimulationNano);
	}
	
	/**
	 * use when loading a simulation
	 * @param lNano
	 */
	public void setToNanoTime(long lNano){
		if(lNano <= this.lSimulationNano)throw new DetailedException("cannot update to same or older time", lNano, lSimulationNano);
		this.lSimulationNano=lNano;
	}
	
	public void setToMilis(long lMilis){
		setToNanoTime(TimeConvertI.i().milisToNano(lMilis));
	}
	
	public void pause(){
		this.bPaused=true;
	}
	
	public void resume(){
		this.bPaused=false;
	}
	
	public void updateAddFrameTime(float fTPF){
		if(bPaused)return;
		
		if(fTPF<=0.0f)throw new DetailedException("cannot update to same or older time", fTPF, lSimulationNano);
		
		this.lSimulationNano+=TimeConvertI.i().secondsToNano(fTPF*fSpeed);
	}
	
	public void setSpeed(float f){
		if(f<=0.0)throw new DetailedException("speed must be positive, to pause use pause()");
		this.fSpeed=f;
	}
}
