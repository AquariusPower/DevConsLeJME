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

import com.github.devconslejme.misc.Annotations.SimpleVarReadOnly;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HWEnvironmentI {
	public static HWEnvironmentI i(){return GlobalManagerI.i().get(HWEnvironmentI.class);}

	private long lTotalFrameCount;
	private float	fTPF;
	private float	fSumTPF;
	private int	iFPSFrameCount;
	private int	iFPS;
	
	/**
	 * just for clarity
	 */
	public void configure(){}
	
	public long getTotalFrameCount() {
		return lTotalFrameCount;
	}

//	protected EnvironmentI setTotalFrameCount(long lTotalFrameCount) {
//		this.lTotalFrameCount = lTotalFrameCount;
//		return this; 
//	}
	
	public void update(float tpf){
		fTPF=tpf;
		
		lTotalFrameCount++; //TODO can this overflow!? :O
		
		calcFPS();
	}

	private void calcFPS() {
		fSumTPF+=fTPF;
		iFPSFrameCount++;
		if(fSumTPF>=1f){
			iFPS=iFPSFrameCount;
			fSumTPF-=1f;//precision keeping the tiny bit for the next sum TODO may cause trouble?
			iFPSFrameCount=0;
		}
	}

	@SimpleVarReadOnly
	public float getTPF() {
		return fTPF;
	}
	
	@SimpleVarReadOnly
	public float getFPS() {
		return iFPS;
	}
}
