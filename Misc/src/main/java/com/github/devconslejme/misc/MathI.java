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

import java.util.HashMap;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MathI {
	public static MathI i(){return GlobalManagerI.i().get(MathI.class);}
	
	public HashMap<Float, Float> parabolaFoV(float fMin, float fMax, int iSteps){
		/**
		 * the idea is to make the farer (smaller) FoV step less than
		 */
		
		// parabole formula values for points c(0,0)b(70,50)a(100,100)
		// int iMaxAX=100;double fA = 0.00952380952381, fB = 0.047619047619, fC = 0;
		
		// parabole formula values for points c(0,0)b(600,130)a(1000,360)
		return parabola(1000,0.000358333333333,0.00166666666667,0,fMin,fMax,iSteps);
	}
	protected Double parabolaFindX(double fA, double fB, double fC,double fMaxX, int iMaxStepsX,float fTargetY){
		Double fFoundX=null;
		for(double fX=0;fX<fMaxX;fX+=(fMaxX/(double)iMaxStepsX)){
			double fY = fA*Math.pow(fX,2) + fB*fX + fC;
			if(fY>=fTargetY){
				fFoundX=fX;
				break;
			}
		}
		return fFoundX;
	}
	public HashMap<Float, Float> parabola(double fMaxX,double fA, double fB, double fC,float fMinY, float fMaxY, int iSteps){
		int iYMax = (int)fMax;
		
		/**
		 * find the nearest X that results in the initial (Y)
		 */
		double dStartX = parabolaFindX(fA,fB,fC,fMaxX);
		int iStartX=0;
		for(int i=0;i<iYMax;i++){
			double fX=i;
			double fY = fA*Math.pow(fX,2) + fB*fX + fC;
			if(fY>=fMin){
				iStartX=i;
				break;
			}
		}
		
		/**
		 * find the nearest X that results in the ending (Y)
		 */
		int iEndingX=0;
		for(int i=iStartX;i<iMaxAX;i++){
			double fX=i;
			double fY = fA*Math.pow(fX,2) + fB*fX + fC;
			if(fY>=fMax){
				iEndingX=i;
				break;
			}
		}
		
		int iDeltaX = (iEndingX-iStartX);
		double dMultX = iDeltaX/(double)iSteps;
		
		HashMap<Float,Float> hmXY = new HashMap<>();
		for(int iX=0;iX<=iSteps;iX++){
			double fX = iStartX+(iX*dMultX);
			double fY = fA*Math.pow(fX,2) + fB*fX + fC;
			hmXY.put((float)fX,(float)fY);
		}
		
		return hmXY;
	}
}
