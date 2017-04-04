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

package com.github.devconslejme.tests;

import java.util.HashMap;


/**
 * DISABLE!!! -> CPU "on demand" frequency, set it to something static/fixed (not necessarily the max)
 * HashMap seems fast enough on this test case.
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestGlobalAccessPerf {
	private static TestGlobalAccessPerf instance = new TestGlobalAccessPerf();
	public static TestGlobalAccessPerf i(){return instance;}
	
	int iSize=1000;
	HashMap<Integer,Integer> hm = new HashMap<Integer,Integer>();
	Integer[] al = new Integer[1000];
	
	public TestGlobalAccessPerf(){
		for(int i=0;i<iSize;i++){
			hm.put(i,i);
			al[i]=i;
		}
	}
	
	public int tstSimpleCall(int i){
		return i+1;
	}
	private long tstHashmap(int i) {
		return hm.get(i)+1;
	}
	private long tstArray(int i) {
		return al[i]+1;
	}
	
	public static void main(String[] args) {
		i().testFull();
	}
	
//	HashMap<Integer,Long> hmRes = new HashMap<Integer,Long>();
	private void testFull() {
		int iTotTests=3;
		for(int i=0;i<iTotTests*10;i++){
			long lNanoStart=System.nanoTime();
			long lTest=0;
			Integer i3=null;
			for(int i2=0;i2<1000;i2++){
				i3=i%iTotTests;
				switch(i3){
					case 0:
						lTest+=TestGlobalAccessPerf.i().tstSimpleCall(i2%iSize);
						break;
					case 1:
						lTest+=TestGlobalAccessPerf.i().tstHashmap(i2%iSize);
						break;
					case 2:
						lTest+=TestGlobalAccessPerf.i().tstArray(i2%iSize);
						break;
				}
			}
			long lNanoEnd=System.nanoTime();
			long lNanoDiff=lNanoEnd-lNanoStart;
			System.out.println(i3+": "+lNanoDiff+"//"+lTest);
		}
	}

}
