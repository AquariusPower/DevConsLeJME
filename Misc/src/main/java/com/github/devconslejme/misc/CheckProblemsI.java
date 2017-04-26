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

import java.util.ArrayList;

/**
 * The problems checker will be called whenever an exception, that closes the application, happens.
 * So more helping info can be displayed.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class CheckProblemsI {
	public static CheckProblemsI i(){return GlobalManagerI.i().get(CheckProblemsI.class);}
	
	public static interface ICheckProblems {
		/**
		 * The exception can be verified by checkers for matching clues to track and pin point problems that are not made 100% clear by some exceptions.
		 * 
		 * @param thr 
		 * @return
		 */
		int checkProblems(Throwable thr);
	}
	
	private ArrayList<ICheckProblems> achkprbList = new ArrayList<ICheckProblems>();
//	private String	strExceptionMessage;
//	private Throwable	thr;
	
	public void addProblemsChecker(ICheckProblems chkprb){
		if(!achkprbList.contains(chkprb)){
			achkprbList.add(chkprb);
			MessagesI.i().debugInfo(this, "added", chkprb);
		}else{
			MessagesI.i().warnMsg(this, "already added", chkprb);
		}
	}
	
//	public void setProblemInfo(String strExceptionMessage, Throwable thr){
//		this.strExceptionMessage=strExceptionMessage;
//		this.thr=thr;
//	}
	
	public Integer checkProblems(Throwable thr){
		int i=0;
		for(ICheckProblems chkprb:achkprbList){
			i+=chkprb.checkProblems(thr);
		}
		return i;
	}
	
//	Callable<Integer> call = new Callable<Integer>(){
//		@Override
//		public Integer call() throws Exception {
//			int i=0;
//			for(ICheckProblems chkprb:achkprbList){
//				if(chkprb.checkProblems(strExceptionMessage, thr)){
//					i++;
//				}
//			}
//			return i;
//		}
//		
//	};
//	
//	public Callable<Integer> getCheckProblemsCall(){
//		return call;
//	}
	
}
