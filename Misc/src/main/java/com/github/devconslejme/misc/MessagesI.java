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
import java.util.HashMap;

import com.github.devconslejme.misc.ReportI.IReport;



// (tab indent=2 spaces)

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MessagesI {
	public static MessagesI i(){return GlobalInstanceManagerI.i().get(MessagesI.class);}
	
	private HashMap<String,MsgData> hmMsgs = new HashMap<String,MsgData>();
	private StringBuilder	sbStack;
	
	public void warnMsg(Object objSource, String strMsg, Object... aobj){
		output(true,"WARN",objSource,strMsg,aobj);
//		output(true,"WARN",objSource,Object... aobj); 
	}

	public void debugInfo(Object objSource, String strMsg, Object... aobj) {
//		output(false,ReportI.i().joinMessageWithObjects("DevInfo["+objSource.getClass().getSimpleName()+"]: "+strMsg, aobj));
		output(false,"DebugInfo",objSource,strMsg,aobj,Thread.currentThread().getStackTrace());
	}
	
	private void output(boolean bStderr,String strMsgType, Object objSource, String strMsg, Object... aobj){
		//TODO log4j?
		String strOutput = ReportI.i().prepareReport(
				strMsgType+"["+objSource.getClass().getSimpleName()+"]: "+strMsg, 
				aobj);

		(bStderr ? (System.err) : (System.out)).println(strOutput);
//			strMsgType+"["+objSource.getClass().getSimpleName()+"]: "+str		); 
		
		hmMsgs.put(getStackAsKey(), new MsgData(strOutput));
	}
	
	private static class MsgData implements IReport{
		private static String strUIdLast = StringI.i().getNextUniqueId("0");
		long lNano = System.nanoTime();
		String str;
		private String	strUId;
		
		public MsgData(String strOutput) {
			this.strUId=strUIdLast=StringI.i().getNextUniqueId(strUIdLast);
			this.str=strOutput;
		}
		
		@Override
		public String getReport(boolean bFull) {
			
			return null;
		}
		
	}
	
	private String getStackAsKey(){
		sbStack = new StringBuilder(); 
		for(StackTraceElement ste:Thread.currentThread().getStackTrace()){
			sbStack.append(ste.toString()+";");
		}
		return sbStack.toString();
	}
	
	/**
	 * 
	 * @param strUId can be null to show all as summary, or set to show one in detail
	 * @return
	 */
	public ArrayList<String> getMessagesReport(String strUId){
		ArrayList<String> astr = new ArrayList<String>();
		for(MsgData md:hmMsgs.values()){
			if(strUId==null || strUId.equalsIgnoreCase(md.strUId)){
				astr.add(md.getReport(strUId!=null));
			}
		}
		return astr;
	}
}
