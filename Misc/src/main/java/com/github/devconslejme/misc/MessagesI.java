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

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.concurrent.Callable;

import com.github.devconslejme.misc.ReportI.IReport;



// (tab indent=2 spaces)

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MessagesI {
	public static MessagesI i(){return GlobalManagerI.i().get(MessagesI.class);}
	
	private HashMap<String,ReviewableMsg> hmMsgs = new HashMap<String,ReviewableMsg>();
	private StringBuilder	sbStack;
	private int	iSummaryReportLineLength = 100;
	private File	flLog;
	private boolean	bLog;
	private String strUIdLastForced="0";
	
	/**
	 * {@link FileI} also depends on this class, so this initializer is necessary
	 */
	public void initializeLogFile(){
		flLog=FileI.i().createNewFile(this,"log",true);
		flLog.delete();
		bLog=true;
	}
	
	@SuppressWarnings("unchecked")
	public <T extends Object> void warnUniqueMsg(Object objSource, String strMsg, T... aobjMoreInfo){
//		new Runnable(){@Override public void run() {warnMsg(objSource,strMsg,aobj);}}.run();
		putReviewableMsg(warnMsgWork(objSource,strMsg,aobjMoreInfo),true);
//		warnMsg(objSource,strMsg,aobj);
	}
	/**
	 * for things that will break non crucial functionalities
	 * @param objSource
	 * @param strMsg
	 * @param aobjMoreInfo
	 */
	@SuppressWarnings("unchecked")
	public <T extends Object> void warnMsg(Object objSource, String strMsg, T... aobjMoreInfo){
		putReviewableMsg(warnMsgWork(objSource,strMsg,aobjMoreInfo),false);
	}
	protected String warnMsgWork(Object objSource, String strMsg, Object... aobjMoreInfo){
		Object[] aobjMoreInfoSte = Arrays.copyOf(aobjMoreInfo, aobjMoreInfo.length+1, (new Object[0]).getClass());//add one slot on the array
		aobjMoreInfoSte[aobjMoreInfoSte.length-1]=Thread.currentThread().getStackTrace()[3]; // 3 is the outside caller calling the public warn one here
		return output(System.err,"WARN",objSource,strMsg,aobjMoreInfoSte);
	}
	
	/**
	 * For any useful info that is not critical enough to keep in memory,
	 * neither will break anything expected to not break.
	 * 
	 * Mainly used to track when, by who and at what stack things were configured or initialized,
	 * so, basically to track things that may happen only once.
	 * 
	 * @param objSource
	 * @param strMsg
	 * @param aobj
	 */
	@SuppressWarnings("unchecked")
	public <T extends Object> void debugInfo(Object objSource, String strMsg, T... aobj) {
//		output(false,ReportI.i().joinMessageWithObjects("DevInfo["+objSource.getClass().getSimpleName()+"]: "+strMsg, aobj));
		output(System.out,"DebugInfo",objSource,strMsg,aobj,Thread.currentThread().getStackTrace());
	}
	
//	public void output(PrintStream ps, Object objSource, String strMsg){
//		output(ps==System.err,"",objSource,strMsg);
//	}
//	
//	public String outputReviewable(PrintStream ps,String strMsgType, Object objSource, String strMsg, Object... aobj){
//		putReviewableMsg(output(), false);
//	}
	/**
	 * 
	 * @param bReviewable will create an object for easy reviewing avoiding repetitions
	 * @param ps can be null
	 * @param strMsgType
	 * @param objSource
	 * @param strMsg
	 * @param aobj
	 */
	@SuppressWarnings("unchecked")
	public <T extends Object> String output(PrintStream ps,String strMsgType, Object objSource, String strMsg, T... aobj){
		//TODO log4j?
		Class cl = Class.class.isInstance(objSource)?(Class)objSource:objSource.getClass(); //can be from a static method
		String strOutput = ReportI.i().prepareReport(
				"["+TimeFormatI.i().getRealTimeFormatted()+"]"
				+strMsgType+"["+cl.getSimpleName()+"]: "+strMsg, 
				aobj);

		if(ps!=null)ps.println(strOutput);
		if(isLog()){
			FileI.i().appendLine(flLog,strOutput);
		}
		
		return strOutput;
		
//		if(bReviewable){
//			putReviewableMsg(strOutput);
////			ReviewableMsg mdNew = new ReviewableMsg(getStackAsKey(),strOutput);
////			
////			ReviewableMsg mdExisting = hmMsgs.get(mdNew.strKey);
////			if(mdExisting!=null){
////				mdExisting.updateMsg(mdNew);
////			}else{
////				hmMsgs.put(mdNew.strKey, mdNew);
////			}
//		}
	}
	
	public ReviewableMsg putReviewableMsg(String strOutput){
		return putReviewableMsg(strOutput,false);
	}
	public ReviewableMsg putReviewableUniqueMsg(String strOutput){
		return putReviewableMsg(strOutput,true);
	}
	/**
	 * 
	 * @param strOutput
	 * @param bMakeItUnique so even being the same stacktrace (used as a string key) it will be made unique  
	 * @return
	 */
	protected ReviewableMsg putReviewableMsg(String strOutput,boolean bMakeItUnique){
		ReviewableMsg mdNew = new ReviewableMsg(
			getStackAsKey()
				+(bMakeItUnique?
					"//UIdForceUniqueMsg="
						+(strUIdLastForced=StringI.i().getNextUniqueId(strUIdLastForced)) //this will make it unique
					:
					""
				),
			strOutput
		);
		
		ReviewableMsg mdExisting = hmMsgs.get(mdNew.strKey);
		ReviewableMsg mdRet = null;
		if(mdExisting!=null){
			mdExisting.updateMsg(mdNew);
			mdRet=mdExisting;
		}else{
			hmMsgs.put(mdNew.strKey, mdNew);
			mdRet=mdNew;
		}
		
		return mdRet;
	}
	
	private String strUIdHelpPrefix="UId=";
	
	private static class ReviewableMsg implements IReport{
		private static String strUIdLast = "0";
		
		/** this key is to avoid repeated messages */
		private  String	strKey;
		
		/** messages are important at real time (not app time) because user may compare with its table clock */
		private long lRealTimeMilis = System.currentTimeMillis();
		
		private  int	iHitCount=1;
		private  int	iMsgChangedCount=0;
		private String strFullMessage;
		private String	strUId;

		public ReviewableMsg(String strKey,String strFullMessage) {
			this.strUId=strUIdLast=StringI.i().getNextUniqueId(strUIdLast);
			this.strKey=strKey;
			this.strFullMessage=strFullMessage;
		}
		
		public void updateMsg(ReviewableMsg other) {
			if(!this.strFullMessage.equals(strFullMessage))iMsgChangedCount++;
			
			this.lRealTimeMilis=other.lRealTimeMilis;
			this.strFullMessage=other.strFullMessage;
			
			this.iHitCount++;
		}

		@Override
		public String getReport(boolean bFull) {
			StringBuilder sb = new StringBuilder();
			
			String strSep=";"; //bFull?"\n":";"
			
			sb.append(MessagesI.i().strUIdHelpPrefix+strUId);
			sb.append(strSep);
			
			sb.append("time="+TimeFormatI.i().getRealTimeFormatted(lRealTimeMilis,null));
			sb.append(strSep);
			
			sb.append("hc="+iHitCount);
			sb.append(strSep);
			
			sb.append("mc="+iMsgChangedCount);
			sb.append(strSep);
			
			sb.append("msg="+strFullMessage+"");
			sb.append(strSep);
			
			if(bFull){
				for(String str:strKey.split("[;]")){
					sb.append(" "+str+"\n");
				}
				return sb.toString();
			}else{
				return StringI.i().truncAndGrantOneLine(sb.toString(),MessagesI.i().getSummaryReportLineLength(),"...");
			}
		}
		
	}
	
	protected String getStackAsKey(){
		sbStack = new StringBuilder(); 
		for(StackTraceElement ste:Thread.currentThread().getStackTrace()){
			sbStack.append(ste.toString()+";");
		}
		return sbStack.toString();
	}
	
	/**
	 * 
	 * @param strUId can be null or empty to show all as summary, or set to show one in detail
	 * @return
	 */
	public ArrayList<String> getMessagesReport(String strUId){
		ArrayList<String> astr = new ArrayList<String>();
		if(strUId!=null && strUId.isEmpty())strUId=null;
		for(ReviewableMsg md:hmMsgs.values()){
			if(strUId==null || strUId.equalsIgnoreCase(md.strUId) || strUId.equalsIgnoreCase(strUIdHelpPrefix+md.strUId)){
				astr.add(md.getReport(strUId!=null));
			}
		}
		return astr;
	}

	public int getSummaryReportLineLength() {
		return iSummaryReportLineLength;
	}

	public void setSummaryReportLineLength(int iSummaryReportLineLength) {
		this.iSummaryReportLineLength = iSummaryReportLineLength;
	}

	public boolean isLog() {
		return bLog;
	}

	public void setLog(boolean bLog) {
		this.bLog = bLog;
	}
}
