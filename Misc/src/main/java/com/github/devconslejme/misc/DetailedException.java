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
import java.util.Arrays;



// (tab indent=2 spaces)

/**
 * This will provide more and better readable information about exceptions.
 * 
 * TODO substitute all NullPointerException by this one.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DetailedException extends NullPointerException{
	private static final long	serialVersionUID	= -2021594801806561912L;
	
	private static Throwable	exRequestExit;
	private static IUserInputDetector	userInputDetector;
	private static String	strErrorMessage;
	private static String strHeader = "["+DetailedException.class.getSimpleName()+"]";

	public interface IUserInputDetector {
		boolean isConsoleCommandRunningFromDirectUserInput();
	}
	
	public static interface ICheckProblems {
		Object performChecks(String strMessage, Throwable thr);
	}
	private static ICheckProblems chkprb = null;

	public static void setProblemsChecker(ICheckProblems chkprb){
		DetailedException.chkprb=chkprb;
	}

	private static String joinMessageWithObjects(String strMessage, Object... aobj){
		if(JavaLangI.i().isRecursiveLoopOnMethod("<init>",DetailedException.class)){
			System.err.println(strHeader+"Recursive loop exception, stack dump below:");
			Thread.dumpStack();
			return strMessage+"\n"+
				strHeader+" Recursive loop (prevented) at exception ("+DetailedException.class+") call...)";
		}
		
		try{
			return ReportI.i().joinMessageWithObjects(strMessage,aobj);
		}catch(Exception ex){
			System.err.println(strHeader+"another exception happened while gathering information to this exception...");
			ex.printStackTrace();
			return "(failed to gather further exception information)";
		}
	}
	private String	strMessageKey;
	public String getMessageKey() {
		return strMessageKey;
	}

	public DetailedException(boolean bExitApplication, String strMessage, Object... aobj) {
		super(joinMessageWithObjects(strMessage, aobj));
		
		this.strMessageKey=strMessage;
		
		if(bExitApplication && (userInputDetector!=null && !userInputDetector.isConsoleCommandRunningFromDirectUserInput())){
			DetailedException.exRequestExit = this;
		}
	}
	public DetailedException(String str, Object... aobj) {
		this(true,str,aobj);
	}
	public DetailedException() {
		this("(no extra info)");
	}
	public DetailedException(Exception ex) {
		this("(just rethrowing the cause)");
		setCauseAndReturnSelf(ex);
	}

	public static void setUserInputDetector(IUserInputDetector u){
		userInputDetector = u;
	}

	public static boolean isExitRequested(){
		return exRequestExit!=null;
	}

	public static Throwable getExitRequestCause(){
		return exRequestExit;
	}

	public static void setExitRequestCause(String strErrMsg, Throwable t){
		DetailedException.strErrorMessage=strErrMsg;
		DetailedException.exRequestExit=t;
	}

	public static void assertNotEmpty(String strDescWhat, String str, Object... aobjMoreObjectsForDebugInfo){
		assertNotNull(str, strDescWhat, aobjMoreObjectsForDebugInfo);
		
		if(str.isEmpty()){
			ArrayList<Object> aobjAll = new ArrayList<Object>();
			aobjAll.add(str);
			aobjAll.addAll(Arrays.asList(aobjMoreObjectsForDebugInfo));
			
			throw new DetailedException(strDescWhat+": is empty", aobjAll.toArray());
		}
	}

	/**
	 * @param strDescWhat
	 * @param obj
	 * @param aobjMoreObjectsForDebugInfo
	 */
	public static <T> T assertNotNull(T obj, Object... aobjMoreObjectsForDebugInfo){
		if(obj==null){
			ArrayList<Object> aobjAll = new ArrayList<Object>();
			aobjAll.add(obj);
			aobjAll.addAll(Arrays.asList(aobjMoreObjectsForDebugInfo));
			
			throw new DetailedException("is null", aobjAll.toArray());
		}
		
		return obj;
	}

	public static <T> void assertNotAlreadyAdded(ArrayList<T> aList, T objNew, Object... aobj){
		String strMsg=null;
		if(objNew==null){
			strMsg="cant be null";
		}
		
		if(aList.contains(objNew)){
			strMsg="already added";
		}
		
		if(strMsg!=null){
			ArrayList<Object> aobjAll = new ArrayList<Object>();
			aobjAll.add(aList);
			aobjAll.add(objNew);
			aobjAll.addAll(Arrays.asList(aobj));
			
			throw new DetailedException("already added", aobjAll);
		}
	}

	public static void assertIsTrue(String strDescWhat, boolean b, Object... aobjMoreObjectsForDebugInfo){
		if(!b){
			throw new DetailedException("NOT "+strDescWhat+"!", aobjMoreObjectsForDebugInfo);
		}
	}

	/**
	 * 
	 * @param objCurrent
	 * @param objNew this one is just for info
	 * @param aobjMoreObjectsForDebugInfo
	 */
	public static void assertNotAlreadySet(Object objCurrent, Object objNew, Object... aobjMoreObjectsForDebugInfo){
		if(objCurrent!=null){
			ArrayList<Object> aobjAll = new ArrayList<Object>();
			aobjAll.add(objCurrent);
			aobjAll.add(objNew);
			aobjAll.addAll(Arrays.asList(aobjMoreObjectsForDebugInfo));
			
			throw new DetailedException("already set", aobjAll.toArray());
		}
	}

	public DetailedException setCauseAndReturnSelf(String strCauseMessage, StackTraceElement[] asteCauseStack) {
		return initCauseAndReturnSelf(strCauseMessage, asteCauseStack);
	}
	/**
	 * this one helps as reads like {@link #initCause(Throwable)}
	 * @param strCauseMessage
	 * @param asteCauseStack
	 * @return
	 */
	public DetailedException initCauseAndReturnSelf(String strCauseMessage, StackTraceElement[] asteCauseStack) {
		Throwable tw = new Throwable(strCauseMessage);
		tw.setStackTrace(asteCauseStack);
		return initCauseAndReturnSelf(tw);
	}

	public DetailedException setCauseAndReturnSelf(Throwable cause) {
		return initCauseAndReturnSelf(cause);
	}
	/**
	 * this one helps as reads like {@link #initCause(Throwable)}
	 * @param cause
	 * @return
	 */
	public DetailedException initCauseAndReturnSelf(Throwable cause) {
		super.initCause(cause);
		return this;
	}

	@Deprecated
	@Override
	public synchronized Throwable initCause(Throwable cause) {
		throw new NullPointerException("do not use, just to avoid ignoring the more useful ones");
	}
	public static String getExitErrorMessage() {
		return strErrorMessage;
	}

	public static void performBugTrackChecks() {
		chkprb.performChecks(getExitErrorMessage(), getExitRequestCause());
	}
}

