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

package com.github.devconslejme.devcons;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.github.devconslejme.misc.AutoCompleteI;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.AutoCompleteI.AutoCompleteResult;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.JavaLangI;
import com.google.common.collect.HashBiMap;
import com.google.common.primitives.Primitives;
import com.jme3.input.KeyInput;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaScriptI {
	public static JavaScriptI i(){return GlobalInstanceManagerI.i().get(JavaScriptI.class);}
	
	private Object	objRetValUser;
	private Object	objRetValFile;
	private ScriptEngine	jse;
	private Bindings	bndJSE;
//	private ArrayList<String> astrJSClassBindList = new ArrayList<String>();
	private boolean bShowAllPublicMembers = false;
	private Method[] amLastReturnValue;
	private HashMap<Object,Method>	hmAllJSClassBindMethods = new HashMap<Object,Method>();
//	private ArrayList<Method>	amLastReturnValueMethods = new ArrayList<Method>();
	private ArrayList<String> astrCmdHistory = new ArrayList<String>();
	private ArrayList<String> astrUserInit = new ArrayList<String>();
	private File	flCmdHistory;
	private File	flUserInit;
	private int iNavigateCmdHistoryIndex = 0;
	private HashBiMap<String,File> hmFileReaderExecJS = HashBiMap.create(); 
	
	enum EJSObjectBind {
		selfScript,
		;
		public String s(){return toString();}
	}
	
	enum EBaseCommand{
		help("[filter]"),
		
		ini("append to user init file"),
		
		showIni,
		
		exec("[file] runs a script file, mainly at "+DevConsPluginStateI.i().getStorageFolder()),
		
		clear("clears the console log (not the log file)"),
		
		echo("print some text on the console log"),
		
		kill("kill running queue by UId"),
		
		history("[filter] show commands history"),
		;
		
		EBaseCommand(){}
		EBaseCommand(String str){
			this.strInfo=str;
		}
		
		private String strInfo = "";
		
		public String s(){return toString();}
		
		private static ArrayList<String> astr = new ArrayList<String>(); 
		static{
			for(EBaseCommand ebc:EBaseCommand.values()){
				astr.add(strCmdChar+ebc.s()+" // "+ebc.strInfo+" ("+EBaseCommand.class.getSimpleName()+")");
			}
		}
		
		public static Collection<? extends String> valuesAsHelp() {
			return astr;
		}
	}
	
	public boolean isAndExecBaseCommand(String strCmd){
		strCmd=strCmd.trim();
		if(strCmd.startsWith(strCmdChar)){
			strCmd=strCmd.substring(1);
		}else{
			return false;
		}
		
		String[] astr = strCmd.split(" ");
		String strBase = astr[0];
		String strParams = "";
		if(!strCmd.equals(strBase))strParams=strCmd.substring(strBase.length()+1);
		strParams=strParams.trim();
		
		if(strParams.startsWith("//"))strParams=""; //remove comments
		
		EBaseCommand ebc = null;
		try{
			ebc=EBaseCommand.valueOf(strBase);
		}catch(IllegalArgumentException e){
			//ignore
		}
		
		if(ebc==null)return false;
		
		switch(ebc){
			case help:
				showHelp(strParams);
				return true;
			case clear:
				LoggingI.i().clear();
				return true;
			case echo:
				LoggingI.i().logEntry(strParams);
				return true;
			case history:
				JavaScriptI.i().showHistory(strParams);
				return true;
			case kill:
				QueueI.i().kill(strParams);
				return true;
			case ini:
				appendUserInitCommand(strParams);
				return true;
			case exec:
				execFile(strParams);
				return true;
			case showIni:
				LoggingI.i().logMarker("Showing User Init");
				for(String str:JavaScriptI.i().getUserInit()){
					LoggingI.i().logEntry(str);
				}
				return true;
		}
		
		return false;
	}
	
	public void showHistory(String strParams) {
		String strPrev = "";
		for(String str:astrCmdHistory){
			if(strParams.isEmpty() || str.toLowerCase().contains(strParams)){
				if(!strPrev.equals(str)){
					LoggingI.i().logSubEntry(str);
					strPrev=str;
				}
			}
		}
		DevConsPluginStateI.i().scrollKeepAtBottom();
	}

	/**
	 * based on: jse.getContext().getWriter() type
	 */
	public static class WriterCapture extends PrintWriter{
		public WriterCapture() {
			super(System.out, true);
		}
		
		@Override
		public void write(String s, int off, int len) {
			super.write(s, off, len);
			if(!s.equals("\n"))LoggingI.i().logEntry("SysO: "+s,false,true);
		}
	}
	WriterCapture wrc = new WriterCapture();
	private static String	strCmdChar = "/";
	
	public void configure() {
		jse  = new ScriptEngineManager().getEngineByMimeType("text/javascript");
		jse.getContext().setWriter(wrc);
		
		bndJSE = jse.createBindings();
//		for(EBaseCommand ebc:EBaseCommand.values()){
//			astrIdList.add(ebc.s());
//		}
		
		setJSBinding(this);
	}
	
	/**
	 * 
	 * @param strBindId
	 * @param objBindValue
	 * @return previous value for id
	 */
	public void setJSBinding(String strBindId, Object objBindValue){
		if(bndJSE.put(strBindId,objBindValue) != null){
			throw new NullPointerException("already set: "+strBindId);
		}
		
//		astrJSClassBindList.add(strBindId);
	}
	/**
	 * 
	 * @param objBindValue automatic id based on object class simple name
	 */
	public void setJSBinding(Object objBindValue){
		setJSBinding(objBindValue.getClass().getSimpleName(), objBindValue);
	}
	protected void submitUserCommand() {
		String strJS = DevConsPluginStateI.i().getInputText();
		
		strJS=strJS.trim();
		if(strJS.isEmpty())return;
		
		addCmdToHistory(strJS);
		
		LoggingI.i().logMarker("User Command");
		LoggingI.i().logEntry(strJS);
		
		if(!isAndExecBaseCommand(strJS)){
			execScript(strJS,true);
		}
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
		DevConsPluginStateI.i().clearInput();
	}
	
	/**
	 * Initially, a short help will be shown with {@link EBaseCommand} and JS bindings.
	 * Only if a full JS class bind id is the filter, its methods will be shown. 
	 * @param strFilter
	 * @return
	 */
	public String showHelp(String strFilter) {
		LoggingI.i().logMarker("Help for: "+strFilter);
		
		ArrayList<String> astr = new ArrayList<String>();
		astr.addAll(EBaseCommand.valuesAsHelp());
		ArrayList<String> astrBnd = new ArrayList<String>(bndJSE.keySet());
		Collections.sort(astrBnd);
		astr.addAll(astrBnd);
		
		String strImprovedPart=strFilter; //still unmodified
		
		ArrayList<String> astrResult = astr;
		if(!strFilter.isEmpty()){
			AutoCompleteResult ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false);
			
//			if(!ar.isPartGotImproved() && ar.getResultList().size()==1){
			if(ar.getResultList().size()==1){
				astr.addAll(getJSClassBindListFilteredHelp());
				
				ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false);
			}
			
			astrResult = ar.getResultList();
			
			strImprovedPart = ar.getImprovedPart();
		}
		
		for(String str:astrResult){
			LoggingI.i().logSubEntry(str);
		}
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
		
		return strImprovedPart;
	}
	
	public void execFile(String strFile){
		execFile(asFile(strFile));
		showRetVal(objRetValFile); //this method will be called by the user, so show the return value
	}
	public File asFile(String strFile){
		if(strFile.isEmpty()){
			LoggingI.i().logWarn("missing file param");
			return null;
		}
		
		File flJS = null;
		try {
			flJS = new File(strFile); //some absolute location
			if(!flJS.exists()){ 
				//relative to storage
				flJS = new File(DevConsPluginStateI.i().getStorageFolder(),strFile);
			}
			
			if(!flJS.exists()){
				flJS.createNewFile();
				FileI.i().appendLine(flJS, "//Classes: "+bndJSE.keySet());
				FileI.i().appendLine(flJS, "//"+EJSObjectBind.class.getSimpleName()+": "+Arrays.toString(EJSObjectBind.values()));
				LoggingI.i().logWarn("created file "+flJS.getAbsoluteFile());
				return null;
			}
			
			if(hmFileReaderExecJS.get(flJS.getCanonicalPath())==null){
				hmFileReaderExecJS.put(flJS.getCanonicalPath(), flJS);
			}
//			File fl = hmFileReaderExecJS.get(flJS.getCanonicalPath());
//			if(fl==null){
////				rd=Files.newReader(flJS, StandardCharsets.UTF_8);
////				rd=new FileReader(flJS);
//				hmFileReaderExecJS.put(flJS.getCanonicalPath(), rd);
//			}
			
			return flJS;
		} catch (IOException e) {
			LoggingI.i().logExceptionEntry(e, flJS.getName());
		}
		
		return null;
	}
	
	public void queueExecFile(String strFile, float fDelaySeconds, boolean bLoop){
		queueExecFile(asFile(strFile), fDelaySeconds, bLoop);
	}
	public void queueExecFile(File flJS, float fDelaySeconds, boolean bLoop){
		QueueI.i().enqueue(new CallableX(flJS.getName(),fDelaySeconds,bLoop) {
				@Override
				public Boolean call() {
					execFile(flJS);
					return true;
				}
			}
			.setUserCanKill(true)
			.setUserCanPause(true)
		);
	}
	
	/**
	 * kept private to prevent infinite self call on the same frame,
	 * use {@link #queueExecFile(File, float)}
	 * @param rd
	 */
	private void execFile(File flJS){
		try {
			bndJSE.put(EJSObjectBind.selfScript.s(), flJS);
//			rd.reset();
			objRetValFile = jse.eval(new FileReader(flJS),bndJSE);
		} catch (ScriptException | IOException e) {
//			LoggingI.i().logExceptionEntry(e, hmFileReaderExecJS.inverse().get(rd));
			LoggingI.i().logExceptionEntry(e, flJS.toString());
			return;
		}
	}
	
	public void execScript(String strJS,boolean bShowRetVal){
		try {
			objRetValUser=jse.eval(strJS,bndJSE);
			if(bShowRetVal)showRetVal(objRetValUser);
		} catch (ScriptException e) {
			LoggingI.i().logExceptionEntry(e, strJS);
		} catch (Exception e){
			LoggingI.i().logExceptionEntry(e, strJS);
		}
	}
	
	public void showRetVal(Object obj){
		if(obj==null){
			LoggingI.i().logSubEntry("Return is null or void.");
		}else{
//				LoggingI.i().logSubEntry("ReturnType: "+objJSLastEval.toString()+" ("+objJSLastEval.getClass()+")");
			LoggingI.i().logSubEntry("Return type: "+obj.getClass());
			if(isCanUserTypeIt(obj)){ // simple types result in simple and readable strings
				LoggingI.i().logSubEntry("Return value = '"+obj+"'");
			}else
			if(!isAndShowArray(obj)){
				showMethods(obj);
			}
		}
	}
	
	private boolean isAndShowArray(Object objValue){
		Object[][] aaobjKeyValue = JavaLangI.i().convertToKeyValueArray(objValue);
		if(aaobjKeyValue==null)return false;
		
		LoggingI.i().logSubEntry("Return array values:");
		for(int i=0;i<aaobjKeyValue.length;i++){
			String strLog = "["+aaobjKeyValue[i][0]+"]='"+aaobjKeyValue[i][1]+"'";
			LoggingI.i().logSubEntry(strLog);
		}
		
		return true;
	}
	
	private void showMethods(Object obj){
		LoggingI.i().logSubEntry("Accessible Methods:");
		
//		amLastReturnValueMethods.clear();
//		amLastReturnValueMethods.addAll(getAllMethodsFrom(obj));
		
		for(Method m:obj.getClass().getMethods()){
			String str = getFilteredHelpFromMethod(obj, m);
			if(str!=null)LoggingI.i().logSubEntry(str);
		}
	}
	
	public ArrayList<String> getJSClassBindListFilteredHelp(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Entry<String, Object> entry:bndJSE.entrySet()){
			Object obj = entry.getValue();
			for(Method m:entry.getValue().getClass().getMethods()){
				String str=getFilteredHelpFromMethod(obj,m);
				if(str!=null)astr.add(str);
			}
		}
		Collections.sort(astr);
		return astr;
	}
	
	/**
	 * 
	 * @param obj
	 * @param m
	 * @return null if did not match filters
	 */
	public String getFilteredHelpFromMethod(Object obj, Method m){
		String strConcreteClassSName = obj.getClass().getSimpleName();
		
		String strM = "";
		String strDeclClassSName=m.getDeclaringClass().getSimpleName();
		
		strM+=strConcreteClassSName+"."+m.getName();
		
		strM+="(";
		String strP="";
		boolean bHasNonUserTypeableParam = false;
		for(Class<?> p:m.getParameterTypes()){
			if(!isCanUserTypeIt(p))bHasNonUserTypeableParam=true;
			if(!strP.isEmpty())strP+=",";
			strP+=p.getSimpleName();
		}
		strM+=strP+")";
		
		/**
		 * as comment to be compatible with scripting 
		 */
		strM+=" //"+m.getReturnType().getSimpleName();
		
		boolean bIsStatic=false;
		if(Modifier.isStatic(m.getModifiers())){
			strM+=" <STATIC>";
			bIsStatic=true;
		}
		
		strM+=" <"+strDeclClassSName+">";
		
		if(
				isShowAllPublicMembers() ||
				(
					!bIsStatic &&
					!bHasNonUserTypeableParam &&
					!isCanUserTypeIt(m.getDeclaringClass()) && // to show methods only for non primitive/simple/basic types
					!m.getDeclaringClass().equals(Object.class)
				)
		){
			return strM;
		}
		
		return null;
	}
	
	public boolean isShowAllPublicMembers() {
		return bShowAllPublicMembers;
	}

	public void setShowAllPublicMembers(boolean bShowAllPublicMembers) {
		this.bShowAllPublicMembers = bShowAllPublicMembers;
	}

	public boolean isCanUserTypeIt(Object obj){
		return isCanUserTypeIt(obj.getClass());
	}
	public boolean isCanUserTypeIt(Class cl){
		if(String.class==cl)return true;
		
		if(cl.isPrimitive())return true;
		if(Primitives.isWrapperType(cl))return true; //last as is probably "slower"
		
		return false;
	}

	public void init() {
		flCmdHistory = new File(DevConsPluginStateI.i().getStorageFolder(),"CommandsHistory.log");
		astrCmdHistory.add(""); //just to avoid empty list when adding new cmd to it
		if(flCmdHistory.exists()){
			for(String str:FileI.i().readAllLines(flCmdHistory)){
				astrCmdHistory.add(str);
			}
			iNavigateCmdHistoryIndex=astrCmdHistory.size()-1;
		}
		
		flUserInit = new File(DevConsPluginStateI.i().getStorageFolder(),"UserInit.cfg");
		if(flUserInit.exists()){
			for(String str:getUserInit()){
				astrUserInit.add(str);
			}
		}else{
			FileI.i().appendLine(flUserInit, "// put initialization commands on this file, one per line");
		}
	}
	
	public List<String> getUserInit(){
		return FileI.i().readAllLines(flUserInit);
	}
	
	public void update() {
		if(astrUserInit.size()>0){
			LoggingI.i().logMarker("UserInit:Begin");
			for(String strJS:astrUserInit){
				LoggingI.i().logSubEntry(strJS);
				execScript(strJS,false);
			}
			astrUserInit.clear();
			LoggingI.i().logMarker("UserInit:End");
		}
	}
	
	public void addCmdToHistory(String strJS) {
		strJS=strJS.trim();
		if(strJS.isEmpty())return;
		
		// ignores equals to last cmd
		boolean b = astrCmdHistory.get(astrCmdHistory.size()-1).equals(strJS);
		if(!b){
			astrCmdHistory.add(strJS);
			FileI.i().appendLine(flCmdHistory, strJS);
		}
		
		// reset navigator index
		iNavigateCmdHistoryIndex=astrCmdHistory.size();
	}
	
	public ArrayList<String> getCmdHistory(){
		return astrCmdHistory;
	}

	protected void navigateCmdHist(int keyCode) {
		switch(keyCode){
			case KeyInput.KEY_UP:
				iNavigateCmdHistoryIndex--;
				break;
			case KeyInput.KEY_DOWN:
				iNavigateCmdHistoryIndex++;
				break;
		}
		
		if(iNavigateCmdHistoryIndex<0){
			iNavigateCmdHistoryIndex=0;
		}
		
		if(iNavigateCmdHistoryIndex>(astrCmdHistory.size()-1)){
			iNavigateCmdHistoryIndex=astrCmdHistory.size()-1;
		}
		
		DevConsPluginStateI.i().setInputText(astrCmdHistory.get(iNavigateCmdHistoryIndex));
	}
	
	public void appendUserInitCommand(String strJS){
//		if(strJS.startsWith(EBaseCommand.ini.s()+" ")){
//			strJS=strJS.substring(EBaseCommand.ini.s().length()+1);
			FileI.i().appendLine(flUserInit, strJS);
//			return true;
//		}
//		return false;
			LoggingI.i().logMarker("Appended User Init Cmd");
	}

	protected void autoComplete() {
		String strInput= DevConsPluginStateI.i().getInputText();
		strInput=strInput.trim();
		String strImprovedPart = JavaScriptI.i().showHelp(strInput);
		if(
				!strInput.isEmpty() && 
				!strInput.startsWith(strCmdChar) && 
				strImprovedPart.equals(strInput) &&
				!strImprovedPart.endsWith(".") //skips an object looking for methods
		){ //nothing changed
			strImprovedPart = JavaScriptI.i().showHelp(strCmdChar+strInput);
		}
		
		String str = strImprovedPart;
		int i = str.indexOf("//");if(i>-1)str=str.substring(0,i).trim()+" "; //remove trailing comment
		DevConsPluginStateI.i().setInputText(str);
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
	}

	public void autoCompleteWord() {
		// boolean
		String str=DevConsPluginStateI.i().getInputLettersBeforeCarat();
		if(Boolean.TRUE.toString().startsWith(str)){
			DevConsPluginStateI.i().insertAtInputTextCaratPos(Boolean.TRUE.toString().substring(str.length()));
		}else
		if(Boolean.FALSE.toString().startsWith(str)){
			DevConsPluginStateI.i().insertAtInputTextCaratPos(Boolean.FALSE.toString().substring(str.length()));
		}
	}
}
