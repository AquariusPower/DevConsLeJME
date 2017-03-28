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

package com.github.devconslejme;

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
import java.util.List;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.github.devconslejme.QueueI.CallableX;
import com.github.devconslejme.misc.AutoCompleteI;
import com.github.devconslejme.misc.AutoCompleteI.AutoCompleteResult;
import com.github.devconslejme.misc.JavaLangI;
import com.google.common.collect.HashBiMap;
import com.google.common.primitives.Primitives;
import com.jme3.input.KeyInput;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaScriptI {
	private static JavaScriptI instance = new JavaScriptI();
	/**instance*/ public static JavaScriptI i(){return instance;}
	
//	static {
//		instance.setJSBinding(instance);
//	}
	
	private Object	objRetValUser;
	private Object	objRetValFile;
	private ScriptEngine	jse;
	private Bindings	bndJSE;
	private ArrayList<String> astrJSClassBindList = new ArrayList<String>();
	private boolean bShowAllPublicMembers = false;
	private Method[] amLastReturnValue;
	private ArrayList<String>	astrLastReturnValueMethods = new ArrayList<String>();
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
		
		exec("[file] runs a script file, mainly at "+ConsolePluginI.i().getStorageFolder()),
		
		clear("clears the console log (not the log file)"),
		
		echo("print some text on the console log"),
		
		kill("kill running queue by UId"),
		;
		
		EBaseCommand(){}
		EBaseCommand(String str){
			this.strInfo=str;
		}
		
		private String strInfo = "";
		
		public String s(){return toString();}
		
		private static ArrayList<String> astr = new ArrayList<String>(); 
		static{
//			if(astr.size()==0){
				for(EBaseCommand ebc:EBaseCommand.values()){
					astr.add(ebc.s()+" // "+ebc.strInfo+" ("+EBaseCommand.class.getSimpleName()+")");
				}
//			}
		}
		
		public static Collection<? extends String> valuesAsStringArray() {
			return astr;
		}
	}
	
	public boolean isAndExecBaseCommand(String strCmd){
		strCmd=strCmd.trim();
		String[] astr = strCmd.split(" ");
		String strBase = astr[0];
		String strParms = "";
		if(!strCmd.equals(strBase))strParms=strCmd.substring(strBase.length()+1);
		strParms=strParms.trim();
		
		if(strParms.startsWith("//"))strParms=""; //remove comments
		
		EBaseCommand ebc = null;
		try{
			ebc=EBaseCommand.valueOf(strBase);
		}catch(IllegalArgumentException e){
			//ignore
		}
		
		if(ebc==null)return false;
		
		switch(ebc){
			case help:
				showHelp(strParms);
				return true;
			case clear:
				LoggingI.i().clear();
				return true;
			case echo:
				LoggingI.i().logEntry(strParms);
				return true;
			case kill:
				QueueI.i().kill(strParms);
				return true;
			case ini:
				appendUserInitCommand(strParms);
				return true;
			case exec:
				execFile(strParms);
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
		
		astrJSClassBindList.add(strBindId);
	}
	/**
	 * 
	 * @param objBindValue automatic id based on object class simple name
	 */
	public void setJSBinding(Object objBindValue){
		setJSBinding(objBindValue.getClass().getSimpleName(), objBindValue);
	}
	protected void submitUserCommand() {
		String strJS = ConsolePluginI.i().getInputText();
		
		strJS=strJS.trim();
		if(strJS.isEmpty())return;
		
		addCmdToHistory(strJS);
		
		LoggingI.i().logMarker("User Command");
		LoggingI.i().logEntry(strJS);
		
		if(!isAndExecBaseCommand(strJS)){
			execScript(strJS,true);
		}
		
		ConsolePluginI.i().scrollToBottom();
		ConsolePluginI.i().clearInput();
	}
	
	public AutoCompleteResult showHelp(String strFilter) {
//		AutoCompleteResult ar = null;
//		ArrayList<String> astr;
//		if(strFilter.isEmpty()){
//			astr = astrIdList;
//		}else{
//			ar = AutoCompleteI.i().autoComplete(strFilter, astrIdList, false, false);
//			astr = ar.getResultList();
//		}
//		
//		for(String str:astr){
//			LoggingI.i().logSubEntry(str);
//		}
		
		LoggingI.i().logMarker("Help for: "+strFilter);
		
		ArrayList<String> astr = new ArrayList<String>();
//		astr.addAll(Arrays.toString(EBaseCommand.values()));
//		Collections.addAll(astr, Arrays.asList(EBaseCommand.values()).toArray(new String[]{}));
		astr.addAll(EBaseCommand.valuesAsStringArray());
		astr.addAll(astrJSClassBindList);
		astr.addAll(astrLastReturnValueMethods);
		
		AutoCompleteResult ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false);
		for(String str:ar.getResultList()){
			LoggingI.i().logSubEntry(str);
		}
		
		ConsolePluginI.i().scrollToBottom();
		
		return ar;
	}
	
//	public ArrayList<String> getLastReturnValueMethods(){
//		ArrayList<String> astr = new ArrayList<String>();
//		for()
//		amLastReturnValue
//	}
	
//	public static class PubMembers{
//		String strMethod;
//		String strDeclClass;
//		String strConcreteClass;
//	}
//	private ArrayList<PubMembers> apmLastReturnValue = new ArrayList<PubMembers>();
	
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
				flJS = new File(ConsolePluginI.i().getStorageFolder(),strFile);
			}
			
			if(!flJS.exists()){
				flJS.createNewFile();
				FileI.i().appendLine(flJS, "//Classes: "+astrJSClassBindList);
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
		});
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
		}
	}
	
	public void showRetVal(Object obj){
		if(obj==null){
			LoggingI.i().logSubEntry("Return is null");
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
		
		String strConcreteClassSName = obj.getClass().getSimpleName();
		amLastReturnValue = obj.getClass().getMethods();
		astrLastReturnValueMethods.clear();
		for(Method m:amLastReturnValue){
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
			
//			if(!strDeclClassSName.equals(strConcreteClassSName)){
				strM+=" <"+strDeclClassSName+">";
//			}
			
			if(
					isShowAllPublicMembers() ||
					(
						!bIsStatic &&
						!bHasNonUserTypeableParam &&
						!isCanUserTypeIt(m.getDeclaringClass()) && // will show methods for non simple types
						!m.getDeclaringClass().equals(Object.class)
					)
			){
				astrLastReturnValueMethods.add(strM);
			}
		}
		
		Collections.sort(astrLastReturnValueMethods);
		
		for(String str:astrLastReturnValueMethods)LoggingI.i().logSubEntry(str);
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
		
//		String str = cl.getSimpleName();
//		
//		if(str.equals(Object.class.getSimpleName()))return true;
//		
//		if(str.equals(Character.class.getSimpleName()))return true;
//		if(str.equals(CharSequence.class.getSimpleName()))return true;
//		if(str.equals(String.class.getSimpleName()))return true;
//		
//		if(str.equals(Integer.class.getSimpleName()))return true;
//		if(str.equals(Long.class.getSimpleName()))return true;
//		if(str.equals(Float.class.getSimpleName()))return true;
//		if(str.equals(Double.class.getSimpleName()))return true;
//		
//		if(str.equals(Boolean.class.getSimpleName()))return true;
		
//		System.out.println(cl.getSimpleName());
		return false;
	}

	public void init() {
		flCmdHistory = new File(ConsolePluginI.i().getStorageFolder(),"CommandsHistory.log");
		astrCmdHistory.add(""); //just to avoid empty list when adding new cmd to it
		if(flCmdHistory.exists()){
			for(String str:FileI.i().readAllLines(flCmdHistory)){
				astrCmdHistory.add(str);
			}
			iNavigateCmdHistoryIndex=astrCmdHistory.size()-1;
		}
		
		flUserInit = new File(ConsolePluginI.i().getStorageFolder(),"UserInit.cfg");
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
		
		ConsolePluginI.i().setInputText(astrCmdHistory.get(iNavigateCmdHistoryIndex));
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
		AutoCompleteResult ar = JavaScriptI.i().showHelp(ConsolePluginI.i().getInputText());
		ConsolePluginI.i().setInputText(ar.getImprovedPart());
		ConsolePluginI.i().scrollToBottom();
	}

	public void autoCompleteWord() {
		// boolean
		String str=ConsolePluginI.i().getInputLettersBeforeCarat();
		if(Boolean.TRUE.toString().startsWith(str)){
			ConsolePluginI.i().insertAtInputTextCaratPos(Boolean.TRUE.toString().substring(str.length()));
		}else
		if(Boolean.FALSE.toString().startsWith(str)){
			ConsolePluginI.i().insertAtInputTextCaratPos(Boolean.FALSE.toString().substring(str.length()));
		}
	}
}
