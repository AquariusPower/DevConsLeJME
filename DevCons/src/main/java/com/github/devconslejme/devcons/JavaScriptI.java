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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.github.devconslejme.misc.AutoCompleteI;
import com.github.devconslejme.misc.AutoCompleteI.AutoCompleteResult;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.IGlobalAddListener;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavadocI;
import com.github.devconslejme.misc.JavadocI.MethodHelp;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.ReportI;
import com.google.common.collect.HashBiMap;
import com.jme3.input.KeyInput;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaScriptI implements IGlobalAddListener {
	public static JavaScriptI i(){return GlobalManagerI.i().get(JavaScriptI.class);}
	
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
	WriterCapture wrc = new WriterCapture();
	private String	strCmdChar = "/";
	private HashMap<Object,ArrayList<MethodHelp>> hmMethodsHelp = new HashMap<Object,ArrayList<MethodHelp>>();
	
	enum EJSObjectBind {
		selfScript,
		;
		public String s(){return toString();}
	}
	
	enum EBaseCommand{
		clear("clears the console log (not the log file)"),
		echo("print some text on the console log"),
		exec("[file] runs a script file, mainly at "+DevConsPluginStateI.i().getStorageFolder()),
		help("[filter]"),
		javadoc("[filter]"),
		history("[filter] show commands history"),
		ini("append to user init file"),
		kill("kill running queue by UId"),
		showIni,
		;
		
		EBaseCommand(){}
		EBaseCommand(String str){
			this.strInfo=str;
		}
		
		private String strInfo = "";
		
		public String s(){return toString();}
	}
	
	public String[] getBaseCommandsValuesAsHelp() {
		String[] astrBaseCommands = new String[EBaseCommand.values().length];
		int i=0;
		for(EBaseCommand ebc:EBaseCommand.values()){
			astrBaseCommands[i++]=(strCmdChar+ebc.s()+" // "+ebc.strInfo+" ("+EBaseCommand.class.getSimpleName()+")");
		}
		return astrBaseCommands;
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
			case javadoc:
				showJavadoc(strParams);
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
				execFileAndShowRetVal(strParams);
				return true;
			case showIni:
				LoggingI.i().logMarker("Showing User Init");
				LoggingI.i().logEntry(flUserInit.getAbsolutePath());
				for(String str:JavaScriptI.i().getUserInit()){
					LoggingI.i().logEntry(str);
				}
				return true;
		}
		
		return false;
	}
	
	private void showJavadoc(String strFullMethodHelp) {
		MethodHelp mh = retrieveMethodHelp(strFullMethodHelp);
		if(mh!=null){
			LoggingI.i().logEntry("Externally browsing javadoc for: "+mh.getFullHelp(false,false));
			JavadocI.i().browseJavadoc(mh);
		}
	}
	
	private String takeOnlyWhatMatters(String str){
		str=str.trim();
		
		// remove comments
		int iComment = str.indexOf("//");
		if(iComment>-1)str=str.substring(0,iComment);
		
		return str.trim();
	}
	
	private boolean cmpEqualsOnlyWhatMatters(String strA,String strB){
		return takeOnlyWhatMatters(strA).equals(takeOnlyWhatMatters(strB));
	}
	
	public MethodHelp retrieveMethodHelp(String strFullMethodHelp){
//		// remove comments
//		int iComment = strFullMethodHelp.indexOf("//");
//		if(iComment>-1)strFullMethodHelp=strFullMethodHelp.substring(0,iComment);
		strFullMethodHelp=strFullMethodHelp.trim();
		
		// parts
		int i1stDot = strFullMethodHelp.indexOf(".");
		String strBind = i1stDot==-1 ? strFullMethodHelp : strFullMethodHelp.substring(0,i1stDot);
		Object objJSBind = bndJSE.get(strBind);
		
		if(objJSBind==null){
			LoggingI.i().logExceptionEntry(new DetailedException("invalid bind id",strBind), strFullMethodHelp);
			return null;
		}
		
		ArrayList<MethodHelp> amh = retrieveAllMethodsHelpFor(objJSBind);
		if(strFullMethodHelp.contains("(")){ //has method anchor
//		if(i1stDot>-1 && strFullMethodHelp.length()>i1stDot){ //has method anchor
			for(MethodHelp mh:amh){
				if(isMethodMatchesFilter(mh)){
					if(cmpEqualsOnlyWhatMatters(strFullMethodHelp,convertToUserHelp(mh))){
						return mh;
					}
				}
			}
		}
		
		LoggingI.i().logWarn("method not found for: "+strFullMethodHelp);
		return null;
	}
	
	public ArrayList<MethodHelp> retrieveAllMethodsHelpFor(Object obj){
		ArrayList<MethodHelp> amh = hmMethodsHelp.get(obj);
		if(amh==null){
			amh = JavadocI.i().prepareAllMethodsHelp(obj);
			DetailedException.assertIsFalse("empty", amh.size()==0, obj);
			hmMethodsHelp.put(obj,amh);
		}
		return amh;
	}
	
	public String convertToUserHelp(MethodHelp mh){
//		return strCmdChar+EBaseCommand.javadoc+" "+mh.getFullHelp(true, true);
		return mh.getFullHelp(true, true);
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
			if(!s.equals("\n"))LoggingI.i().logEntry("SysO: "+s,false,null);
		}
	}
	
	public void configure() {
		jse  = new ScriptEngineManager().getEngineByMimeType("text/javascript");
		jse.getContext().setWriter(wrc);
		
		bndJSE = jse.createBindings();
//		for(EBaseCommand ebc:EBaseCommand.values()){
//			astrIdList.add(ebc.s());
//		}
		
//		setJSBinding(this);
		/**
		 * add all existing
		 */
		for(Object obj:GlobalManagerI.i().getListCopy()){
			if(bndJSE.get(genKeyFor(obj))==null)setJSBinding(obj);
		}
		
		/**
		 * listen for new ones
		 */
		GlobalManagerI.i().addGlobalAddListener(this);
	}
	
	private String genKeyFor(Object obj){
		return obj.getClass().getSimpleName();
	}
	
	/**
	 * 
	 * @param strBindId
	 * @param objBindValue
	 * @return previous value for id
	 */
	public void setJSBinding(String strBindId, Object objBindValue){
		if(bndJSE.get(strBindId)!=null){
			throw new DetailedException("already set: "+strBindId);
		}
		
		bndJSE.put(strBindId,objBindValue);
		
		MessagesI.i().debugInfo(this,"created JS bind: "+strBindId,objBindValue);
		
//		astrJSClassBindList.add(strBindId);
	}
	/**
	 * 
	 * @param objBindValue automatic id based on object class simple name
	 */
	public void setJSBinding(Object objBindValue){
		setJSBinding(genKeyFor(objBindValue), objBindValue);
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
	public AutoCompleteResult showHelp(String strFilter) {
		LoggingI.i().logMarker("Help for: "+strFilter);
		
		ArrayList<String> astr = new ArrayList<String>();
		astr.addAll(Arrays.asList(getBaseCommandsValuesAsHelp()));
		ArrayList<String> astrBnd = new ArrayList<String>(bndJSE.keySet());
		Collections.sort(astrBnd);
		astr.addAll(astrBnd);
		
		String strImprovedPart=strFilter; //still unmodified
		
//		ArrayList<String> astrResult = new ArrayList<String>(astr);
		AutoCompleteResult ar=new AutoCompleteResult(
			strFilter, strImprovedPart, new ArrayList<String>(astr), false, false);
		if(!strFilter.isEmpty()){
			ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false);
			
//			if(!ar.isPartGotImproved() && ar.getResultList().size()==1){
			if(ar.getResultList().size()==1){
				astr.addAll(getJSClassBindListFilteredHelp());
				
				ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false);
			}
			
//			astrResult = ar.getResultList();
			
//			strImprovedPart = ar.getImprovedPart();
		}
		
//		for(String str:astrResult){
		for(String str:ar.getResultList()){
			LoggingI.i().logSubEntry(str);
		}
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
		
//		return strImprovedPart;
		return ar;
	}
	
	public boolean execFileAndShowRetVal(String strFile){
		boolean b=execFile(asFile(strFile));
		showRetVal(objRetValFile); //this method will be called by the user, so show the return value
		return b;
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
		queueExec(null, flJS, fDelaySeconds, bLoop);
	}
	public void queueExecScript(String strSimpleScript, float fDelaySeconds, boolean bLoop){
		queueExec(strSimpleScript, null, fDelaySeconds, bLoop);
	}
	/**
	 * 
	 * @param strSimpleScript can be null
	 * @param flJS priority if not null
	 * @param fDelaySeconds
	 * @param bLoop
	 */
	private void queueExec(String strSimpleScript, File flJS, float fDelaySeconds, boolean bLoop){
		QueueI.i().enqueue(new CallableX() {
				@Override
				public Boolean call() {
					if(flJS!=null){
						execFile(flJS);
					}else{
						execScript(strSimpleScript, true);
					}
					
					return true;
				}
			}
			.setName(flJS!=null?flJS.getName():"SimpleScript")
			.setDelaySeconds(fDelaySeconds)
			.setLoop(bLoop)
			.setUserCanKill(true)
			.setUserCanPause(true)
		);
	}
	
	/**
	 * kept private to prevent infinite self call on the same frame,
	 * use {@link #queueExecFile(File, float)}
	 * @param rd
	 * @return 
	 */
	private boolean execFile(File flJS){
		try {
			bndJSE.remove(EJSObjectBind.selfScript.s());
			setJSBinding(EJSObjectBind.selfScript.s(), flJS);
//			bndJSE.put(EJSObjectBind.selfScript.s(), flJS);
//			rd.reset();
			objRetValFile = jse.eval(new FileReader(flJS),bndJSE);
			return true;
		} catch (ScriptException | IOException e) {
//			LoggingI.i().logExceptionEntry(e, hmFileReaderExecJS.inverse().get(rd));
			LoggingI.i().logExceptionEntry(e, flJS.toString());
		}
		
		return false;
	}
	
	public boolean execScript(String strJS,boolean bShowRetVal){
		try {
			objRetValUser=jse.eval(strJS,bndJSE);
			if(bShowRetVal)showRetVal(objRetValUser);
			return true;
		} catch (ScriptException e) {
			LoggingI.i().logExceptionEntry(e, strJS);
		} catch (Exception e){
			LoggingI.i().logExceptionEntry(e, strJS);
		}
		
		return false;
	}
	
	public void showRetVal(Object obj){
		if(obj==null){
			LoggingI.i().logSubEntry("Return is null or void.");
		}else{
//				LoggingI.i().logSubEntry("ReturnType: "+objJSLastEval.toString()+" ("+objJSLastEval.getClass()+")");
			LoggingI.i().logSubEntry("Return type: "+obj.getClass());
			if(JavaLangI.i().isCanUserTypeIt(obj)){ // simple types result in simple and readable strings
				LoggingI.i().logSubEntry("Return value = '"+obj+"'");
			}else
			if(!isAndShowArray(obj)){
				showMethods(obj);
			}
		}
	}
	
	private boolean isAndShowArray(Object objValue){
		if(JavaLangI.i().getArrayTypeFor(objValue)==null)return false;
		
		ArrayList<String> astr = ReportI.i().prepareReportLines("Return array values:", objValue);
		for(String str:astr)LoggingI.i().logSubEntry(str);
		
		return true;
	}
	
	private void showMethods(Object obj){
		LoggingI.i().logSubEntry("Accessible Methods:");
		for(MethodHelp mh:retrieveAllMethodsHelpFor(obj)){
			if(isMethodMatchesFilter(mh)){
				LoggingI.i().logSubEntry(convertToUserHelp(mh));
			}
		}
	}
	
	public ArrayList<String> getJSClassBindListFilteredHelp(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Entry<String, Object> entry:bndJSE.entrySet()){
			Object objJSBindValue = entry.getValue();
			for(MethodHelp mh:retrieveAllMethodsHelpFor(objJSBindValue)){
				if(isMethodMatchesFilter(mh)){
					astr.add(convertToUserHelp(mh));
				}
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
	public boolean isMethodMatchesFilter(MethodHelp mh){
		if(
				isShowAllPublicMembers() ||
				(
					!mh.isStatic() &&
					mh.getNonUserTypeableParamsCount()==0 &&
					// to skip all "user typeable super class type" methods to avoid "useless" cluttering (like Boolean.class methods)
					!JavaLangI.i().isCanUserTypeIt(mh.getDeclaring()) && 
					!mh.getDeclaring().equals(Object.class)
				)
		){
			return true;
		}
		
		return false;
	}
	
	public boolean isShowAllPublicMembers() {
		return bShowAllPublicMembers;
	}

	public void setShowAllPublicMembers(boolean bShowAllPublicMembers) {
		this.bShowAllPublicMembers = bShowAllPublicMembers;
	}

	public void init() {
		// restore cmd history
		flCmdHistory = new File(DevConsPluginStateI.i().getStorageFolder(),"CommandsHistory.log");
		astrCmdHistory.add(""); //just to avoid empty list when adding new cmd to it
		if(flCmdHistory.exists()){
			for(String str:FileI.i().readAllLines(flCmdHistory)){
				astrCmdHistory.add(str);
			}
			iNavigateCmdHistoryIndex=astrCmdHistory.size()-1;
		}
		
		// load user init cmds
		flUserInit = new File(DevConsPluginStateI.i().getStorageFolder(),"UserInit.cfg");
		if(flUserInit.exists()){
			for(String str:getUserInit()){
				astrUserInit.add(str);
			}
			
			QueueI.i().enqueue(new CallableX() {
				@Override
				public Boolean call() {
					processUserInit();
					return true;
				}
			});
		}else{
			FileI.i().appendLine(flUserInit, "// put initialization commands on this file, one per line");
		}
	}
	
	public void processUserInit() {
		if(astrUserInit.size()>0){
			LoggingI.i().logMarker("UserInit:Begin");
			boolean bFail=false;
			for(String strJS:astrUserInit){
				LoggingI.i().logSubEntry(strJS);
				if(!execScript(strJS,false)){
					bFail=true;
					LoggingI.i().logMarker("UserInit:FAIL");
					break;
				}
			}
			astrUserInit.clear();
			if(!bFail)LoggingI.i().logMarker("UserInit:End");
		}
	}
	
	public List<String> getUserInit(){
		return FileI.i().readAllLines(flUserInit);
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
		String strInputTilCarat=DevConsPluginStateI.i().getInputTextBeforeCarat(null);
		int iInitialLength = strInputTilCarat.length();
//		String strInput= DevConsPluginStateI.i().getInputText();
		strInputTilCarat=strInputTilCarat.trim();
		AutoCompleteResult ar = JavaScriptI.i().showHelp(strInputTilCarat);
		String strNewInput=ar.getImprovedPart();
		if( // is to auto guess and complete with a base command?
				!strInputTilCarat.isEmpty() && 
				!strInputTilCarat.startsWith(strCmdChar) && 
				!ar.isPartGotImproved() && //ar.getImprovedPart().equals(strInput) && //so if it was not improved
				!ar.getImprovedPart().contains(".") && //skips an object looking for methods
				!ar.isImprovedAnExactMatch() && 
				ar.getResultList().size()==1
		){ //if nothing changed, try again as base command
			AutoCompleteResult arTryAgain = JavaScriptI.i().showHelp(strCmdChar+strInputTilCarat);
			if(arTryAgain.isPartGotImproved()){
				strNewInput = arTryAgain.getImprovedPart();
				ar = arTryAgain;
			}
		}
		
//		int i = strNewInput.indexOf("//");if(i>-1)strNewInput=strNewInput.substring(0,i).trim()+" "; //remove trailing comments
		int i = strNewInput.indexOf("//");if(i>-1)strNewInput=strNewInput.substring(0,i).trim(); //remove trailing comments
//		DevConsPluginStateI.i().setInputText(strNewInput);
		if(ar.isImprovedAnExactMatch())strNewInput+=" ";
		DevConsPluginStateI.i().insertAtInputTextCaratPos(
				strNewInput,
				iInitialLength);
		
		addCmdToHistory(strInputTilCarat);
		if(ar.isPartGotImproved())addCmdToHistory(strNewInput);
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
	}

	public void autoCompleteWord() {
		// boolean
		String str=DevConsPluginStateI.i().getInputLettersBeforeCarat();
		if(Boolean.TRUE.toString().startsWith(str)){ //actually lowercase
			DevConsPluginStateI.i().insertAtInputTextCaratPos(Boolean.TRUE.toString().substring(str.length()),null);
		}else
		if(Boolean.FALSE.toString().startsWith(str)){ //actually lowercase
			DevConsPluginStateI.i().insertAtInputTextCaratPos(Boolean.FALSE.toString().substring(str.length()),null);
		}
	}

	@Override
	public void globalAddedEvent(Object objInst) {
		setJSBinding(objInst);
	}

}
