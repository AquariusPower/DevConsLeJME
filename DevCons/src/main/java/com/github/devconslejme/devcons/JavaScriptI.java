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

import java.awt.Desktop;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URISyntaxException;
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
import com.github.devconslejme.misc.AutoCompleteI.AutoCompleteResult;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.IGlobalAddListener;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.ReportI;
import com.google.common.collect.HashBiMap;
import com.google.common.primitives.Primitives;
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
	WriterCapture wrc = new WriterCapture();
	private static String	strCmdChar = "/";
	
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
	
	private void showJavadoc(String strParams) {
		int iComment=strParams.indexOf("//");
		if(iComment>-1)strParams=strParams.substring(0, iComment);
		strParams=strParams.trim();
		
		String strURI="";
		URI uri=null;
		try {
			String[] astr=strParams.split("[.]");
			String strBind=astr[0];
			String strTag = null;
			if(astr.length>1)strTag=astr[1];
			Object objBind = bndJSE.get(strBind);
//			strURI+="./";
//			strURI+=objBind.getClass().getPackage().getName().replace(".","/");
			if(objBind!=null){
				strURI+="doc/";
				strURI+=objBind.getClass().getName().replace(".","/");
				strURI+=".html";
				uri = new File(strURI).toURI();
				if(strTag!=null){
					for(Method m:objBind.getClass().getMethods()){
						if(strTag.equals(getFilteredHelpFromMethod(objBind, m, true, true))){ //compare with cleaned param type mode
							strTag=getFilteredHelpFromMethod(objBind, m, false, true); //collect the with the full param type mode
							break;
						}
					}
					
					int iL=strTag.indexOf("(");
					String strMethod=strTag.substring(0, iL);
					String strParamTypes=strTag.substring(iL+1, strTag.length()-1);
					String[] astrPT=strParamTypes.split("[,]");
					strParamTypes="";
					for(String strPT:astrPT){
						if(!strParamTypes.isEmpty())strParamTypes+="-";
						if(strPT.contains(".")){
							strParamTypes+=Class.forName(strPT).getName();
						}else{
							strParamTypes+=strPT; //primitives has no dots (wrappers does)
						}
					}
//					astr=strTag.split("[(]")[0]
//					String strMethod=
//					strTag=strTag.replace("(", "-");
//					strTag=strTag.replace(")", "-");
					strURI=uri.toString()+"#"+strMethod+"-"+strParamTypes+"-";
					uri=new URI(strURI);
				}
			}else{
				strURI+=strParams;
				uri = new URI(strURI);
			}
			Desktop.getDesktop().browse(uri);
		} catch (IOException|URISyntaxException | ClassNotFoundException e) {
			LoggingI.i().logExceptionEntry(e, "URI='"+strURI+"'");//, strURI, uri);
		}
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
		astr.addAll(EBaseCommand.valuesAsHelp());
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
			if(isCanUserTypeIt(obj)){ // simple types result in simple and readable strings
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
		
//		amLastReturnValueMethods.clear();
//		amLastReturnValueMethods.addAll(getAllMethodsFrom(obj));
		
		for(Method m:obj.getClass().getMethods()){
			String str = getFilteredHelpFromMethod(obj, m, true, false);
			if(str!=null)LoggingI.i().logSubEntry(str);
		}
	}
	
	public ArrayList<String> getJSClassBindListFilteredHelp(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Entry<String, Object> entry:bndJSE.entrySet()){
			Object objJSBindValue = entry.getValue();
			for(Method m:entry.getValue().getClass().getMethods()){
				String str=getFilteredHelpFromMethod(objJSBindValue,m,true, false);
				if(str!=null){
					astr.add(str);
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
	public String getFilteredHelpFromMethod(Object obj, Method m, boolean bUseSimpleParamTypeName, boolean bOnlyMethodAndParamTypes){
		String strConcreteClassSName = obj.getClass().getSimpleName();
		
		String strM = "";
		String strDeclClassSName=m.getDeclaringClass().getSimpleName();
		
		if(!bOnlyMethodAndParamTypes){
			strM+=strConcreteClassSName+".";
		}
		strM+=m.getName();
		
		strM+="(";
		String strP="";
		boolean bHasNonUserTypeableParam = false;
		for(Class<?> p:m.getParameterTypes()){
			if(!isCanUserTypeIt(p))bHasNonUserTypeableParam=true;
			if(!strP.isEmpty())strP+=",";
			strP+=bUseSimpleParamTypeName?p.getSimpleName():p.getName();
		}
		strM+=strP+")";
		
		boolean bIsStatic=Modifier.isStatic(m.getModifiers());
		if(!bOnlyMethodAndParamTypes){
			/**
			 * as comment to be compatible with scripting 
			 */
			strM+=" //"+m.getReturnType().getSimpleName();
			
			if(bIsStatic)strM+=" <STATIC>";
			
			strM+=" <"+strDeclClassSName+">";
		}
		
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
		String strInput= DevConsPluginStateI.i().getInputText();
		strInput=strInput.trim();
		AutoCompleteResult ar = JavaScriptI.i().showHelp(strInput);
		String strNewInput=ar.getImprovedPart();
		if(
				!strInput.isEmpty() && 
				!strInput.startsWith(strCmdChar) && 
				!ar.isPartGotImproved() && //ar.getImprovedPart().equals(strInput) && //so if it was not improved
				!ar.getImprovedPart().contains(".") && //skips an object looking for methods
				!ar.isImprovedAnExactMatch() && //				ar.getResultList().size()==2
				!ar.isImprovedAnExactMatch() && 
				ar.getResultList().size()==1
		){ //nothing changed
			strNewInput = JavaScriptI.i().showHelp(strCmdChar+strInput).getImprovedPart();
		}
		
//		String str = strImprovedPart;
		int i = strNewInput.indexOf("//");if(i>-1)strNewInput=strNewInput.substring(0,i).trim()+" "; //remove trailing comment
		DevConsPluginStateI.i().setInputText(strNewInput);
		
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

	@Override
	public void globalAddedEvent(Object objInst) {
		setJSBinding(objInst);
	}
}
