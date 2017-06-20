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
import java.io.Reader;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
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
import com.github.devconslejme.misc.FileI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.GlobalManagerI.IGlobalAddListener;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavadocI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallUserCustomCmd;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.MethodX;
import com.github.devconslejme.misc.PkgCfgAbs;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.ReportI;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.SystemAlertI;
import com.github.devconslejme.misc.TimeFormatI;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.google.common.collect.HashBiMap;
import com.jme3.app.Application;
import com.jme3.input.KeyInput;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaScriptI implements IGlobalAddListener {
	public static JavaScriptI i(){return GlobalManagerI.i().get(JavaScriptI.class);}
	
	private Object	objExecScriptRetVal;
	private Object	objRetValFile;
	private ScriptEngine	jse;
	private Bindings	bndJSE;
//	private ArrayList<String> astrJSClassBindList = new ArrayList<String>();
	private boolean bShowAllPublicMembers = false;
	private Method[] amLastReturnValue;
	private HashMap<Object,Method>	hmAllJSClassBindMethods = new HashMap<Object,Method>();
//	private ArrayList<Method>	amLastReturnValueMethods = new ArrayList<Method>();
	private ArrayList<Class> aclForbidJS = new ArrayList<Class>();
	private ArrayList<String> astrCmdHistory = new ArrayList<String>();
	private ArrayList<String> astrAutoInit = new ArrayList<String>();
	private File	flCmdHistory;
	private File	flUserInit;
	private int iNavigateCmdHistoryIndex = 0;
	private HashBiMap<String,File> hmFileReaderExecJS = HashBiMap.create(); 
	WriterCapture wrc = new WriterCapture();
	private String	strBaseCmdToken = "/";
	private HashMap<Object,ArrayList<MethodX>> hmMethodsHelp = new HashMap<Object,ArrayList<MethodX>>();
	private Comparator	cmpMethodX = new Comparator<MethodX>() {
		@Override
		public int compare(MethodX o1, MethodX o2) {
			String str1=o1.getConcrete().getSimpleName()+o1.getMethod().getName();
			String str2=o2.getConcrete().getSimpleName()+o2.getMethod().getName();
			return str1.compareTo(str2);
		}
	};
	
	private String	strCustomVarPrefix="$";
	private String strLastRetValId=strCustomVarPrefix+"LastScriptExecRetVal";
	private String strLastShownValue=strCustomVarPrefix+"LastShownValue";
	
	private String strCommentLineToken="//";
	private static String	strSelfCallEnqueuedId="SelfCallEnqueued";
	
//	enum EJSObjectBind {
//		selfScript,
//		;
//		public String s(){return toString();}
//	}
	
	public static enum EBaseCommand{
		clear("clears the console log (not the log file)"),
		echo("print some text on the console log"),
		exec("[file] runs a script file, mainly at "+FileI.i().getStorageFolder()+". Extra JSBind:"+SelfScriptFile.class.getSimpleName()),
		exit,
		help("[filter]"),
		javadoc("[filter]"),
		history("[filter] show commands history"),
		ini("append to user init file"),
		kill("kill running queue by UId"),
		quit,
		set("<JSBindId> <CommandReturningNonVoid>"),
		showIni,
		bind("<KeyCfg> <[<HardCmdId>|<CustomJS>]> Extra JSBind: "+strSelfCallEnqueuedId), 
		msgRep("[msgUId] show reviewable messages"),
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
			astrBaseCommands[i++]=(strBaseCmdToken+ebc.s()+" // "+ebc.strInfo+" ("+EBaseCommand.class.getSimpleName()+")");
		}
		return astrBaseCommands;
	}
	
	public boolean isBaseCommand(String strCmd){
		return getBaseCommand(strCmd)!=null;
	}
	
	public EBaseCommand getBaseCommand(String strCmd){
		strCmd=strCmd.trim();
		if(strCmd.startsWith(strBaseCmdToken)){
			strCmd=strCmd.substring(strBaseCmdToken.length());
		}else{
			return null;
		}
		
		String strBase = StringI.i().extractPart(strCmd," ",0);
		EBaseCommand ebc = null;
		try{
			ebc=EBaseCommand.valueOf(strBase);
		}catch(IllegalArgumentException e){
			//ignore
		}
		
//		if(ebc==null)return false;
		return ebc;
		
	}
	
	/**
	 * TODO allow each base command to fail and return false
	 * @param strCmd
	 * @return
	 */
	public boolean execBaseCommand(String strCmd){
		EBaseCommand ebc = getBaseCommand(strCmd);
		
//		String strBase=ebc.toString();
//		String strParams = "";
		String strParams = StringI.i().extractPart(strCmd," ",1,-1);
//		if(!strCmd.equals(strBase))strParams=strCmd.substring(strBase.length()+1);
		strParams=strParams.trim();
		
		if(strParams.startsWith(strCommentLineToken))strParams=""; //clear if is comment
		
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
			case quit:
			case exit:
				LoggingI.i().logMarker("exiting by user request");
				AppI.i().stop();
//				G.i(Application.class).stop();
				return true;
			case exec:
				execFileAndShowRetVal(strParams);
				return true;
			case set:
				setCustomUserVar(strParams);
				return true;
			case showIni:
				LoggingI.i().logMarker("Showing User Init");
				LoggingI.i().logEntry(flUserInit.getAbsolutePath());
				for(String str:JavaScriptI.i().getUserInit()){
					LoggingI.i().logEntry(str);
				}
				return true;
			case bind:
				KeyBindCommandManagerI.i().loadConfig(strParams,true,true);
				return true;
			case msgRep:
				showMsgReport(strParams);
				return true;
			default:
//				throw new UnsupportedOperationException("not implemented yet "+ebc);
				LoggingI.i().logWarn("not implemented yet "+ebc);
		}
		
		return false;
	}
	
	public void showMsgReport(String strParams) {
		String strFilter="";
		if(strParams!=null){
			strFilter = StringI.i().extractPart(strParams,";",0);
			if(strFilter.isEmpty())strFilter = StringI.i().extractPart(strParams," ",0);
		}
		LoggingI.i().logMarker("Messages Report: "+strFilter);
		for(String str:MessagesI.i().getMessagesReport(strFilter)){
//			str=str.replace("\n"," ");
			if(strFilter.isEmpty()){
				str=(JavaScriptI.i().prepareCmd(EBaseCommand.msgRep,str));
			}
			LoggingI.i().logSubEntry(str);
		}
	}

	/**
	 * 
	 * @param strParams "<[$]customJSVarBindId> <JSCmdToCaptureReturnValueFrom>"
	 */
	public void setCustomUserVar(String strParams) {
//		CommandLineParser clp = new CommandLineParser(strParams);
//		String strCustomUserBind = clp.getCommand();
		String strCustomUserBind = StringI.i().extractPart(strParams, " ", 0);
		String strJSCmd = strParams.substring(strCustomUserBind.length()+1);
		
		// prefix
		if(!strCustomUserBind.startsWith(strCustomVarPrefix))strCustomUserBind=strCustomVarPrefix+strCustomUserBind;
		
		if(strCustomUserBind.equals(strLastRetValId)){
			LoggingI.i().logWarn("this var id is volatile, will be replaced often");
		}
		
		if(execScript(strJSCmd, false)){
			if(objExecScriptRetVal!=null){
				setJSBindingCanReplace(strCustomUserBind,objExecScriptRetVal);
			}else{
				LoggingI.i().logWarn("not set: return was null");
			}
		}
	}

	private void showJavadoc(String strFullMethodHelp) {
		MethodX mh = retrieveMethodHelp(strFullMethodHelp);
		if(mh!=null){
			LoggingI.i().logEntry("Externally browsing javadoc for: "+mh.getFullHelp(false,false));
			LoggingI.i().logEntry(JavadocI.i().browseJavadoc(mh).toString());
		}
	}
	
	private String takeOnlyWhatMatters(String str){
		str=str.trim();
		
		// remove comments
		int iComment = str.indexOf(strCommentLineToken);
		if(iComment>-1)str=str.substring(0,iComment);
		
		return str.trim();
	}
	
	private boolean cmpEqualsOnlyWhatMatters(String strA,String strB){
		return takeOnlyWhatMatters(strA).equals(takeOnlyWhatMatters(strB));
	}
	
//	public void getJSBinds(){
//		if(aclForbidJS.contains(objBindValue.getClass())){
//			MessagesI.i().warnMsg(this, "forbidden JS access to class", objBindValue.getClass());
//			return;
//		}
//	}
	
	public MethodX retrieveMethodHelp(String strFullMethodHelp){
//		// remove comments
//		int iComment = strFullMethodHelp.indexOf(strCommentLineToken);
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
		
		ArrayList<MethodX> amh = retrieveAllMethodsHelpFor(objJSBind);
		if(strFullMethodHelp.contains("(")){ //has method anchor
//		if(i1stDot>-1 && strFullMethodHelp.length()>i1stDot){ //has method anchor
			for(MethodX mh:amh){
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
	
	public ArrayList<MethodX> retrieveAllMethodsHelpFor(Object obj){
		ArrayList<MethodX> amh = hmMethodsHelp.get(obj);
		if(amh==null){
			amh = JavadocI.i().prepareAllMethodsHelp(obj);
			Collections.sort(amh,cmpMethodX );
			DetailedException.assertIsFalse("empty", amh.size()==0, obj);
			hmMethodsHelp.put(obj,amh);
		}
		return amh;
	}
	
	public String convertToUserHelp(MethodX mh){
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
		
		/**
		 * add all existing globals and enums
		 */
		for(Object obj:GlobalManagerI.i().getListCopy()){
			String strKey = genKeyFor(obj);
			Object objExisting = bndJSE.get(strKey);
			if(objExisting==null){
				setJSBinding(obj);
			}else{
				/**
				 * ignore overriders
				 */
				if(!obj.getClass().isInstance(objExisting) && !objExisting.getClass().isInstance(obj)){
					MessagesI.i().warnUniqueMsg(this, "key already used", strKey, objExisting, obj);
				}
			}
		}
		for(Entry<Class<Enum>,Enum[]> entry:GlobalManagerI.i().getGlobalEnumsListCopy().entrySet()){
			globalEnumAddedEvent(entry.getKey(),entry.getValue());
		}
		
		setJSBindingCanReplace("New", new New());
		
		/**
		 * listen for new ones
		 */
		GlobalManagerI.i().addGlobalAddListener(this);
		
		KeyBindCommandManagerI.i().putBindCommandsLater("Ctrl+R",new CallBoundKeyCmd(){@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
			repeatLastCommand(0);return true;}}.setName("RepeatLastConsCmd"));
	}
	
	public static class New{
		public Vector3f Vector3f(){return new Vector3f();}
		public Quaternion Quaternion(){return new Quaternion();}
	}
	
	/**
	 * 
	 * @param obj
	 * @return basically the classSimpleName or the simpleEnclosingName$simpleTypeName, works for enums too.
	 */
	public String genKeyFor(Object obj){
		if(JavaLangI.i().isEnumClass(obj)){
			Class cl = (Class)obj;
			String[] astr = cl.getName().split("[.]");
			String strBindIdNew=astr[astr.length-1];
			return strBindIdNew;
		}
		
		if(obj instanceof CallUserCustomCmd){
			return strSelfCallEnqueuedId; 
		}
		
		String str=obj.getClass().getSimpleName();
		if(!str.isEmpty())return str;
		
		throw new DetailedException("unable to generate JS key for "+obj.getClass().getName(),obj,this);
	}
	
	/**
	 * will have automatic id based on object class simple name, 
	 * it can also be a .class of static enums
	 * 
	 * @param strBindId
	 * @param objBindValue
	 * @return previous value for id
	 */
//	private void setJSBinding(String strBindId, Object objBindValue){
	public void setJSBinding(Object objBindValue){
		if(isAccessForbidden(objBindValue))return;
		
		@SuppressWarnings("unchecked")
		Class<Enum> clEnum = (JavaLangI.i().isEnumClass(objBindValue)) ? (Class<Enum>)objBindValue : null;
		
		String strBindId = genKeyFor(objBindValue);
		
		/**
		 * check for restrictions
		 */
		if(!CallUserCustomCmd.class.isInstance(objBindValue)){
			Object objExisting = bndJSE.get(strBindId);
			if(objExisting!=null){
				if(clEnum!=null)return; //np if already set, just skip/ignore as enums are global statics (not instances)
				throw new DetailedException("already set: "+strBindId, objExisting);
			}
			
			if(clEnum!=null){
				try {
					objBindValue = evalJS("Java.type('"+clEnum.getName()+"');"); //prepares a static class access
				} catch (ScriptException e) {
					throw new DetailedException(e, strBindId, objBindValue);
				}
			}
		}
		
		setJSBindingCanReplace(strBindId,objBindValue);
		
//		setJSBindingForEnumsOf(objBindValue.getClass());
	}
	
	protected Object evalJS(Reader r) 		throws ScriptException{return jse.eval(r		,bndJSE);}
	protected Object evalJS(String strJS) throws ScriptException{return jse.eval(strJS,bndJSE);}
	
	private void setJSBindingCanReplace(String strBindId,Object objBindValue){
		if(isAccessForbidden(objBindValue)){ //restrictions still apply
			throw new DetailedException("forbidden bind type", objBindValue);
		}
		
		String strInfoMode="created";
		if(bndJSE.get(strBindId)!=null)strInfoMode="replaced";//LoggingI.i().logSubEntry("replacing "+strBindId);
		
		// do it
		bndJSE.put(strBindId,objBindValue);
		
		// logs
		String strMsg=strInfoMode+" JS bind: "+strBindId+" ("+objBindValue.getClass().getSimpleName()+")";
		
		if(!strBindId.equals(strLastRetValId)){
			LoggingI.i().logMarker(strMsg,-1);
			if(!strBindId.startsWith(strCustomVarPrefix)){
				MessagesI.i().debugInfo(this,strMsg,objBindValue); //intended mainly for globals
			}
		}
	}
	
	private boolean isAccessForbidden(Object objBindValue) {
		DetailedException.assertNotNull(objBindValue);
		
		boolean b=false;
		
		if(aclForbidJS.contains(objBindValue.getClass())){
			MessagesI.i().warnMsg(this, "forbidden JS access to class", objBindValue.getClass());
			b=true;
		}
		
		/** special case to prevent unnecesary access while still keeping its overridable globals */
		if(objBindValue instanceof PkgCfgAbs){
			MessagesI.i().output(System.out, "info:", this, "skipping JS access to "+objBindValue.getClass());
			b=true;
		}
		
		return b;
	}

//	/**
//	 * will have automatic id based on object class simple name, 
//	 * it can also be a .class of static enums
//	 * @param objBindValue 
//	 */
//	public void setJSBinding(Object objBindValue){
//		setJSBinding(genKeyFor(objBindValue), objBindValue);
//	}
	
	protected void submitUserCommand() {
		DetailedException.addStackTraceElementExitPreventer();
		
		String strCmd = DevConsPluginStateI.i().getInputText();
		
		strCmd=strCmd.trim();
		if(strCmd.isEmpty())return;
		
		LoggingI.i().logMarker("User Command");
		LoggingI.i().logEntry(strCmd);
		
		try{
			execCommand(strCmd,true,true);
		}catch(DetailedException ex){
			LoggingI.i().logExceptionEntry(ex, strCmd);
		}
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
		DevConsPluginStateI.i().clearInput();
	}
	
	/**
	 * 
	 * @param iIndexFromLast 0=last, 1=last-1, 2=last-2, ... 
	 */
	public void repeatLastCommand(int iIndexFromLast){
		int i=astrCmdHistory.size()-iIndexFromLast-1;
		if(i<0)i=0;
		String strCmd = astrCmdHistory.get(i);
		execCommand(strCmd, true, true); //must change the history because it depends on it!
	}
	
	protected boolean execCommand(String strCmd,boolean bShowRetVal, boolean bAddToHistory) {
		if(bAddToHistory)addCmdToHistory(strCmd);
		
		String strLastCmds="";
		int iTot=10;
		int iCount=1;
//		for(int i=astrCmdHistory.size()-1;i>=0 && i>astrCmdHistory.size()-1-iTot;i--){
//			strLastCmds+=" ["+(iCount++)+"]"+astrCmdHistory.get(i)+";";
//			strLastCmds+=" "+astrCmdHistory.get(astrCmdHistory.size()-1)+";";
//		}
//		EnvironmentJmeI.i().putCustomInfo("Ctrl+R", 
//			"{(last@"+TimeFormatI.i().getRealTimeFormatted(null,"HH:mm:ss")+")"+strLastCmds+"}");
		HWEnvironmentJmeI.i().putCustomInfo("LastCmd", 
			"@"+TimeFormatI.i().getRealTimeFormatted(null,"HH:mm:ss")+" '"+astrCmdHistory.get(astrCmdHistory.size()-1)+"'");
		
		if(isBaseCommand(strCmd)){
			return execBaseCommand(strCmd);
		}else{
			return execScript(strCmd,bShowRetVal);
		}
		
//		if(!isAndExecBaseCommand(strJS)){
//			return execScript(strJS,bShowRetVal);
//		}
//		return true;
	}

	/**
	 * Initially, a short help will be shown with {@link EBaseCommand} and JS bindings.
	 * Only if a full JS class bind id is the filter, its methods will be shown. 
	 * @param strFilter
	 * @return
	 */
	public AutoCompleteResult showHelp(String strFilter) {
		strFilter=strFilter.trim();
		LoggingI.i().logMarker("Help for: "+strFilter);
		
		ArrayList<String> astr = new ArrayList<String>();
		astr.addAll(Arrays.asList(getBaseCommandsValuesAsHelp()));
		ArrayList<String> astrBnd = new ArrayList<String>(bndJSE.keySet());
		Collections.sort(astrBnd);
		astr.addAll(astrBnd);
		
		String strImprovedPart=strFilter; //still unmodified
		
		AutoCompleteResult ar=new AutoCompleteResult(
			strFilter, strImprovedPart, new ArrayList<String>(astr), false, false);
		if(!strFilter.isEmpty()){
			ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false); //base ones 1st
			
//			if(ar.getResultList().size()==1){ 
//			if(!isBaseCommand(strFilter) && !ar.isPartGotImproved()){ 
			if(!strFilter.startsWith(strBaseCmdToken) && !ar.isPartGotImproved()){ 
				astr.addAll(getJSClassBindListFilteredHelp());
				
				ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false);
				
				if(!ar.isPartGotImproved()) {
					/**
					 * if nothing is found, try anywhere on the strings: contains, if nothing matches,try fuzzy
					 */
					ar = AutoCompleteI.i().autoComplete(strFilter, astr, true, true);
					
//					if(!ar.isPartGotImproved()) {
//						/**
//						 * if nothing is found, try fuzzy
//						 */
//						ar = AutoCompleteI.i().autoComplete(strFilter, astr, true, false);
//					}
				}
			}
		}
		
		for(String str:ar.getResultList()){
			LoggingI.i().logSubEntry(str);
		}
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
		
		return ar;
	}
	
	public boolean execFileAndShowRetVal(String strFile){
		boolean b=execFile(asFile(strFile));
		showValue(objRetValFile); //this method will be called by the user, so show the return value
		return b;
	}
	public SelfScriptFile asFile(String strFile){
		if(strFile.isEmpty()){
			LoggingI.i().logWarn("missing file param");
			return null;
		}
		
		SelfScriptFile flJS = null;
		try {
			flJS = new SelfScriptFile(strFile); //first try to find it at some absolute location
			if(!flJS.exists()){ 
				//now try relatively to default storage path
				flJS = new SelfScriptFile(FileI.i().createNewFileHandler(strFile,true).toURI());
			}
			
			if(!flJS.exists()){
				flJS.createNewFile();
				FileI.i().appendLine(flJS, "//Classes: "+bndJSE.keySet());
//				FileI.i().appendLine(flJS, strCommentLineToken+EJSObjectBind.class.getSimpleName()+": "+Arrays.toString(EJSObjectBind.values()));
				FileI.i().appendLine(flJS, strCommentLineToken+genKeyFor(flJS));
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
	
	public void execFileLater(String strFile, float fDelaySeconds, boolean bLoop, boolean bShowRetVal){
		execFileLater(asFile(strFile), fDelaySeconds, bLoop, bShowRetVal);
	}
	public void execFileLater(File flJS, float fDelaySeconds, boolean bLoop, boolean bShowRetVal){
		execLater(null, flJS, fDelaySeconds, bLoop, bShowRetVal);
	}
	public void execScriptLater(String strSimpleScript, float fDelaySeconds, boolean bLoop, boolean bShowRetVal){
		execLater(strSimpleScript, null, fDelaySeconds, bLoop, bShowRetVal);
	}
	/**
	 * 
	 * @param strSimpleScript can be null
	 * @param flJS priority if not null
	 * @param fDelaySeconds
	 * @param bLoop
	 */
	private void execLater(String strSimpleScript, File flJS, float fDelaySeconds, boolean bLoop,boolean bShowRetVal){
		@SuppressWarnings("unchecked")
		CallableX cx = QueueI.i().enqueue(new CallableX() {
				@Override
				public Boolean call() {
					if(flJS!=null){
						execFile(flJS);
					}else{
						execScript(strSimpleScript,bShowRetVal);
					}
					
					return true;
				}
			}
			.setName(flJS!=null?flJS.getName():"SimpleScript")
			.setDelaySeconds(fDelaySeconds)
			.setUserCanKill(true)
			.setUserCanPause(true)
		);
		
		if(bLoop)cx.enableLoopMode();
	}
	
	public static class SelfScriptFile extends File{
		private static final long	serialVersionUID	= -1605739267004772815L;
		public SelfScriptFile(URI uri) {super(uri);}
		public SelfScriptFile(File parent, String child) {super(parent, child);}
		public SelfScriptFile(String parent, String child) {super(parent, child);}
		public SelfScriptFile(String pathname) {super(pathname);}
	}
	
	/**
	 * kept private to prevent infinite self call on the same frame,
	 * use {@link #queueExecFile(File, float)}
	 * @param rd
	 * @return 
	 */
	private boolean execFile(File flJS){
		try {
			SelfScriptFile flsc = new SelfScriptFile(flJS.toURI());
			bndJSE.remove(genKeyFor(flsc)); //to refresh it
			setJSBinding(flsc);
			objRetValFile = evalJS(new FileReader(flJS));
			return true;
		} catch (ScriptException | IOException e) {
//			LoggingI.i().logExceptionEntry(e, hmFileReaderExecJS.inverse().get(rd));
			LoggingI.i().logExceptionEntry(e, flJS.toString());
		}
		
		return false;
	}
	
	public boolean execScript(String strJS,boolean bShowRetVal){
		DetailedException.assertNotEmpty("Javascript", strJS, bShowRetVal);
		try {
			objExecScriptRetVal=evalJS(strJS);
			if(objExecScriptRetVal!=null)setJSBindingCanReplace(strLastRetValId, objExecScriptRetVal);
			if(bShowRetVal)showValue(objExecScriptRetVal);
			return true;
		} catch (ScriptException e) {
			LoggingI.i().logExceptionEntry(e, strJS);
			LoggingI.i().logWarn("May lead to inconsistency/instability.");
		} catch (Exception e){ //yes, catch it all as is a user command, may leave inconsistencies tho
			LoggingI.i().logExceptionEntry(e, strJS);
			LoggingI.i().logWarn("May lead to inconsistency/instability.");
		}
		
		return false;
	}
	
	public void showValue(Object obj){
		if(obj==null){
			LoggingI.i().logSubEntry("Value is null or void.");
		}else{
			String str="";
			String strCl=obj.toString();
			String strJCS = "JavaClassStatics["; //it is javascript's internal stuff
			if(strCl.startsWith(strJCS)){
				strCl=strCl.substring(strJCS.length(), strCl.length()-1);
				Enum[] ae = GlobalManagerI.i().getGlobalEnumValuesClNm(strCl);
				if(ae!=null)obj=ae;//JavaLangI.i().enumUId(ae);
			}
			
			LoggingI.i().logSubEntry("Value type: "+obj.getClass());
			if(JavaLangI.i().isCanUserTypeIt(obj)){ // simple types result in simple and readable strings
				if (obj instanceof String) {
					str+="'"+obj+"'";
				}else{
					str+=""+obj; //int long etc
				}
				LoggingI.i().logSubEntry(str);
			}else
			if(!isAndShowArray(obj)){
				if (obj instanceof Vector3f) {
					Vector3f v3f = (Vector3f) obj;
					str+="new Vector3f("+StringTextJmeI.i().fmtVector3f(v3f,2)+")";
				}else
				if (obj instanceof Quaternion) {
					Quaternion qua = (Quaternion) obj;
					str+="new Quaternion().fromAngles("+StringTextJmeI.i().fmtToDegrees(qua,2)+")";
				}else{
					str+="Object value: #"+obj.hashCode()+", .toString(): '"+obj+"'";
				}
				
				LoggingI.i().logSubEntry(str); //yes, show before and after to be easier to find/use
				showMethods(obj);
				LoggingI.i().logSubEntry(str); //yes, show before and after to be easier to find/use
			}
			
			setJSBindingCanReplace(strLastShownValue, obj);
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
		for(MethodX mh:retrieveAllMethodsHelpFor(obj)){
			if(isMethodMatchesFilter(mh)){
				LoggingI.i().logSubEntry(convertToUserHelp(mh));
			}
		}
	}
	
	public ArrayList<String> getJSClassBindListFilteredHelp(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Entry<String, Object> entry:bndJSE.entrySet()){
			Object objJSBindValue = entry.getValue();
			for(MethodX mh:retrieveAllMethodsHelpFor(objJSBindValue)){
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
	public boolean isMethodMatchesFilter(MethodX mh){
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
		flCmdHistory = FileI.i().createNewFileHandler("CommandsHistory.log", true); //new File(DevConsPluginStateI.i().getStorageFolder(),"CommandsHistory.log");
		astrCmdHistory.add(""); //just to avoid empty list when adding new cmd to it
		if(flCmdHistory.exists()){
			for(String str:FileI.i().readAllLines(flCmdHistory)){
				astrCmdHistory.add(str);
			}
			iNavigateCmdHistoryIndex=astrCmdHistory.size()-1;
		}
		
		File flSetup = new File("./","Setup.cfg");
		if(flSetup.exists()) {
			astrAutoInit.addAll(FileI.i().readAllLines(flSetup));
		}else {
			FileI.i().appendLine(flSetup, "// internal setup");
		}
		
		// load user init cmds
		flUserInit = FileI.i().createNewFileHandler("UserInit.cfg", true); //new File(DevConsPluginStateI.i().getStorageFolder(),"UserInit.cfg");
		if(flUserInit.exists()){
			astrAutoInit.addAll(getUserInit());
			
			QueueI.i().enqueue(new CallableXAnon() {@Override public Boolean call() {
				processUserInit();
				return true;
			} });
			
		}else{
			FileI.i().appendLine(flUserInit, "// put initialization commands on this file, one per line");
		}
	}
	
	public void processUserInit() {
		if(astrAutoInit.size()>0){
			LoggingI.i().logMarker("UserInit:Begin");
			QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
//					boolean bFail=false;
					String strCmd = astrAutoInit.remove(0);
					if(strCmd.isEmpty())return false; //next cmd on next frame
					
					LoggingI.i().logSubEntry(strCmd);
					if(!execCommand(strCmd,false,false)){
//						bFail=true;
						String strMsg="UserInit:FAIL";
						LoggingI.i().logMarker(strMsg);
						
						SystemAlertI.i().showTemporarySystemAlert(strMsg+"\n"+strCmd, 5f);
						return true; //user cmd failed, end queue
					}
					
					if(astrAutoInit.size()>0)return false; //next cmd on next frame
					
//					astrUserInit.clear();
					
//					if(!bFail)
					LoggingI.i().logMarker("UserInit:CompletedSuccessfully");
					
					return true; //success, end queue
				}
			}).setName("InitUserCfg");
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
			FileI.i().appendLine(flUserInit, strJS);
			LoggingI.i().logMarker("Appended User Init Cmd");
	}
	
	protected AutoCompleteResult autoCompleteCustomUserBind(String strInputTilCarat){
		int iDotAt = strInputTilCarat.indexOf(".");
		if(iDotAt>-1){
			String strJSBindId=strInputTilCarat.substring(0, iDotAt);
			String strAfterDot=strInputTilCarat.substring(iDotAt+1);
			Object objInstance = bndJSE.get(strJSBindId);
			if(objInstance!=null){
				String strConcreteTypeFilter = objInstance.getClass().getSimpleName();
				AutoCompleteResult arConverted = JavaScriptI.i().showHelp(strConcreteTypeFilter+"."+strAfterDot);
				if(arConverted.isPartGotImproved()){
					strAfterDot=StringI.i().extractPart(arConverted.getImprovedPart(), ".", 1, -1);
				}
//					strNewInput=arConverted.getImprovedPart().replaceFirst("^"+strConcreteTypeFilter, strJSBindId);
					arConverted.setNewCustomImprovedPart(strJSBindId+"."+strAfterDot);
//				}
				return arConverted;
			}
		}
		return null;
	}
	
	protected void autoComplete() {
		String strInputTilCarat=DevConsPluginStateI.i().getInputTextBeforeCarat(null);
		int iInitialLength = strInputTilCarat.length();
		strInputTilCarat=strInputTilCarat.trim();
		
		AutoCompleteResult ar = JavaScriptI.i().showHelp(strInputTilCarat);
//		String strNewInput=strInputTilCarat;
		String strNewInput=ar.getImprovedPart();
		if(strInputTilCarat.startsWith(strCustomVarPrefix)){ //custom user var bound
			AutoCompleteResult arTmp = autoCompleteCustomUserBind(strInputTilCarat);
			if(arTmp!=null){
				strNewInput = arTmp.getNewCustomImprovedPart();
				ar=arTmp;
			}
		}else{
//			ar = JavaScriptI.i().showHelp(strInputTilCarat);
//			strNewInput=ar.getImprovedPart();
			
			AutoCompleteResult arTmp=autoCompleteBaseCmd(strInputTilCarat,ar);
			if(arTmp!=null){
				strNewInput = arTmp.getNewCustomImprovedPart();
				ar=arTmp;
			}
		}
		
		int i = strNewInput.indexOf(strCommentLineToken);if(i>-1)strNewInput=strNewInput.substring(0,i).trim(); //remove trailing comments
		if(ar.isImprovedAnExactMatch())strNewInput+=" ";
		DevConsPluginStateI.i().insertAtInputTextCaratPos(strNewInput,	iInitialLength);
		
		addCmdToHistory(strInputTilCarat);
		if(ar.isPartGotImproved())addCmdToHistory(strNewInput);
		
		DevConsPluginStateI.i().scrollKeepAtBottom();
	}

	private AutoCompleteResult autoCompleteBaseCmd(String strInputTilCarat,			AutoCompleteResult ar) {
		if( // is to auto guess and complete with a base command?
				!strInputTilCarat.isEmpty() && 
				!strInputTilCarat.startsWith(strBaseCmdToken) && 
				!ar.isPartGotImproved() && //ar.getImprovedPart().equals(strInput) && //so if it was not improved
				!ar.getImprovedPart().contains(".") && //skips an object looking for methods
				!ar.isImprovedAnExactMatch() && 
				ar.getResultList().size()==1
		){ //if nothing changed, try again as base command
			AutoCompleteResult arBaseCmds = JavaScriptI.i().showHelp(strBaseCmdToken+strInputTilCarat);
			if(arBaseCmds.isPartGotImproved()){
				arBaseCmds.setNewCustomImprovedPart(arBaseCmds.getImprovedPart());
				return arBaseCmds;
			}
		}
		
		return null;
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

	public void setJSBindingForEnumsOf(Class clWithEnums){
		Class<?>[] acl = clWithEnums.getDeclaredClasses();
		for(Class<?> cl:acl){
			if(JavaLangI.i().isEnumClass(cl)){
				setJSBinding(cl);
			}
		}
	}
	@Override
	public void globalEnumAddedEvent(Class<Enum> cle, Enum[] ae) {
		if(ae==null){ //will help the globals manager providing the values
			QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
					Enum[] aeRet=null;
					try {
						if(Modifier.isPublic(cle.getModifiers())){
							setJSBinding(cle); //1st make it available in JS
							aeRet = (Enum[])evalJS("Java.type('"+cle.getName()+"').values();");
							GlobalManagerI.i().putEnumClass(cle, aeRet);
						}
					} catch (ScriptException ex) {
//						if( 
//							ex.getMessage().equals("TypeError: Java.type(\""+cle.getName()+"\").values "
//								+"is not a function in <eval> at line number 1")
//						){
//							return false; //retry
//						}else{
							throw new DetailedException(ex, cle, ae, aeRet);
//						}
					}
					return true;
				}
			}).setName("FillGlobalEnumValues:"+cle.getName());
		}
	}
	@Override
	public void globalInstanceAddedEvent(Class clType, Object objValue) {
		setJSBinding(objValue); //TODO use the "specific class"'s name as bind id? 
	}
	
	/**
	 * mainly to deal with {@link GlobalManagerI} assignments
	 * @param cl
	 */
	public void addForbidClassAccessJS(Class cl){
		if(!aclForbidJS.contains(cl))aclForbidJS.add(cl);
		for(Entry<String, Object> entry:bndJSE.entrySet()){
			if(entry.getValue().getClass().equals(cl)){
				MessagesI.i().warnMsg(this, "removed JS access to", cl, bndJSE.remove(cl.getSimpleName()));
			}
		}
	}

//	public String getBindCmdUId() {
//		return strBaseCmdToken+EBaseCommand.bind.toString();
//	}
	
	public String prepareCmd(EBaseCommand e, String strParams){
		return strBaseCmdToken+e+" "+strParams;
	}
}
