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

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.github.devconslejme.misc.AutoCompleteI;
import com.github.devconslejme.misc.AutoCompleteI.AutoCompleteResult;
import com.github.devconslejme.misc.JavaLangI;
import com.google.common.primitives.Primitives;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaScriptI {
	private static JavaScriptI instance = new JavaScriptI();
	/**instance*/ public static JavaScriptI i(){return instance;}
	
	static {
		instance.setJSBinding(instance);
	}
	
	private Object	objLastReturnValueFromEval;
	private ScriptEngine	jse;
	private Bindings	bndJSE;
	private ArrayList<String> astrIdList = new ArrayList<String>();
	private boolean bShowAllPublicMembers = false;
	private Method[] amLastReturnValue;
	private ArrayList<String>	astrLastReturnValueMethods = new ArrayList<String>();
	
	enum ECommands{
		help,
		
		/** append to user init file */
		ini,
		;
		public String s(){return toString();}
	}
	
	public JavaScriptI() {
		jse  = new ScriptEngineManager().getEngineByMimeType("text/javascript");
		bndJSE = jse.createBindings();
		astrIdList.add(ECommands.help.s());
		astrIdList.add(ECommands.ini.s()); 
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
		
		astrIdList.add(strBindId);
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
		
		ConsolePluginI.i().addCmdToHistory(strJS);
		
		LoggingI.i().logMarker("UserCommand");
		
		if(strJS.equalsIgnoreCase("help")){
			showHelp("");
		}else
		if(strJS.startsWith(ECommands.ini.s()+" ")){
//			ConsolePluginI.i().appendUserInitCommand();
//			FileI.i().appendLine(fluser, str);
//			strJS.substring(ECommands.ini.s().length()+1);
		}else{
			execScript(strJS);
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
		astr.addAll(astrIdList);
		astr.addAll(astrLastReturnValueMethods);
		
		AutoCompleteResult ar = AutoCompleteI.i().autoComplete(strFilter, astr, false, false);
		for(String str:ar.getResultList()){
			LoggingI.i().logSubEntry(str);
		}
		
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
	
	public void execScript(String strJS){
		try {
			objLastReturnValueFromEval=jse.eval(strJS,bndJSE);
			if(objLastReturnValueFromEval==null){
				LoggingI.i().logSubEntry("Return is null");
			}else{
//				LoggingI.i().logSubEntry("ReturnType: "+objJSLastEval.toString()+" ("+objJSLastEval.getClass()+")");
				LoggingI.i().logSubEntry("Return type: "+objLastReturnValueFromEval.getClass());
				if(isCanUserTypeIt(objLastReturnValueFromEval)){ // simple types result in simple and readable strings
					LoggingI.i().logSubEntry("Return value = '"+objLastReturnValueFromEval+"'");
				}else
				if(!isAndShowArray(objLastReturnValueFromEval)){
					showMethods(objLastReturnValueFromEval);
				}
			}
		} catch (ScriptException e) {
			LoggingI.i().logExceptionEntry(e, strJS);
		}
	}
	
	private boolean isAndShowArray(Object objValue){
		Object[][] aaobjKeyValue = JavaLangI.i().convertToKeyValueArray(objValue);
		
//		Object[] aobjVal=null;
//		Object[] aobjKey=null;
//		
//		if(objValue instanceof Map) { //HashMap TreeMap etc
//			Set<Map.Entry> es = ((Map)objValue).entrySet();
//			aobjVal=new Object[es.size()];
//			aobjKey=new Object[es.size()];
//			int i=0;
//			for(Entry entry:es){
//				aobjKey[i]=entry.getKey();
//				aobjVal[i]=entry.getValue();
//				i++;
//			}
//		}else
//		if(objValue instanceof ArrayList) {
//			ArrayList<?> aobjList = (ArrayList<?>) objValue;
//			aobjVal=aobjList.toArray();
//		}else
//		if(objValue.getClass().isArray()){
//			aobjVal = (Object[])objValue;
//		}
		
		if(aaobjKeyValue==null)return false;
		
		LoggingI.i().logSubEntry("Return array values:");
		for(int i=0;i<aaobjKeyValue.length;i++){
//			Object objVal=aaobjKeyValue[i][1];
//			String strLog="";
////			if(aobjKey!=null)strLog+=""+aobjKey[i];
//			strLog+=""+aaobjKeyValue[i][0];
//			strLog+=""+objVal;
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
	
}
