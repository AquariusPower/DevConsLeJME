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

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.github.devconslejme.misc.AutoCompleteI;
import com.github.devconslejme.misc.AutoCompleteI.AutoCompleteResult;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaScriptI {
	private static JavaScriptI instance = new JavaScriptI();
	/**instance*/ public static JavaScriptI i(){return instance;}
	
	static {
		instance.setJSBinding(instance);
	}
	
	private Object	objJSLastEval;
	private ScriptEngine	jse;
	private Bindings	bndJSE;
	private ArrayList<String> astrIdList = new ArrayList<String>();
	private boolean bShowAllPublicMembers = false;
	
	public JavaScriptI() {
		jse  = new ScriptEngineManager().getEngineByMimeType("text/javascript");
		bndJSE = jse.createBindings();
		astrIdList.add("help");
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
		}else{
			execScript(strJS);
		}
		
		ConsolePluginI.i().scrollToBottom();
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
		
		AutoCompleteResult ar = AutoCompleteI.i().autoComplete(strFilter, astrIdList, false, false);
		
		LoggingI.i().logMarker("Help for: "+strFilter);
		for(String str:ar.getResultList()){
			LoggingI.i().logSubEntry(str);
		}
		
		return ar;
	}

	public void execScript(String strJS){
		try {
			objJSLastEval=jse.eval(strJS,bndJSE);
			if(objJSLastEval==null){
				LoggingI.i().logSubEntry("Return: null");
			}else{
				LoggingI.i().logSubEntry("Return: "+objJSLastEval.toString()+" ("+objJSLastEval.getClass()+"), accessible methods:");
				
				String strConcreteClassSName = objJSLastEval.getClass().getSimpleName();
				Method[] am = objJSLastEval.getClass().getMethods();
				ArrayList<String> astr = new ArrayList<String>();
				for(Method m:am){
					String strM = "";
					String strDeclClassSName=m.getDeclaringClass().getSimpleName();
//					String strClassSName=strDeclClassSName;
//					if(!strClassSName.equals(strConcreteClassSName)){
//						strClassSName=strConcreteClassSName+"<"+strClassSName+">";
//					}
					
					strM+=strConcreteClassSName+"."+m.getName();
					
					strM+="(";
					String strP="";
					boolean bHasNonPrimitiveParam = false;
					for(Class<?> p:m.getParameterTypes()){
						if(!p.isPrimitive())bHasNonPrimitiveParam=true;
						if(!strP.isEmpty())strP+=",";
						strP+=p.getSimpleName();
					}
					strM+=strP+")";
					
					strM+=":"+m.getReturnType().getSimpleName();
					
					boolean bIsStatic=false;
					if(Modifier.isStatic(m.getModifiers())){
						strM+=" <STATIC>";
						bIsStatic=true;
					}
					
//					if(!strDeclClassSName.equals(strConcreteClassSName)){
						strM+=" <"+strDeclClassSName+">";
//					}
					
					if(
							isShowAllPublicMembers() ||
							(
								!bIsStatic &&
								
								!bHasNonPrimitiveParam &&
								
								!strDeclClassSName.equals(Object.class.getSimpleName()) &&
								!strDeclClassSName.equals(CharSequence.class.getSimpleName())  &&
								!strDeclClassSName.equals(String.class.getSimpleName()) &&
								!strDeclClassSName.equals(Integer.class.getSimpleName()) &&
								!strDeclClassSName.equals(Long.class.getSimpleName()) &&
								!strDeclClassSName.equals(Float.class.getSimpleName()) &&
								!strDeclClassSName.equals(Double.class.getSimpleName()) &&
								!strDeclClassSName.equals(Boolean.class.getSimpleName()) 
							)
					){
						astr.add(strM);
					}
				}
				
				Collections.sort(astr);
				
				for(String str:astr)LoggingI.i().logSubEntry(str);
			}
		} catch (ScriptException e) {
			LoggingI.i().logExceptionEntry(e, strJS);
		}
	}

	public boolean isShowAllPublicMembers() {
		return bShowAllPublicMembers;
	}

	public void setShowAllPublicMembers(boolean bShowAllPublicMembers) {
		this.bShowAllPublicMembers = bShowAllPublicMembers;
	}
	
}
