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

package devcons;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class JavaScriptI {
	private static JavaScriptI instance = new JavaScriptI();
	/**instance*/ public static JavaScriptI i(){return instance;}
	
	private Object	objJSLastEval;
	private ScriptEngine	jse;
	private Bindings	bndJSE;
	private ArrayList<String> astrIdList = new ArrayList<String>(); 
	
	public JavaScriptI() {
		jse  = new ScriptEngineManager().getEngineByMimeType("text/javascript");
		bndJSE = jse.createBindings();
	}
	
	/**
	 * 
	 * @param strBindId
	 * @param objBindValue
	 * @return previous value for id
	 */
	private void setIdValue(String strBindId, Object objBindValue){
		if(bndJSE.put(strBindId,objBindValue) != null){
			throw new NullPointerException("already set: "+strBindId);
		}
		
		astrIdList.add(strBindId);
	}
	public void setIdValue(Object objBindValue){
		setIdValue(objBindValue.getClass().getSimpleName(), objBindValue);
	}
	protected void submitUserCommand() {
		String strJS = ConsolePluginI.i().getInputText();
		
		strJS=strJS.trim();
		if(strJS.isEmpty())return;
		
		LoggingI.i().logSubEntry("_____________ UserCommand ____________");
		
		if(strJS.equalsIgnoreCase("help")){
			for(String str:astrIdList)LoggingI.i().logSubEntry(str);
		}else{
			execScript(strJS);
		}
		
		ConsolePluginI.i().scrollToBottom();
	}
	
	public void execScript(String strJS){
		try {
			objJSLastEval=jse.eval(strJS,bndJSE);
			if(objJSLastEval==null){
				LoggingI.i().logSubEntry("Return: null");
			}else{
				LoggingI.i().logSubEntry("Return: "+objJSLastEval.toString()+" ("+objJSLastEval.getClass()+"), accessible methods:");
				
				Method[] am = objJSLastEval.getClass().getMethods();
				ArrayList<String> astr = new ArrayList<String>();
				for(Method m:am){
					String strM = "";
					String strClassSName=m.getDeclaringClass().getSimpleName();
					strM+=strClassSName+"."+m.getName();
					
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
					
					if(
							!bIsStatic &&
							!bHasNonPrimitiveParam && 
							!strClassSName.equals(Object.class.getSimpleName()) &&
							!strClassSName.equals(CharSequence.class.getSimpleName())  &&
							!strClassSName.equals(String.class.getSimpleName())
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
	
}
