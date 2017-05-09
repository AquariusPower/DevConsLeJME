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
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class MethodX{
	/** mainly to help on debug */
	private String strLastFullHelp;
	private Method method;
	private Object objConcrete;
//	private Class clDeclaring;
//	private Class clConcrete;
	private Boolean bStatic=null;
	private Integer iNonUserTypeableParamsCount=null;
	
	public Method getMethod() {
		return method;
	}
	public boolean isStatic() {
		return bStatic==null?false:bStatic;
	}
	public int getNonUserTypeableParamsCount() {
		return iNonUserTypeableParamsCount==null ? 0 : iNonUserTypeableParamsCount;
	}
	public Class getDeclaring() {
//		return clDeclaring;
		return method.getDeclaringClass();
	}
	public Class getConcrete() {
//		return clConcrete;
		return objConcrete.getClass();
	}
	public MethodX setMethod(Method m) {
		DetailedException.assertNotAlreadySet(this.method, m, this);
		this.method = m;
//		this.clDeclaring=m.getDeclaringClass();
		return this;
	}
	public MethodX setStatic(boolean bStatic) {
		DetailedException.assertNotAlreadySet(this.bStatic, bStatic, this);
		this.bStatic = bStatic;
		return this;
	}
	public MethodX setNonUserTypeableParamsCount(int iNonUserTypeableParamsCount) {
		DetailedException.assertNotAlreadySet(this.iNonUserTypeableParamsCount, iNonUserTypeableParamsCount, this);
		this.iNonUserTypeableParamsCount = iNonUserTypeableParamsCount;
		return this;
	}
//	private void setDeclaring(Class clDeclaring) {
//		this.clDeclaring = clDeclaring;
//	}
//	private void setConcrete(Class clConcrete) {
//		this.clConcrete = clConcrete;
//	}
	
	public Class getMethodReturnType(){
		return method.getReturnType();
	}
	
	public static enum EClassToStrMode{
		OnlyAnon,
		OnlyClass,
		Both,
		;
	}
	
	public String classToStr(Class cl,boolean bUseSimpleNames, EClassToStrMode e){
		String strAnon="";
		if(cl.isAnonymousClass()){
			if(bUseSimpleNames){
				// there is no getSimpleName() for anonymous class...
				String[] astr = cl.getName().split("[.]");
				strAnon=astr[astr.length-1];
			}else{
				strAnon=cl.getName();
			}
			
			cl = cl.getSuperclass();
		}
		
		String strClass = (bUseSimpleNames ? cl.getSimpleName() : cl.getName());
		
		switch(e){
			case Both:
				return strClass+"/"+strAnon;
			case OnlyAnon:
				return strAnon;
			case OnlyClass:
				return strClass;
		}
		
		throw new UnsupportedOperationException("mode not implemented: "+e);
	}
	
	public String getFullHelp(boolean bUseSimpleNames, boolean bOverrideWithConcrete){
		String strFull="";
		
		Class cl = bOverrideWithConcrete ? getConcrete() : getDeclaring();
//		String strAnon="";
//		if(cl.isAnonymousClass()){
//			String[] astr = cl.getName().split("[.]");
//			strAnon="<"+astr[astr.length-1]+">";
//			cl = cl.getSuperclass();
//		}
//		String strClassName=(bUseSimpleNames ? cl.getSimpleName() : cl.getName());;
//		strClassName+=strAnon;
//		strFull+=strClassName+strAnon+".";
		strFull+=classToStr(cl,bUseSimpleNames,EClassToStrMode.OnlyClass)+"."+getMethodHelp(bUseSimpleNames);
		
//		String strDecl = (bUseSimpleNames ? getDeclaring().getSimpleName() : getDeclaring().getName());
		
//		if(bOverrideWithConcrete){
//			String strConcrete = "";
//			if(getConcrete().isAnonymousClass()){
//				if(bUseSimpleNames){
//					String[] astr = getConcrete().getName().split("[.]");
//					strConcrete = "(anonymous)"+astr[astr.length-1];
//				}else{
//					String[] astr = getConcrete().getName().split("[.]");
//					strConcrete = "(anonymous)"+astr[astr.length-1];
//				}
//			}else{
//				strConcrete=(bUseSimpleNames ? getConcrete().getSimpleName() : getConcrete().getName());
//			}
//			
//			strFull+=strConcrete;
//		}else{
//			strFull+=strDecl;
//		}
//		strFull+=".";
		
//		strFull+=getMethodHelp(bUseSimpleNames);
		
		/**
		 * as a comment that is compatible with java scripting 
		 */
		strFull+=" //";
		
		Class clRet = getMethodReturnType();
		if(!Void.class.isAssignableFrom(clRet) && !void.class.isAssignableFrom(clRet)){
			strFull+="=";
		}
		strFull+=(bUseSimpleNames?clRet.getSimpleName():clRet.getName());
		
		if(isStatic())strFull+=" <STATIC>";
		
		if(bOverrideWithConcrete){
			strFull+=" <"+classToStr(getDeclaring(),bUseSimpleNames,EClassToStrMode.Both)+">";
		}else{
			// TODO for CallableXAnon, the method call() appears 2 time, one returning Boolean, other returning Object, and there seems to have no way to distinguish between them other than that return type? mostly because both are the same, what is actually pointless..., prefer to show only the ret boolean one?
			String strAnon = classToStr(getDeclaring(),bUseSimpleNames,EClassToStrMode.OnlyAnon);
			if(!strAnon.isEmpty())strFull+=" <"+strAnon+">";
		}
		
		if(getNonUserTypeableParamsCount()>0)strFull+=" <UserCannotTypeCount="+getNonUserTypeableParamsCount()+">";
		
		this.strLastFullHelp=strFull.trim();
		
		return strLastFullHelp;
	}
	
	public URI getAsJavadocURI(){
		URI uri=null;
		try {
			// the html file
			String strURI=JavadocI.i().getJavadocFolder();
			strURI+=getDeclaring().getName().replace(".","/");
			strURI+=".html";
			
			uri = new File(strURI).toURI();
			
			// the method anchor
			String strParamTypes="";
			for(Class clPT:method.getParameterTypes()){
				if(!strParamTypes.isEmpty())strParamTypes+="-"; //in between
				strParamTypes+=clPT.getName(); //primitives has no dots (only wrappers does)
			}
			strParamTypes=strParamTypes.replace("$", "."); //inner classes fix
			strURI=uri.toString().replace("$", ".")+"#"+method.getName()+"-"+strParamTypes+"-";
			
			uri=new URI(strURI);
		} catch (URISyntaxException e) {
			throw new DetailedException(e,uri);
		}
		
		return uri;
	}
	
	public String getMethodHelp(boolean bUseSimpleParamNames){
		String strM = "";
		
		
		strM+=method.getName();
		
		strM+="(";
		String strP="";
		for(Class<?> p:method.getParameterTypes()){
			if(!strP.isEmpty())strP+=",";
			strP+=bUseSimpleParamNames?p.getSimpleName():p.getName();
		}
		strM+=strP+")";
		
		return strM;
	}
	public MethodX setObject(Object obj) {
		DetailedException.assertNotAlreadySet(this.objConcrete, obj, this);
		this.objConcrete=obj;
//		this.clConcrete=obj.getClass();
		return this;
	}
	public Object getConcreteObjectInstance(){
		return objConcrete;
	}
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("MethodHelp [strLastFullHelp=");
		builder.append(strLastFullHelp);
		builder.append(", m=");
		builder.append(method);
		builder.append(", obj=");
		builder.append(objConcrete);
		builder.append(", bStatic=");
		builder.append(bStatic);
		builder.append(", iNonUserTypeableParamsCount=");
		builder.append(iNonUserTypeableParamsCount);
		builder.append("]");
		return builder.toString();
	}
}
