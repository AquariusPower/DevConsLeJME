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
public class MethodHelp{
	/** mainly to help on debug */
	private String strLastFullHelp;
	private Method m;
	private Object obj;
//	private Class clDeclaring;
//	private Class clConcrete;
	private Boolean bStatic=null;
	private Integer iNonUserTypeableParamsCount=null;
	
	public Method getMethod() {
		return m;
	}
	public boolean isStatic() {
		return bStatic==null?false:bStatic;
	}
	public int getNonUserTypeableParamsCount() {
		return iNonUserTypeableParamsCount==null ? 0 : iNonUserTypeableParamsCount;
	}
	public Class getDeclaring() {
//		return clDeclaring;
		return m.getDeclaringClass();
	}
	public Class getConcrete() {
//		return clConcrete;
		return obj.getClass();
	}
	public MethodHelp setMethod(Method m) {
		DetailedException.assertNotAlreadySet(this.m, m, this);
		this.m = m;
//		this.clDeclaring=m.getDeclaringClass();
		return this;
	}
	public MethodHelp setStatic(boolean bStatic) {
		DetailedException.assertNotAlreadySet(this.bStatic, bStatic, this);
		this.bStatic = bStatic;
		return this;
	}
	public MethodHelp setNonUserTypeableParamsCount(int iNonUserTypeableParamsCount) {
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
		return m.getReturnType();
	}
	
	public String getFullHelp(boolean bUseSimpleNames, boolean bOverrideWithConcrete){
		
		String strFull="";
		
		String strDecl		=(bUseSimpleNames?getDeclaring().getSimpleName():getDeclaring().getName());
		strFull+=(bOverrideWithConcrete?
			(bUseSimpleNames?getConcrete().getSimpleName():getConcrete().getName()):
			strDecl
		)+".";
		
		strFull+=getMethodHelp(bUseSimpleNames);
		
		/**
		 * as a comment that is compatible with java scripting 
		 */
		strFull+=" //";
		
		Class clRet = getMethodReturnType();
		strFull+=(bUseSimpleNames?clRet.getSimpleName():clRet.getName());
		
		if(isStatic())strFull+=" <STATIC>";
		
		if(bOverrideWithConcrete)strFull+=" <"+strDecl+">";
		
		if(getNonUserTypeableParamsCount()>0)strFull+=" <UserCannotType="+getNonUserTypeableParamsCount()+">";
		
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
			for(Class clPT:m.getParameterTypes()){
				if(!strParamTypes.isEmpty())strParamTypes+="-"; //in between
				strParamTypes+=clPT.getName(); //primitives has no dots (only wrappers does)
			}
			strParamTypes=strParamTypes.replace("$", "."); //inner classes fix
			strURI=uri.toString()+"#"+m.getName()+"-"+strParamTypes+"-";
			
			uri=new URI(strURI);
		} catch (URISyntaxException e) {
			throw new DetailedException(e,uri);
		}
		
		return uri;
	}
	
	public String getMethodHelp(boolean bUseSimpleParamNames){
		String strM = "";
		
		
		strM+=m.getName();
		
		strM+="(";
		String strP="";
		for(Class<?> p:m.getParameterTypes()){
			if(!strP.isEmpty())strP+=",";
			strP+=bUseSimpleParamNames?p.getSimpleName():p.getName();
		}
		strM+=strP+")";
		
		return strM;
	}
	public MethodHelp setObject(Object obj) {
		DetailedException.assertNotAlreadySet(this.obj, obj, this);
		this.obj=obj;
//		this.clConcrete=obj.getClass();
		return this;
	}
	public Object getConcreteObjectInstance(){
		return obj;
	}
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("MethodHelp [strLastFullHelp=");
		builder.append(strLastFullHelp);
		builder.append(", m=");
		builder.append(m);
		builder.append(", obj=");
		builder.append(obj);
		builder.append(", bStatic=");
		builder.append(bStatic);
		builder.append(", iNonUserTypeableParamsCount=");
		builder.append(iNonUserTypeableParamsCount);
		builder.append("]");
		return builder.toString();
	}
}
