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

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavadocI {
	public static JavadocI i(){return GlobalManagerI.i().get(JavadocI.class);}
	
	private String	strJavadocFolder = "javadoc/";
	
	public static class MethodHelp{
		/** mainly to help on debug */
		private String strLastFullHelp;
		private Method m;
		private Object obj;
//		private Class clDeclaring;
//		private Class clConcrete;
		private boolean bStatic;
		private int iNonUserTypeableParamsCount;
		
		public Method getMethod() {
			return m;
		}
		public boolean isStatic() {
			return bStatic;
		}
		public int getNonUserTypeableParamsCount() {
			return iNonUserTypeableParamsCount;
		}
		public Class getDeclaring() {
//			return clDeclaring;
			return m.getDeclaringClass();
		}
		public Class getConcrete() {
//			return clConcrete;
			return obj.getClass();
		}
		private void setMethod(Method m) {
			this.m = m;
//			this.clDeclaring=m.getDeclaringClass();
		}
		private void setStatic(boolean bStatic) {
			this.bStatic = bStatic;
		}
		private void setNonUserTypeableParamsCount(int iNonUserTypeableParamsCount) {
			this.iNonUserTypeableParamsCount = iNonUserTypeableParamsCount;
		}
//		private void setDeclaring(Class clDeclaring) {
//			this.clDeclaring = clDeclaring;
//		}
//		private void setConcrete(Class clConcrete) {
//			this.clConcrete = clConcrete;
//		}
		
		public Class getMethodReturnType(){
			return m.getReturnType();
		}
		
		public String getFullHelp(boolean bUseSimpleNames, boolean bOverrideWithConcrete){
			
			String strFull="";
			
			String strConcrete=(bUseSimpleNames?getConcrete().getSimpleName():getConcrete().getName());
			String strDecl		=(bUseSimpleNames?getDeclaring().getSimpleName():getDeclaring().getName());
			strFull+=(bOverrideWithConcrete?strConcrete:strDecl)+".";
			
			strFull+=getMethodHelp(bUseSimpleNames);
			
			/**
			 * as a comment that is compatible with java scripting 
			 */
			strFull+=" //";
			
			Class clRet = getMethodReturnType();
			strFull+=(bUseSimpleNames?clRet.getSimpleName():clRet.getName());
			
			if(bStatic)strFull+=" <STATIC>";
			
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
		private void setObject(Object obj) {
			this.obj=obj;
//			this.clConcrete=obj.getClass();
		}
		public Object getObject(){
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
	
	public void browseJavadoc(MethodHelp mh) {
		URI uri = mh.getAsJavadocURI();
		try {
			// external web browser 
			Desktop.getDesktop().browse(uri);
		} catch (IOException e) {
			MessagesI.i().warnMsg(this, e.getMessage(), e, mh, uri);
		}
	}

	public ArrayList<MethodHelp> prepareAllMethodsHelp(Object obj){
		ArrayList<MethodHelp> amh = new ArrayList<MethodHelp>();
		
		for(Method m:obj.getClass().getMethods()){
			MethodHelp mh = new MethodHelp();
			mh.setObject(obj);
			mh.setMethod(m);
//			mh.setConcrete(obj.getClass());
//			mh.setDeclaring(m.getDeclaringClass());
			mh.setStatic(Modifier.isStatic(m.getModifiers()));
			
			int i=0;
			for(Class<?> p:m.getParameterTypes()){
				if(!JavaLangI.i().isCanUserTypeIt(p))i++;
			}
			mh.setNonUserTypeableParamsCount(i);
			
			amh.add(mh);
		}
		
		return amh;
	}
	
	public String getJavadocFolder() {
		return strJavadocFolder;
	}

	public void setJavadocFolder(String strJavadocFolder) {
		this.strJavadocFolder = strJavadocFolder;
	}
}
