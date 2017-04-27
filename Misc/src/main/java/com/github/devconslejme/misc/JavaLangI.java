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
import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.web.WebView;

import javax.swing.JFrame;

import com.google.common.collect.Lists;
import com.google.common.primitives.Primitives;
import com.jme3.app.Application;
import com.jme3.system.JmeCanvasContext;
import com.jme3.system.SystemListener;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaLangI {
	public static JavaLangI i(){return GlobalManagerI.i().get(JavaLangI.class);}
	
	public static enum EArrayType{
		/** HashMap TreeMap etc */
		Map,
		
		/** Object[] */
		Simple,
		
		/** Collection, ArrayList, List etc */
		Iterable,
		;
	}
	
//	public static enum EPrimivesAndString{
//		String(String.class),
//		Boolean(boolean.class),
//		;
//		private Class	cl;
//		EPrimivesAndString(Class cl){
//			this.cl=cl;
//		}
//		public String s(){return toString();}
//	}
	
	private Class[]	aclType = new Class[]{
			String.class,
			
			boolean.class,
			
			float.class,
			double.class,
			
			short.class,
			int.class,
			long.class,
			
			char.class,
			byte.class,
	};
	private String	strJavadocFolder = "javadoc/";
//	private String[] astrType;
	
	public JavaLangI(){
//		astrType = new String[aclType.length];
//		for(Class cl:aclType){
//			astrType[i++]=cl.getSimpleName();
//		}
	}
	
	public boolean isSomeArrayType(Object objValue){
		return getArrayTypeFor(objValue)!=null;
	}
	
	public EArrayType getArrayTypeFor(Object objValue){
		if(objValue==null)return null;
		
		if(objValue instanceof Map) { //HashMap TreeMap etc
			return EArrayType.Map;
		}else{
			if(objValue.getClass().isArray()){
				return EArrayType.Simple;
			}else
			if(objValue instanceof Iterable){
				return EArrayType.Iterable;
			}
		}
		
		return null;
	}
	
	/**
	 * 
	 * @param objValue must be a Map (HashMap, TreeMap etc), an Iterable (AttayList, Collection etc) or Object[] (any simple array)
	 * @return Object[][0]=key,Object[][1]=value, the key will be an index for simple arrays
	 */
	public Object[][] convertToKeyValueArray(Object objValue){
		Object[][] aaobjKeyVal=null;
		
		EArrayType e = getArrayTypeFor(objValue);
		Object[] aobjVal=null;
		switch(e){
			case Map:{
				Set<Map.Entry> es = ((Map)objValue).entrySet();
				aaobjKeyVal = new Object[es.size()][2];
				int i=0;
				for(Entry entry:es){
					aaobjKeyVal[i][0]=entry.getKey();
					aaobjKeyVal[i][1]=entry.getValue();
					i++;
				}
				}break;
			case Simple:
				aobjVal = (Object[])objValue;
			case Iterable:{
				if(aobjVal==null){
					aobjVal = Lists.newLinkedList((Iterable<?>)objValue).toArray();
				}
				
				if(aobjVal!=null){
					aaobjKeyVal = new Object[aobjVal.length][2];
					for(int i=0;i<aobjVal.length;i++){
						aaobjKeyVal[i][0]=i;
						aaobjKeyVal[i][1]=aobjVal[i];
					}
				}
				}break;
		}
		
		return aaobjKeyVal;
	}

	/**
	 * TODO add enclosings
	 * @param obj
	 * @param bSimpleName
	 * @return
	 */
	public String getClassTreeReportFor(Object obj,boolean bSimpleName){
		ArrayList<Class<?>> ac = getSuperClassesOf(obj,true);
		String strClassTree="";
		for(Class<?> cl:ac){
			if(!strClassTree.isEmpty())strClassTree+="/";
			strClassTree+= bSimpleName ? cl.getSimpleName() : cl.getName();
		}
		return strClassTree;
	}
	/**
	 * Differs from: obj.getClass().getDeclaredClasses()
	 * 
	 * Will include the concrete/instanced one too.
	 * 
	 * @param obj
	 * @return
	 */
	public ArrayList<Class<?>> getSuperClassesOf(Object obj,boolean bAddConcreteToo){
		ArrayList<Class<?>> ac = new ArrayList<Class<?>>();
		
		Class<?> cl = obj.getClass();
		while(cl!=null){
			boolean bAdd=true;
			
			if(!bAddConcreteToo && cl.toString().equals(obj.getClass().toString()))bAdd=false;
			if(cl.toString().equals(Object.class.toString()))bAdd=false; //avoid unnecessary base class
			
			if(bAdd)ac.add(cl);
			
			cl=cl.getSuperclass();
		}
		
		return ac;
	}
	
	public ArrayList<Class<?>> getEnclosingClassesOf(Object obj){
		ArrayList<Class<?>> ac = new ArrayList<Class<?>>();
		
		Class cl = obj.getClass().getEnclosingClass();
		while(cl!=null){
			ac.add(cl);
			cl = cl.getEnclosingClass();
		}
		
		return ac;
	}
	
	public String getClassName(Object obj, boolean bSimple){
		String str =  bSimple ? obj.getClass().getSimpleName() : obj.getClass().getName();
		if(str.isEmpty()){
			throw new DetailedException(
				"empty class name, do not use anonymous inner class if you want to use their name...",
				obj, getClassTreeReportFor(obj,true), bSimple);
		}
		return str;
	}

	public boolean isInnerClassOfConcrete(Object objInnerToCheck, Object objConcreteOwner){
		return (objInnerToCheck.getClass().getTypeName().startsWith(
			objConcreteOwner.getClass().getTypeName()+"$"));
	}
	
	public boolean isAnonymousClass(Object obj){
		/**
		 * TODO could something like this be less guessing?
		obj.getClass().getDeclaredClasses(); //[]
		obj.getClass().getDeclaringClass(); //null
		obj.getClass().getEnclosingClass(); //ex.: CommandsDelegator
		obj.getClass().getGenericSuperclass(); //ex.: CallQueueI$CallableX
		obj.getClass().getTypeName(); //ex.: CommandsDelegator$1
		Modifier.isStatic(obj.getClass().getModifiers()); //false
		Modifier.isTransient(obj.getClass().getModifiers()); //false
		Modifier.isVolatile(obj.getClass().getModifiers()); //false
		 */
		String str = obj.getClass().getTypeName();
		return (str.matches("^.*[$][0-9]*$"));
	}

	public boolean isRecursiveLoopOnMethod(String strMethodName, Class clOwner){
		StackTraceElement[] aste = Thread.currentThread().getStackTrace();
		ArrayList<StackTraceElement> asteList = new ArrayList<StackTraceElement>(Arrays.asList(aste));
		int iCountConstructor=0;
		int iCountGap=0;
		for(StackTraceElement ste:asteList){
			if(ste.getMethodName().equals(strMethodName) && ste.getClassName().equals(clOwner.getName())){
				iCountConstructor++;
				if(iCountGap>0){ // a gap between this previous and current exception
					return true;
				}
			}else{
				if(iCountConstructor>0){
					iCountGap++;
				}
			}
		}
		
		return false;
	}
	
	public Class[] getPrimitivesAndString(){
		return aclType;
	}
	
	/**
	 * 
	 * @param bFull if false will be simple name
	 * @return
	 */
	public ArrayList<String> getPrimitivesAndStringStrArray(boolean bFull) {
		ArrayList<String> astrType = new ArrayList<String>();
		for(Class cl:aclType){
			astrType.add(bFull?cl.getName():cl.getSimpleName());
		}
		return astrType;
	}
	
	public static class MethodHelp{
		/** mainly to help on debug */
		private String strLastFullHelp;
		private Method m;
		private Object obj;
		private Class clDeclaring;
		private Class clConcrete;
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
			return clDeclaring;
		}
		public Class getConcrete() {
			return clConcrete;
		}
		private void setMethod(Method m) {
			this.m = m;
		}
		private void setStatic(boolean bStatic) {
			this.bStatic = bStatic;
		}
		private void setNonUserTypeableParamsCount(int iNonUserTypeableParamsCount) {
			this.iNonUserTypeableParamsCount = iNonUserTypeableParamsCount;
		}
		private void setDeclaring(Class clDeclaring) {
			this.clDeclaring = clDeclaring;
		}
		private void setConcrete(Class clConcrete) {
			this.clConcrete = clConcrete;
		}
		
		public Class getMethodReturnType(){
			return m.getReturnType();
		}
		
		public String getFullHelp(boolean bUseSimpleNames, boolean bOverrideWithConcrete){
			
			String strFull="";
			
			String strConcrete=(bUseSimpleNames?clConcrete.getSimpleName():clConcrete.getName());
			String strDecl		=(bUseSimpleNames?clDeclaring.getSimpleName():clDeclaring.getName());
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
				String strURI=JavaLangI.i().getJavadocFolder();
				strURI+=clDeclaring.getName().replace(".","/");
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
			builder.append(", clDeclaring=");
			builder.append(clDeclaring);
			builder.append(", clConcrete=");
			builder.append(clConcrete);
			builder.append(", bStatic=");
			builder.append(bStatic);
			builder.append(", iNonUserTypeableParamsCount=");
			builder.append(iNonUserTypeableParamsCount);
			builder.append("]");
			return builder.toString();
		}
		
		
	}
	
	public ArrayList<MethodHelp> prepareAllMethodsHelp(Object obj){
		ArrayList<MethodHelp> amh = new ArrayList<MethodHelp>();
		
		for(Method m:obj.getClass().getMethods()){
			MethodHelp mh = new MethodHelp();
			mh.setConcrete(obj.getClass());
			mh.setObject(obj);
			mh.setDeclaring(m.getDeclaringClass());
			mh.setMethod(m);
			mh.setStatic(Modifier.isStatic(m.getModifiers()));
			
			int i=0;
			for(Class<?> p:m.getParameterTypes()){
				if(!isCanUserTypeIt(p))i++;
			}
			mh.setNonUserTypeableParamsCount(i);
			
			amh.add(mh);
		}
		
		return amh;
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
	
	public String getJavadocFolder() {
		return strJavadocFolder;
	}

	public void setJavadocFolder(String strJavadocFolder) {
		this.strJavadocFolder = strJavadocFolder;
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
}
