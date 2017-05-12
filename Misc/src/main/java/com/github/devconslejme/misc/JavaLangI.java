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

import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.collect.Lists;
import com.google.common.primitives.Primitives;

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
	
	
	public boolean isCanUserTypeIt(Object obj){
		return isCanUserTypeIt(obj.getClass());
	}
	public boolean isCanUserTypeIt(Class cl){
		if(String.class.isAssignableFrom(cl))return true;
		
		if(Enum.class.isAssignableFrom(cl))return true; //enums are essentially a convertion from string or int
		
		if(cl.isPrimitive())return true;
		if(Primitives.isWrapperType(cl))return true; //TODO last as is possibly "slower"?
		
		return false;
	}
	
	/**
	 * as the simple enum name toString() may conflict with other enum classes in a same HashMap for ex.
	 * @param e
	 * @return
	 */
	public String enumUId(Enum e) {
		return e.getClass().getName()+"."+e.toString();
	}

//	public Method getSetterFor(Object objInstanced, String strGetterMethodName) {
	/**
	 * like bean
	 * @param mGetter
	 * @return
	 */
	public Method getBeanSetterFor(Method mGetter, boolean bIsRequired) {
		try {
			String strBaseName = null;
			if(mGetter.getName().startsWith("is" ))strBaseName=mGetter.getName().substring(2);
			if(mGetter.getName().startsWith("get"))strBaseName=mGetter.getName().substring(3);
			
			return mGetter.getDeclaringClass().getMethod("set"+strBaseName, mGetter.getReturnType());
		} catch (NoSuchMethodException | SecurityException ex) {
			if(bIsRequired){
				throw new DetailedException(ex, mGetter, bIsRequired);
//				MessagesI.i().warnMsg(this, e.getMessage(), mGetter, e);
			}
		}
		
		return null;
	}
	/**
	 * 
	 * @param objInstance
	 * @param mSetter
	 * @param clType
	 * @param strValue
	 * @return true on success
	 */
	public boolean setBeanValueAt(Object objInstance, Method mSetter, Class clType, String strValue) {
		try {
			mSetter.invoke(objInstance, ESimpleType.forClass(clType,true).parse(strValue)); //TODO returns anything useful?
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
			MessagesI.i().warnMsg(this, ex.getMessage(), mSetter, clType, strValue, objInstance, ex);
			return false;
		}
		
		return true;
	}
	
	/**
	 * to be a bean, must have getter and setter,
	 * if there is only a getter, it may not be a bean!!!
	 * and calling the "pseudo-getter" may execute code that will create some problem.
	 * @param m
	 * @return
	 */
	public boolean isBeanGetter(Method m){
		return 				
			(m.getName().startsWith("get") || m.getName().startsWith("is"))
			&&
			JavaLangI.i().isCanUserTypeIt(m.getReturnType())
			&&
			m.getParameterCount()==0
			&&
			getBeanSetterFor(m,false)!=null
		;
	}
	
	/**
	 * ensures types constraints 
	 * only valid if neither K nor V are Object type
	 * and K differs from V type on the concrete class type
	 * TODO complete with the missing overridable constraint related methods...
	 */
	@SuppressWarnings({ "unchecked"})
	public static class LinkedHashMapX<K,V> extends LinkedHashMap<K,V>{
		private static final long	serialVersionUID	= -2511773191940212404L;
		
		@Override
		public V put(K key, V value) {
			assert(key.getClass()!=Object.class);
			assert(value.getClass()!=Object.class);
			assert(key.getClass()!=value.getClass());
			return super.put(key, value);
		}
		
		@Deprecated
		@Override
		public boolean containsKey(Object key) {
			return super.containsKey((K)key);
		}
		public boolean containsKeyX(K key) {
			return super.containsKey(key);
		}
		
		@Deprecated
		@Override
		public V remove(Object key) {
			return super.remove((K)key);
		}
		public V removeX(K key) {
			return super.remove(key);
		}
		
	}

	public String copyToClipboard(String str) {
		if(str==null)return null;
		
		StringSelection ss = new StringSelection(str);
		Toolkit.getDefaultToolkit().getSystemClipboard()
			.setContents(ss, ss);
		
		return str;
	}

	public String readFromClipboard(boolean bEscapeNL){
		try{
			Transferable tfbl = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null);
			String str = (String) tfbl.getTransferData(DataFlavor.stringFlavor);
			if(bEscapeNL){
				str=str.replace("\n", "\\n");
			}
			
			return str;
		} catch (UnsupportedFlavorException | IOException e) {
			MessagesI.i().warnMsg(this, e.getMessage(), bEscapeNL, e);
//			LoggingI.i().logExceptionEntry(e,null);
		}
		
		return null;
	}
	
	public Class getClassForName(String str){
		try {
			return Class.forName(str);
		} catch (ClassNotFoundException e) {
			MessagesI.i().warnMsg(this, "failed to retrieve class for", str);
		}
		
		return null;
	}
}
