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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.collect.Lists;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaLangI {
	public static JavaLangI i(){return GlobalInstanceManagerI.i().get(JavaLangI.class);}
	
	public static enum EArrayType{
		/** HashMap TreeMap etc */
		Map,
		
		/** Object[] */
		Simple,
		
		/** Collection, ArrayList, List etc */
		Iterable,
		;
	}
	
	public boolean isSomeArrayType(Object objValue){
		return getArrayTypeFor(objValue)!=null;
	}
	
	public EArrayType getArrayTypeFor(Object objValue){
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
}
