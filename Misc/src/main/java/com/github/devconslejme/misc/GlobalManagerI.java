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
import java.util.HashMap;

/**
 * This makes it easy to:
 * - override a global instance.
 * - list all globals (to use on javascript for ex.)
 * 
 * Obs.: The classes tha have globals may also have alternative instances for specific usages.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GlobalManagerI {
	/** code shortener */
	public static class G{
		/** returns the instance for the specified class */
		public static <T> T i(Class<T>cl){
			return GlobalManagerI.i().get(cl);
		}
	}
	
  private static GlobalManagerI instance=new GlobalManagerI();
  public static void setGlobalOverride (GlobalManagerI inst){
  	if(GlobalManagerI.instance!=null)throw new DetailedException("already set "+instance+", "+inst);
    GlobalManagerI.instance=inst;
  }
  public static GlobalManagerI i (){return instance;}
  
  private ArrayList<IGlobalAddListener> aigalList = new ArrayList<IGlobalAddListener>();
	private HashMap<Class<Enum>,Enum[]> hmEnumVals = new HashMap<Class<Enum>,Enum[]>();
	
  public static interface IGlobalAddListener{
  	void globalInstanceAddedEvent(Class clType, Object objValue);

		void globalEnumAddedEvent(Class<Enum> cle, Enum[] ae);
  }
  
  public void addGlobalAddListener(IGlobalAddListener igal){
  	if(!aigalList.contains(igal)){
  		aigalList.add(igal);
  	}else{
  		MessagesI.i().warnMsg(this, "already added "+igal);
  	}
  }
  
  protected HashMap<Class,Object> hmInst = new HashMap<Class,Object>();
  
  public boolean isSet(Class cl){
  	return hmInst.get(cl)!=null;
  }
  /**
   * see {@link #retrieveOverridingSupers(Class, boolean, Class...)}
   * @param cl
   * @return
   */
  public <T> T get(Class<T> cl){
  	return retrieveOverridingSupers(cl,true);
  }
  /**
   * Use this one to auto set/put global overrides
   * 
   * @param cl
   * @param bCreateNewInstanceIfNull
   * @param aclSuperAttrToo super class types to attribute this instance to them too, on the hashmap
   * @return
   */
  @SuppressWarnings("unchecked")
	public <T> T retrieveOverridingSupers(Class<T> cl, boolean bCreateNewInstanceIfNull, Class... aclSuperAttrToo){
    Object obj = hmInst.get(cl);
    if (obj==null && bCreateNewInstanceIfNull){
      try {
      	putGlobal(cl, ((T)(obj=cl.newInstance())) );
      	for(Class clSuper:aclSuperAttrToo){
      		putGlobal(clSuper,obj);
      	}
//      	putConcrete(obj=cl.newInstance()); // 
			} catch (InstantiationException | IllegalAccessException e) {
				NullPointerException npe = new DetailedException("unable to create new instance")
					.initCauseAndReturnSelf(e);
				throw npe;
			}
    }
    return (T)obj;
  }
  
  /**
   * the concrete class will be used as key
   * @param obj
   * @return
   */
  @SuppressWarnings("unchecked")
	public <T> T putConcrete(T obj){
  	putGlobal((Class<T>)obj.getClass(), obj);
//  	for(Class cl : new JavaLangI().getSuperClassesOf(obj, true)){
//    	putGlobal(cl,obj);
//  	}
  	
  	return obj;
  }
  /**
   * instead of put() to distinguish on IDE searching for it's usages TODO this is not good... 
   * @param cl
   * @param obj
   * @return
   */
  public <T> T putGlobal(Class<? extends T> cl,T obj){
  	Object objAlreadySet=hmInst.get(cl);
    if (objAlreadySet!=null){
      throw new DetailedException("already set: "+cl+", "+objAlreadySet+", "+obj+" "
      	+(obj==objAlreadySet?"REDUNDANT/equalToAlreadySet!":""));
    }
    
    // inheritance consistency check
    for(Object objExisting:hmInst.values()){
    	if(obj==objExisting)continue;
    	if(
    			objExisting.getClass().isInstance(obj) || 
    			objExisting.getClass().isAssignableFrom(obj.getClass()) ||
    			obj.getClass().isInstance(objExisting) || 
    			obj.getClass().isAssignableFrom(objExisting.getClass())
    	){
    		throw new DetailedException("there should have only one inherited global type",
    			objExisting,objExisting.getClass(),objExisting.getClass().getClasses(),
    			obj				 ,obj				 .getClass(),obj				.getClass().getClasses()
    		);
    	}
    }
    
    hmInst.put(cl,obj);
    callListeners(cl,obj);
    
		MessagesI.i().debugInfo(this,"created global instance: "+cl.getName(),obj);
		
		prepareEnumsOf(cl);
		
		return obj;
  }
	@SuppressWarnings("unchecked")
	protected void prepareEnumsOf(Class clWithEnums){
		Class<?>[] acl = clWithEnums.getDeclaredClasses();
		for(Class<?> cl:acl){
			if(JavaLangI.i().isEnumClass(cl)){
				Class<Enum> cle = (Class<Enum>)cl;
				putEnumClass(cle,null); //TODO can enum .values() be collected in some way?
			}
		}
	}
	public void putEnumClass(Class<Enum> cle,Enum[] values){
		if(hmEnumVals.get(cle)==null){
//			hmEnumVals.forcePut(cle,values);
			hmEnumVals.put(cle,values);
			callListeners(cle,values); // if values is null, the listener may help on providing them and setting again this enum here with the values filled up
		}
	}
	@SuppressWarnings("unchecked")
	protected void callListeners(Class cl,Object obj) {
    for(IGlobalAddListener igal:aigalList){
			if(JavaLangI.i().isEnumClass(cl)){
				igal.globalEnumAddedEvent((Class<Enum>)cl,(Enum[])obj);
			}else{
				igal.globalInstanceAddedEvent(cl,obj);
			}
    }
	}
	public Enum parseToEnum(String strFullEnumId){
//		for(Class<Enum> cle:hmEnumVals.inverse().values()){ //keys
		for(Class<Enum> cle:hmEnumVals.keySet()){ //keys
			Enum e = JavaLangI.i().parseToEnum(cle,strFullEnumId);
			if(e!=null)return e;
		}
		return null;
	}
	
	public HashMap<Class<Enum>,Enum[]> getGlobalEnumsListCopy(){
		return new HashMap<Class<Enum>,Enum[]>(hmEnumVals);
	}
	
  public ArrayList<Object> getListCopy(){
  	return new ArrayList<Object>(hmInst.values());
  }
	@SuppressWarnings("unchecked")
	public <T extends Enum> T[] getGlobalEnumValuesClNm(String strClassName) {
		try {
			Class cl = Class.forName(strClassName);
			if(JavaLangI.i().isEnumClass(cl)){
				return (T[]) getGlobalEnumValuesClE(cl);
			}
		} catch (ClassNotFoundException ex) {
			throw new DetailedException(ex, strClassName);
		}
		return null;
	}
	@SuppressWarnings("unchecked")
	public <T extends Enum> T[] getGlobalEnumValuesCl(Class cl) {
		if(JavaLangI.i().isEnumClass(cl)){
			return (T[]) getGlobalEnumValuesClE(cl);
		}
		return null;
	}
	@SuppressWarnings("unchecked")
	public <T extends Enum> T[] getGlobalEnumValuesClE(Class<T> cle) {
		return (T[]) hmEnumVals.get(cle);
	}
}