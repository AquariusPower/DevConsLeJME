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
 * - list all globals (to use on scripts for ex.)
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GlobalInstanceManagerI {
  private static GlobalInstanceManagerI instance=new GlobalInstanceManagerI();
  public static void setGlobalOverride (GlobalInstanceManagerI inst){
  	if(GlobalInstanceManagerI.instance!=null)throw new DetailedException("already set "+instance+", "+inst);
    GlobalInstanceManagerI.instance=inst;
  }
  public static GlobalInstanceManagerI i (){return instance;}
  
  public static interface IGlobalAddListener{
  	void attendToGlobalAdded(Object objInst);
  }
  private ArrayList<IGlobalAddListener> aigalList = new ArrayList<IGlobalAddListener>();
  public void addGlobalAddListener(IGlobalAddListener igal){
  	if(!aigalList.contains(igal)){
  		aigalList.add(igal);
  	}else{
  		MessagesI.i().warnMsg(this, "already added "+igal);
  	}
  }
  
  protected HashMap<Class,Object> hmInst = new HashMap<Class,Object>();
  
  @SuppressWarnings("unchecked")
	public <T> T get (Class<T> cl){
    Object obj = hmInst.get (cl);
    if (obj==null){
      try {
      	put(cl, ((T)(obj=cl.newInstance())) );
//				hmInst.put(cl,obj=cl.newInstance ());
			} catch (InstantiationException | IllegalAccessException e) {
				NullPointerException npe = new DetailedException("unable to create new instance");
				npe.initCause(e);
				throw npe;
			}
    }
    return (T)obj;
  }

  public <T> void put(Class<T> cl,T obj){
  	Object objAlreadySet=hmInst.get(cl);
    if (objAlreadySet!=null){
      throw new DetailedException("already set: "+cl+", "+objAlreadySet+", "+obj);
    }
    
    hmInst.put(cl,obj);
    for(IGlobalAddListener igal:aigalList){
    	igal.attendToGlobalAdded(obj);
    }
    
		MessagesI.i().debugInfo(this,"created global instance: "+cl.getName(),obj);
  }
  
  public ArrayList<Object> getListCopy(){
  	return new ArrayList<Object>(hmInst.values());
  }
}