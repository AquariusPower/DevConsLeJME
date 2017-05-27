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

package com.github.devconslejme.misc.jme;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavaLangI.FuncOut;
import com.github.devconslejme.misc.MainThreadI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.jme3.scene.Spatial;


/**
 * Do not use UserData with Spatials you instance.
 * It is much better to extend the required class and add clearly recognizable fields on it.
 * 
 * The UserData is mainly to apply at other Spatials that will easify extra functionalities without
 * having to deal with local HashMaps. 
 * (Opinion) Still then, a single custom class with any amount of fields will be more readable/reconizable and 
 * much less confusing than several spread values set directly using UserData. 
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class UserDataI {
	public static UserDataI i(){return GlobalManagerI.i().get(UserDataI.class);}
	
	/**
	 * all super classes of the object will be the keys
	 * see {@link #setUserDataPSHSafely(Spatial, String, Object)}
	 */
	private <T extends Spatial> boolean setUserDataPSHSafely(T spt, Object obj) {
		boolean b=false;
		for(Class<?> cl:JavaLangI.i().getSuperClassesOf(obj,true)){
			b=setUserDataPSHSafely(spt, cl.getName(), obj);
		}
		return b;
	}
	/**
	 * 
	 * @param spt
	 * @param strKey
	 * @param obj each key will be one super class of it
	 * @return if was set now or will be at the main thread 
	 */
	private <T extends Spatial> boolean setUserDataPSHSafely(T spt, String strKey, Object obj) {
		CallableX cx = new CallableX() {
			@SuppressWarnings("unchecked")
			@Override
			public Boolean call() {
				try{
					spt.setUserData(strKey, obj);
				}catch(IllegalArgumentException ex){
					spt.setUserData(strKey, new PseudoSavableHolder(obj));
				}
				return true;
			}
		};
		
		if(MainThreadI.i().isCurrentMainThread()){
			cx.call();
			return true;
		}else{
			QueueI.i().enqueue(cx);
		}
		
		return false;
	}
	
	public static interface IUDKey{
		Class getType();
		/** use the full enum package + class name + enum id */
		String getUId();
	}
	/**
	 * 
	 * @param spt
	 * @param cl
	 * @param bCreateIfNull requires class to have empty constructor
	 * @return
	 */
	private <R> R getUserDataPSH(Spatial spt, Class<R> cl, boolean bCreateIfNull){
		R ret = getUserDataPSH(spt, cl);
		if(ret==null && bCreateIfNull){
			try {
				setUserDataPSHSafely(spt, ret=cl.newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new DetailedException(e,spt,cl,bCreateIfNull);
			}
		}
		return ret;
	}
	private <R> R getUserDataPSH(Spatial spt, Class<R> cl){
		return getUserDataPSH(spt, cl.getName());
	}
	private <R> R getUserDataPSH(Spatial spt, String strKey){
		R ret = spt.getUserData(strKey);
		if(ret==null)return null;
		
		if(ret instanceof PseudoSavableHolder){
			@SuppressWarnings("unchecked")
			PseudoSavableHolder<R> sh = (PseudoSavableHolder<R>)ret;
			return sh.getRef();
		}
		
		return ret;
	}
	/**
	 * 
	 * @param spt
	 * @param eKey enum
	 * @return
	 */
	@SuppressWarnings({ "unchecked", "unused" })
	private <R> R getUserDataPSH(Spatial spt, IUDKey eKey){
		R ret = getUserDataPSH( spt, eKey.getUId() );
		if(ret!=null && !eKey.getType().isAssignableFrom(ret.getClass())){
			throw new DetailedException("incompatible types",ret.getClass(),eKey.getType(),spt,eKey);
		}
		return ret;
	}
	
	@SuppressWarnings({ "unchecked", "unused" })
	private boolean setUserDataPSH(Spatial spt, IUDKey eKey, Object obj){
		if(obj!=null && !eKey.getType().isAssignableFrom(obj.getClass())){
			throw new DetailedException("incompatible types",obj.getClass(),eKey,eKey.getType(),spt);
		}
		return setUserDataPSHSafely(spt, eKey.getUId(), obj);
	}
	
	/**
	 * 
	 * @param spt
	 * @param cl
	 * @param funcInstanceFactory if null will instance using this function
	 * @return
	 */
	private <T> T retrieve(Spatial spt, Class<T> cl, FuncOut<T> funcInstanceFactory){
		T ret = getUserDataPSH(spt, cl);
//		T obj = getUserDataPSH(spt, strKey);
		if(ret==null){
			ret=funcInstanceFactory.applyOut();
//			setUserDataPSHSafely(spt, strKey, ret);
			setUserDataPSHSafely(spt, ret);
		}
		return ret;
	}
	
	public void overwriteSafely(Spatial spt, Object obj){
		setUserDataPSHSafely(spt, obj);
	}
	
	public void putSafelyMustNotExist(Spatial spt, Object obj){
		Object objCurrent=getUserDataPSH(spt, obj.getClass(), false);
		if(objCurrent!=null){
			throw new DetailedException("already set", spt, objCurrent,obj);
		}
		setUserDataPSHSafely(spt, obj);
	}
	
	public <R> R getMustExistOrNull(Spatial spt, Class<R> cl){
		return getUserDataPSH(spt, cl, false);
	}
	
	public <R> R retrieveExistingOrCreateNew(Spatial spt, Class<R> cl){
		return getUserDataPSH(spt, cl, true);
	}
	@SuppressWarnings("unchecked")
	public boolean contains(Spatial spt, Class cl){
		return getUserDataPSH(spt, cl, false)!=null;
	}
	
	private <R> R retrieve(Spatial spt, Class<R> cl, boolean bCreateIfNull){
		return getUserDataPSH(spt, cl, bCreateIfNull);
	}
	
//	private class OldMethodsJustToAvoidUsingOrReimplementingSelfNote{
//		private <R> R retrieve(Spatial spt, Class<R> cl, boolean bCreateIfNull){
//			return getUserDataPSH(spt, cl, bCreateIfNull);
//		}
////		@SuppressWarnings("unchecked")
////		private <T> T put(Spatial spt, T objToStore){
////			return retrieve(spt, (Class<T>)objToStore.getClass(), new FuncOut<T>(){
////				@Override
////				public T applyOut() {
////					return objToStore;
////				}
////			});
////		}
//	}
}
