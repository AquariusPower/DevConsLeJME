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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.MainThreadI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.jme3.scene.Spatial;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class UserDataI {
	public static UserDataI i(){return GlobalManagerI.i().get(UserDataI.class);}
	
	/**
	 * all super classes of the object will be the keys
	 * see {@link #setUserDataPSH(Spatial, String, Object)}
	 */
	public <T extends Spatial> boolean setUserDataPSH(T spt, Object obj) {
		boolean b=false;
		for(Class<?> cl:JavaLangI.i().getSuperClassesOf(obj,true)){
			b=setUserDataPSH(spt, cl.getName(), obj);
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
	public <T extends Spatial> boolean setUserDataPSH(T spt, String strKey, Object obj) {
		CallableX cx = new CallableX() {
			@SuppressWarnings("unchecked")
			@Override
			public Boolean call() {
				spt.setUserData(strKey, new PseudoSavableHolder(obj));
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
	}
	public <R> R getUserDataPSH(Spatial spt, Class<R> cl){
		return getUserDataPSH(spt, cl.getName());
	}
	public <R> R getUserDataPSH(Spatial spt, String strKey){
		@SuppressWarnings("unchecked")
		PseudoSavableHolder<R> sh = (PseudoSavableHolder<R>)spt.getUserData(strKey);
		if(sh==null)return null;
		return sh.getRef();
	}
}
