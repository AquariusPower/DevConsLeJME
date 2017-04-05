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

package com.github.devconslejme.devcons;

import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.jme3.app.Application;

/**
 * Globally accessible access other single instances easily.
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
@Deprecated
public class DevConsGlobalsI {
  private static DevConsGlobalsI instance=new DevConsGlobalsI();
  public static void setGlobalOverride (DevConsGlobalsI inst){
  	if(DevConsGlobalsI.instance!=null)throw new NullPointerException("already set "+instance+", "+inst);
  	DevConsGlobalsI.instance=inst;
  }
  public static DevConsGlobalsI i (){return instance;}
  
	public <T> void put(Class<T> cl, T obj){
		GlobalInstanceManagerI.i().put(cl, obj);
	}
	
//	public Application app(){return GlobalInstanceManagerI.i().get(Application.class);}
	
//	public void assertIsCurrentMainThread(){
//		Thread threadMain = GlobalInstanceManagerI.i().get(Thread.class);
//		if(!threadMain.equals(Thread.currentThread())){
//			throw new DetailedException("should be the main thread!!!", i(), Thread.currentThread());
//		}
//	}
	
}
