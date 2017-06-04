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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;

import com.github.devconslejme.misc.QueueI.CallableXAnon;

/**
 * use as:
 * SomeDevConsClass.someMethodThere(){assert(AssertionsI.i().someMethodHere());}
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class AssertionsI {
	public static AssertionsI i(){return GlobalManagerI.i().get(AssertionsI.class);}
	
	/**
	 * devcons project shall stick to the alternative methods
	 * @return
	 */
	public boolean useAlternativeMethods(int iIncStack){
		String strPkg = this.getClass().getPackage().getName();
		strPkg = strPkg.substring(0, strPkg.lastIndexOf(".")-1);
		
		return !Thread.currentThread().getStackTrace()[1+iIncStack].getClassName().startsWith(strPkg);
	}
	
	public boolean restrictedCaller(Class clCaller,int iIncStack){
		return Thread.currentThread().getStackTrace()[2+iIncStack].getClassName().equals(clCaller.getName());
	}
	
	private boolean	bAllEveryFrame;
	private CallableXAnon	cx;
	public void configure(){
		cx=(new CallableXAnon() {
			private Iterator<Entry<Object, Asserter>>	it;

			@Override
			public Boolean call() {
				if(isAllEveryFrame()){
					assertAllRemainUnmodified(false);
				}else{
					if(it==null)it = hmAssert.entrySet().iterator();
					if(it.hasNext()){
						assertEntry(it.next()); //default is one per frame
					}else{
						it=null; //to restart
					}
				}
				return true;
			}
		}.enableLoopMode());
		
		QueueI.i().enqueue(cx);
	}
	
	public void setCheckDelay(float f){
		cx.setDelaySeconds(f);
	}
	
	public static class Asserter{
		Object objOriginalClone;
		StackTraceElement[] asteConfiguredAt;
	}
	
	private HashMap<Object,Asserter> hmAssert = new HashMap<Object,Asserter>();
	public <T> void putAssertRemainUnmodified(T objThatShouldNotChangeItsFieldValues, T objCopyOrCloneWithCurrentFieldsValues){
		assert(hmAssert.get(objThatShouldNotChangeItsFieldValues)==null);
		Asserter ass = new Asserter();
		ass.asteConfiguredAt=Thread.currentThread().getStackTrace();
		ass.objOriginalClone=objCopyOrCloneWithCurrentFieldsValues;
		hmAssert.put(objThatShouldNotChangeItsFieldValues,ass);
		assertObjects(objThatShouldNotChangeItsFieldValues, ass);//objCopyOrCloneWithCurrentFieldsValues
	}
	private void assertObjects(Object obj, Asserter ass){
		if(!obj.equals(ass.objOriginalClone)){
			throw new DetailedException("object changed somewhere...", obj, ass.objOriginalClone, "Configured At:", ass.asteConfiguredAt);
		}
	}
	private void assertEntry(Entry<Object, Asserter> entry){
		assertObjects(entry.getKey(), entry.getValue());
//		if(!entry.getKey().equals(entry.getValue())){
//			throw new DetailedException("object changed", entry.getKey(), entry.getValue());
//		}
	}
	public void assertAllRemainUnmodified(boolean bLogAllOk){
		for(Entry<Object, Asserter> entry:hmAssert.entrySet()){
			if(bLogAllOk){
				MessagesI.i().output(System.out, "Assert", this, 
					entry.getKey().getClass().getSimpleName()+": Original="+entry.getValue().objOriginalClone+", Current="+entry.getKey());
			}
			
			assertEntry(entry);
		}
	}

	public boolean isAllEveryFrame() {
		return bAllEveryFrame;
	}

	public void setAllEveryFrame(boolean bAllEveryFrame) {
		this.bAllEveryFrame = bAllEveryFrame;
	}
	
}
