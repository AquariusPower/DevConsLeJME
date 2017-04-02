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

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.collect.Lists;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavaLangI {
	private static JavaLangI instance = new JavaLangI();
	/**instance*/ public static JavaLangI i(){return instance;}
	
	/**
	 * 
	 * @param objValue must be a Map, an Iterable or Object[]
	 * @return Object[][0]=key,Object[][1]=value, the key will be an index for simple arrays
	 */
	public Object[][] convertToKeyValueArray(Object objValue){
		Object[][] aaobjKeyVal=null;
		
		if(objValue instanceof Map) { //HashMap TreeMap etc
			Set<Map.Entry> es = ((Map)objValue).entrySet();
			aaobjKeyVal = new Object[es.size()][2];
			int i=0;
			for(Entry entry:es){
				aaobjKeyVal[i][0]=entry.getKey();
				aaobjKeyVal[i][1]=entry.getValue();
				i++;
			}
		}else{
			Object[] aobjVal=null;
			if(objValue.getClass().isArray()){
				aobjVal = (Object[])objValue;
			}else
			if(objValue instanceof Iterable){
				aobjVal = Lists.newLinkedList((Iterable<?>)objValue).toArray();
			}
			
			if(aobjVal!=null){
				aaobjKeyVal = new Object[aobjVal.length][2];
				for(int i=0;i<aobjVal.length;i++){
					aaobjKeyVal[i][0]=i;
					aaobjKeyVal[i][1]=aobjVal[i];
				}
			}
		}
		
		return aaobjKeyVal;
	}

}
