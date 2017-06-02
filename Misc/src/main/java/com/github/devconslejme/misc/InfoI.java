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
import java.util.Collection;
import java.util.HashMap;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class InfoI {
	public static InfoI i(){return GlobalManagerI.i().get(InfoI.class);}
	
	private String strInfoSeparator=", ";
	private int iInfoValueFloatScale=2;
	
	public String getInfoSeparator() {
		return strInfoSeparator;
	}

	public InfoI setInfoSeparator(String strInfoSeparator) {
		this.strInfoSeparator = strInfoSeparator;
		return this;
	}

	public int getInfoValueFloatScale() {
		return iInfoValueFloatScale;
	}

	public InfoI setInfoValueFloatScale(int iInfoValueFloatScale) {
		this.iInfoValueFloatScale = iInfoValueFloatScale;
		return this;
	}

	public static class Info{
		protected String strKey;
		protected Object objValue;
		protected Integer iFloatScale=null;
		
		public Info(String strKey, Object objValue) {
			this.strKey = strKey;
			assert this.strKey!=null;
			
			this.objValue = objValue;
			assert this.objValue!=null;
		}
		public Info(String strKey, float f, int iFloatScale) {
			this(strKey,f);
			this.iFloatScale=iFloatScale;
		}
		
		public String getKey() {
			return strKey;
		}
		
		@SuppressWarnings("unchecked")
		public <T> T getValue() {
			return (T)objValue;
		}
		public Integer getFloatScale() {
			return iFloatScale;
		}
		
	}
	
	public String fmtInfoValue(Info inf){
		if(Float.class.isInstance(inf.objValue)){
			return StringI.i().fmtFloat(
				(float)inf.objValue, 
				inf.iFloatScale==null ? iInfoValueFloatScale : inf.iFloatScale
			);
		}else
		if(Integer.class.isInstance(inf.objValue) || Long.class.isInstance(inf.objValue)){
			return ""+inf.objValue; //TODO fmt long?
		}else
		if(String.class.isInstance(inf.objValue)){
			return (String)inf.objValue;
		}
		
		throw new UnsupportedOperationException("type not supported "+inf.objValue.getClass());
	}
	
	public String prepareFullInfo(HashMap<String,Info>... ahminf){
		StringBuilder sb = new StringBuilder("");
		int i=0;
		for(HashMap<String, Info> hm:ahminf){
			sb.append(prepareFullInfo(hm));
			if(i<ahminf.length-1)sb.append(strInfoSeparator);
			i++;
		}
		return sb.toString();
	}
	public String prepareFullInfo(HashMap<String,Info> hminf){
		StringBuilder sb = new StringBuilder("");
		ArrayList<Info> ainf = new ArrayList<Info>(hminf.values());
		for(int i=0;i<ainf.size();i++){
			Info inf = ainf.get(i);
			sb.append(inf.strKey);
			sb.append("=");
			sb.append(fmtInfoValue(inf));
			
			if(i<hminf.size()-1)sb.append(strInfoSeparator);
		}
		return sb.toString();
	}
	
	/**
	 * 
	 * @param hm
	 * @param strKey
	 * @param f if null removes the key too
	 * @param iFloatScale
	 */
	public void putAt(HashMap<String, Info> hm, String strKey, Float f, int iFloatScale) {
		if(f==null)hm.remove(strKey);
		hm.put(strKey, new Info(strKey,f,iFloatScale));
	}
	
	public String prepareFullInfo(Collection<HashMap<String, Info>> values) {
		@SuppressWarnings("unchecked") HashMap<String, Info>[] ahm = new HashMap[values.size()];
		int i=0;
		for(HashMap<String, Info> hm:values){
			ahm[i]=hm;
			i++;
		}
		return prepareFullInfo(ahm);
	}

}
