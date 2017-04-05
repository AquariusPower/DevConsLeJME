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

import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class StringI {
	public static StringI i(){return GlobalInstanceManagerI.i().get(StringI.class);}
	
	private String	strLastUid;
	
	public static enum EStringMatchMode{
		Exact,
		Contains,
		
		StartsWith,
		EndsWith,
		
		Regex,
		
		/**
		 * like a regex for each letter ex.: "Test"
		 * regex = ".*[T].*[e].*[s].*[t].*"
		 */
		Fuzzy,
		;
	}

	public boolean containsFuzzyMatch(String strToCheck, String strMatch, EStringMatchMode eMode, boolean bIgnoreCase){
		if(bIgnoreCase){ 
			strToCheck=strToCheck.toLowerCase();
			strMatch=strMatch.toLowerCase();
		}
		
	//	 allow multiline check
	//	strToCheck=strToCheck.replace("\n"," "); //space to prevent joining words
		
		switch (eMode) {
			case StartsWith:
				return strToCheck.startsWith(strMatch);
			case Contains:
				return strToCheck.contains(strMatch);
			case EndsWith:
				return strToCheck.endsWith(strMatch);
			case Exact:
				return strToCheck.equals(strMatch);
			case Fuzzy:
				int iFuzzyIndex = 0;
				for(char c : strToCheck.toCharArray()){
					if(c == strMatch.charAt(iFuzzyIndex)){
						iFuzzyIndex++;
						if(strMatch.length()==iFuzzyIndex){
							return true;
						}
					}
				}
				return false;
			case Regex:
				/**
				 * about "(?s)" see the javadoc of Pattern.DOTALL, 
				 * it will let dots match '\n' so if the string is a multiline, it will work!
				 */
				return strToCheck.matches("(?s)"+strMatch); 
		}
		
		return false;
	}

	/**
	 * Practically unlimited UId.
	 * This allows for unrelated/non-conflicting things to have same UId.
	 * 
	 * @param strLastId
	 * @return
	 */
	public String getNextUniqueId(String strLastId){
		int iRadix=Character.MAX_RADIX;
		/**
		 * Do not fix if null like in `if(strLastId==null)strLastId="0";`
		 * because the last id must be controlled by a manager String field,
		 * or be a static String field of the class...
		 * 
		 * fixing the null would just be prone to developer coding bugs...
		 * 
		 * TODO better not fix if empty either!?
		 */
		BigInteger bi = new BigInteger(strLastId,iRadix);
		bi=bi.add(new BigInteger("1"));
		return bi.toString(iRadix);
	}
	
	/**
	 * This uses a global uid.
	 * @return
	 */
	public String getNextUniqueId(){
		strLastUid=getNextUniqueId(strLastUid);
		return strLastUid;
	}
	
	public boolean isBlank(char ch){
		switch(ch){
			case ' ':return true;
			case '\t':return true;
		}
		return false;
	}

	public String asReport(String strid,Object objValue, boolean bSingle){
		String str="";
		
		if(!bSingle)str+="\t";
		
		str += strid+"("+asReport(objValue)+")";
		
		if(!bSingle)str+=",\n";
		
		return str;
	}
	public String asReportLine(Object objKey,Object obj,boolean bSingle){
		String str="";
		
		if(!bSingle)str+="\t";
		
		if(bSingle){
			str+=obj.getClass().getName();
		}else{
			str+=obj.getClass().getSimpleName();
		}
		
		if(objKey!=null){
			str+="["+objKey+"]";
		}
		
		str+="='"+obj+"'";
		
		if(!bSingle)str+=",\n";
		
		return str;
	}
	public String asReport(Object objValue){
		String str="(";
		
		if(objValue==null){
			str+=""+null;
		}else{
//			if(objValue instanceof IDebugReport){
//				return ((IDebugReport)objValue).getFailSafeDebugReport();
//			}
			
			Object[] aobjKey=null;
			Object[] aobjVal=null;
			if(objValue instanceof Map) { //HashMap TreeMap etc
				Set<Map.Entry> es = ((Map)objValue).entrySet();
				aobjVal=new Object[es.size()];
				aobjKey=new Object[es.size()];
				int i=0;
				for(Entry entry:es){
					aobjKey[i]=entry.getKey();
					aobjVal[i]=entry.getValue();
					i++;
				}
			}else
			if(objValue instanceof ArrayList) {
				ArrayList<?> aobjList = (ArrayList<?>) objValue;
				aobjVal=aobjList.toArray();
			}else
			if(objValue.getClass().isArray()){
				aobjVal = (Object[])objValue;
			}
			
			if(aobjVal!=null){ //array
				// this is just guessing all elements are of the same type
				str+=""+objValue.getClass().getName()+"["+aobjVal.length+"]{"; //TODO check if all elements are same type?
				if(aobjVal.length>0)str+="\n";
				for(int i=0;i<aobjVal.length;i++){//Object obj:aobjVal){
					Object objKey=null;if(aobjKey!=null)objKey=aobjKey[i];
					str+=asReportLine(objKey,aobjVal[i],false);
				}
				str+="}";
			}else{
				str+=asReportLine(null,objValue,true);
			}
		}
		
		return str+")";
	}
	
	/**
	 * TODO mix this with {@link #asReport(Object)}
	 * @param bPrependCurrentTime
	 * @param strMessage
	 * @param aobjCustom
	 * @return
	 */
	public String joinMessageWithObjects(boolean bPrependCurrentTime, String strMessage, Object... aobjCustom){
		String strTime=new SimpleDateFormat("HH:mm:ss").format(new Date(System.currentTimeMillis())); //can be realtime as this is just information
		if(bPrependCurrentTime){
			strMessage=strTime+strMessage;
		}
		return joinMessageWithObjects(strMessage,aobjCustom);
	}
	/**
	 * TODO mix this with {@link #asReport(Object)}
	 * @param strMessage
	 * @param aobjCustom
	 * @return
	 */
	public String joinMessageWithObjects(String strMessage, Object... aobjCustom){
		if(aobjCustom!=null){
			for(int i=0;i<aobjCustom.length;i++){
				Object obj = aobjCustom[i];
				strMessage+=multilineIfArray("\n\t["+i+"]",obj);
			}
		}
		
		return strMessage;
	}
	/**
	 * TODO mix this with {@link #asReport(Object)}
	 * @param strIndexPrefix
	 * @param obj
	 * @return
	 */
	private String multilineIfArray(String strIndexPrefix, Object obj){
		String strOut="";
		
		Object[] aobj = null; //obj instanceof Object[]
		Exception ex = null;
		if(obj instanceof Exception){
			ex = ((Exception)obj);
		}
		
		if(obj!=null){
			if (obj instanceof ArrayList) {
				ArrayList aobjList = (ArrayList) obj;
				aobj=aobjList.toArray();
			}else
			if(obj.getClass().isArray()){
				aobj = (Object[])obj;
	//			for(Object objInner:aobj){
			}
		}
		
		if(ex!=null){
			strOut+=strIndexPrefix+" "+ex.getClass().getSimpleName()+": "+ex.getMessage();
			aobj = ex.getStackTrace();
		}else
		if(aobj==null){
			return strIndexPrefix+fmtObj(obj,true);
		}
		
		String str1stObjClass=null;
		for(int i=0;i<aobj.length;i++){
			Object objInner=aobj[i]; //objInner.getClass().getName()
			
			String strFmtObj=null;
			if(objInner==null){
				strFmtObj = fmtObj(objInner,false);
			}else{
				if(str1stObjClass==null){
					str1stObjClass=objInner.getClass().getName();
					strOut+=strIndexPrefix+"ArrayOf: "+aobj.getClass().getTypeName();
//					strOut+=strIndexPrefix+"ArrayOf?(1stNotNullObjType):"+str1stObjClass;
				}
				
				strFmtObj = fmtObj(objInner, !str1stObjClass.equals(objInner.getClass().getName()));
			}
			
			strOut+=strIndexPrefix+"["+i+"] "+strFmtObj;
		}
		
		if(ex!=null){
			Throwable exCause = ex.getCause();
			if(exCause!=null){
				strOut+=multilineIfArray(strIndexPrefix+"[CAUSE]", exCause);
			}
		}
		
		return strOut;
	}
	/**
	 * TODO mix this with {@link #asReport(Object)}
	 * @param obj
	 * @param bShowClassName
	 * @return
	 */
	private String fmtObj(Object obj,boolean bShowClassName){
		String strCl = "";
		if(obj!=null && bShowClassName)strCl=obj.getClass().getName();
		
		String strObj = "";
//		if(strObj.isEmpty()){
//			if(obj instanceof IConstructed){
//				IConstructed ic = (IConstructed) obj;
//				if(!ic.isConstructed()){
//					strObj="(not constructed yet)";
//				}
//			}
//		}
		
//		if(strObj.isEmpty()){
//			if(obj instanceof IDebugReport){
//				IDebugReport ir = (IDebugReport) obj;
//				strObj=ir.getFailSafeDebugReport();
//			}
//		}
		
//		if(strObj.isEmpty()){
//			strObj=""+(obj==null?null:obj.toString());
//		}
		
		return ""+(obj==null ? null : strCl+": "+strObj); //this is better when dumping a sub-stacktrace
	}
}
