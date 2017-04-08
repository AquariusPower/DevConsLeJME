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

/**
 * Differently from toString(), this is intended to provide to  
 * end-user (or developer) high quality readable output.
 * 
 * TODO review/rework/join the methods, see JavaLangI.convertToKeyValueArray() too.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ReportI {
	public static ReportI i(){return GlobalInstanceManagerI.i().get(ReportI.class);}
	
	public static interface IReport{
		public String getReport(boolean bFull);
	}

	private boolean	bShowType=false;
	
//	/**
//	 * TODO needs review rework
//	 */
//	private String _asDebugInfo(String strid,Object objValue, boolean bSingle){
//		String str="";
//		
//		if(!bSingle)str+="\t";
//		
//		str += strid+"("+_asDebugInfo(objValue)+")";
//		
//		if(!bSingle)str+=",\n";
//		
//		return str;
//	}
//	/**
//	 * TODO needs review rework
//	 */
//	private String _asDebugInfoLine(Object objKey,Object obj,boolean bSingle){
//		String str="";
//		
//		if(!bSingle)str+="\t";
//		
//		if(bSingle){
//			str+=obj.getClass().getName();
//		}else{
//			str+=obj.getClass().getSimpleName();
//		}
//		
//		if(objKey!=null){
//			str+="["+objKey+"]";
//		}
//		
//		str+="='"+obj+"'";
//		
//		if(!bSingle)str+=",\n";
//		
//		return str;
//	}
//	/**
//	 * TODO needs review rework
//	 */
//	private String _asDebugInfo(Object objValue){
//		String str="(";
//		
//		if(objValue==null){
//			str+=""+null;
//		}else{
////			if(objValue instanceof IDebugReport){
////				return ((IDebugReport)objValue).getFailSafeDebugReport();
////			}
//			
//			Object[] aobjKey=null;
//			Object[] aobjVal=null;
//			if(objValue instanceof Map) { //HashMap TreeMap etc
//				Set<Map.Entry> es = ((Map)objValue).entrySet();
//				aobjVal=new Object[es.size()];
//				aobjKey=new Object[es.size()];
//				int i=0;
//				for(Entry entry:es){
//					aobjKey[i]=entry.getKey();
//					aobjVal[i]=entry.getValue();
//					i++;
//				}
//			}else
//			if(objValue instanceof ArrayList) {
//				ArrayList<?> aobjList = (ArrayList<?>) objValue;
//				aobjVal=aobjList.toArray();
//			}else
//			if(objValue.getClass().isArray()){
//				aobjVal = (Object[])objValue;
//			}
//			
//			if(aobjVal!=null){ //array
//				// this is just guessing all elements are of the same type
//				str+=""+objValue.getClass().getName()+"["+aobjVal.length+"]{"; //TODO check if all elements are same type?
//				if(aobjVal.length>0)str+="\n";
//				for(int i=0;i<aobjVal.length;i++){//Object obj:aobjVal){
//					Object objKey=null;if(aobjKey!=null)objKey=aobjKey[i];
//					str+=_asDebugInfoLine(objKey,aobjVal[i],false);
//				}
//				str+="}";
//			}else{
//				str+=_asDebugInfoLine(null,objValue,true);
//			}
//		}
//		
//		return str+")";
//	}
//	
//	/**
//	 * TODO mix this with {@link #_asDebugInfo(Object)}
//	 * @param bPrependCurrentTime
//	 * @param strMessage
//	 * @param aobjCustom
//	 * @return
//	 */
//	private String joinMessageWithObjects(boolean bPrependCurrentTime, String strMessage, Object... aobjCustom){
//		String strTime=new SimpleDateFormat("HH:mm:ss").format(new Date(System.currentTimeMillis())); //can be realtime as this is just information
//		if(bPrependCurrentTime){
//			strMessage=strTime+strMessage;
//		}
//		return joinMessageWithObjects(strMessage,aobjCustom);
//	}
//	/**
//	 * TODO mix this with {@link #_asDebugInfo(Object)}
//	 * @param strMessage
//	 * @param aobjCustom
//	 * @return
//	 */
//	private String joinMessageWithObjects(String strMessage, Object... aobjCustom){
//		if(aobjCustom!=null){
//			for(int i=0;i<aobjCustom.length;i++){
//				Object obj = aobjCustom[i];
//				strMessage+=_multilineIfArray("\n\t["+i+"]",obj);
//			}
//		}
//		
//		return strMessage;
//	}
//	/**
//	 * TODO mix this with {@link #_asDebugInfo(Object)}
//	 * @param strIndexPrefix
//	 * @param obj
//	 * @return
//	 */
//	private String _multilineIfArray(String strIndexPrefix, Object obj){
//		String strOut="";
//		
//		Object[] aobj = null; //obj instanceof Object[]
//		Exception ex = null;
//		if(obj instanceof Exception){
//			ex = ((Exception)obj);
//		}
//		
//		if(obj!=null){
//			if (obj instanceof ArrayList) {
//				ArrayList aobjList = (ArrayList) obj;
//				aobj=aobjList.toArray();
//			}else
//			if(obj.getClass().isArray()){
//				aobj = (Object[])obj;
//	//			for(Object objInner:aobj){
//			}
//		}
//		
//		if(ex!=null){
//			strOut+=strIndexPrefix+" "+ex.getClass().getSimpleName()+": "+ex.getMessage();
//			aobj = ex.getStackTrace();
//		}else
//		if(aobj==null){
//			return strIndexPrefix+fmtObj(obj,true);
//		}
//		
//		String str1stObjClass=null;
//		for(int i=0;i<aobj.length;i++){
//			Object objInner=aobj[i]; //objInner.getClass().getName()
//			
//			String strFmtObj=null;
//			if(objInner==null){
//				strFmtObj = fmtObj(objInner,false);
//			}else{
//				if(str1stObjClass==null){
//					str1stObjClass=objInner.getClass().getName();
//					strOut+=strIndexPrefix+"ArrayOf: "+aobj.getClass().getTypeName();
////					strOut+=strIndexPrefix+"ArrayOf?(1stNotNullObjType):"+str1stObjClass;
//				}
//				
//				strFmtObj = fmtObj(objInner, !str1stObjClass.equals(objInner.getClass().getName()));
//			}
//			
//			strOut+=strIndexPrefix+"["+i+"] "+strFmtObj;
//		}
//		
//		if(ex!=null){
//			Throwable exCause = ex.getCause();
//			if(exCause!=null){
//				strOut+=_multilineIfArray(strIndexPrefix+"[CAUSE]", exCause);
//			}
//		}
//		
//		return strOut;
//	}
//	/**
//	 * TODO mix this with {@link #_asDebugInfo(Object)}
//	 * @param obj
//	 * @param bShowClassName
//	 * @return
//	 */
//	private String fmtObj(Object obj,boolean bShowClassName){
//		String strCl = "";
//		if(obj!=null && bShowClassName)strCl=obj.getClass().getName();
//		
//		String strObj = "";
////		if(strObj.isEmpty()){
////			if(obj instanceof IConstructed){
////				IConstructed ic = (IConstructed) obj;
////				if(!ic.isConstructed()){
////					strObj="(not constructed yet)";
////				}
////			}
////		}
//		
////		if(strObj.isEmpty()){
////			if(obj instanceof IDebugReport){
////				IDebugReport ir = (IDebugReport) obj;
////				strObj=ir.getFailSafeDebugReport();
////			}
////		}
//		
////		if(strObj.isEmpty()){
////			strObj=""+(obj==null?null:obj.toString());
////		}
//		
//		return ""+(obj==null ? null : strCl+": "+strObj); //this is better when dumping a sub-stacktrace
//	}
	
	public ArrayList<String> prepareReportLines(String strMsg, Object... aobjCustom){
		ArrayList<String> astrReport=new ArrayList<String>();
		astrReport.add(strMsg);
		
		for(Object obj:aobjCustom){
			recursivelyAddLinesIfHasAnyArray(astrReport," ",obj);
		}
		
		return astrReport;
	}
	
	public String prepareReport(String strMsg, Object... aobjCustom){
		return String.join("\n", prepareReportLines(strMsg, aobjCustom));
	}
	
	private String formatObject(Object obj,boolean bShowType){
		if(obj==null)return ""+null;
			
		String strType="";
		if(bShowType)strType="<"+obj.getClass().getSimpleName()+">";
		
		String strValue=obj.toString();
		if(obj.getClass()==String.class)strValue="'"+strValue+"'";
		
		return strType+strValue;
	}
	
	private String prepareKey(Object objKey){
		//key can be an index or anyhthing else that must be finally shown in a simple way...
		return StringI.i().truncAndGrantOneLine(objKey.toString(), 20, "...");
	}
	
	private void recursivelyAddLinesIfHasAnyArray(ArrayList<String> astrReport, String strPrepend, Object... aobj) {
		if(aobj==null)return;
		
		for(int i=0;i<aobj.length;i++){
			Object obj = aobj[i];
			strPrepend+="["+i+"]"; //hierarchy sub-item
			
			// obj
			if(!JavaLangI.i().isSomeArrayType(obj)){
				astrReport.add(strPrepend+formatObject(obj,bShowType));
			}else{ //array
				astrReport.add(strPrepend+"Array of: "+obj.getClass().getTypeName());
				Object[][] akvobj = JavaLangI.i().convertToKeyValueArray(obj);
				for(int j=0;j<akvobj.length;j++){
					String strKey = prepareKey(akvobj[j][0]);
					Object objValue = akvobj[j][1];
					
					String strPrependWithKey=strPrepend+"["+strKey+"] ";
					if(!JavaLangI.i().isSomeArrayType(objValue)){
						astrReport.add(strPrependWithKey+formatObject(objValue,bShowType));
					}else{
						astrReport.add(strPrependWithKey+"Array of: "+obj.getClass().getTypeName());
						recursivelyAddLinesIfHasAnyArray(astrReport, strPrepend, bShowType, objValue);
					}
				}
			}
		}
		
//		return;
//		
//		////////////////
//		
//		Object[][] convertToKeyValueArray = JavaLangI.i().convertToKeyValueArray(obj);
//		
//		///////////////////////////////////
//		
//		String str="(";
//		
//		if(objValue==null){
//			str+=""+null;
//		}else{
////			if(objValue instanceof IDebugReport){
////				return ((IDebugReport)objValue).getFailSafeDebugReport();
////			}
//			
//			Object[] aobjKey=null;
//			Object[] aobjVal=null;
//			if(objValue instanceof Map) { //HashMap TreeMap etc
//				Set<Map.Entry> es = ((Map)objValue).entrySet();
//				aobjVal=new Object[es.size()];
//				aobjKey=new Object[es.size()];
//				int i=0;
//				for(Entry entry:es){
//					aobjKey[i]=entry.getKey();
//					aobjVal[i]=entry.getValue();
//					i++;
//				}
//			}else
//			if(objValue instanceof ArrayList) {
//				ArrayList<?> aobjList = (ArrayList<?>) objValue;
//				aobjVal=aobjList.toArray();
//			}else
//			if(objValue.getClass().isArray()){
//				aobjVal = (Object[])objValue;
//			}
//			
//			if(aobjVal!=null){ //array
//				// this is just guessing all elements are of the same type
//				str+=""+objValue.getClass().getName()+"["+aobjVal.length+"]{"; //TODO check if all elements are same type?
//				if(aobjVal.length>0)str+="\n";
//				for(int i=0;i<aobjVal.length;i++){//Object obj:aobjVal){
//					Object objKey=null;if(aobjKey!=null)objKey=aobjKey[i];
//					str+=_asDebugInfoLine(objKey,aobjVal[i],false);
//				}
//				str+="}";
//			}else{
//				str+=_asDebugInfoLine(null,objValue,true);
//			}
//		}
//		
//		return str+")";
//		
//		///////////////////////////////
//		
//		String strOut="";
//		
//		Object[] aobj = null; //obj instanceof Object[]
//		Exception ex = null;
//		if(obj instanceof Exception){
//			ex = ((Exception)obj);
//		}
//		
//		if(obj!=null){
//			if (obj instanceof ArrayList) {
//				ArrayList aobjList = (ArrayList) obj;
//				aobj=aobjList.toArray();
//			}else
//			if(obj.getClass().isArray()){
//				aobj = (Object[])obj;
//	//			for(Object objInner:aobj){
//			}
//		}
//		
//		if(ex!=null){
//			strOut+=strIndexPrefix+" "+ex.getClass().getSimpleName()+": "+ex.getMessage();
//			aobj = ex.getStackTrace();
//		}else
//		if(aobj==null){
//			return strIndexPrefix+fmtObj(obj,true);
//		}
//		
//		String str1stObjClass=null;
//		for(int i=0;i<aobj.length;i++){
//			Object objInner=aobj[i]; //objInner.getClass().getName()
//			
//			String strFmtObj=null;
//			if(objInner==null){
//				strFmtObj = fmtObj(objInner,false);
//			}else{
//				if(str1stObjClass==null){
//					str1stObjClass=objInner.getClass().getName();
//					strOut+=strIndexPrefix+"ArrayOf: "+aobj.getClass().getTypeName();
////					strOut+=strIndexPrefix+"ArrayOf?(1stNotNullObjType):"+str1stObjClass;
//				}
//				
//				strFmtObj = fmtObj(objInner, !str1stObjClass.equals(objInner.getClass().getName()));
//			}
//			
//			strOut+=strIndexPrefix+"["+i+"] "+strFmtObj;
//		}
//		
//		if(ex!=null){
//			Throwable exCause = ex.getCause();
//			if(exCause!=null){
//				strOut+=_multilineIfArray(strIndexPrefix+"[CAUSE]", exCause);
//			}
//		}
//		
//		return strOut;
		
	}

	public boolean isShowType() {
		return bShowType;
	}

	public void setShowType(boolean bShowType) {
		this.bShowType = bShowType;
	}
	
}
