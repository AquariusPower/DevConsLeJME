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
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Locale;

import com.google.common.base.Strings;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class StringI {
	public static StringI i(){return GlobalManagerI.i().get(StringI.class);}
	
	private String	strLastGlobalUid="0";
	private int	iUIdRadix = Character.MAX_RADIX;
	
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
		
		public String s(){return toString();}
		
//		public EStringMatchMode[] valuesSortByName(){
//			a values()
//		}
	}

	public boolean contains(String strToCheck, String strMatch, EStringMatchMode eMode, boolean bIgnoreCase){
		if(strMatch.isEmpty())return true; //empty matches everything
		
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
		/**
		 * Do not fix if null like in `if(strLastId==null)strLastId="0";`
		 * because the last id must be controlled by a manager String field,
		 * or be a static String field of the class...
		 * 
		 * fixing the null would just be prone to developer coding bugs...
		 * 
		 * TODO better not fix if empty either!?
		 */
		BigInteger bi = new BigInteger(strLastId,iUIdRadix);
		bi=bi.add(new BigInteger("1"));
		return bi.toString(iUIdRadix);
	}
	
	public String convertToUniqueId(long l){
		return new BigInteger(""+l,10).toString(iUIdRadix);
	}
	
	/**
	 * This uses a global uid, mainly useful for things that must have an id that doesnt conflict with others,
	 * but not necessarily requires to be sequential on its list, like randomly spawning boxes.
	 * @return
	 */
	public String getNextUniqueGlobalId(){
		strLastGlobalUid=getNextUniqueId(strLastGlobalUid);
		return strLastGlobalUid;
	}
	
	public boolean isBlank(char ch){
		switch(ch){
			case ' ':return true;
			case '\t':return true;
		}
		return false;
	}
	
	/**
	 * escape new lines
	 * @param str
	 * @param iMaxLength
	 * @param strAppendOnTrunc if not null
	 * @return
	 */
	public String truncAndGrantOneLine(String str, int iMaxLength, String strAppendOnTrunc){
		str=str.replace("\n","\\n");
		
		if(strAppendOnTrunc!=null){
			iMaxLength-=strAppendOnTrunc.length();
		}
		
		if(str.length()>iMaxLength){
			str=str.substring(0,iMaxLength);
			
			if(strAppendOnTrunc!=null)str+=strAppendOnTrunc;
		}
		return str;
	}
	
	public String getAllCharactersSymbols(int iColumns){
		String[] astr=new String[256];
		for(int i=0;i<256;i++){
			String str="'"+(char)i+"'";
			str+="="+i+"=0x"+String.format("%02X",i);
			astr[i]=str;
		}
		return StringI.i().createTable(iColumns, astr);
	}
	
	public String createTable(int iColumns, String... astr){
		int iMaxWidth=0;
		for(int i=0;i<astr.length;i++){
			String str=astr[i];
			str=str.replace("\0","\\0");
			str=str.replace("\r","\\r");
			str=str.replace("\n","\\n");
			str=str.replace("\t","\\t");
			astr[i]=str;
			if(str.length()>iMaxWidth)iMaxWidth=str.length();
		}
		iMaxWidth++;
		
		StringBuilder sb = new StringBuilder("");
		int iCount=0;
		for(String str:astr){
			str=Strings.padStart(str, iMaxWidth, ' ');//+" ";
			sb.append(str);
			iCount++;
			if(iCount==iColumns){
				sb.append("\n");
				iCount=0;
			}
		}
		
		return sb.toString();
	}
	
	/**
	 * this will also be careful to not break words
	 * @param strHelp
	 * @param iWrapAt
	 * @return
	 */
	public ArrayList<String> splitInLines(String strHelp, int iWrapAt) {
		String[] astrWords = strHelp.split(" ");
		
		ArrayList<String> astrLines  = new ArrayList<String>();
//		for(char ch:strHelp.toCharArray()){
		String strLine="";
		int iWordCountPerLine=0;
		for(int i=0;i<astrWords.length;i++){
			String strWord = astrWords[i];
			
			if(!strLine.isEmpty()){
				if((strLine+" "+strWord).length()>=iWrapAt){
					addLine(strLine, strWord, astrLines, iWrapAt, iWordCountPerLine);
					strLine="";iWordCountPerLine=0; //reset line
//					if(iWordCountPerLine==1 && strLine.length()>iWrapAt){ //too big word
//						astrLines.add(strLine.substring(0, iWrapAt));
//						strLine=strLine.substring(iWrapAt);
//					}
//					astrLines.add(strLine);
//					strLine="";
//					iWordCountPerLine=0;
				}
			}
			
			if(!strLine.isEmpty())strLine+=" ";
			strLine+=strWord;
			iWordCountPerLine++;
			
			if(i==astrWords.length-1){
				addLine(strLine, strWord, astrLines, iWrapAt, iWordCountPerLine);
//				astrLines.add(strLine);
//				iWordCountPerLine=0;
//				break; //redundant
			}
		}
		
		return astrLines;
	}
	private void addLine(String strLine, String strWord, ArrayList<String> astrLinesToModify, int iWrapAt, int iWordCountPerLine){
		if(iWordCountPerLine==1 && strLine.length()>iWrapAt){ //too big word
			astrLinesToModify.add(strLine.substring(0, iWrapAt));
			strLine=strLine.substring(iWrapAt);
		}
		astrLinesToModify.add(strLine);
//		strLine="";iWordCountPerLine=0;
	}
	
	/**
	 * scale 2
	 * @param fValue
	 * @return
	 */
	public String fmtFloat(float fValue) {
		return fmtFloat(fValue,2);
	}
//	StringBuilder sbFloatArray = new StringBuilder();
	public String[] fmtFloat(int iScale, Float... afValue) {
//		sbFloatArray.delete(0, sbFloatArray.length());
		String[] astr = new String[afValue.length];
		int i=0;
		for(float f:afValue){
			astr[i++]=fmtFloat(f,iScale);
		}
//		return sbFloatArray.toString();
		return astr;
	}
	public String fmtFloat(double fValue, int iScale) {
		return String.format(Locale.ENGLISH,"%."+iScale+"f", fValue);
	}
	
//	DecimalFormat df = new DecimalFormat("##,##,##,##,##,##,##0.00");
	DecimalFormat dfLong = new DecimalFormat("##,##,##,##,##,##,##0");
	public String fmtLong(long l){
		return String.format(Locale.ENGLISH,dfLong.format(l));
	}

	public String extractPart(String strText, String strSeparator, int iIndex) {
		return extractPart(strText, strSeparator, iIndex, iIndex+1);
	}
	/**
	 * 
	 * @param strText
	 * @param strSeparator
	 * @param iStartIndexInclusive
	 * @param iEndIndexExclusive can be -1 to go till the end
	 * @return
	 */
	public String extractPart(String strText, String strSeparator, int iStartIndexInclusive, int iEndIndexExclusive) {
		String strSeparatorRegex="";
		for(char ch:strSeparator.toCharArray()){
			strSeparatorRegex+="["+ch+"]";
		}
		String[] astr = strText.split(strSeparatorRegex);
		
		if(iEndIndexExclusive==-1){
			iEndIndexExclusive=astr.length;
		}
		
		String strRet="";
		for(int i=0;i<astr.length;i++){
			String str=astr[i];
			
			if(!strRet.isEmpty())strRet+=strSeparator;
			
			if(i>=iStartIndexInclusive)strRet+=str;
			
			if(i==(iEndIndexExclusive-1))break;
			
//			if(iEndIndex==-1){
//				if(i>=iStartIndex){
//					strRet+=str;
//				}
//			}else{
//				if(i==iStartIndex){
//					strRet+=str;
//					break;
//				}
//			}
			
//			if(!bTilEnd && i==iIndex){
//				strRet+=str;
//				break;
//			}else
//			if(bTilEnd && i>=iIndex){
//				strRet+=str;
//			}
		}
		
		return strRet;
	}
	
	private String strValidateUIdRegex="[0-9A-Za-z_ +-]*"; //regex: "-" must be the last thing within []
	
	/**
	 * Allows a lot of flexibility and readability.
	 * But also helps to forbid characters that could be identified as a javascript code ex.: "." "(" ";"
	 * making it easier to distinguish.
	 * btw, "*" char is restricted use/indicator.
	 */
	public String validateUId(String strUId) throws IllegalArgumentException{
		if(strUId.trim().isEmpty()){
			throw new DetailedException("empty id "+strUId); //to prevent messy ids
		}
		
		if(strUId.trim().length()!=strUId.length()){
			throw new DetailedException("is not trimmed "+strUId); //to prevent messy ids outside here
		}
		
		if(!strUId.matches(strValidateUIdRegex)){ 
			throw new DetailedException("invalid unique id characters "+strUId);
		}
		
		return strUId;
	}

	public String getValidateUIdRegex() {
		return strValidateUIdRegex;
	}

	public StringI setValidateUIdRegex(String strValidateUIdRegex) {
		this.strValidateUIdRegex = strValidateUIdRegex;
		return this; 
	}
}
