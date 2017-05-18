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

import com.google.common.base.Strings;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class StringI {
	public static StringI i(){return GlobalManagerI.i().get(StringI.class);}
	
	private String	strLastUid="0";
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
	
	public String createTable(int iColumns, String... astr){
//		ArrayList<String> astrList = new ArrayList<String>();
		int iMaxWidth=0;
		for(String str:astr){
			if(str.length()>iMaxWidth)iMaxWidth=str.length();
		}
		
		String strOut="";
		int iCount=0;
		for(String str:astr){
//			strOut+=iCount+":"
			strOut+=Strings.padStart(str, iMaxWidth, ' ')+" ";
			iCount++;
			if(iCount==3){
				strOut+="\n";
				iCount=0;
			}
		}
		
		return strOut;
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
	public String fmtFloat(float fValue, int iScale) {
		return String.format("%."+iScale+"f", fValue);
	}
	
//	DecimalFormat df = new DecimalFormat("##,##,##,##,##,##,##0.00");
	DecimalFormat dfLong = new DecimalFormat("##,##,##,##,##,##,##0");
	public String fmtLong(long l){
		return String.format(dfLong.format(l));
	}
}
