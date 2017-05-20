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

package com.github.devconslejme.extras;

import java.util.ArrayList;

import com.github.devconslejme.misc.DetailedException;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class CommandLineParser {
	private String strLine;
	
	private ArrayList<Object>	aobjList;
	
	private String strCommand;
	private ArrayList<Object>	aobjParamsList;
	
	enum EType{
		Command, //unquoted string
		StringDQ, //will basically include an escaped double quotes as normal character " in the string 
		StringSQ, 
		Boolean, 
		Number,
		;
	}
	
	public CommandLineParser(String strLine){
		this.strLine=strLine;
		
		parse();
		
		strCommand = (String)aobjList.get(0);
		
		aobjParamsList=new ArrayList<Object>(aobjList);
		aobjParamsList.remove(0);
	}
	
	/**
	 * or param 0
	 * @return
	 */
	public String getCommand(){
		return strCommand;
	}
	
	@SuppressWarnings("unchecked")
	public <T> T getPart(int iIndex){
		return (T)aobjList.get(iIndex);
	}
	
	@SuppressWarnings("unchecked")
	public <T> T getParam(int iIndex){
		return (T)aobjParamsList.get(iIndex);
	}
	
	public ArrayList<Object> getAllParamsCopy(){
		return new ArrayList<Object>(aobjParamsList); 
	}
	public ArrayList<String> getAllParamsStrCopy(){
		return getAllParamsStrCopy(3);
	}
	public ArrayList<String> getAllParamsStrCopy(int iFloatPrecision){
		ArrayList<String> astr= new ArrayList<String>();
		
		for(Object obj:aobjParamsList){
			if(Float.class.isInstance(obj) || Double.class.isInstance(obj)){
				astr.add(String.format("%."+iFloatPrecision+"f", obj));
			}else{
				astr.add(obj.toString());
			}
		}
		
		return astr; 
	}
//	public String getAllParamsStr(int iFloatPrecision){
//		ArrayList<String> astr= new ArrayList<String>();
//		
//		String str="";
//		for(Object obj:aobjList){
//			if(!str.isEmpty())str+=" ";
//			
//			if(obj instanceof String){
//				str+="\""+obj.toString().replace("\"","\\\"")+"\"";
//			}else
//				if(Float.class.isInstance(obj) || Double.class.isInstance(obj)){
//					str+=String.format("%."+iFloatPrecision+"f", obj);
//				}else{
//					str+=obj.toString()+"";
//				}
//		}
//		
//		return str; 
//	}
	
	public void parse(){
		strLine=strLine.trim();
		String strParam="";
		EType et=EType.Command;
		boolean bEscaped=false;
		aobjList = new ArrayList<Object>();
		for(char ch:strLine.toCharArray()){
			if(et!=null){ // fill the param by type 
				switch (et) {
					case Command:
						if(isBlank(ch)){ //finalize
							aobjList.add(strParam);
							strParam="";et=null;continue; //reset
						}
						
						strParam+=ch;
						continue;
					case StringDQ:
						if(!bEscaped && ch=='\\'){
							bEscaped=true;
							continue;
						}
						
						if(!bEscaped && ch=='"'){ //finalize
							aobjList.add(strParam);
							strParam="";et=null;continue; //reset
						}
						
						if(bEscaped){
							switch(ch){
//								case 'n':
//									strParam+="\n";
//									break;
//								case 'r':
//									strParam+="\r";
//									break;
//								case 't':
//									strParam+="\t";
//									break;
								case '"':
									strParam+="\"";
									break;
								default:
//									strParam+=ch; //no conversion detected, will just ignore the escaping
									strParam+="\\"+ch;
									break;
							}
							
							bEscaped=false;
						}else{
							strParam+=ch;
						}
						
						continue;
					case StringSQ:
						if(ch=='\''){ //finalize
							aobjList.add(strParam);
							strParam="";et=null;continue; //reset
						}
						
						strParam+=ch;
						continue;
					case Boolean:
						if(isBlank(ch)){ //finalize
							if(strParam.equals("true")){
								aobjList.add(true);
							}else
							if(strParam.equals("false")){
								aobjList.add(false);
							}else{
								throw new DetailedException("invalid boolean parsing: "+strParam);
							}
							strParam="";et=null;continue; //reset
						}
							
						strParam+=ch;
						continue;
					case Number:
						if(isBlank(ch)){ //finalize
							Object objConv=null;
							if(objConv==null)try{objConv=Long  .parseLong  (strParam);}catch(NumberFormatException e){}
							if(objConv==null)try{objConv=Double.parseDouble(strParam);}catch(NumberFormatException e){}
							if(objConv==null)throw new DetailedException("invalid number parsing: "+strParam);
							aobjList.add(objConv);
							strParam="";et=null;continue; //reset
						}
							
						strParam+=ch;
						continue;
				}
			}
			
			// detect and initialize type
			switch(ch){ 
				case '"' : 
					et=EType.StringDQ; continue;
				case '\'': 
					et=EType.StringSQ; continue;
				case 't' : case 'f' : 
					et=EType.Boolean; continue;
				case '\t': case ' ' : 
					continue; //still seeking non blank
				default: 
					et=EType.Number; continue; //will be parsed to confirm later
			}
		}
	}
	
	public boolean isBlank(char ch){
		switch(ch){
			case ' ':return true;
			case '\t':return true;
		}
		return false;
	}
	
}