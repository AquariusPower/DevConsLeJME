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
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class CommandLineParser {
	private String strLine;
	
	private ArrayList<Object>	aobjList;
	
	private String strCommand=null;
	private ArrayList<Object>	aobjParamsList;
	
	int iDefaultFloatPrecision=3;
	
	enum EType{
		Command, //unquoted string
		StringDQ, //will basically include an escaped double quotes as normal character " in the string 
		StringSQ, 
		Boolean, 
		Number,
		Enum, //TODO add Enum type support? will required list of known enums (from globals should suffice) and each unique id JavaLangI.i().enumUId(e)
		;
	}
	
	/**
	 * use this constructor as build (reversed) mode, to create a command line from objects
	 */
	public CommandLineParser(){}
	
	/**
	 * strings must be enclosed in single or double quotes, otherwise a parse number will be tried
	 * @param strLine
	 */
	public CommandLineParser(String strLine){
		this.strLine=strLine;
		
		parse();
		
		strCommand = (String)aobjList.get(0);
		
		aobjParamsList=new ArrayList<Object>(aobjList);
		aobjParamsList.remove(0);
	}
	
	public CommandLineParser setCommand(String str){
		if(this.strCommand!=null)throw new DetailedException("Command already set",this.strCommand,str);
		if(aobjList!=null)throw new DetailedException("objs list should be unset",aobjList,str);
		if(aobjParamsList!=null)throw new DetailedException("params objs list should be unset",aobjParamsList,str);
		
		this.strCommand=str;
		aobjList=new ArrayList<>();
		aobjList.add(strCommand);
		aobjParamsList=new ArrayList<>();
		
		return this;
	}

	public CommandLineParser appendParams(Object... aobj){
		assert aobjList!=null && aobjParamsList!=null;
		for(Object obj:aobj){
			if(obj instanceof String){
				if("null".equals(obj))obj=null; //special nullifier case
			}
			aobjList.add(obj);
			aobjParamsList.add(obj);
		}
		return this;
	}
	
	/**
	 * or param 0
	 * @return
	 */
	public String getCommand(){
		return strCommand;
	}
	
	/**
	 * the main command will be at 0
	 * @param iIndex
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T> T getParsedPart(int iIndex){
		if(iIndex>=aobjList.size())return null;
		return (T)aobjList.get(iIndex);
	}
	
	/**
	 * The main command will not be here.
	 * the index 0 will have the 1st param
	 * @param iIndex
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T> T getParsedParam(int iIndex){
		if(iIndex>=aobjParamsList.size())return null;
		return (T)aobjParamsList.get(iIndex);
	}
	
	public ArrayList<Object> getAllParamsParsedListCopy(){
		return new ArrayList<Object>(aobjParamsList); 
	}
	public ArrayList<String> getAllParamsStrListCopy(){
		return getAllParamsStrListCopy(iDefaultFloatPrecision);
	}
	public ArrayList<String> getAllParamsStrListCopy(int iFloatPrecision){
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
	
	protected void parse(){
		strLine=strLine.trim();
		strLine+=" "; // a blank at the end will easify the finalization of the last part fill/detection
		String strParam="";
		EType et=EType.Command; //always use the first unquoted string as the command (that is the main thing)
		boolean bEscaped=false;
		aobjList = new ArrayList<Object>();
//		boolean bPartDetected=false;
		for(char ch:strLine.toCharArray()){
			/**
			 * detect and initialize part/substring type
			 */
			if(et==null){
				switch(ch){ 
					case '"' : 
						et=EType.StringDQ;
						continue;
					case '\'': 
						et=EType.StringSQ; 
						continue;
					case 't' : case 'f' : 
						et=EType.Boolean; 
						break; //the 1st char is not a part delimiter
					case '\t': case ' ' : 
						continue; //still seeking non blank
					default:
						if("+-.0123456789".contains(""+ch)){
							et=EType.Number; //will be parsed to confirm later TODO (of couse right?)
							break; //the 1st char is not a part delimiter
						}else{
							et=EType.Enum;
							break; //the 1st char is not a part delimiter
						}
				}
			}
			
			/**
			 * fill the param by type
			 */
//			if(et!=null){  
				switch (et) {
					case Command:
						if(isBlank(ch)){ //finalize
							//it is equivalent to already trimmed
							aobjList.add(strParam);strParam="";et=null;continue; //add&reset
						}
						
						strParam+=ch;
						continue;
					case StringDQ:
						if(!bEscaped && ch=='\\'){
							bEscaped=true;
							continue;
						}
						
						if(!bEscaped && ch=='"'){ //finalize
							aobjList.add(strParam);strParam="";et=null;continue; //add&reset
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
							aobjList.add(strParam);strParam="";et=null;continue; //add&reset
						}
						
						strParam+=ch;
						continue;
					case Boolean:
						if(isBlank(ch)){ //finalize
							boolean b=false;
							if(strParam.equals("true")){
								b=(true);
							}else
							if(strParam.equals("false")){
								b=(false);
							}else{
								throw new DetailedException("invalid boolean parsing: "+strParam);
							}
							aobjList.add(b);strParam="";et=null;continue; //add&reset
						}
							
						strParam+=ch;
						continue;
					case Number:
						if(isBlank(ch)){ //finalize
							Object objConv=null;
							if(objConv==null)try{objConv=Long  .parseLong  (strParam);}catch(NumberFormatException e){}
							if(objConv==null)try{objConv=Double.parseDouble(strParam);}catch(NumberFormatException e){}
							if(objConv==null)throw new DetailedException("invalid number parsing: "+strParam);
							aobjList.add(objConv);strParam="";et=null;continue; //add&reset
						}
							
						strParam+=ch;
						continue;
					case Enum:
						if(isBlank(ch)){ //finalize
							Enum e=GlobalManagerI.i().parseToEnum(strParam);
//							for(Object obj:GlobalManagerI.i().getListCopy()){
//								if(JavaLangI.i().isEnumClass(obj)){
//									Enum e = (Enum)obj;
//									if(strParam.startsWith(e.getClass().getName())){
//										String strEnumId=strParam.substring(e.getClass().getName().length()+1);
//										objEnumValue = Enum.valueOf(e.getClass(), strEnumId);
//									}
//								}
//							}
							if(e==null)DetailedException.assertNotNull(e, strParam, strLine);
							aobjList.add(e);strParam="";et=null;continue; //add&reset
						}
						
						strParam+=ch;
						continue;
				}
//			}
			
//			// detect and initialize type
//			switch(ch){ 
//				case '"' : 
//					et=EType.StringDQ; continue;
//				case '\'': 
//					et=EType.StringSQ; continue;
//				case 't' : case 'f' : 
//					et=EType.Boolean; continue;
//				case '\t': case ' ' : 
//					continue; //still seeking non blank
//				default: 
//					et=EType.Number; continue; //will be parsed to confirm later
//			}
		}
	}
	
	public boolean isBlank(char ch){
		switch(ch){
			case ' ':return true;
			case '\t':return true;
		}
		return false;
	}

	public ArrayList<String> getAllPartsStrListCopy() {
		ArrayList<String> astrList = getAllParamsStrListCopy();
		astrList.add(0,getCommand());
		return astrList;
	}

	public String recreateCommandLine() {
		return recreateCommandLine(iDefaultFloatPrecision);
	}
	public String recreateCommandLine(int iFloatPrecision) {
		StringBuilder sb=new StringBuilder(strCommand);
		for(Object obj:aobjParamsList){
			sb.append(" ");
			if(obj instanceof String){
				sb.append("\""+obj+"\""); //TODO ? if there are escaped double quotes, use double quotes. if there are non escaped double quotes, use single quotes. as enclosing token
			}else
			if(Float.class.isInstance(obj) || Double.class.isInstance(obj)){
				sb.append(String.format("%."+iFloatPrecision+"f", obj));
			}else
			if(Integer.class.isInstance(obj) || Long.class.isInstance(obj)){
				sb.append(""+obj);
			}else
			if(Enum.class.isInstance(obj)){
				sb.append(JavaLangI.i().enumUId((Enum)obj));
			}else{
				throw new DetailedException("unsupported type",obj.getClass(),obj);
			}
		}
		return sb.toString();
	}
	
}