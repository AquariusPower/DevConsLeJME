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

package com.github.devconslejme;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import com.google.common.base.Charsets;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.io.Files;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.core.VersionedList;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class LoggingI {
	private static LoggingI instance = new LoggingI();
	/**instance*/ public static LoggingI i(){return instance;}
	
	private VersionedList<String>	vlstrLogEntries;
	private int iLogEntriesLimit = 10000;
	private int iWrapAtColumn = 80;
	private File	flLog;
	
	public LoggingI() {
		JavaScriptI.i().setJSBinding(this);
		
		flLog = new File(ConsolePluginI.i().getStorageFolder(), LoggingI.class.getSimpleName()+".log");
		flLog.delete();
		
		vlstrLogEntries = new VersionedList<String>();
		/**
		 * The existance of at least one entry is very important to help on initialization.
		 * Actually is useful to determine the listbox entry height too.
		 */
		logEntry("Initializing console.");
	}
	
	public void logExceptionEntry(Exception ex, String strJS) {
		logExceptionEntry(ex,strJS,false);
	}
	public void logExceptionEntry(Exception ex, String strJS, boolean bSkipLogFile) {
		logEntry("CmdException: "+strJS,bSkipLogFile);
		
		logEntry(ex.getMessage(),bSkipLogFile);
		
		Throwable cause = ex;
		while(true){
			for(StackTraceElement ste:cause.getStackTrace()){
				logEntry(" "+ste,bSkipLogFile);
			}
			
			cause=cause.getCause();
			if(cause!=null){
				logEntry("Caused by:",bSkipLogFile);
			}else{
				break;
			}
		}
	}
	
	public void logEntry(String str){
		logEntry(str,false);
	}
	public void logEntry(String str, boolean bSkipLogFile){
		// log at application console
		for(String strLine : str.split("\n")){
			Iterable<String> itstr = Splitter.fixedLength(iWrapAtColumn).split(strLine);
			String[] astr = Iterables.toArray(itstr, String.class);
			for(int i=0;i<astr.length;i++){
				String strPart = astr[i];
				if(i<(astr.length-1))strPart+="\\"; //indicates that continue on the next line
				vlstrLogEntries.add(strPart);
			}
		}
		
		// log to file
		if(!bSkipLogFile){
			FileI.i().appendLine(flLog, str);
		}
		
		// limit log size in memory
		while(vlstrLogEntries.size()>iLogEntriesLimit){
			vlstrLogEntries.remove(0);
		}
		
		System.out.println(str);
	}
	
	public void logSubEntry(String string) {
		logEntry(" "+string);
	}
	
	public void logMarker(String strInfo){
		strInfo = Strings.padStart(strInfo, iWrapAtColumn/2 +strInfo.length()/2, '_');
		strInfo = Strings.padEnd(strInfo, iWrapAtColumn, '_');
		logEntry(strInfo);
	}

	public void setModelAt(ListBox<String> lstbx) {
		lstbx.setModel(vlstrLogEntries);
	}

	public double getLogEntriesSize() {
		return vlstrLogEntries.size();
	}

	public String getLogEntry(Integer selection) {
		if(selection==null)return null;
		return vlstrLogEntries.get(selection);
	}
	
	public String getSelectedEntry() {
		return LoggingI.i().getLogEntry(ConsolePluginI.i().getSelectedIndex());
	}

	public void deleteLogEntry(Integer index) {
		if(index==null)return;
		vlstrLogEntries.remove(index.intValue());
	}

	public int getWrapAtColumn() {
		return iWrapAtColumn;
	}

	public void setWrapAtColumn(int iWrapAtColumn) {
		this.iWrapAtColumn = iWrapAtColumn;
	}
	
}
