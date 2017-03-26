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

import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.core.VersionedList;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class LoggingI {
	private static LoggingI instance = new LoggingI();
	/**instance*/ public static LoggingI i(){return instance;}
	
	private VersionedList<String>	vlstrLogEntries;
	
	public LoggingI() {
		JavaScriptI.i().setJSBinding(this);
		
		vlstrLogEntries = new VersionedList<String>();
		/**
		 * The existance of at least one entry is very important to help on initialization.
		 * Actually is useful to determine the listbox entry height too.
		 */
		logEntry("Initializing console.");
	}
	
	public void logExceptionEntry(Exception ex, String strJS) {
		logEntry("CmdException: "+strJS);
		
		logEntry(ex.getMessage());
		
		Throwable cause = ex;
		while(true){
			for(StackTraceElement ste:cause.getStackTrace()){
				logEntry(" "+ste);
			}
			
			cause=cause.getCause();
			if(cause!=null){
				logEntry("Caused by:");
			}else{
				break;
			}
		}
	}
	
	public void logEntry(String str){
		vlstrLogEntries.add(str);
		System.out.println(str);
	}
	
	public void logSubEntry(String string) {
		logEntry(" "+string);
	}
	
	public void logMarker(String strInfo){
		logEntry("_______________ '"+strInfo+"' _______________");
	}

	public void setModelAt(ListBox<String> lstbx) {
		lstbx.setModel(vlstrLogEntries);
	}

	public double getLogEntriesSize() {
		return vlstrLogEntries.size();
	}
}
