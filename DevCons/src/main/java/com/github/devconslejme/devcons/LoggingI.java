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

package com.github.devconslejme.devcons;

import java.io.File;
import java.io.PrintStream;

import com.github.devconslejme.devcons.DevConsPluginStateI.CallableVarMonX;
import com.github.devconslejme.devcons.DevConsPluginStateI.EStatPriority;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.FileI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.TimeFormatI;
import com.github.devconslejme.misc.jme.AppI;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.jme3.app.Application;
import com.jme3.system.Timer;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.core.VersionedList;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class LoggingI {
	public static LoggingI i(){return GlobalManagerI.i().get(LoggingI.class);}
	
	private VersionedList<String>	vlstrLogEntries;
	private int iLogEntriesLimit = 100000;
	private int iWrapAtColumn = 80;
	private File	flLog;
//	DateFormat dateFormat = new SimpleDateFormat("HH:mm:ss"); //yyyy/MM/dd
//	Date dateRealTimeForMarker = new Date();
//	private VarMon	stWrapAt;
	
	public void configure() {
//		flLog = new File(DevConsPluginStateI.i().getStorageFolder(), LoggingI.class.getSimpleName()+".log");
		flLog = FileI.i().createNewFile(this,"log",true);
		flLog.delete();
		
		vlstrLogEntries = new VersionedList<String>();
		/**
		 * The existance of at least one entry is very important to help on initialization.
		 * Actually is useful to determine the listbox entry height too.
		 */
		logEntry("Initializing console.");
		
		DevConsPluginStateI.i().createVarMon(EStatPriority.Normal, "WrapAt", LoggingI.class.getSimpleName()+": Wrap at column",new CallableVarMonX() {
			@Override
			public Boolean call() {
				getVarMon().set(""+getWrapAtColumn());
				return true;
			}
		});
	}
	
	public void logExceptionEntry(Exception ex, String strJS) {
		logExceptionEntry(ex,strJS,false);
	}
	public void logExceptionEntry(Exception ex, String strJS, boolean bSkipLogFile) {
		String str="!!!";
		if(strJS!=null)logEntry(str+"Cmd:"+strJS,bSkipLogFile,System.err);
		logEntry(str+ex.getClass().getSimpleName()+":"+ex.getMessage(),bSkipLogFile,System.err);
		
		Throwable cause = ex;
		while(true){
			for(StackTraceElement ste:cause.getStackTrace()){
				logEntry(" "+ste,bSkipLogFile,System.err);
			}
			
			cause=cause.getCause();
			if(cause==null)break;
			
			logEntry("Caused by:",bSkipLogFile,System.err);
		}
		MessagesI.i().putReviewableMsg(
			MessagesI.i().output(null,"DevConsLog",this,strJS,ex)); 
	}
	
	public void logEntry(String str){
		logEntry(str,false,System.out);
	}
	public void logEntry(String str, boolean bSkipLogFile, PrintStream ps){
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
			while(vlstrLogEntries.size()>(iLogEntriesLimit/2)){
				vlstrLogEntries.remove(0);
			}
		}
		
//		if(ps!=null)ps.println(str);
		MessagesI.i().output(ps,"DevConsLog",this,str);
		
		//dont! ConsolePluginI.i().scrollToBottom();
	}
	
	public void logSubEntry(String string) {
		logEntry(" "+string);
	}
	
	public String prepareLogMarkerText(String strInfo){ //center
		strInfo = "[R="+TimeFormatI.i().getRealTimeFormatted(null,"HH:mm:ss")+"] "+strInfo;
		//TODO start app time at year0 month0 day0 ... 
//		Timer tm=GlobalManagerI.i().get(Application.class).getTimer();
//		strInfo = strInfo+" [A="+TimeFormatI.i().formatElapsed(tm.getResolution(),tm.getTime())+"]";
		strInfo = strInfo+" [A="+TimeFormatI.i().formatElapsed(
			AppI.i().getTimerResolution(),AppI.i().getTime())+"]";
		return strInfo;
	}
	/**
	 * 
	 * @param strInfo
	 * @param iAlign -1 left, 0 center,  1 right
	 */
	public void logMarker(String strInfo, int iAlign){
		strInfo = prepareLogMarkerText(strInfo);
		switch(iAlign){
			case -1:
				strInfo = Strings.padEnd(strInfo, iWrapAtColumn, '_');
				break;
			case 0:
				strInfo = Strings.padStart(strInfo, iWrapAtColumn/2 +strInfo.length()/2, '_');
				strInfo = Strings.padEnd(strInfo, iWrapAtColumn, '_');
				break;
			case 1:
				strInfo = Strings.padStart(strInfo, iWrapAtColumn, '_');
				break;
			default:
				throw new DetailedException("invalid alignment", iAlign, strInfo);
		}
		
		logEntry(strInfo);
	}
	public void logMarker(String strInfo){
		logMarker(strInfo,0);
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
		return LoggingI.i().getLogEntry(DevConsPluginStateI.i().getSelectedIndex());
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
		MessagesI.i().setSummaryReportLineLength(iWrapAtColumn);
	}

	public void logWarn(String string) {
		logEntry("!WARN: "+string+"; STE "+Thread.currentThread().getStackTrace()[2]);
	}

	public void clear() {
		vlstrLogEntries.clear();
	}

	public int getLogEntriesLimit() {
		return iLogEntriesLimit;
	}

	public void setLogEntriesLimit(int iLogEntriesLimit) {
		if(iLogEntriesLimit<1000)iLogEntriesLimit=1000;
		this.iLogEntriesLimit = iLogEntriesLimit;
	}
	
}
