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

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.Callable;

import com.github.devconslejme.misc.DetailedException;

/**
 * Locks have a short timeout.
 * 
 * Is not part of the simulation, therefore must use realtime: System.currentTimeMillis()
 * 
 * Single Mandatory "Application Instance"
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class SingleAppInstance { //implements IReflexFillCfg{
	public static final boolean bDebugIDE=
		ManagementFactory.getRuntimeMXBean().getInputArguments().toString().indexOf("-agentlib:jdwp") > 0;
	private String strPrefix;
	private String strSuffix;
	private String strId;
	private File	flSelfLock;
	private BasicFileAttributes	attrSelfLock;
	private FilenameFilter	fnf;
	private File	flFolder;
	private long lLockUpdateTargetDelayMilis;
	private long	lSelfLockCreationTimeMilis;
	private String	strExitReasonOtherInstance;
	private int	lCheckCountsTD;
	private boolean	bExitApplicationTD;
	private long	lCheckTotalDelay;
	private boolean	bUseFilesystemFileAttributeModifiedTime;
	private boolean	bRecreateLockEveryLoop;
	//String strDebugMode="DebugMode";
	//String strReleaseMode="ReleaseMode";
	private boolean	bConfigured;
	//private String	strErrorMissingValue="ERROR_MISSING_VALUE";
	//private Boolean	bSelfIsDebugMode;
	private Thread	threadMain; // threadApplicationRendering
	private Thread	threadChecker;
	private int	lWaitCount;
	private boolean	bAllowCfgOutOfMainMethod = false;
	private boolean bCreateLockOutputOnce=true;
	private Throwable	twbExitErrorCause;
//	private String	strExitErrorMessage;
	private Object	app;
	private File	flAppStorageBaseFolder;
	private ArrayList<CallChkProblemsAbs>	acallCheckProblemsList = new ArrayList<CallChkProblemsAbs>();
	
	public SingleAppInstance() {
//		if(instance!=null)throw new DetailedException("already instanced");
		lLockUpdateTargetDelayMilis=3000;
		strPrefix=SingleAppInstance.class.getSimpleName()+"-";
		strSuffix=".lock";
		strExitReasonOtherInstance = "";
		setUseFilesystemFileAttributeModifiedTime(false);
	}
	
	private boolean isDevModeExitIfThereIsANewerInstance(){
		return bDebugIDE;
	}
	
	/**
	 * instead of the one written on the lock file
	 * @param b
	 * @return 
	 */
	public SingleAppInstance setUseFilesystemFileAttributeModifiedTime(boolean b) {
		this.bUseFilesystemFileAttributeModifiedTime = b;
		this.bRecreateLockEveryLoop = !this.bUseFilesystemFileAttributeModifiedTime;
		return this;
	}
	
	private File[] getAllLocksTD(){
		return flFolder.listFiles(fnf);
	}
	
	/**
	 * Clear locks that have not been updated lately.
	 */
	private void clearOldLocksTD(){
		File[] afl = getAllLocksTD();
		if(afl==null)return;
		
		for(File fl:afl){
			if(cmpSelfWithTD(fl))continue;
			
			BasicFileAttributes attr = fileReadAttributes(fl);
			if(attr==null)continue;
			
			Long lOtherLastUpdateTimeMilis = null;
			if(bUseFilesystemFileAttributeModifiedTime){
				/**
				 * this may fail on some filesystems
				 */
				lOtherLastUpdateTimeMilis = attr.lastModifiedTime().toMillis();
			}else{
				lOtherLastUpdateTimeMilis = getLastUpdateTimeOfTD(fl);
			}
			
			boolean bDelete=false;
			
			if(lOtherLastUpdateTimeMilis==null)bDelete=true;
			
			if(!bDelete){
				long lMaxDelayWithoutUpdate = lLockUpdateTargetDelayMilis*5;
				
				long lOtherLastUpdateDelayMilis = System.currentTimeMillis()-lOtherLastUpdateTimeMilis;
				boolean bOtherIsAlive = lOtherLastUpdateDelayMilis < lMaxDelayWithoutUpdate;
				if(!bOtherIsAlive)bDelete=true;
			}
			
			if(bDelete){
				msgOutputTD("Cleaning old lock: "+fl.getName());
				fl.delete();
			}
		}
	}
	
	private boolean cmpSelfWithTD(File fl){
		return flSelfLock.getName().equalsIgnoreCase(fl.getName());
	}
	
	/**
	 * In debug mode, the newest will win and the oldest will exit. Developer mode.
	 * In release mode, the oldest will win and the newers will exit. End user mode.
	 * @return
	 */
	private boolean checkExitTD(){
//		if(!btgSingleInstaceMode.b())return false;
		
		bExitApplicationTD=false;
		String strReport="";
		strReport+="-----------------SimultaneousLocks--------------------\n";
		strReport+="ThisLock:  "+flSelfLock.getName()+" "+getSelfMode(true)+"\n";
		int iSimultaneousLocksCount=0;
		for(File flOtherLock:getAllLocksTD()){
			if(cmpSelfWithTD(flOtherLock))continue;
			
			Long lOtherCreationTimeMilis = getCreationTimeOfTD(flOtherLock);
			if(lOtherCreationTimeMilis==null)continue;
			
//			Long lOtherLastUpdateTimeMilis = getLastUpdateTimeOfTD(flOtherLock);
//			if(lOtherLastUpdateTimeMilis==null)continue;
			
			ERunMode ermOther = getLockRunModeOfTD(flOtherLock);
			strReport+="OtherLock: "+flOtherLock.getName()+" "+getMode(ermOther,true)+"\n";
			
			boolean  bOtherIsNewer = lSelfLockCreationTimeMilis < lOtherCreationTimeMilis;
				if(isDevModeExitIfThereIsANewerInstance() && bOtherIsNewer){
					/**
					 * exits if there is a newer debug application instance (development machine)
					 */
					applyExitReasonAboutOtherInstanceStatus("NEWER"+getMode(ermOther,true));
				}else
				if(!isDevModeExitIfThereIsANewerInstance() && !bOtherIsNewer){
					/**
					 * exits if there is an older release application instance (end user machine)
					 */
					applyExitReasonAboutOtherInstanceStatus("OLDER"+getMode(ermOther,true));
				}
//			}
			
			/**
			 * the priority is to keep the release mode instance running
			 * despite.. it may never happen at the end user machine...
			 */
			if(bExitApplicationTD){
				if(!bDebugIDE){ // self is release mode
					if(ermOther.compareTo(ERunMode.Debug)==0){
						/**
						 * will ignore exit request if the other is in debug mode.
						 */
						bExitApplicationTD=false;
					}
				}
			}else{
				/**
				 * If the other instance is in release mode,
				 * this debug mode instance will exit.
				 * 
				 * To test, start a release mode, and AFTER that, start a debug mode one.
				 */
				if(bDebugIDE){
					if(ermOther.compareTo(ERunMode.Release)==0){
						applyExitReasonAboutOtherInstanceStatus(ermOther.toString());
					}
				}
			}
			
			iSimultaneousLocksCount++;
		}
		
		if(bExitApplicationTD){
			msgOutputTD(strReport);
		}else{
			if(iSimultaneousLocksCount>0){
				msgOutputTD(strReport+"This instance will continue running.");
				clearOldLocksTD();
			}
		}
		
		lCheckCountsTD++;
		
		return bExitApplicationTD;
	}
	
	private void applyExitReasonAboutOtherInstanceStatus(String str){
		strExitReasonOtherInstance=str;
		bExitApplicationTD=true;
	}
	
	private class SingleAppInstanceRunnableChecker implements Runnable{
		@Override
		public void run() {
			long lStartMilis = System.currentTimeMillis();
			
			long lIncStep=50;
			long lLockUpdateFastInitDelayMilis=lIncStep;
			while(threadMain==null || threadMain.isAlive()){ //null means not configured yet
				try {
					boolean bWasDeleted=false;
					if(!flSelfLock.exists()){
						msgOutputTD("WARNING!!! Lock was deleted, recreating: "+flSelfLock.getName());
						bWasDeleted=true;
					}
					
					/**
					 * to recreate it every loop is to show the application is alive
					 * by updating its contents with last update time
					 */
					if(bWasDeleted||bRecreateLockEveryLoop){
						createSelfLockFileTD(); 
					}
					
					if(checkExitTD()){
						msgOutputTD("Other "+strExitReasonOtherInstance+" instance is running, exiting this...");
						cleanup();
//						flSelfLock.delete();
						break;
					}
					
					/**
					 * This will also update the file creation time...
					 */
					flSelfLock.setLastModified(System.currentTimeMillis());
					
					/**
					 * sleep after to help on avoiding allocating resources.
					 */
					Thread.sleep(lLockUpdateFastInitDelayMilis);//Thread.getAllStackTraces()
					lCheckTotalDelay+=lLockUpdateFastInitDelayMilis;
					
					if(lLockUpdateFastInitDelayMilis<lLockUpdateTargetDelayMilis){
						lLockUpdateFastInitDelayMilis+=lIncStep;
					}
					
				} catch (InterruptedException e) {
					e.printStackTrace();
					flSelfLock.delete(); //do not use cleanup() here as it may clean more than just this file that will just be recreated above...
				}
			}
			
			long lDelayMilis = System.currentTimeMillis()-lStartMilis;
			
			if(threadMain!=null && !threadMain.isAlive()){
				msgOutputTD("Main thread ended.");
			}
			
			msgOutputTD("Checked times: "+lCheckCountsTD);
			msgOutputTD("Checked total delay (milis): "+lCheckTotalDelay);
			msgOutputTD("Lasted for "+String.format("%.3f", lDelayMilis/1000f)+"s");
			if(twbExitErrorCause!=null){
				/**
				 * this is good to repeat the exception message to the end of the log
				 */
				msgOutputTD("Exit because of exception:");
//				msgOutputTD("ErrorMessage:"+twbExitErrorCause.getMessage());
				runCheckProblems();
				twbExitErrorCause.printStackTrace();
			}
			
			System.exit(0);
		}

	}
	
	/**
	 * 
	 * @param call must return true if problems are found
	 * @return
	 */
	public SingleAppInstance addCheckProblemsCall(CallChkProblemsAbs call) {
		this.acallCheckProblemsList.add(call);
		return this;
	}
	
	public static abstract class CallChkProblemsAbs implements Callable<Integer>{
//		private String	strExitErrorMessage;
		private Throwable	twbExitErrorCause;

		public void setError(Throwable twbExitErrorCause){
//			this.strExitErrorMessage = strExitErrorMessage;
			this.twbExitErrorCause = twbExitErrorCause;
		}

//		public String getExitErrorMessage() {
//			return strExitErrorMessage;
//		}

		public Throwable getExitErrorCause() {
			return twbExitErrorCause;
		}
	}
	
	private void runCheckProblems(){
		int i=0;
		for(CallChkProblemsAbs call:acallCheckProblemsList){
			try {
				call.setError(twbExitErrorCause);
				i+=call.call();
			} catch (Exception e) {
				e.printStackTrace();
				i++;
			}
		}
		msgOutputTD("Problems found: "+i);
	}
	
	private Long getCreationTimeOfTD(File fl){ //LINE 1
		ArrayList<String> astr = fileLoad(fl);
		Long l = null;if(astr.size()>0){try{l = Long.parseLong(astr.get(0));}catch(NumberFormatException|IndexOutOfBoundsException ex){};}
		return l;
	}
	private enum ERunMode{Debug,Release,Undefined,;}
	private ERunMode getLockRunModeOfTD(File fl){ //LINE 2
		ArrayList<String> astr = fileLoad(fl);
		try{return ERunMode.valueOf(astr.get(1));}catch(IllegalArgumentException|IndexOutOfBoundsException e){}
		return ERunMode.Undefined;
	}
	private Long getLastUpdateTimeOfTD(File fl){ //LINE 3
		ArrayList<String> astr = fileLoad(fl);
		Long l = null;if(astr.size()>0){try{l = Long.parseLong(astr.get(2));}catch(NumberFormatException|IndexOutOfBoundsException ex){};}
		return l;
	}
	
	private String getSelfMode(boolean bReportMode){
		return getMode((bDebugIDE?ERunMode.Debug:ERunMode.Release), bReportMode);
	}
	private String getMode(ERunMode erm, boolean bReportMode){
		return ""
			+(bReportMode?"(":"")
			+erm
			+(bReportMode?")":"")
			;
	}
	
	private void createSelfLockFileTD() {
			ArrayList<String> astr = new ArrayList<String>();
			// line 1
			astr.add(""+lSelfLockCreationTimeMilis);
			// line 2
			astr.add(getSelfMode(false));
			// line 3
			astr.add(""+System.currentTimeMillis()); //last update time
			
			flSelfLock.delete();
			if(fileAppendList(flSelfLock, astr)){
				attrSelfLock = fileReadAttributes(flSelfLock);
				
				if(bCreateLockOutputOnce){
					msgOutputTD("Created lock: "+flSelfLock.getName()+" "+getSelfMode(true));
					bCreateLockOutputOnce=false;
				}
				
				flSelfLock.deleteOnExit();
			}else{
				throw new DetailedException("unable to create lock file "+flSelfLock.getAbsolutePath());
			}
	}
	
	/**
	 * at main()
	 * 
	 * This will allow for a very fast exit avoiding resources allocation.
	 * The {@link #configureRequiredAtApplicationInitialization(CommandsDelegator)} call is still required!
	 * 
	 * @param flAppStorageBaseFolder to store the lock file
	 */
	public void configureOptionalAtMainMethod(File flAppStorageBaseFolder){
		configureAndInitialize(true,flAppStorageBaseFolder);
	}
	
	/**
	 * If used alone, without {@link #configureOptionalAtMainMethod()},
	 * this alternative will ignore the resources allocation preventer code.
	 *  
	 * @param flAppStorageBaseFolder can be null
	 */
	public void configureRequiredAtApplicationInitialization(File flAppStorageBaseFolder){//ConsoleCommands cc){
//	public void configureRequiredAtApplicationInitialization(File flPath){//ConsoleCommands cc){
		if(!bConfigured)configureAndInitialize(false,flAppStorageBaseFolder);
//		this.cc=cc;
		this.threadMain=Thread.currentThread();
//		this.flFolder=flPath;
	}
	
	private String findClassWithMainMethod(){
		Map<Thread, StackTraceElement[]> maste = Thread.getAllStackTraces();
		String strMainClass=null;
		for(StackTraceElement[] aste:maste.values()){
			for(StackTraceElement ste:aste){
				if(ste.getMethodName().equals("main")){
					strMainClass=ste.getClassName();
					break;
				}
			}
		}
		if(strMainClass==null)throw new DetailedException("unable to determine class with main method?");
		return strMainClass;
	}
	
	private void configureAndInitialize(boolean bAllowCfgOutOfMainMethod,File flAppStorageBaseFolder){
		if(bConfigured)throw new DetailedException("already configured."); // KEEP ON TOP
		
//		if(flAppStorageBaseFolder!=null && flAppStorageBaseFolder!=this.flAppStorageBaseFolder){
//			throw new DetailedException("app storage folder already configured");
//		}
		this.flAppStorageBaseFolder=flAppStorageBaseFolder;
	
		this.bAllowCfgOutOfMainMethod=bAllowCfgOutOfMainMethod;
		if(!bAllowCfgOutOfMainMethod){
			msgOutputTD("DEVELOPER: if too much resources are being allocated, try the 'configuration at main()' option.");
		}
		
		lSelfLockCreationTimeMilis = System.currentTimeMillis();
		
		strId=strPrefix
			+new SimpleDateFormat("yyyyMMdd-HHmmss-SSS").format(new Date(lSelfLockCreationTimeMilis))
			+strSuffix;
		
		String strPkgAndClassWithMainMethod = findClassWithMainMethod();
		
		flFolder = new File(
			this.flAppStorageBaseFolder,
			strPkgAndClassWithMainMethod.replace(".",File.separator) //package of main class
				+File.separator+SingleAppInstance.class.getSimpleName() //self
		);
		
		flSelfLock = new File(flFolder,strId);
		
//		flFolder = new File("./");
		fnf = new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				if(name.startsWith(strPrefix) && name.endsWith(strSuffix))return true;
				return false;
			}
		};
		
		if(bDebugIDE){
			msgOutputTD("This instance is in DEBUG mode. ");
		}
		
		/**
		 * creating the new thread here will make the application ends faster if it can.
		 */
		clearOldLocksTD();
		createSelfLockFileTD();
		threadChecker = new Thread(new SingleAppInstanceRunnableChecker());
		threadChecker.setName(SingleAppInstanceRunnableChecker.class.getSimpleName());
		threadChecker.start();
		
		/**
		 * this will sleep the main thread (the thread configuring this class)
		 */
		threadSleepWaitSingleInstanceFastCheck();
		
		bConfigured=true;
	}
	
	private void assertFlowAtMainMethodThread(){
		StackTraceElement[] ast = Thread.currentThread().getStackTrace();
		boolean bIsFromMainMethod=false;
		for(StackTraceElement ste:ast){
			if(ste.getMethodName()=="main"){
				bIsFromMainMethod=true;
				break;
			}
		}
		
		if(!bIsFromMainMethod){
			msgOutputTD(
				"The flow that reaches this method must be called at 'main()'. " 
				+"This must be called before the main window shows up, what will allocate resources."
				+"Alternatively, skip it by allowing configuration out of 'main()' method."
			);
			Thread.dumpStack();
			System.exit(1);
		}
	}
	
	/**
	 * This helps on avoiding allocating too much resources.
	 */
	private void threadSleepWaitSingleInstanceFastCheck(){
		if(!bAllowCfgOutOfMainMethod)assertFlowAtMainMethodThread();
		
		try {
			long lWaitDelayMilis=100;
			long lMaxDelayToWaitForChecksMilis=1000;
			while(true){
				if(bExitApplicationTD){
					/**
					 * if the application is exiting, keep sleeping.
					 */
					Thread.sleep(lWaitDelayMilis);
				}else{
					if(lCheckTotalDelay<lMaxDelayToWaitForChecksMilis){
						Thread.sleep(lWaitDelayMilis);
					}else{
						/**
						 * initial check allowed this instance of the application
						 * to continue running 
						 */
						break;
					}
				}
				lWaitCount++;
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		msgOutputTD("Waited times: "+lWaitCount);
	}
	
	private void msgOutputTD(String str){
		System.err.println(""
			+"["+SingleAppInstance.class.getSimpleName()+"]"
			+"["+new SimpleDateFormat("HH:mm:ss.SSS").format(Calendar.getInstance().getTime())+"]"
			+": "
			+str.replace("\n", "\n\t"));
	}
	
	private void cleanup() {
		flSelfLock.delete();
	}

	synchronized private boolean fileAppendList(File fl, ArrayList<String> astr) {
		boolean bWrote=false;
		try {
			fl.getParentFile().mkdirs();
			Files.write(fl.toPath(),astr,Charset.defaultCharset());
			bWrote=true;
		} catch (IOException e) {e.printStackTrace();}
		return bWrote;
	}

	synchronized private BasicFileAttributes fileReadAttributes(File fl){
		try {
			return Files.readAttributes(fl.toPath(), BasicFileAttributes.class);
		} catch (IOException e) {e.printStackTrace();}
		return null;
	}
	
	synchronized private ArrayList<String> fileLoad(File fl)  {
		ArrayList<String> astr = new ArrayList<String>();
		try {
			astr.addAll( Files.readAllLines(fl.toPath(), StandardCharsets.UTF_8) );
		} catch (IOException e) {
			e.printStackTrace();
		}
		return astr;
	}

	public void setExitRequestCause(Throwable t) {
//		this.strExitErrorMessage=strErrMsg;
		this.twbExitErrorCause=t;
	}
	
}
