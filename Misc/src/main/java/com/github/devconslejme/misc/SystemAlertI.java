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

import java.io.File;

import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.lemur.SystemAlertLemurI;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SystemAlertI {
	public static SystemAlertI i(){return GlobalManagerI.i().get(SystemAlertI.class);}
	
	private File	flBaseSaveDataPath;
	private boolean	bExiting=false;
	private String	strAlertMsg;
	private long	lLastAlertMilis;
	private StackTraceElement[]	asteStackKeyRequestOrigin;
	private boolean	bFirstTimeQuickUpdate;
	private String	strDynamicInfo="";
	private StackTraceElement[]	asteLastValidHideRequestOrigin;
	private Object	objActionSourceElement;
	
	/**
	 * 
	 * @param asteStackKeyRequestOrigina
	 * @param bKeepGuiBlockerOnce useful when going to retry having subsequent alerts (DevNote: will only work if overriden by GUI)
	 */
	public void hideSystemAlert(StackTraceElement[] asteStackKeyRequestOrigin, boolean bKeepGuiBlockerOnce) {
		if(this.strAlertMsg!=null){
			this.asteLastValidHideRequestOrigin=Thread.currentThread().getStackTrace();
		}else{
			MessagesI.i().warnMsg(this,"already hidden", this, this.asteLastValidHideRequestOrigin);
		}
		
		DetailedException.assertIsTrue("alert origin matches", asteStackKeyRequestOrigin==this.asteStackKeyRequestOrigin, asteStackKeyRequestOrigin, this.asteStackKeyRequestOrigin, this);
		this.strAlertMsg=null;
		this.asteStackKeyRequestOrigin=null;
	}
	public void hideSystemAlert(StackTraceElement[] asteFrom){
		hideSystemAlert(asteFrom, false);
	}
	
	public boolean isValidRequestOriginKey(StackTraceElement[] asteStackKeyRequestOrigin){
		return this.asteStackKeyRequestOrigin==asteStackKeyRequestOrigin;
	}
	
	/**
	 * 
	 * @param strMsg
	 * @param objActionSourceElement
	 * @return stack trace key to allow hiding the alert
	 */
	public StackTraceElement[] showSystemAlert(String strMsg, Object objActionSourceElement){
		DetailedException.assertNotAlreadySet(this.strAlertMsg, strMsg, "system alert message", asteStackKeyRequestOrigin, this);
		this.asteStackKeyRequestOrigin=Thread.currentThread().getStackTrace();
		this.strAlertMsg=strMsg;
		bFirstTimeQuickUpdate=true;
		this.objActionSourceElement=objActionSourceElement;
//		dumpAlert(); //just in case another one happens before the update...
		
		MessagesI.i().output(true, System.out, "ALERT!!!", this, strAlertMsg, strDynamicInfo, objActionSourceElement);
		enqueueCaptureUserInput();
		
		return this.asteStackKeyRequestOrigin;
	}
	
//	/**
//	 * Override also to disable this, if other means will be used than direct input reading
//	 */
	/** */
	protected void enqueueCaptureUserInput() {
		QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
					updateCaptureUserInput(getTPF(),this);
					return true;
				}
			}
			.enableLoopMode()
			.setInitialDelay(0)
			.setDelaySeconds(2f)
		);
	}
	
	protected boolean updateCaptureUserInput(float fTPF, CallableX cx){
		if(strAlertMsg==null){
			cx.endLoopMode();
			endCaptureUserInput();
			return true;
		}
		
		doCapture(fTPF,cx);
		
		return true;
	}
	
	protected void endCaptureUserInput() {
		//TODO do something here (one day...)
	}
	
	/**
	 * Override if other means will be used to capture input
	 */
	protected void doCapture(float fTPF, CallableX cxQueuedCall) {
		MessagesI.i().output(true, System.out, "ALERT!!!", this, strAlertMsg, strDynamicInfo);
		//TODO do capture text terminal input (one day...)
		throw new UnsupportedOperationException("not implemented");
	}
	
	public boolean isShowingAlert(){
		return strAlertMsg!=null;
	}
	
	public void setDynamicInfo(String str) {
		this.strDynamicInfo=str;
	}
	
	public String getDynamicInfo(){
		return strDynamicInfo;
	}
	
	public boolean isDynamicInfoSet(){
		return !strDynamicInfo.isEmpty();
	}
	
	public String getFullMessage(){
		return "ALERT!!!\n"
				+getAlertMessage()+"\n"
				+(isDynamicInfoSet() ? "DynamicInfo: "+getDynamicInfo()+"\n" : "");
	}

	public String getAlertMessage(){
		return strAlertMsg;
	}
	
	public Object getActionSourceElement() {
		return objActionSourceElement;
	}
	
	public void showTemporarySystemAlert(String strMsg, float fDelay){
		String strName="TemporarySystemAlert";
		QueueI.i().enqueue(new CallableXAnon() {
			TimedDelay td = new TimedDelay(fDelay, strName);
			private StackTraceElement[]	aste;
			@Override
			public Boolean call() {
				if(!SystemAlertLemurI.i().isShowingAlert()){
					aste=SystemAlertLemurI.i().showSystemAlert(strMsg, null);
					td.setActive(true);
				}else{
					if(!isValidRequestOriginKey(aste))return false; //there is some other alert going on, wait it end
					
					if(td.isReady()){
						SystemAlertLemurI.i().hideSystemAlert(aste);
						return true;//end
					}
				}
				
				return false;
			}
		}.setName("TemporarySystemAlert"));
	}
	
	public boolean isAlertReady() {
		return true;
	}
}

