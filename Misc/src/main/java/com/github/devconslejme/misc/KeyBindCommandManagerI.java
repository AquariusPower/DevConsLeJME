/* 
Copyright (c) 2016-2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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
import java.util.HashMap;
import java.util.TreeMap;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class KeyBindCommandManagerI {
	public static KeyBindCommandManagerI i(){return GlobalManagerI.i().get(KeyBindCommandManagerI.class);}
	
	private TreeMap<String,BindCommand> tmbindList = new TreeMap<String, BindCommand>(String.CASE_INSENSITIVE_ORDER);
	private HashMap<Integer,ArrayList<BindCommand>> hmKeyCodeVsActivatedBind = new HashMap<Integer, ArrayList<BindCommand>>();
	private BindCommand	bindCaptureToTarget;
	private KeyBind kbCaptured;
	private KeyBind kbWaitBeReleased;
//	private IRefresh	refreshOwnerAfterCapture;
	private StackTraceElement[]	asteAlertFrom;
	private BindCommand	bindConflict;
	private String strRequestUserDecision="Press ESC to cancel or Enter to retry.\n";
	private Object objLinkedGuiElement;
	
	public static abstract class CaptureKeyBindAbs{
		public abstract void hideSystemAlert(StackTraceElement[] asteAlertFrom);
		public abstract StackTraceElement[] showSystemAlert(String strMsg,	Object objLinkedGuiElement);
		public abstract void hideSystemAlert(StackTraceElement[] asteAlertFrom, boolean b);
		public abstract boolean isShowingAlert(boolean b);
		public abstract void setDynamicInfo(String allPressedKeysSimpleReport);
		public abstract void setupRecreateFile();
	}
	private CaptureKeyBindAbs capture;
	
	public void configure(CaptureKeyBindAbs capture){
		this.capture=capture;
	}
	
	/**
	 * TODO tmp placeholder dummy based on old KeyBoundVarField
	 */
	public static class BindCommand{

		public boolean isField() {
			// TODO Auto-generated method stub
			return false;
		}

		public String getUniqueVarId(boolean b) {
			// TODO Auto-generated method stub
			return null;
		}

		public String getBindCfg() {
			// TODO Auto-generated method stub
			return null;
		}

//		public void addManager(ConfigureKeyBindManagerI keyBindManagerI) {
//			// TODO Auto-generated method stub
//			
//		}

		public KeyBind getKeyBind() {
			// TODO Auto-generated method stub
			return null;
		}

		public void setValue(KeyBind kbCaptured) {
			// TODO Auto-generated method stub
			
		}

		public String getKeyBindRunCommand() {
			// TODO Auto-generated method stub
			return null;
		}

		public boolean isValueNull() {
			// TODO Auto-generated method stub
			return false;
		}

		public void runIfActivatedOrResetIfDeactivating() {
			// TODO Auto-generated method stub
			
		}

		public String getUserCommand() {
			// TODO Auto-generated method stub
			return null;
		}
		
	}
	
//	public ConfigureKeyBindManagerI() {
//		DelegateManagerI.i().addManager(this,KeyBoundVarField.class);
//	}
//	public void configure(){}
	
	public String getMappingFrom(BindCommand bind){
		String strMapping=null;
		
		if(bind.isField()){
			strMapping=bind.getUniqueVarId(true);
			if(tmbindList.containsKey(strMapping)){
				BindCommand bindExisting = tmbindList.get(strMapping);
				if(bindExisting!=bind){
					throw new DetailedException("conflicting key mapping ids", bindExisting, bind);
				}
			}
		}else{
			strMapping = bind.getBindCfg();
		}
		
		return strMapping;
	}
	
	public void removeKeyBind(BindCommand bind){
		String strMapping=getMappingFrom(bind);
		removeKeyBind(strMapping);
	}
	public void removeKeyBind(String strBindCfg){
		tmbindList.remove(new KeyBind().setFromKeyCfg(strBindCfg).getBindCfg());
	}
	
	public boolean addBindCommand(BindCommand bind){
		String strMapping=getMappingFrom(bind);
		
		BindCommand kbExisting = tmbindList.get(strMapping);
		
		tmbindList.put(strMapping,bind);
		
//		DetailedException.assertIsTrue("manager not set", !bind.isHasManagers(), bind, this);
//		bind.addManager(this);
		
		return kbExisting!=bind;
	}
	
	
	private boolean isWaitKeyRelease(){
		/**
		 * this also prevents executing the bind's target command just after capturing its bind!
		 */
		if(kbWaitBeReleased!=null){
			if(kbWaitBeReleased.isActivated()){ 
				//still pressed (with modifiers)
				MessagesI.i().output(false, System.out, "Info", this, "waiting captured key bind to be released");
				return true; 
			}else
			if(kbWaitBeReleased.getActionKey().isPressed()){ 
				//still pressed (only the action key is still being holded)
				MessagesI.i().output(false, System.out, "Info", this, "waiting captured action key to be released");
				return true;
			}else
			{ //released
				if(bindCaptureToTarget!=null && bindCaptureToTarget.getKeyBind()==kbWaitBeReleased){
					captureKeyStep(ECaptureUserDecision.Success);
				}else{
					captureKeyStep(ECaptureUserDecision.KeyReleased);
				}
			}
			
			return true;
		}
		
		return false;
	}
	
	private boolean isCaptureKeyModeAndWorkIt(){
		if(isWaitKeyRelease())return true;
		
		if(bindCaptureToTarget!=null){
			
			capture.setDynamicInfo(KeyCodeManagerI.i().getAllPressedKeysSimpleReport());
			
			kbCaptured=KeyCodeManagerI.i().getPressedKeysAsKeyBind();
			
			if(bindConflict!=null){
				ECaptureUserDecision eud=null;
				if(isCapturedThisKeyCodeWithoutMods(KeyCodeManagerI.i().getKeyCodeForEscape())){
					eud=ECaptureUserDecision.Cancelled;
				}else
				if(isCapturedThisKeyCodeWithoutMods(KeyCodeManagerI.i().getKeyCodeForEnter())){
					eud=ECaptureUserDecision.Retry;
				}
				
				if(eud!=null){
					kbWaitBeReleased=kbCaptured;
//					GlobalAppOSI.i().hideSystemAlert(asteAlertFrom, eud.compareTo(ECaptureUserDecision.Retry)==0);
					captureKeyStep(eud);
				}
				
				return true;
			}
			
			if(kbCaptured==null){
				captureKeyStep(ECaptureUserDecision.KeepTrying);
//				if(!GlobalAppOSI.i().isShowingAlert()){
//					asteAlertFrom = GlobalAppOSI.i().showSystemAlert(
//						 " Press a key combination to be captured (where modifiers are ctrl, shift or alt).\n"
//						+" More complex keybindings can be set thru console commands.\n"
//						+" Press ESC to cancel.\n"
//						+" Re-binding keys for command:\n"
//						+"  "+bindCaptureToTarget.getKeyBindRunCommand()
//					);
//				}
			}else{
//				if(GlobalAppOSI.i().isShowingAlert()){
//					GlobalAppOSI.i().hideSystemAlert(asteAlertFrom);
//				}
				
				if(isCapturedThisKeyCodeWithoutMods(KeyCodeManagerI.i().getKeyCodeForEscape())){
					captureKeyStep(ECaptureUserDecision.Cancelled);
					return true;
				}
				
				bindConflict=null;
				for(BindCommand bind:getHandledListCopy()){
					if(bind==bindCaptureToTarget)continue;
					if(bind.getKeyBind().isEquivalentTo(kbCaptured)){
						bindConflict=bind;
						break;
					}
				}
				
				if(bindConflict!=null){
					captureKeyStep(ECaptureUserDecision.HasConflict);
					return true;
				}else{
//					bindCaptureToTarget.setAllowCallerAssignedToBeRun(false);
					bindCaptureToTarget.setValue(kbCaptured);
//					bindCaptureToTarget.setAllowCallerAssignedToBeRun(true);
					
					MessagesI.i().output(false, System.out, "Info", this, 
						"captured key bind "+kbCaptured.getBindCfg()
						+" for "+bindCaptureToTarget.getKeyBindRunCommand(),
						bindCaptureToTarget,kbCaptured,this);
				}
				
				kbWaitBeReleased=kbCaptured;
			}
			
			return true;
		}
		
		return false;
	}
	
	public void update(float fTpf){
		if(isCaptureKeyModeAndWorkIt())return;
		
		runActiveBinds();
	}
	
	private void runActiveBinds() {
		/**
		 * fill all binds for each action keycode to check which will win.
		 */
		hmKeyCodeVsActivatedBind.clear();
		for(BindCommand bind:tmbindList.values()){
			if(bind.isValueNull())continue; //not set yet
			
			if(bind.getKeyBind().isActivated()){ //pressed
				Integer iKeyCode = bind.getKeyBind().getActionKey().getKeyCode();
				
				ArrayList<BindCommand> abindForActKeyCode = hmKeyCodeVsActivatedBind.get(iKeyCode);
				if(abindForActKeyCode==null){
					abindForActKeyCode=new ArrayList<BindCommand>();
					hmKeyCodeVsActivatedBind.put(iKeyCode, abindForActKeyCode);
				}
				
				abindForActKeyCode.add(bind);
			}else{ //released
				if(bind.getKeyBind().isWasAlreadyActivatedAtLeastOnce()){
					bind.runIfActivatedOrResetIfDeactivating();
				}
			}
		}
		
		/**
		 * only the activated bind with most modifiers will be run! 
		 */
		for(ArrayList<BindCommand> abindForActKeyCode:hmKeyCodeVsActivatedBind.values()){
			BindCommand bindWin=abindForActKeyCode.get(0);
			
			for(BindCommand bind:abindForActKeyCode){
				if(bindWin.getKeyBind().getKeyModListSize() < bind.getKeyBind().getKeyModListSize()){
					bindWin=bind;
				}
			}
			
			bindWin.runIfActivatedOrResetIfDeactivating();
		}
	}

	private boolean isCapturedThisKeyCodeWithoutMods(int iKeyCode) {
		if(
				kbCaptured!=null &&
				kbCaptured.getKeyModListSize()==0 //looking for just ESC or Enter
		){
			if(kbCaptured.getActionKey().getKeyCode() == iKeyCode){
				return true;
			}
		}
		
		return false;
	}

	enum ECaptureUserDecision{
		Cancelled,
		Retry,
		Success, 
		
		/** wait user decision */
		HasConflict, 
		
		KeyReleased, 
		
		KeepTrying,
	}
	
	private void captureKeyStep(ECaptureUserDecision e) {
		switch(e){
			case KeepTrying:
				if(!capture.isShowingAlert(true)){
					asteAlertFrom = capture.showSystemAlert(
							 " Press a key combination to be captured (where modifiers are ctrl, shift or alt).\n"
							+" More complex or specific keybindings can be set thru console commands.\n"
							+" Press ESC to cancel.\n"
							+"\n"
							+" Re-binding keys for command:\n"
							+"  "+bindCaptureToTarget.getKeyBindRunCommand()+"\n"
							+" Current key bind: "+bindCaptureToTarget.getBindCfg()+"\n",
							objLinkedGuiElement
						);
				}
				break;
			case Retry:
				kbCaptured=null;
				bindConflict=null;
				
				capture.hideSystemAlert(asteAlertFrom,true);
				break;
			case KeyReleased:
				kbWaitBeReleased=null;
				break;
			case HasConflict:
				capture.hideSystemAlert(asteAlertFrom);
				
				String strMsg = "captured key bind "+kbCaptured.getBindCfg()
					+" is already being used by "+bindConflict.getKeyBindRunCommand()+"\n"
					+strRequestUserDecision;
				MessagesI.i().warnMsg(this,strMsg,	bindConflict,bindCaptureToTarget,kbCaptured,this);
				
				asteAlertFrom = capture.showSystemAlert(strMsg,objLinkedGuiElement);
				
				kbCaptured=null;
				break;
			case Cancelled:
				kbCaptured=null;
				bindConflict=null;
				bindCaptureToTarget=null;
				kbWaitBeReleased=null;
				
				capture.hideSystemAlert(asteAlertFrom);
				break;
			case Success:
				captureKeyStep(ECaptureUserDecision.Cancelled);
				
//				refreshOwnerAfterCapture.requestRefresh();
				capture.setupRecreateFile();
				break;
		}
		
	}

	public ArrayList<BindCommand> getHandledListCopy(){
		return new ArrayList<BindCommand>(tmbindList.values());
	}

//	public ArrayList<String> getReportAsCommands(StringCmdField scfBindKey, boolean bOnlyUserCustomOnes) {
	public ArrayList<String> getReportAsCommands(boolean bOnlyUserCustomOnes) {
		ArrayList<String> astr = new ArrayList<String>();
		for(BindCommand bind:getHandledListCopy()){
			if(bOnlyUserCustomOnes && bind.isField())continue;
			
			String strMapping = getMappingFrom(bind);
			if(strMapping.equals(bind.getBindCfg())){
				strMapping="";
			}else{
				strMapping="#ActionMappingId("+strMapping+")";
			}
			
			String strUserCmd = bind.getUserCommand();
			if(strUserCmd==null){
				strUserCmd="";
			}else{
				strUserCmd+=" ";
			}
			
//			astr.add(GlobalCommandsDelegatorI.i().getCommandPrefixStr()+scfBindKey.getSimpleId()+" "
			astr.add(""
				+bind.getBindCfg()+" "
				+strUserCmd
				+strMapping);
		}
		return astr;
	}

//	public void captureAndSetKeyBindAt(BindCommand bindTarget, IRefresh refreshOwner, Object objLinkedGuiElement) {
	public void captureAndSetKeyBindAt(BindCommand bindTarget, Object objLinkedGuiElement) {
		if(this.bindCaptureToTarget!=null){
//			MessagesI.i().warnMsg(this, "already capturing keybind for", this.bindCaptureToTarget, this.refreshOwnerAfterCapture, this);
			MessagesI.i().warnMsg(this, "already capturing keybind for", this.bindCaptureToTarget, this);
			return;
		}
		
//		this.refreshOwnerAfterCapture=refreshOwner;
		this.bindCaptureToTarget=bindTarget;
		this.objLinkedGuiElement=objLinkedGuiElement;
//		MessagesI.i().info("For unconventional (more complex) key bindings, use the console command.", bindTarget); 
//		bindTarget.setValue(captureKeyBind(bindTarget));
	}
	
}