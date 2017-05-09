/* 
Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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
* 
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*
*/
public class KeyBindManagerI {
	
	private TreeMap<String,KeyBoundVarField> tmbindList = new TreeMap<String, KeyBoundVarField>(String.CASE_INSENSITIVE_ORDER);
	private HashMap<Integer,ArrayList<KeyBoundVarField>> hmKeyCodeVsActivatedBind = new HashMap<Integer, ArrayList<KeyBoundVarField>>();

	private KeyBoundVarField	bindCaptureToTarget;
	
	private KeyBind kbCaptured;
	private KeyBind kbWaitBeReleased;

//	private boolean	bResetCaptureBeaconOnSuccess;
	

	private IRefresh	refreshOwnerAfterCapture;

	private StackTraceElement[]	asteAlertFrom;

	private KeyBoundVarField	bindConflict;

//	private boolean	bWaitingUserDecision;
	
	private String strRequestUserDecision="Press ESC to cancel or Enter to retry.\n";

	private boolean	bConfigured;

	private Object objLinkedGuiElement;
	
	public KeyBindManagerI() {
		DelegateManagerI.i().addManager(this,KeyBoundVarField.class);
	}
	public void configure(){}
	
	public String getMappingFrom(KeyBoundVarField bind){
		String strMapping=null;
		
		if(bind.isField()){
			strMapping=bind.getUniqueVarId(true);
			if(tmbindList.containsKey(strMapping)){
				KeyBoundVarField bindExisting = tmbindList.get(strMapping);
				if(bindExisting!=bind){
					throw new DetailedException("conflicting key mapping ids", bindExisting, bind);
				}
			}
		}else{
			strMapping = bind.getBindCfg();
		}
		
		return strMapping;
	}
	
	public void removeKeyBind(KeyBoundVarField bind){
		String strMapping=getMappingFrom(bind);
		removeKeyBind(strMapping);
	}
	public void removeKeyBind(String strBindCfg){
		tmbindList.remove(new KeyBind().setFromKeyCfg(strBindCfg).getBindCfg());
	}
	
	@Override
	public boolean addHandled(KeyBoundVarField bind){
		String strMapping=getMappingFrom(bind);
		
		KeyBoundVarField kbExisting = tmbindList.get(strMapping);
		
		tmbindList.put(strMapping,bind);
		
//		DetailedException.assertIsTrue("manager not set", !bind.isHasManagers(), bind, this);
		bind.addManager(this);
		
		return kbExisting!=bind;
	}
	
	
	private boolean isWaitKeyRelease(){
		/**
		 * this also prevents executing the bind's target command just after capturing its bind!
		 */
		if(kbWaitBeReleased!=null){
			if(kbWaitBeReleased.isActivated()){ 
				//still pressed (with modifiers)
				MessagesI.i().info("waiting captured key bind to be released");
				return true; 
			}else
			if(kbWaitBeReleased.getActionKey().isPressed()){ 
				//still pressed (only the action key is still being holded)
				MessagesI.i().info("waiting captured action key to be released");
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
			
			GlobalOSAppI.i().setDynamicInfo(GlobalManageKeyCodeI.i().getAllPressedKeysSimpleReport());
			
			kbCaptured=GlobalManageKeyCodeI.i().getPressedKeysAsKeyBind();
			
			if(bindConflict!=null){
				ECaptureUserDecision eud=null;
				if(isCapturedThisKeyCodeWithoutMods(GlobalManageKeyCodeI.i().getKeyCodeForEscape())){
					eud=ECaptureUserDecision.Cancelled;
				}else
				if(isCapturedThisKeyCodeWithoutMods(GlobalManageKeyCodeI.i().getKeyCodeForReturn())){
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
				
				if(isCapturedThisKeyCodeWithoutMods(GlobalManageKeyCodeI.i().getKeyCodeForEscape())){
					captureKeyStep(ECaptureUserDecision.Cancelled);
					return true;
				}
				
				bindConflict=null;
				for(KeyBoundVarField bind:getHandledListCopy()){
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
					
					MessagesI.i().info(
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
		for(KeyBoundVarField bind:tmbindList.values()){
			if(bind.isValueNull())continue; //not set yet
			
			if(bind.getKeyBind().isActivated()){ //pressed
				Integer iKeyCode = bind.getKeyBind().getActionKey().getKeyCode();
				
				ArrayList<KeyBoundVarField> abindForActKeyCode = hmKeyCodeVsActivatedBind.get(iKeyCode);
				if(abindForActKeyCode==null){
					abindForActKeyCode=new ArrayList<KeyBoundVarField>();
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
		for(ArrayList<KeyBoundVarField> abindForActKeyCode:hmKeyCodeVsActivatedBind.values()){
			KeyBoundVarField bindWin=abindForActKeyCode.get(0);
			
			for(KeyBoundVarField bind:abindForActKeyCode){
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
				if(!GlobalOSAppI.i().isShowingAlert(true)){
					asteAlertFrom = GlobalOSAppI.i().showSystemAlert(
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
				
				GlobalOSAppI.i().hideSystemAlert(asteAlertFrom,true);
				break;
			case KeyReleased:
				kbWaitBeReleased=null;
				break;
			case HasConflict:
				GlobalOSAppI.i().hideSystemAlert(asteAlertFrom);
				
				String strMsg = "captured key bind "+kbCaptured.getBindCfg()
					+" is already being used by "+bindConflict.getKeyBindRunCommand()+"\n"
					+strRequestUserDecision;
				MessagesI.i().warn(strMsg,	bindConflict,bindCaptureToTarget,kbCaptured,this);
				
				asteAlertFrom = GlobalOSAppI.i().showSystemAlert(strMsg,objLinkedGuiElement);
				
				kbCaptured=null;
				break;
			case Cancelled:
				kbCaptured=null;
				bindConflict=null;
				bindCaptureToTarget=null;
				kbWaitBeReleased=null;
				
				GlobalOSAppI.i().hideSystemAlert(asteAlertFrom);
				break;
			case Success:
				captureKeyStep(ECaptureUserDecision.Cancelled);
				
				refreshOwnerAfterCapture.requestRefresh();
				GlobalCommandsDelegatorI.i().setupRecreateFile();
				break;
		}
		
	}

	public ArrayList<KeyBoundVarField> getHandledListCopy(){
		return new ArrayList<KeyBoundVarField>(tmbindList.values());
	}

	public ArrayList<String> getReportAsCommands(StringCmdField scfBindKey, boolean bOnlyUserCustomOnes) {
		ArrayList<String> astr = new ArrayList<String>();
		for(KeyBoundVarField bind:getHandledListCopy()){
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
			
			astr.add(GlobalCommandsDelegatorI.i().getCommandPrefixStr()+scfBindKey.getSimpleId()+" "
				+bind.getBindCfg()+" "
				+strUserCmd
				+strMapping);
		}
		return astr;
	}

	public void captureAndSetKeyBindAt(KeyBoundVarField bindTarget, IRefresh refreshOwner, Object objLinkedGuiElement) {
		if(this.bindCaptureToTarget!=null){
			MessagesI.i().devWarn("already capturing keybind for", this.bindCaptureToTarget, this.refreshOwnerAfterCapture, this);
			return;
		}
		
		this.refreshOwnerAfterCapture=refreshOwner;
		this.bindCaptureToTarget=bindTarget;
		this.objLinkedGuiElement=objLinkedGuiElement;
//		MessagesI.i().info("For unconventional (more complex) key bindings, use the console command.", bindTarget); 
//		bindTarget.setValue(captureKeyBind(bindTarget));
	}

//	protected abstract KeyBind captureKeyBind(KeyBoundVarField bindTarget);

	@Override public String getUniqueId() {return MiscI.i().prepareUniqueId(this);}

	@Override
	public boolean isConfigured() {
		return bConfigured;
	}
	
	@Override
	public KeyBindManagerI configure(ICfgParm icfg) {
		bConfigured=true;
		return this;
	}

}