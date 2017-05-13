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

import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.google.common.base.Function;


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
	private StackTraceElement[]	asteAlertFrom;
	private BindCommand	bindConflict;
	private String strRequestUserDecision="Press ESC to cancel or Enter to retry.\n";
	private Object objLinkedGuiElement;
	private Function<String,Boolean> funcRunUserCommand;
	
	/**
	 * TODO tmp placeholder dummy based on old KeyBoundVarField
	 */
	public static class BindCommand{
		private CallableX	cxHardCommand;
		private KeyBind	kb;
//		private boolean	bReadOnly;
		private String	strUserCommand;
		
//		public boolean isReadOnly() {
//			return bReadOnly;
//		}
//		
//		/**
//		 * locks the keybind preventing changing the bound keys and the command
//		 * @return
//		 */
//		public BindCommand setReadOnly(){
//			this.bReadOnly=true;
//			return this;
//		}

		public KeyBind getKeyBind() {
			return kb;
		}

		public BindCommand setKeyBind(KeyBind kb) {
			this.kb=kb;
			return this;
		}

		public boolean isCommandSet() {
			return cxHardCommand!=null;
		}

		public String getCommandsInfo(){
			String str="";
			if(strUserCommand!=null)str+=strUserCommand;
			if(!str.isEmpty())str+=";";
			if(cxHardCommand!=null)str+=cxHardCommand.getName();
			return str;
		}
		
		public String getUserCommand() {
			return strUserCommand;
		}
		public CallableX getHardCommand(){
			return cxHardCommand;
		}
		
		/**
		 * TODO syntax for executing on-press, on-release, b4 hard-command, after hard-command.
		 * @param strUserCommand
		 */
		public void setUserCommand(String strUserCommand){
			this.strUserCommand=strUserCommand;
		}
		public void setHardCommand(CallableX cx){
//			if(this.cxCommand!=null && isReadOnly())throw new DetailedException("readonly",this);
//			if(isReadOnly())throw new DetailedException("readonly",this);
			assertHardCommandNotSet(cx); //DetailedException.assertNotAlreadySet(this.cxHardCommand, cx, this);
			DetailedException.assertIsTrue("name set (a description is necessary as the command is hard coded)", !cx.getName().isEmpty(), cx, this);
			this.cxHardCommand = cx;
		}
		
		public void assertHardCommandNotSet(Object... aobjDbg){
			if(this.cxHardCommand!=null)throw new DetailedException("already set",this,aobjDbg);
		}
		
//		private boolean isReadOnly() {
//			return cxCommand!=null;
//		}

	}
	
	public void configure(){
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}.enableLoopMode());
	}
	
//	public String getMappingFrom(BindCommand bind){
//		String strMapping=bind.getKeyBind().getBindCfg();
//		
//		BindCommand bindExisting = tmbindList.get(strMapping);
//		if(bindExisting!=bind){ //TODO this should happen when the bind is added...
//			throw new DetailedException("conflicting key mapping ids", bindExisting, bind);
//		}
//		
////		if(bind.isField()){
////			strMapping=bind.getUniqueVarId(true);
////			if(tmbindList.containsKey(strMapping)){
////				BindCommand bindExisting = tmbindList.get(strMapping);
////				if(bindExisting!=bind){
////					throw new DetailedException("conflicting key mapping ids", bindExisting, bind);
////				}
////			}
////		}else{
////			strMapping = bind.getKeyBind().getBindCfg();
////		}
//		
//		return strMapping;
//	}
	
	public void removeKeyBind(BindCommand bc){
		removeKeyBind(bc.getKeyBind().getBindCfg());
	}
	public void removeKeyBind(String strBindCfg){
		strBindCfg=new KeyBind().setFromKeyCfg(strBindCfg).getBindCfg(); //fixed/corrected cfg
		BindCommand bc = tmbindList.get(strBindCfg);
		bc.assertHardCommandNotSet(strBindCfg);
//		if(bc.isReadOnly())throw new DetailedException("readonly",bc);
		tmbindList.remove(strBindCfg);
	}
	
	/**
	 * 
	 * @param bc
	 * @return previously set one if any
	 */
	public BindCommand putBindCommand(BindCommand bc){
		String str=bc.getKeyBind().getBindCfg();
		
		BindCommand bcExisting = tmbindList.get(str);
//		if(bcExisting.isReadOnly())throw new DetailedException("readonly",bcExisting,bc);
		if(bcExisting!=null)bcExisting.assertHardCommandNotSet(bc);
		
		return tmbindList.put(str,bc);
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
	
	/**
	 * 
	 * @return true if capturing is on going
	 */
	private boolean updateCaptureKey(){
		if(isWaitKeyRelease())return true;
		
		if(bindCaptureToTarget!=null){
			SystemAlertI.i().setDynamicInfo(KeyCodeManagerI.i().getAllPressedKeysSimpleReport());
			
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
					captureKeyStep(eud);
				}
				
				return true;
			}
			
			if(kbCaptured==null){
				captureKeyStep(ECaptureUserDecision.KeepTrying);
			}else{
				if(isCapturedThisKeyCodeWithoutMods(KeyCodeManagerI.i().getKeyCodeForEscape())){
					captureKeyStep(ECaptureUserDecision.Cancelled);
					return true;
				}
				
				bindConflict=null;
				for(BindCommand bind:getKeyBindListCopy()){
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
					bindCaptureToTarget.setKeyBind(kbCaptured);
					
					MessagesI.i().output(false, System.out, "Info", this, 
						"captured key bind "+kbCaptured.getBindCfg()
						+" for "+bindCaptureToTarget.getUserCommand(),
						bindCaptureToTarget,kbCaptured,this);
				}
				
				kbWaitBeReleased=kbCaptured;
			}
			
			return true;
		}
		
		return false;
	}
	
	public void update(float fTpf){
		if(updateCaptureKey())return;
		
		runActiveBinds();
	}
	
	private void runActiveBinds() {
		/**
		 * fill all binds for each action keycode to check which will win.
		 */
		hmKeyCodeVsActivatedBind.clear();
		for(BindCommand bc:tmbindList.values()){
			if(!bc.isCommandSet())continue; //not set yet
			
			if(bc.getKeyBind().isActivated()){ //pressed
				Integer iKeyCode = bc.getKeyBind().getActionKey().getKeyCode();
				
				ArrayList<BindCommand> abindForActKeyCode = hmKeyCodeVsActivatedBind.get(iKeyCode);
				if(abindForActKeyCode==null){
					abindForActKeyCode=new ArrayList<BindCommand>();
					hmKeyCodeVsActivatedBind.put(iKeyCode, abindForActKeyCode);
				}
				
				abindForActKeyCode.add(bc);
			}else{ //released
				bc.getKeyBind().reset(); //np if keep reseting, it should/is fast
//				runCommandOnKeyRelease(bc);
			}
		}
		
		/**
		 * only the activated bind with most modifiers will be run! 
		 */
		for(ArrayList<BindCommand> abindForActKeyCode:hmKeyCodeVsActivatedBind.values()){
			BindCommand bcWin=abindForActKeyCode.get(0);
			
			for(BindCommand bind:abindForActKeyCode){
				if(bcWin==bind)continue;
				if(bcWin.getKeyBind().getKeyModListSize() < bind.getKeyBind().getKeyModListSize()){
					bcWin=bind;
				}
			}
			
			runCommands(bcWin);
		}
	}
	
//	private void runCommandOnKeyRelease(BindCommand bc){
//		bc.getKeyBind().isCanBeRunNowOrReset();
//		if(true)return; //TODO enable on key release?
//		if(bc.getKeyBind().isWasAlreadyActivatedAtLeastOnce()){
//			runCommands(bc);
//		}
//	}
	
	private void runCommands(BindCommand bc){
		if(!bc.getKeyBind().isCanBeRunNow())return;
		
//		bc.getKeyBind().isActivated()
		bc.getHardCommand().call(); //TODO put on the queue?
		
		if(bc.getUserCommand()!=null){
			if(getFuncRunUserCommand()==null){
				MessagesI.i().warnMsg(this, "function to run user command is not set", this, bc);
			}else{
				getFuncRunUserCommand().apply(bc.getUserCommand());
			}
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
				if(!SystemAlertI.i().isShowingAlert(true)){
					asteAlertFrom = SystemAlertI.i().showSystemAlert(
							 " Press a key combination to be captured (where modifiers are ctrl, shift or alt).\n"
							+" More complex or specific keybindings can be set thru console commands.\n"
							+" Press ESC to cancel.\n"
							+"\n"
							+" Re-binding keys for command:\n"
							+"  "+bindCaptureToTarget.getCommandsInfo()+"\n"
							+" Current key bind: "+bindCaptureToTarget.getKeyBind().getBindCfg()+"\n",
							objLinkedGuiElement
						);
				}
				break;
			case Retry:
				kbCaptured=null;
				bindConflict=null;
				
				SystemAlertI.i().hideSystemAlert(asteAlertFrom,true);
				break;
			case KeyReleased:
				kbWaitBeReleased=null;
				break;
			case HasConflict:
				SystemAlertI.i().hideSystemAlert(asteAlertFrom);
				
				String strMsg = "captured key bind "+kbCaptured.getBindCfg()
					+" is already being used by "+bindConflict.getCommandsInfo()+"\n"
					+strRequestUserDecision;
				MessagesI.i().warnMsg(this,strMsg,	bindConflict,bindCaptureToTarget,kbCaptured,this);
				
				asteAlertFrom = SystemAlertI.i().showSystemAlert(strMsg,objLinkedGuiElement);
				
				kbCaptured=null;
				break;
			case Cancelled:
				kbCaptured=null;
				bindConflict=null;
				bindCaptureToTarget=null;
				kbWaitBeReleased=null;
				
				SystemAlertI.i().hideSystemAlert(asteAlertFrom);
				break;
			case Success:
				captureKeyStep(ECaptureUserDecision.Cancelled); //"recursive" in a sense
				
//				refreshOwnerAfterCapture.requestRefresh();
				recreateKeyBindFile();
				break;
		}
		
	}
	private void recreateKeyBindFile() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented");
	}

	public ArrayList<BindCommand> getKeyBindListCopy(){
		return new ArrayList<BindCommand>(tmbindList.values());
	}

//	public ArrayList<String> getReportAsCommands(StringCmdField scfBindKey, boolean bOnlyUserCustomOnes) {
	public ArrayList<String> getUserCommands() {
		ArrayList<String> astr = new ArrayList<String>();
		for(BindCommand bc:getKeyBindListCopy()){
			String strMapping = bc.getKeyBind().getBindCfg();
			if(strMapping.equals(bc.getKeyBind().getBindCfg())){
				strMapping="";
			}else{
				strMapping="#ActionMappingId("+strMapping+")";
			}
			
			String strUserCmd = bc.getUserCommand();
			
			astr.add(""
				+bc.getKeyBind().getBindCfg()+" "
				+(strUserCmd==null?"":strUserCmd+" ")
				+(bc.getHardCommand()==null?"":bc.getHardCommand().getName()+" ")
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

	public Function<String,Boolean> getFuncRunUserCommand() {
		return funcRunUserCommand;
	}

	public KeyBindCommandManagerI setFuncRunUserCommand(Function<String,Boolean> funcRunUserCommand) {
		assert(this.funcRunUserCommand==null);
		this.funcRunUserCommand = funcRunUserCommand;
		return this;
	}

}