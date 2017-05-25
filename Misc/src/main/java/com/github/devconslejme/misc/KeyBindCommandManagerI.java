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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.function.Function;

import javax.swing.text.Position.Bias;

import com.github.devconslejme.misc.KeyBindCommandManagerI.BindCommand;
import com.github.devconslejme.misc.KeyCodeManagerI.KeyCodeManCompositeControl;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;


/**
 * intended to:
 * - dismiss conflicting key bindings, 
 * - to suspend all commands, 
 * - to allow custom key modifiers,
 * - and to help on capturing keys to change key bindings.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class KeyBindCommandManagerI {
	public static KeyBindCommandManagerI i(){return GlobalManagerI.i().get(KeyBindCommandManagerI.class);}
	
	private TreeMap<String,CallBoundKeyCmd> tmCmdIdVsCmd = new TreeMap<String, CallBoundKeyCmd>(String.CASE_INSENSITIVE_ORDER);
	private TreeMap<String,BindCommand> tmCfgVsBCmd = new TreeMap<String, BindCommand>(String.CASE_INSENSITIVE_ORDER);
	private HashMap<Integer,ArrayList<BindCommand>> hmKeyCodeVsActivatedBind = new HashMap<Integer, ArrayList<BindCommand>>();
	private BindCommand	bcCaptureToTarget;
	private KeyBind kbCaptured;
	private KeyBind kbWaitBeReleased;
	private StackTraceElement[]	asteAlertFrom;
	private BindCommand	bcConflict;
	private String strRequestUserDecision="Press ESC to cancel or Enter to retry.\n";
	private Object objLinkedGuiElement;
	private CallUserCustomCmd callcmdRunUserCommand;
	private ECaptureStep	eCaptureStep;
//	private String strCmdBindId="bind";
	private boolean	bDebug;
	private Key	keyNewBindHelper;
	private KeyBindCmdManCompositeControl ccSelf = new KeyBindCmdManCompositeControl();
	public static class KeyBindCmdManCompositeControl implements ICompositeRestrictedAccessControl{
		private KeyBindCmdManCompositeControl(){}
	}
	
	/**
	 * TODO tmp placeholder dummy based on old KeyBoundVarField
	 */
	public static class BindCommand{
		private ArrayList<CallBoundKeyCmd>	acxHardCommand = new ArrayList<CallBoundKeyCmd>();
		private KeyBind	kb;
//		private boolean	bReadOnly;
		private String	strUserCommand;
//		private String strUniqueId
//		private int	iCurrentIndex;
		
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
			return acxHardCommand.size()>0 || strUserCommand!=null;
		}
		
		@Override
		public String toString(){
			String str="";
			if(getKeyBind()==null)str+="[KeyBind not set]"; //essential have this
			if(getHardCommandsCount()==0 && getUserCmd()==null){ //only one of them set would suffice
				if(getHardCommandsCount()==0)str+="[HardCmd not set]";
				if(getUserCmd()==null)str+="[UserCmd not set]";
			}
			if(!str.isEmpty())return str;
			
			return KeyBindCommandManagerI.i().prepareConfigStr(this);
		}
		
//		public String getCommandsInfo(){
//			String str="";
//			
//			if(cxHardCommand!=null)str+=cxHardCommand.getName();
//			if(!str.isEmpty())str+=";";
//			
//			if(strUserCommand!=null)str+=strUserCommand;
//			
//			return str;
//		}
		
		public String getUserCmd() {
			return strUserCommand;
		}
		public CallBoundKeyCmd getHardCommand(int iIndex){
			return acxHardCommand.get(iIndex);
		}
		public CallBoundKeyCmd getCurrentHardCommand(){
			int i=kb.getMultiClickIndex();
			if(i>(acxHardCommand.size()-1))i=acxHardCommand.size()-1;
			return acxHardCommand.get(i);
		}
//		public BindCommand setCurrentHardCommand(int iIndex){
//			this.iCurrentIndex=iIndex;
//			return this;
//		}
		
		/**
		 * TODO syntax for executing on-press, on-release, b4 hard-command, after hard-command.
		 * @param strUserCommand can be null
		 */
		public void setUserCmd(String strUserCommand){
			this.strUserCommand=strUserCommand;
		}
		public BindCommand addHardCommand(CallBoundKeyCmd cx){
			assert(cx!=null);
			DetailedException.assertIsTrue("name set (a description is necessary as the command is hard coded)", !cx.getName().isEmpty(), cx, this);
//			assert !acxHardCommand.contains(cx);
			acxHardCommand.add(cx); //callcmds can be repeated, the same can be used as multiclick mode
			return this;
		}
		
		public String getInfo(boolean bKeybindPrepend) {
			CommandLineParser cl = KeyBindCommandManagerI.i().prepareConfig(this);
			
			String str="["+cl.getParsedParam(0)+"]";
			String strCmds="";
			if(cl.getParsedParam(1)!=null)strCmds+="\""+cl.getParsedParam(1)+"\""; //hard command
			if(cl.getParsedParam(2)!=null)strCmds+="\""+cl.getParsedParam(2)+"\""; //user custom js command
			if(bKeybindPrepend){
				str+=" -> ";
				str+=strCmds;
			}else{
				str=" -> "+str;
				str=strCmds+str;
			}
			
			return str;
		}

		public int getHardCommandsCount() {
			return acxHardCommand.size();
		}

		public String getHardCommandsStr() {
			String str="";
			for(CallBoundKeyCmd cx:acxHardCommand){
				if(!str.isEmpty())str+=",";
				str+=cx.getName();
			}
			return str;
		}
		
//		private boolean isReadOnly() {
//			return cxCommand!=null;
//		}

	}
	
	public void configure(){//String strCmdBindId){
//		this.strCmdBindId=strCmdBindId;
		
//		if(this.strCmdBindId==null){
//			MessagesI.i().warnMsg(this, "wont  be able to load/save key bind cfg file and other related functionalities", this);
//		}
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}.enableLoopMode());
		
		keyNewBindHelper = KeyCodeManagerI.i().createSpecialSimpleKey(strDummyTempAddNewBindHelperId);
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
//		BindCommand bc = tmCfgVsBCmd.get(strBindCfg);
//		bc.assertHardCommandNotSet(strBindCfg);
		BindCommand bc = tmCfgVsBCmd.remove(strBindCfg);
		MessagesI.i().output(false, System.out, "KeyBindCmd", this, "removed '"+strBindCfg+"': "+bc);
	}
	
	public static abstract class CallBoundKeyCmd extends CallableX<CallBoundKeyCmd>{
		public StackTraceElement[]	asteDbgConfiguredAt;
//		private BindCommand	bc;
		private boolean	bExpectingKeyReleasedOverriden;
//		private boolean	bIsPressed;
		private float	fAnalogValue;
		private int iClickCountIndex=0;
		
		/** This callcmd can be simultaneously called by many different key bindings.  */
		private ArrayList<KeyBind>	abcSimultaneouslyPressed = new ArrayList<>();
		private int	iMultiClicksAccepted = 1; //min is 1 of course
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("CallBoundKeyCmd [asteDbgConfiguredAt=");
			builder.append(Arrays.toString(asteDbgConfiguredAt));
			builder.append(", bExpectingKeyReleasedOverriden=");
			builder.append(bExpectingKeyReleasedOverriden);
			builder.append(", fAnalogValue=");
			builder.append(fAnalogValue);
			builder.append(", iClickCountIndex=");
			builder.append(iClickCountIndex);
			builder.append(", abcSimultaneouslyPressed=");
			builder.append(abcSimultaneouslyPressed);
			builder.append(", iMultiClicksAccepted=");
			builder.append(iMultiClicksAccepted);
			builder.append("]");
			return builder.toString();
		}

		public CallBoundKeyCmd setClickCountIndex(int iClickCountIndex) {
			this.iClickCountIndex=iClickCountIndex;
			return this;
		}
		
		public int getClickCountIndex(){
			return iClickCountIndex;
		}
		
//		private CallBoundKeyCmd setNameRestricted(String str) {
//			super.setName(str);
//			return this;
//		}
		
		@Override
		public CallBoundKeyCmd setName(String strName) {
//			if(strName.contains("*"))throw new DetailedException("'*' is restricted use/indicator of multiclick mode",strName,this);
//			return setNameRestricted(KeyBindCommandManagerI.i().validateHardCommandUId(strName));
			return super.setName(KeyBindCommandManagerI.i().validateHardCommandUId(strName));
		}
		
		@Deprecated
		@Override
		public Boolean call() {
			if(isPressed()){
				Boolean bRet = callOnKeyPressed(iClickCountIndex);
				if(bRet!=null)return bRet;
				
				bRet=callOnKeyStatus(true);
				if(bRet!=null)return bRet;
				
				bExpectingKeyReleasedOverriden=true;
			}
			return false; // in this case, it is expected that the on key released have been overriden and is returning not null
		}
		
//	public abstract Boolean callOnKeyPressed();
		public Boolean callOnKeyPressed(int iClickCountIndex){return null;}
		public Boolean callOnKeyReleased(int iClickCountIndex){return null;}
		
		/**
		 * use this one or the other two (pressed/released) not both kinds!
		 * @param bPressed
		 * @return
		 */
		public Boolean callOnKeyStatus(boolean bPressed){return null;}
		
		@Deprecated
		@Override
		public void callAfterRemovedFromQueue() {
			if(!isPressed()){
				Boolean bRet = callOnKeyReleased(iClickCountIndex);
				if(bRet!=null)return;
				
				bRet=callOnKeyStatus(false);
				if(bRet!=null)return;
				
				if(bExpectingKeyReleasedOverriden){
//					throw new DetailedException("neither 'on key pressed' nor 'on key released' methods were overriden!",this,asteConfiguredAt,bc);
					throw new DetailedException("neither 'on key pressed' nor 'on key released' methods were overriden!",this,asteDbgConfiguredAt,fAnalogValue);
				}
			}
		}
		
		/**
		 * mainly for clarification
		 * @return 
		 */
		public CallBoundKeyCmd holdKeyPressedForContinuousCmd(){
			super.enableLoopMode();
			return getThis();
		}
		
		/**
		 * see {@link #holdKeyPressedForContinuousCmd()}
		 */
		@Deprecated
		@Override
		public CallBoundKeyCmd enableLoopMode() {
			return super.enableLoopMode();
		}
		
//		public CallBoundKeyCmd setBindCommand(BindCommand bc) {
//			assert(this.bc==null);
//			this.bc = bc;
//			return getThis();
//		}
//		
//		public BindCommand getBindCommand() {
//			return bc;
//		}
		
		public float getAnalogValue(){
//			return bc.getKeyBind().getActionKey().getAnalogValue();
			return fAnalogValue;
		}
		
//		/**
//		 * This callcmd can be simultaneously called by many different key bindings.
//		 */
//		public boolean isSimultaneouslyPressedKeyBindsListEmpty(){
//			return abcSimultaneouslyPressed.isEmpty();
//		}
		
		public boolean isPressed(){
////			return getBindCommand().getKeyBind().getActionKey().isPressed();
//			/**
//			 * it is the mods+actionKey that matters and is controlled/setup here
//			 */
//			return getBindCommand().getKeyBind().isActivated();
//			return bIsPressed;
//			return abcSimultaneouslyPressed.size()>0;
			return !abcSimultaneouslyPressed.isEmpty();
		}

		private void updateCurrentStatus(KeyBind kbLastPressed) {
			if(kbLastPressed.isActivated()){
				if(kbLastPressed.getActivationCount()==1){
					assert !abcSimultaneouslyPressed.contains(kbLastPressed) : "lacking consistency, should have been removed"; //TODO is such precision useful? other parts of the code may be improved keeping this check tho.
					
					/**
					 * only at the 1st activation (if being hold will ignore subsequent ones)
					 */
//					if(!abcSimultaneouslyPressed.contains(kbLastPressed)){
						abcSimultaneouslyPressed.add(kbLastPressed);
//					}
//					setClickCountIndex(kbLastPressed.getMultiClickIndex());
				}
			}else{
				abcSimultaneouslyPressed.remove(kbLastPressed);
			}
			
			if(abcSimultaneouslyPressed.isEmpty()){
//				this.bIsPressed=false;
				this.fAnalogValue=0f; //reset
			}else{
				if(abcSimultaneouslyPressed.get(abcSimultaneouslyPressed.size()-1)==kbLastPressed){
//					this.bIsPressed = kbLastPressed.isActivated();
					this.fAnalogValue = kbLastPressed.getActionKey().getAnalogValue();
					setClickCountIndex(kbLastPressed.getMultiClickIndex());
				}
			}
		}

		public CallBoundKeyCmd setMaxMultiClicksAccepted(int iCallcmdAcceptsMultiClicksCount) {
			this.iMultiClicksAccepted=iCallcmdAcceptsMultiClicksCount;
			return this;
		}
		
		public int getMaxMultiClicksAccepted(){
			return iMultiClicksAccepted;
		}

//		public void applyMultiClickName(int iCallcmdAcceptsMultiClicksCount) {
//			setNameRestricted(getName()+"*"+iCallcmdAcceptsMultiClicksCount);
//		}

//		/**
//		 * This callcmd can be simultaneously called by two different key bindings.
//		 * Only the last one pressed, on release, must trigger the removal of this callcmd from the queue. 
//		 * @param bc
//		 * @return
//		 */
//		public boolean isLastPressed(BindCommand bc) {
//			return bcLastPressed==bc;
//		}
	}
	
	/**
	 * use this method if the callcmd accepts more than 1 click
	 * @param strKeyBindCfg
	 * @param iCallcmdAcceptsMultiClicksCount
	 * @param cx
	 */
	public void putBindCommandsLater(String strKeyBindCfg, int iCallcmdAcceptsMultiClicksCount, CallBoundKeyCmd cx){
		assert iCallcmdAcceptsMultiClicksCount>=1; //min is 1 tho, will not be a problem...
//		cx.applyMultiClickName(iCallcmdAcceptsMultiClicksCount);
		cx.setMaxMultiClicksAccepted(iCallcmdAcceptsMultiClicksCount);
//		putCommand(cx);
		
		CallBoundKeyCmd[] acx = new CallBoundKeyCmd[iCallcmdAcceptsMultiClicksCount];
		for(int i=0;i<iCallcmdAcceptsMultiClicksCount;i++)acx[i]=cx;
		putBindCommandsLater(strKeyBindCfg, true, acx);
//		putBindCommands(strKeyBindCfg,createIdList(acx));//astr.toArray(new String[0]));
	}
	
	private void putBindCommandsLater(String strKeyBindCfg, boolean bOneCmdIsMultiClickMode, CallBoundKeyCmd... acx){
		if(bOneCmdIsMultiClickMode){
			putCommand(acx[0]); //put only the first that is equal to the others
		}else{
			putCommands(acx);
		}
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(KeyCodeManagerI.i().getKeyListCopy().size()==0)return false;
				putBindCommands(strKeyBindCfg,createIdList(acx));//astr.toArray(new String[0]));
				return true;
			}
		});
	}
	
	/**
	 * see {@link #putBindCommand(String, String, CallableX)}
	 * @param strKeyBindCfg
	 * @param strName
	 * @param cx
	 * @return
	 */
	public void putBindCommandsLater(String strKeyBindCfg, CallBoundKeyCmd... acx){
		putBindCommandsLater(strKeyBindCfg,false,acx);
//		putCommands(acx);
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				if(KeyCodeManagerI.i().getKeyListCopy().size()==0)return false;
//				putBindCommands(strKeyBindCfg,createIdList(acx));//astr.toArray(new String[0]));
//				return true;
//			}
//		});
	}
	
	protected void putCommands(CallBoundKeyCmd[] acx) {
		for(CallBoundKeyCmd cx:acx){
//			if(tmCmdIdVsCmd.get(cx.getName())==null)
			putCommand(cx);
		}
	}
	protected String[] createIdList(CallBoundKeyCmd[] acx) {
		ArrayList<String> astr=new ArrayList<String>();
		for(CallBoundKeyCmd cx:acx){
			astr.add(cx.getName());
		}
		return astr.toArray(new String[0]);
	}
	
	/*********
	 * The only unique thing (that must not conflict) is the key binding cfg.
	 * Different key bindings can call/execute the same commands tho!
	 * 
	 * @param strKeyBindCfg see {@link KeyBind#setFromKeyCfg(String)}
	 * @param callcmd
	 * @return
	 */
//	public BindCommand putBindCommand(String strKeyBindCfg, String strName, CallBoundKeyCmd callcmd){
//		BindCommand bc = new BindCommand()
//			.setKeyBind(new KeyBind().setFromKeyCfg(strKeyBindCfg))
//			.addHardCommand(callcmd.setName(strName));
//		return putBindCommand(bc);
//	}
	public BindCommand putBindCommands(String strKeyBindCfg, String... astrNames){
		BindCommand bc = new BindCommand().setKeyBind(new KeyBind().setFromKeyCfg(strKeyBindCfg));
		for(String strName:astrNames)bc.addHardCommand(tmCmdIdVsCmd.get(strName));
		return putBindCommand(bc);
	}
	
	public CallBoundKeyCmd putCommand(CallBoundKeyCmd callcmd){
		assert !callcmd.getName().isEmpty();
		CallBoundKeyCmd callcmdExisting = tmCmdIdVsCmd.get(callcmd.getName());
		if(callcmdExisting!=null){
			throw new DetailedException("id already used: "+callcmd.getName(),callcmdExisting,callcmd);
		}
		tmCmdIdVsCmd.put(callcmd.getName(), callcmd);
		return callcmd;
	}
	
	/**
	 * 
	 * @param bc
	 * @return previously set one if any
	 */
	public BindCommand putBindCommand(BindCommand bc){
		String strHashMapKeyAsBindCfg=bc.getKeyBind().getBindCfg(); //this bind cfg must be a uniquely granted recreation of the former setup
		
		BindCommand bcExisting = tmCfgVsBCmd.get(strHashMapKeyAsBindCfg);
		if(bcExisting!=null){
			if(bcExisting!=bc){
				throw new DetailedException("keybind conflict",this,bcExisting,bc);
			}else{
				throw new DetailedException("already set",this,bc);
			}
		}
		
		for(CallBoundKeyCmd callcmd:bc.acxHardCommand){
			CallBoundKeyCmd callcmdExisting = tmCmdIdVsCmd.get(callcmd.getName());
			if(callcmdExisting==null){
				putCommand(callcmd);
			}else{
				//consistency
				if(callcmd!=callcmdExisting){
					throw new DetailedException("cmd ids must be unique",callcmdExisting,callcmd);
				}
			}
		}
		
		if(bc.getHardCommandsCount()==0 && bc.getUserCmd()==null){
			throw new DetailedException("hardcmds or a user cmd must be set",bc);
		}
		
		return tmCfgVsBCmd.put(strHashMapKeyAsBindCfg,bc);
	}
	
	public String[] validateHardCommandUId(String[] astrUId) throws IllegalArgumentException{
		for(String str:astrUId){
			validateHardCommandUId(str);
		}
		return astrUId;
	}
	/**
	 * Allows a lot of flexibility and readability.
	 * But also helps to forbid characters that could be on a javascript code ex.: "." "(" ";"
	 * making it easier to distinguish.
	 * btw, "*" char is restricted use/indicator.
	 */
	public String validateHardCommandUId(String strUId) throws IllegalArgumentException{
		if(strUId.trim().isEmpty()){
			throw new DetailedException("empty id "+strUId); //to prevent messy ids
		}
		
		if(strUId.trim().length()!=strUId.length()){
			throw new DetailedException("is not trimmed "+strUId); //to prevent messy ids
		}
		
//		if(!strUId.matches("[0-9A-Za-z_ *+-]*")){ //regex: "-" must be the last thing within []
		if(!strUId.matches("[0-9A-Za-z_ +-]*")){ //regex: "-" must be the last thing within []
			throw new DetailedException("invalid hard command unique id "+strUId);
		}
		
		return strUId;
	}
	
	public ArrayList<String> getHardCommandsIdListCopy(){
		return new ArrayList<String>(tmCmdIdVsCmd.keySet());
	}
	
	private boolean isWaitKeyRelease(){
		/**
		 * this also prevents executing the bind's target command just after capturing its bind!
		 */
		if(kbWaitBeReleased!=null){
			if(kbWaitBeReleased.isActivated()){ // KeyCodeManagerI.i().getAllPressedKeys()
				//still pressed (with modifiers)
				MessagesI.i().output(false, System.out, "Info", this, "waiting captured key bind to be released: "+kbWaitBeReleased.getBindCfg());
				return true; 
			}else
			if(kbWaitBeReleased.getActionKey().isPressed()){ 
				//still pressed (only the action key is still being holded)
				MessagesI.i().output(false, System.out, "Info", this, "waiting captured action key to be released: "+kbWaitBeReleased.getBindCfg());
				return true;
			}else
			{ //released
				if(bcCaptureToTarget!=null && bcCaptureToTarget.getKeyBind()==kbWaitBeReleased){
					captureKeyStep(ECaptureStep.Success);
				}else{
					captureKeyStep(ECaptureStep.KeyReleased);
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
		
		if(bcCaptureToTarget!=null){
//			if(!SystemAlertI.i().isAlertReady())return true; //wait the alert be ready before doing other things
			
			SystemAlertI.i().setDynamicInfo(KeyCodeManagerI.i().getAllPressedKeysSimpleReport());
			
			kbCaptured=KeyCodeManagerI.i().getPressedKeysAsKeyBind();
			
			if(bcConflict!=null){
				ECaptureStep eud=null;
				if(isCapturedThisKeyCodeWithoutMods(KeyCodeManagerI.i().getKeyCodeForEscape())){
					eud=ECaptureStep.Cancelled;
				}else
				if(isCapturedThisKeyCodeWithoutMods(KeyCodeManagerI.i().getKeyCodeForEnter())){
					eud=ECaptureStep.Retry;
				}
				
				if(eud!=null){
					kbWaitBeReleased=kbCaptured;
					captureKeyStep(eud);
				}
				
				return true;
			}
			
			if(kbCaptured==null){
				captureKeyStep(ECaptureStep.KeepTrying);
			}else{
				if(isCapturedThisKeyCodeWithoutMods(KeyCodeManagerI.i().getKeyCodeForEscape())){
					captureKeyStep(ECaptureStep.Cancelled);
					return true;
				}
				
				bcConflict=null;
				for(BindCommand bind:getKeyBindListCopy()){
					if(bind==bcCaptureToTarget)continue;
					if(bind.getKeyBind().isEquivalentTo(kbCaptured)){
						bcConflict=bind;
						break;
					}
				}
				
				if(bcConflict!=null){
					captureKeyStep(ECaptureStep.HasConflict);
					return true;
				}else{
					boolean bAdd=isNewBindHelper(bcCaptureToTarget);
					bcCaptureToTarget.setKeyBind(kbCaptured);
					if(bAdd)putBindCommand(bcCaptureToTarget);
					
					MessagesI.i().output(false, System.out, "InfoKeyBind", this,
						prepareConfigStr(bcCaptureToTarget));
//					MessagesI.i().output(false, System.out, "Info", this, 
//						"captured key bind "+kbCaptured.getBindCfg()
//						+" for "+bcCaptureToTarget.getUserCommand(),
//						bcCaptureToTarget,kbCaptured,this);
				}
				
				kbWaitBeReleased=kbCaptured;
			}
			
			return true;
		}
		
		return false;
	}
	
	public void applyNewKeyBindHelperAt(BindCommand bc){
		bc.setKeyBind(new KeyBind());
		bc.getKeyBind().setActionKey(keyNewBindHelper);
	}
	
	private String	strDummyTempAddNewBindHelperId = "_DummyTempAddNewBindHelper_";
//	public Key getNewBindHelperKey(){
//		return KeyCodeManagerI.i().getKeyForId(strDummyTempAddNewBindHelperId);
//	}

	public boolean isNewBindHelper(BindCommand bc) {
		return bc.getKeyBind().getActionKey()==keyNewBindHelper;
	}

	public void update(float fTpf){
		if(eCaptureStep==ECaptureStep.KeepTrying){
			if(!SystemAlertI.i().isAlertReady())return; //wait the alert be ready before doing other things
		}
		if(updateCaptureKey())return;
		if(SystemAlertI.i().isShowingAlert())return;
		
		runActiveBinds();
	}
	
	/**
	 * for key released, see {@link CallBoundKeyCmd} new methods
	 */
	private void runActiveBinds() {
		/**
		 * fill all binds for each action keycode to check which will win.
		 */
		hmKeyCodeVsActivatedBind.clear();
		for(BindCommand bcChkActivated:tmCfgVsBCmd.values()){
			if(!bcChkActivated.isCommandSet())continue; //not set yet
			
			if(bcChkActivated.getKeyBind().isActivated()){ //pressed
				Integer iKeyCode = bcChkActivated.getKeyBind().getActionKey().getKeyCode();
				
				ArrayList<BindCommand> abindForActKeyCode = hmKeyCodeVsActivatedBind.get(iKeyCode);
				if(abindForActKeyCode==null){
					abindForActKeyCode=new ArrayList<BindCommand>();
					hmKeyCodeVsActivatedBind.put(iKeyCode, abindForActKeyCode);
				}
				
				abindForActKeyCode.add(bcChkActivated);
			}else{ 
				/****************************
				 * KEY RELEASED EVENT (pressed=false)
				 * will happen, when the callable is removed from the queue
				 ****************************/
				reset(bcChkActivated);
			}
		}
		
		/**
		 * only the activated bind with most modifiers will be run! 
		 */
		for(ArrayList<BindCommand> abindForActKeyCode:hmKeyCodeVsActivatedBind.values()){
			BindCommand bcWin=abindForActKeyCode.get(0);
			
			for(BindCommand bcChkMostMods:abindForActKeyCode){
				if(bcWin==bcChkMostMods)continue;
				if(bcWin.getKeyBind().getKeyModListSize() < bcChkMostMods.getKeyBind().getKeyModListSize()){
					bcWin=bcChkMostMods;
				}
			}
			
			runCommandsOf(bcWin);
		}
	}
	
	private void updateHardCommandStatus(BindCommand bc){
		/**
		 * This will update the status related to the currently being used key binding,
		 * as there can have many key bindings for the same command.
		 */
		bc.getCurrentHardCommand().updateCurrentStatus(bc.getKeyBind());
//			bc.getKeyBind(),
//			bc.getKeyBind().isActivated(), //set if is pressed 
//			bc.getKeyBind().getActionKey().getAnalogValue()
//		);
	}
	
	private void reset(BindCommand bc) {
		if(bc.getKeyBind().isResetted())return;
		
		if(bc.getHardCommandsCount()>0){
			updateHardCommandStatus(bc);
			
//			if(bc.getHardCommand().isLastPressed(bc)){
//			if(bc.getHardCommand().isSimultaneouslyPressedKeyBindsListEmpty()){
			if(!bc.getCurrentHardCommand().isPressed()){
				QueueI.i().forceRemoveFromQueue(bc.getCurrentHardCommand());
//				updateHardCommandStatus(bc);
			}
		}
		
		if(bc.getUserCmd()!=null){
			QueueI.i().forceRemoveFromQueue(getCallRunUserCommand());
		}
//		QueueI.i().removeLoopFromQueue(bc.getHardCommand()); //bc.getHardCommand().justRemoveFromQueueOnce();
		
		bc.getKeyBind().reset();
	}

	private void runCommandsOf(BindCommand bc){
//		if(!bc.getKeyBind().isActivated())return;
		
		bc.getKeyBind().incHoldedActivationCount(ccSelf);
		
		if(bc.getKeyBind().getActivationCount()==1){
			if(bc.getHardCommandsCount()>0){
				if(isDebug()){
					System.out.println("Enqueue:RunHardCmd:"
						+bc.getCurrentHardCommand().getName()
						+",clicks="+(bc.getCurrentHardCommand().getClickCountIndex()+1)
					);
				}
				QueueI.i().enqueue(bc.getCurrentHardCommand());
			}
		
			/**
			 * TODO review custom user command? it is quite limited (lacking features) compared to the queued hardcommand call... allow user to configure every command may be using options on the very user command to set at the CallableX? :D
			 */
			if(bc.getUserCmd()!=null){
				if(getCallRunUserCommand()==null){
					MessagesI.i().warnMsg(this, "function to run user command is not set", this, bc); //TODO create a warnOnce at messagesi?
				}else{
					if(isDebug())System.out.println("Enqueue:RunJSCmd:"+bc.getUserCmd());
					QueueI.i().enqueue(getCallRunUserCommand().setCmd(bc.getUserCmd()));
				}
			}
		}
		
		updateHardCommandStatus(bc);
	}
	
	public CallUserCustomCmd getCallRunUserCommand() {
		return callcmdRunUserCommand;
	}
	
	public static abstract class CallUserCustomCmd extends CallableX<CallUserCustomCmd>{
		private String strCmd;
		
		/**
		 * use {@link #execUserCustomCmd(String)} instead
		 */
		@Deprecated
		@Override
		public Boolean call() {
			assert strCmd!=null && !strCmd.isEmpty();
			execUserCustomCmd(strCmd);
			return true;
		}
		
		public abstract Boolean execUserCustomCmd(String strCmd);

		public CallUserCustomCmd setCmd(String strCmd) {
			this.strCmd = strCmd;
			return this; 
		}
	}
	
	public KeyBindCommandManagerI setCallRunUserCommand(CallUserCustomCmd funcRunUserCommand) {
		assert(this.callcmdRunUserCommand==null);
		this.callcmdRunUserCommand = funcRunUserCommand;
		return this;
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

	enum ECaptureStep{
		Cancelled, EndReset, //same thing
		Retry,
		Success, 
		
		/** wait user decision */
		HasConflict, 
		
		KeyReleased, 
		
		KeepTrying,
	}
	
	private void captureKeyStep(ECaptureStep e) {
		eCaptureStep=e;
		switch(e){
			case KeepTrying:
				if(!SystemAlertI.i().isShowingAlert()){
					asteAlertFrom = SystemAlertI.i().showSystemAlert(
							 " Press a key combination to be captured (where modifiers are ctrl, shift or alt).\n"
							+" More complex or specific keybindings can be set thru console commands.\n"
							+" (Restricted keys will be ignored)\n"
							+" Press ESC to cancel.\n"
							+"\n"
							+" Re-binding keys for command:\n"
							+"  "+bcCaptureToTarget+"\n"
							+" Current key bind: "+bcCaptureToTarget.getKeyBind().getBindCfg()+"\n",
							objLinkedGuiElement
						);
				}
				break;
			case Retry:
				kbCaptured=null;
				bcConflict=null;
				
				SystemAlertI.i().hideSystemAlert(asteAlertFrom,true);
				break;
			case KeyReleased:
				kbWaitBeReleased=null;
				break;
			case HasConflict:
				SystemAlertI.i().hideSystemAlert(asteAlertFrom);
				
				String strMsg = "captured key bind "+kbCaptured.getBindCfg()
					+" is already being used by '"+bcConflict+"'\n"
					+strRequestUserDecision;
				MessagesI.i().warnMsg(this,strMsg,	bcConflict,bcCaptureToTarget,kbCaptured,this);
				
				asteAlertFrom = SystemAlertI.i().showSystemAlert(strMsg,objLinkedGuiElement);
				
				kbCaptured=null;
				break;
			case EndReset: //same thing
			case Cancelled:
				kbCaptured=null;
				bcConflict=null;
				bcCaptureToTarget=null;
				kbWaitBeReleased=null;
				
				SystemAlertI.i().hideSystemAlert(asteAlertFrom);
				break;
			case Success:
				captureKeyStep(ECaptureStep.EndReset); //"recursive" in a sense
				
//				refreshOwnerAfterCapture.requestRefresh();
				recreateKeyBindFile();
				break;
		}
		
	}
	private void recreateKeyBindFile() {
		File flCfg = FileI.i().createNewFile(this, "cfg", true);
		flCfg.delete();
		for(BindCommand bc:getKeyBindListCopy()){
			FileI.i().appendLine(flCfg, prepareConfigStr(bc));
		}
	}
	
//	public void loadConfigParamsOnly(String strParams) {
//		loadConfig(getCmdBindId()+" "+strParams);
//	}
	/**
	 * accepts:
	 * <bindCmdId> "<hardCmdId>" "[JSuserCmd]"
	 * <bindCmdId> "<hardCmdId>,<hardCmdId>,<hardCmdId>,..." "[JSuserCmd]"
	 * <bindCmdId> null <JSuserCmd>
	 * <bindCmdId> "<JSuserCmd>"
	 * 
	 * @param strCommandLine 
	 * @return
	 */
	public void loadConfig(String strCommandLine, boolean bPrependCmdBindId, boolean bRebindReplacing){
		if(bPrependCmdBindId){
			strCommandLine=getCmdBindId()+" "+strCommandLine;
		}
		
		CommandLineParser cl = new CommandLineParser(strCommandLine);
		if(!cl.getCommand().equals(getCmdBindId()))throw new DetailedException("invalid main bind cmd",strCommandLine,getCmdBindId());
		
		String strKeyBindCfgId = cl.getParsedParam(0);
		
		String[] astrHardCommandUId=null;
		String strJSUserCmd=null;
		String strChk = cl.getParsedParam(1);
		String[] astrChk = strChk.split(",");
		try{
			validateHardCommandUId(astrChk);
			astrHardCommandUId=astrChk;
		}catch(DetailedException ex){
			strJSUserCmd=strChk;
		}
		if(strJSUserCmd==null)strJSUserCmd = cl.getParsedParam(2);
		
		if(astrHardCommandUId==null && strJSUserCmd==null){
			throw new DetailedException("no command specified",strCommandLine);
		}
		
		KeyBind kb = new KeyBind().setFromKeyCfg(strKeyBindCfgId);
		BindCommand bcExistingToRemove = tmCfgVsBCmd.get(kb.getBindCfg());
		if(!bRebindReplacing && bcExistingToRemove!=null){
			throw new DetailedException("bindcmd already set",bcExistingToRemove,kb,strCommandLine);
		}
		
		BindCommand bc = new BindCommand();
		bc.setKeyBind(kb);
		bc.setUserCmd(strJSUserCmd);
		
		if(astrHardCommandUId!=null){
			String[] astrHardCommandUIdLazy=astrHardCommandUId;
			QueueI.i().enqueue(new CallableXAnon() {@Override	public Boolean call() {
				for(String strHardCmd:astrHardCommandUIdLazy){
					if(tmCmdIdVsCmd.get(strHardCmd)==null)return false; //wait all cmds be available
				}
				
				for(String strHardCmd:astrHardCommandUIdLazy){
					bc.addHardCommand(tmCmdIdVsCmd.get(strHardCmd));
					if(bcExistingToRemove!=null)removeKeyBind(bcExistingToRemove);
					putBindCommand(bc);
				}
				
				return true;
			}});
		}
		
	}
	
	public String prepareConfigStr(BindCommand bc) {
		return prepareConfig(bc).recreateCommandLine();
	}
	public CommandLineParser prepareConfig(BindCommand bc) {
		KeyBind kb = bc.getKeyBind();
		if(kb==null)throw new DetailedException("has no key bind",bc);
		
		CommandLineParser cl = new CommandLineParser();
		cl.setCommand(getCmdBindId());
		cl.appendParams(kb.getBindCfg());
		if(bc.getHardCommandsCount()>0){
			cl.appendParams(bc.getHardCommandsStr());
		}
		String strJSCmd=bc.getUserCmd();
		if(strJSCmd!=null){
			cl.appendParams(strJSCmd);
		}
		
		return cl;
	}

	private String getCmdBindId() {
		return "bind";
	}

	public ArrayList<BindCommand> getKeyBindListCopy(){
		return new ArrayList<BindCommand>(tmCfgVsBCmd.values());
	}

//	public ArrayList<String> getReportAsCommands(StringCmdField scfBindKey, boolean bOnlyUserCustomOnes) {
	public ArrayList<String> getBindAndCmdList() {
		ArrayList<String> astr = new ArrayList<String>();
		for(BindCommand bc:getKeyBindListCopy()){
			astr.add(prepareConfigStr(bc));
		}
		
		return astr;
	}

//	public void captureAndSetKeyBindAt(BindCommand bindTarget, IRefresh refreshOwner, Object objLinkedGuiElement) {
	public void captureAndSetKeyBindAt(BindCommand bindTarget, Object objLinkedGuiElement) {
		if(this.bcCaptureToTarget!=null){
			MessagesI.i().warnMsg(this, "already capturing keybind for", this.bcCaptureToTarget, this);
			return;
		}
		
		this.bcCaptureToTarget=bindTarget;
		this.objLinkedGuiElement=objLinkedGuiElement;
	}

	public boolean isCapturing(){
		return bcCaptureToTarget!=null;
	}

	public boolean isDebug() {
		return bDebug;
	}

	public KeyBindCommandManagerI setDebug(boolean bDebug) {
		this.bDebug = bDebug;
		return this; 
	}

	public BindCommand applyCommandAt(BindCommand bc, String strCmdId) {
		bc.addHardCommand(tmCmdIdVsCmd.get(strCmdId));
		return bc;
	}

	public String getHardCommandInfo(String strCmdId) {
		CallBoundKeyCmd callcmd = tmCmdIdVsCmd.get(strCmdId);
		int i = callcmd.getMaxMultiClicksAccepted();
		return callcmd.getName()+(i>1 ? " (accepts "+i+" clicks)" : "");
	}


//	public String getCmdBindId() {
//		return strCmdBindId;
//	}
//
//	public KeyBindCommandManagerI setCmdBindId(String strCmdBindId) {
//		this.strCmdBindId = strCmdBindId;
//		return this; 
//	}
}