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
package com.github.devconslejme.misc.jme;

import java.lang.reflect.Field;

import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.Key;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.KeyCodeManagerI.EKeyMod;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.app.Application;
import com.jme3.input.InputManager;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.AnalogListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.input.controls.MouseAxisTrigger;
import com.jme3.input.controls.MouseButtonTrigger;

/**
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class KeyCodeConfigureForJme {//implements AnalogListener,ActionListener{
	AnalogListener anlAxis;
	ActionListener aclTriggers;
	
//	private ActionListener	alGeneralJmeTriggerListener;
//	private boolean	bCaptureKeyModifiersMode;
//	private InputManager	inputman;
//	private ActionListener	alGeneralJmeAnalogListener;
	private Key[]	akAxis;
	
	public void configure(int iMaxMouseButtons) {
//		if(true)return; //TODO remove this line
		
//  	KeyCodeManagerI.i().configure();
  	
		KeyCodeManagerI.i().setKeyCodeForEnter(KeyInput.KEY_RETURN);
		KeyCodeManagerI.i().setKeyCodeForEscape(KeyInput.KEY_ESCAPE);
  	fillKeyIdCode();
  	addSpecialKeys();
  	
  	// JME listener/mapping
//  	inputman = GlobalManagerI.i().get(Application.class).getInputManager();
  	
//		alGeneralJmeTriggerListener = new ActionListener() {@Override	public void onAction(String strKeyId, boolean bPressed, float tpf) {
//			KeyCodeManagerI.i().refreshPressedState(strKeyId, bPressed);	
//		}};
//		alGeneralJmeAnalogListener = new ActionListener() {@Override	public void onAction(String strKeyId, boolean bPressed, float tpf) {
//			KeyCodeManagerI.i().refreshPressedState(strKeyId, bPressed);
//			pseudoReleaseAxisKey(strKeyId,bPressed);
//		}}; 
		
  	aclTriggers = new ActionListener() {
  		/**
  		 * TPF ignored as the {@link CallableX#getTPF()} will be available when then
  		 * related bound command (for the specified keys combination) is called.  
  		 */
  		@Override
  		public void onAction(String strKeyId, boolean bPressed, float fTPF) {
  			KeyCodeManagerI.i().refreshPressedState(strKeyId, bPressed);
  		}
		};
		
		anlAxis = new AnalogListener() {
			/**
			 * see the onAction() one for more info
			 */
			@Override
			public void onAnalog(String strKeyId, float fValue, float fTPF) {
				KeyCodeManagerI.i().refreshAnalogState(strKeyId, fValue);
			}
		};
  	
		// add all possible/compatible keyboard keys as input manager/mapping/trigger
		for(Key key:KeyCodeManagerI.i().getKeyListCopy()){
			if(key.isKeyGroupMonitor())continue;
			
			if(key.getKeyCode()<=255){ //keytrigger limit TODO JME's only? or is a default to all keyboards?
				String strMapping=key.getFullId();
				
				AppI.i().addKeyMappingAndListener(strMapping,new KeyTrigger(key.getKeyCode()),aclTriggers);
//				if(!inputman.hasMapping(strMapping)){
//					inputman.addMapping(strMapping, new KeyTrigger(key.getKeyCode()));
//				}
//				
//				/**
//				 * if the "keycode id" mapping already existed, it will just add a listener to it!
//				 */
//				inputman.addListener(aclTriggers, strMapping);
			}
		}
		
		// mouse buttons
		for(int i=0;i<iMaxMouseButtons;i++){
			Key key = KeyCodeManagerI.i().addMouseTriggerCode(i);
			String strId=key.getFullId();
			AppI.i().addKeyMappingAndListener(strId,new MouseButtonTrigger(i),aclTriggers);
//			inputman.addMapping(strId, new MouseButtonTrigger(i));
//			inputman.addListener(aclTriggers,strId);
		}
		
		// mouse axes
		akAxis = new Key[3*2];
		int iCount=0;
		for(int iAxis=0;iAxis<3;iAxis++){
			for(int iPositive=0;iPositive<2;iPositive++){
				boolean bPositive=iPositive==1;
				Key key=KeyCodeManagerI.i().getMouseAxisKey(iAxis,bPositive);
				akAxis[iCount++]=key;
				String strId=key.getFullId();
				AppI.i().addKeyMappingAndListener(strId,new MouseAxisTrigger(iAxis,!bPositive),anlAxis);
//		    inputman.addMapping(strId, new MouseAxisTrigger(iAxis,!bPositive));
//		    inputman.addListener(anlAxis,strId);
//				inputman.addListener(alGeneralJmeAnalogListener,strId);
			}
		}
		
		initUpdateLoop();
		
		// TODO joystick
	}
	
	protected void initUpdateLoop() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				simulateAxisKeyReleased();
				
				HWEnvironmentJmeI.i().putCustomInfo("ActiveContext(s)", KeyCodeManagerI.i().getContextsStatusReport());
				
				return true;
			}
		}).enableLoopMode();
	}

	//	@Workaround
//	private void pseudoAxisKeyUpdatePressed(String strKeyId,boolean bPressed){
//		if(!bPressed)return;
//		
//		for(Key key:akAxis){
//			if(key.getFullId().equals(strKeyId)){
//				key.lPseudoPressedLastFrameId=EnvironmentJmeI.i().getCurrentFrameId();
//			}
//		}
//	}
	/**
	 * the listener only receives the "pressed" event, not the "released"
	 * as being an axis there it no actual trigger (neither press), but... on mouse stop
	 * moving on that axis direction, could be a "relase" event... :(
	 * @param strKeyId
	 */
	@Workaround
	private void simulateAxisKeyReleased(){
//		if(true)return;//TODO revieweing
		for(Key key:akAxis){
//			kax.getLastPressedFrameId()=EnvironmentJmeI.i().getCurrentFrameId();
		
			/**
			 * must be on next frame so the "key press" is detected/forwarded by/thru {@link KeyCodeManagerI} 
			 * on the frame it happened and can be "holded" (non stop moving)
			 */
			if( key.getLastPressedFrameId() == HWEnvironmentJmeI.i().getCurrentFrameId()   )continue;
			/**
			 * wait a '1 frame gap' (a lacking "pressed event" for the axis that updates the frame id) 
			 */
			if( key.getLastPressedFrameId() == HWEnvironmentJmeI.i().getCurrentFrameId()-1 )continue;
			
			if(key.isPressed())KeyCodeManagerI.i().refreshState(key.getFullId(), false, 0f);
		}
	}
//	public static class PseudoKeyAxis{
//		public long	lPseudoPressedLastFrameId;
//		Key key;
//		CallableX cxPseudoRelease = new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				if( lPseudoPressedLastFrameId >= EnvironmentJmeI.i().getFrameId(-1) )return false; //wait a gap (a lacking "pressed event" for the axis)
//				KeyCodeManagerI.i().refreshPressedState(key.getFullId(), false);
//				return true;
//			}
//		};
//	}
	
	/** 
	 * TODO should this be allowed to be called only once? other classes without conflicts would be no problem tho...
	 * @param iKeyCodeForEscape to setup a generic cancel fail-safe key
	 * @param iKeyCodeForReturn to setup a generic accept fail-safe key
	 * @param cl
	 * @param strKeyIdPrefixFilter can be null
	 * @return
	 */
//	public boolean fillKeyIdCodeFrom(int iKeyCodeForEscape, int iKeyCodeForReturn, Class<?> cl, String strKeyIdPrefixFilter){
	private boolean fillKeyIdCode(){
		Class<?> cl = KeyInput.class;
		String strKeyIdPrefixFilter="KEY_";
//		this.strKeyIdPrefixFilter=strKeyIdPrefixFilter;
//		this.iKeyCodeForEscape=iKeyCodeForEscape;
//		this.iKeyCodeForEnter=iKeyCodeForReturn;
		
		KeyCodeManagerI.i().setKeyIdPrefixFilter(strKeyIdPrefixFilter);
		
//		if(tmIdCode.size()>0){			return;		}
		try {
			int iMaxCode=-1;
			for(Field fld:cl.getFields()){
				if(strKeyIdPrefixFilter!=null && !strKeyIdPrefixFilter.isEmpty()){
					if(!fld.getName().startsWith(strKeyIdPrefixFilter))continue;
				}
				
				int iCode=(Integer)fld.get(null);
				if(iCode>iMaxCode)iMaxCode=iCode;
				
				String strId=fld.getName();//.substring(4); //removes the KEY_ prefix
				
				if(KeyCodeManagerI.i().addKeyWorkFull(strKeyIdPrefixFilter,strId,iCode)==null){
					throw new DetailedException("keycode filling failed",strId,iCode,cl,strKeyIdPrefixFilter);
				}
			}
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new DetailedException("unexpected").setCauseAndReturnSelf(e);
		}
		
		return true;
	}

	/**
	 * here are mergers, id shorteners etc
	 * all are key monitors tho.
	 */
	public void addSpecialKeys() {
//		super.addSpecialKeys();
		
		EKeyMod.Ctrl .setKey(KeyCodeManagerI.i().addKey(EKeyMod.Ctrl.s(),	KeyInput.KEY_LCONTROL,KeyInput.KEY_RCONTROL));
		EKeyMod.Alt  .setKey(KeyCodeManagerI.i().addKey(EKeyMod.Alt.s(),	KeyInput.KEY_LMENU,		KeyInput.KEY_RMENU));
		EKeyMod.Shift.setKey(KeyCodeManagerI.i().addKey(EKeyMod.Shift.s(),KeyInput.KEY_LSHIFT,	KeyInput.KEY_RSHIFT));
		
		KeyCodeManagerI.i().addKey("Enter", KeyInput.KEY_RETURN, KeyInput.KEY_NUMPADENTER); //merger
		
		KeyCodeManagerI.i().addKey("ESC", KeyInput.KEY_ESCAPE); //short sinonym
		
		//other sinonyms
		KeyCodeManagerI.i().addKey("LAlt", KeyInput.KEY_LMENU);
		KeyCodeManagerI.i().addKey("RAlt", KeyInput.KEY_RMENU);
		KeyCodeManagerI.i().addKey("CapsLock", KeyInput.KEY_CAPITAL);
		KeyCodeManagerI.i().addKey("ScrollLock", KeyInput.KEY_SCROLL);
	}


//	private void addKeyCodeMapping(Key key){
//		assert key.isKeyWithCode();
//		
//		String strMapping=key.getFullId();
//		
//		if(key.getKeyCode()<=255){ //keytrigger limit TODO JME's only? or is a default to all keyboards?
//			if(!inputman.hasMapping(strMapping)){
//				inputman.addMapping(strMapping, new KeyTrigger(key.getKeyCode()));
//			}
//			/**
//			 * if the "keycode id" mapping already existed, it will just add a listener to it!
//			 */
//			inputman.addListener(aclTriggers, strMapping);
//		}else{
//			throw new DetailedException("not supported keycode by input manager/mapping/trigger",key);
////			MessagesI.i().warnMsg(this, "still not supported", key);
//		}
//	}

	/**
	 * @DevSelfNote Deprecated! keep as reference/info/reason to prevent reimplementation...
	 * This would needlessly remove the keycode mappings for other already set before here.
	 * The listener can be removed from all mappings if it becomes necessary...
	 */
	@Deprecated
	private void removeKeyCodeMaping(Key key){
		String strMapping=key.getFullId();
		/*******************************************************
		 **************************** KEEP *********************
		if(inputman.hasMapping(strMapping)){
			MessagesI.i().warnMsg(this,"removing already existing keycode mapping", strMapping,key);
			inputman.deleteMapping(strMapping);
		}
		*/
	}

//	/**
//	 * TPF ignored as the {@link CallableX#getTPF()} will be available when then
//	 * related bound command (for the specified keys combination) is called.  
//	 */
//	@Override
//	public void onAction(String strKeyId, boolean bPressed, float fTPF) {
//		KeyCodeManagerI.i().refreshPressedState(strKeyId, bPressed);
//	}
	
//	/**
//	 * see {@link #onAction(String, boolean, float)}
//	 */
//	@Override
//	public void onAnalog(String strKeyId, float fValue, float fTPF) {
//		KeyCodeManagerI.i().refreshAnalogState(strKeyId, fValue);
//	}
}
