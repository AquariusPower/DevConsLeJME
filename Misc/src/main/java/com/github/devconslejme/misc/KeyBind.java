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

import com.github.devconslejme.misc.KeyBindCommandManagerI.BindCommand;

/**
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class KeyBind {
	
	/** the last/main key to be pressed */
	private Key keyAction = null;
	private ArrayList<Key> akeyModifierList;// = new ArrayList<Key>();
//	private long lLastActivationMilis=-1;
	private long	lActivationCount = 0;
	
	/**
	 * TODO see {@link KeyBindCommandManagerI#runCommandOnKeyRelease(BindCommand)}, the 'on pressed' can already be limited to a single activation, what would have the same meaning/usage. would it still be interesging to allow 'on released' activation mode anyway or is just pointletss and could still complexify/break something?
	 * 
	 * @return
	 */
	public boolean isActivated(){
		if(!keyAction.isPressed())return false;
		
		if(akeyModifierList!=null){
			for(Key keyMod:akeyModifierList){
				if(!keyMod.isPressed())return false;
				if(keyAction.getLastPressedNano()<=keyMod.getLastPressedNano())return false; //action key must be the last one
			}			
		}
		
//		lActivationCount++;
		return true;
	}
	
	public Key getActionKey(){
		return keyAction;
	}
	
	public void addModifier(String... astrKeyId){
		for(String strId:astrKeyId){
			addModifier(KeyCodeManagerI.i().getKeyForId(strId));
		}
	}
	public void addModifier(int... aiKeyCode){
		for(Integer iKeyCode:aiKeyCode){
			addModifier(KeyCodeManagerI.i().getFirstKeyForCode(iKeyCode));
		}
	}
	public void addModifier(Key... akey){
//		akeyModifierList.addAll(Arrays.asList(akey));
		if(akeyModifierList==null)akeyModifierList = new ArrayList<Key>();
		
		for(Key key:akey){
			DetailedException.assertNotNull(key, "mod key", this);
			akeyModifierList.add(key);
		}
	}
	
	public ArrayList<Key> getKeyModifiersCopy(){
		if(akeyModifierList==null)return null;
		return new ArrayList<Key>(akeyModifierList);
	}
	
	public void setActionKey(Key key) {
		DetailedException.assertNotAlreadySet(this.keyAction, key, "action key", this);
		DetailedException.assertNotNull(key, "action key", this);
		
		this.keyAction=key;
	}
	
	public void setActionKey(String strId) {
		setActionKey(KeyCodeManagerI.i().getKeyForId(strId));
	}
	public void setActionKey(int iKeyCode) {
		setActionKey(KeyCodeManagerI.i().getFirstKeyForCode(iKeyCode));
	}
	
	/**
	 * last one is action key
	 * @return
	 */
	public Integer[] getAllKeyCodes(){
		if(akeyModifierList==null){
			return new Integer[]{keyAction.getKeyCode()};
		}
		
		Integer[] ai = new Integer[akeyModifierList.size()+1]; 
		
		int i=0;
		for(Key key:akeyModifierList){
			ai[i++]=key.getKeyCode();
		}
		ai[i]=keyAction.getKeyCode(); //last
		
		return ai;
	}
	
	/**
	 * last one is action key
	 * @return 
	 * @return
	 */
	public KeyBind setFromKeyCodes(Integer... ai){
		setActionKey(KeyCodeManagerI.i().getFirstKeyForCode(ai[ai.length-1])); //last
		
		for(int i=0;i<ai.length-1;i++){ //least last
			addModifier(ai[i]);
		}
		
		return this;
	}

	public KeyBind setFromKeyIds(String[] astr) {
		setActionKey(KeyCodeManagerI.i().getKeyForId(astr[astr.length-1])); //last
		
		for(int i=0;i<astr.length-1;i++){ //least last
			addModifier(astr[i]);
		}
		
		return this;
	}

	public String getBindCfg(){
		String str="";
		
		if(akeyModifierList!=null){
			for(Key key:akeyModifierList){
				String strId=key.getSimpleId();
				str+=strId+"+";
	//			str+=ManageKeyCode.i().getKeyIdFromCode(key.getKeyCode())+"+";
			}
		}
		
		str+=getActionKey().getSimpleId(); //ManageKeyCode.i().getKeyIdFromCode(getActionKey().getKeyCode());
		
		return str;
	}

	public boolean isHasKeyModifiers() {
		return akeyModifierList!=null;
	}
	
	public int getKeyModListSize(){
		return akeyModifierList!=null ? akeyModifierList.size() : 0;
	}
	
	/**
	 * expected syntax ex:
	 * LCONTROL+RSHIFT+LMENU+F1 (specific mod keys)
	 * Ctrl+Alt+k (grouped mod keys)
	 * 
	 * @param strCfg
	 * @return
	 */
	public KeyBind setFromKeyCfg(String strCfg) {
		return setFromKeyIds(strCfg.split("[+]"));
	}

//	public boolean isCanBeRunNow() {
//		if(isActivated()){
//			lLastActivationMilis = SimulationTimeI.i().getMillis();
//			return true;
//		}
//		
//		return false;
//	}
//	
//	public void reset(){
//		lLastActivationMilis=-1;
//	}
	
//	/**
//	 * was already activated at least once
//	 * @return
//	 */
////	public boolean isWaitingDeactivation(){
//	public boolean isWasAlreadyActivatedAtLeastOnce(){
//		return lLastActivationMilis!=-1;
//	}
	
//	public Comparator<KeyBind> getComparatorById(){
//		return new Comparator<KeyBind>() {
//			@Override
//			public int compare(KeyBind o1, KeyBind o2) {
//				return o1.get;
//			}
//		};
//	}
	
	public boolean isEquivalentTo(KeyBind kbOther) {
		if(getActionKey().getFullId().equals(kbOther.getActionKey().getFullId())){
			if(getKeyModListSize()==kbOther.getKeyModListSize()){
				if(getKeyModListSize()==0)return true;
				
				ArrayList<Key> akeyModOther = kbOther.getKeyModifiersCopy();
				for(Key keyMod:getKeyModifiersCopy()){
					if(akeyModOther.contains(keyMod))akeyModOther.remove(keyMod);
					if(akeyModOther.size()==0)return true;
				}
			}
		}
		
		return false;
	}

	public boolean isResetted() {
		return lActivationCount==0;
	}

	public void reset() {
		lActivationCount=0;
	}

	public void incActivationCount() {
		lActivationCount++;
	}

	public long getActivationCount() {
		return lActivationCount;
	}
	
}
