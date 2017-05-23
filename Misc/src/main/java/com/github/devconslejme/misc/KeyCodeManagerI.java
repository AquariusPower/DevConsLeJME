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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.github.devconslejme.misc.jme.UserDataI.IUDKey;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class KeyCodeManagerI {
	public static KeyCodeManagerI i(){return GlobalManagerI.i().get(KeyCodeManagerI.class);}
	
	private ArrayList<Key> akeyList = new ArrayList<Key>();
	private TreeMap<String,Key> tmKey = new TreeMap<String,Key>(String.CASE_INSENSITIVE_ORDER);
	private TreeMap<Integer,HashMap<String,Key>> tmCodeKeys = new TreeMap<Integer,HashMap<String,Key>>();
	private String	strKeyIdPrefixFilter;
	private int	iKeyCodeForEscape=-1;
	private int	iKeyCodeForEnter=-1;
	private boolean	bConfigured;
	private int	iSpecialCodeStart=Integer.MAX_VALUE;
	private boolean	bDebug;
	
	public Key addKey(String strId, Integer... aiCodeToMonitor){
		ArrayList<Key> akey = new ArrayList<Key>();
		for(Integer iKeyCode:aiCodeToMonitor){
			akey.add(getFirstKeyForCode(iKeyCode));
		}
		
		return addKey(strId, akey.toArray(new Key[0]));
	}
	public Key addKey(String strId, Key... akeyToMonitor){
		return addKeyWorkFull(strId,null,akeyToMonitor);
	}
	
	public Key addKeyWorkFull(String strId, Integer iCode, Key... akeyToMonitor){
		return addKeyWorkFull(null, strId, iCode, akeyToMonitor);
	}
	/**
	 * 
	 * @param strPrefix can be null
	 * @param strId
	 * @param iCode
	 * @return true if added or already set with same code, false if already set with different code
	 */
	public Key addKeyWorkFull(String strPrefix, String strId, Integer iCode, Key... akeyToMonitor){
		if(iCode!=null && akeyToMonitor.length>0){
			throw new DetailedException("it can only have a key code or be a key group monitor, not both!", iCode, akeyToMonitor);
		}
		
//		strId=prepareFullKeyId(strId);
		
		Key keyExisting = tmKey.get(strId);
		if(keyExisting!=null){
			if(keyExisting.isKeyWithCode()){
				Integer iCodeExisting = keyExisting.getKeyCode();
				if(iCodeExisting==iCode){
					MessagesI.i().warnMsg(this,"already set", strId, iCode);
					return keyExisting;
				}else{
					MessagesI.i().warnMsg(this,"cannot modify the code for", strId, iCodeExisting, iCode);
					return null;
				}
			}
		}
		
		if(iCode!=null){
			for(Entry<String,Key> entry:tmKey.entrySet()){
				Key key = entry.getValue();
				if(!key.isKeyWithCode())continue;
				if(key.getKeyCode()==iCode){
					String strExistingId=entry.getKey();
					/**
					 * The source of ids and codes may contain more than one id with the same code.
					 * A new source (class) may also contain new ids for already setup codes.
					 */
					MessagesI.i().warnMsg(this,"(multiplicity) already contains code", strExistingId, iCode, strId);
				}
			}
			
			Key keyNew = new Key(strId,iCode);
			keyNew.prepareSimpleId(strPrefix);
			DetailedException.assertNotAlreadySet(tmKey.get(strId), keyNew, "key id", strId, iCode, this);
			tmKey.put(strId, keyNew);
			akeyList.add(keyNew);
			
			/**
			 * populate list of keys with the same code
			 */
			HashMap<String,Key> hm = tmCodeKeys.get(iCode);
			if(hm==null){
				hm=new HashMap<String,Key>();
				tmCodeKeys.put(iCode,hm);
			}
			hm.put(keyNew.getFullId(), keyNew);
			
			return keyNew;
		}else
		if(akeyToMonitor.length>0){
			if(keyExisting!=null && !keyExisting.isKeyGroupMonitor()){
				MessagesI.i().warnMsg(this,"existing is not a key group monitor", keyExisting);
				return null;
			}
			
			Key keyGroupMonitor = keyExisting;
			if(keyGroupMonitor==null){
				keyGroupMonitor = new Key(strId);
			}
			
			if(keyGroupMonitor.addKeysToMonitor(akeyToMonitor)){
				tmKey.put(strId, keyGroupMonitor);
			}else{
				MessagesI.i().warnMsg(this,"no key added to monitoring", strId, akeyToMonitor);
				return null;
			}
			
			return keyGroupMonitor;
		}else{
			throw new DetailedException("both are not set", iCode, akeyToMonitor);
		}
		
	}
	
	protected ArrayList<Key> getKeyList(){
		return akeyList;
	}
	public ArrayList<Key> getKeyListCopy(){
//		if(tmKey.size()!=akeyList.size())
		return new ArrayList<Key>(akeyList);
	}
	
//	public ArrayList<Key> getKeyListCopy() {
//		ArrayList<Key> akeyList = new ArrayList<Key>();
//		for(Entry<String,Key> entry:tmKey.entrySet()){
//			akeyList.add(entry.getValue());
//		}
//		return akeyList;
//	}
	
	public ArrayList<String> getKeyCodeListReport(){
		ArrayList<String> astr = new ArrayList<String>();
		for(Key key:getKeyListCopy()){
			astr.add(key.getFullId()+"="+key.getKeyCode());
		}
		Collections.sort(astr);
		return astr;
	}
	
	public Integer getKeyCodeFromId(String strId){
		Key key = tmKey.get(strId);
		if(key.isKeyWithCode())return key.getKeyCode();
		return null;
	}
	
	public String getKeyIdFromCode(int iCode){
		for(Entry<String,Key> entry:tmKey.entrySet()){
			Key key = entry.getValue();
			if(!key.isKeyWithCode())continue;
			if(key.getKeyCode()==iCode)return key.getFullId();
		}
		return null;
	}
	
	public Key refreshMouseButtonPressedState(int iButtonIndex, boolean bPressed){
		return refreshPressedState(strMouseTriggerKeyPrefix+iButtonIndex, bPressed);
	}
	public Key refreshPressedState(String strKeyId, boolean bPressed){
		return refreshState(strKeyId, bPressed, null);
	}
	public Key refreshState(String strKeyId, boolean bPressed, Float fValue){
		Key key=tmKey.get(strKeyId);
		key.setPressed(bPressed);
		if(fValue!=null)key.setAnalogValue(fValue);
		if(bDebug && bPressed && !key.isIgnoreKeyCode())System.out.println(strKeyId+","+bPressed+","+fValue);
		return key;
	}
	public Key refreshAnalogState(String strKeyId, float fValue){
//		return refreshPressedState(strKeyId, true).setAnalogValue(fValue);
		return refreshState(strKeyId, true, fValue);
	}
	
	public void refreshPressedState(int iKeyCode, boolean bPressed){
		for(Key key:tmCodeKeys.get(iKeyCode).values()){
			key.setPressed(bPressed);
		}
	}
	
	/**
	 * @param iKeyCode
	 * @return the first key (id) assigned to the specified keyCode
	 */
	public Key getFirstKeyForCode(Integer iKeyCode) {
		HashMap<String, Key> hm = tmCodeKeys.get(iKeyCode);
		if(hm==null)return null;
		
		return hm.entrySet().iterator().next().getValue();
	}

	public Key getKeyForId(String strId) {
		Key key = tmKey.get(strId);
		if(key==null && strKeyIdPrefixFilter!=null){
			key = tmKey.get(strKeyIdPrefixFilter+strId);
		}
		return key;
	}
	
//	public static enum EMouseTrigger implements IUDKey{
//		;
//
//		@Override
//		public Class getType() {
//			return Integer.class;
//		}
//
//		@Override
//		public String getUId() {
//			return JavaLangI.i().enumUId(this);
//		}
//		
//	}
	
	private void addSpecialKeys() {
//		// a mouse listener can be used to set these 
//		addKeyWorkFull("mouseWheelUp",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseWheelDown",	iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton0",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton1",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton2",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton3",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton4",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton5",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton6",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton7",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton8",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton9",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton10",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton11",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton12",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton13",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton14",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton15",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton16",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton17",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton18",		iSpecialCodeStart--);
//		addKeyWorkFull("mouseButton19",		iSpecialCodeStart--);
		prepareMouseAxisCodes();
		
		//TODO joystick
	}
	
	private String strAxisPrefix="mouseAxis";
	private String	strMouseTriggerKeyPrefix="mouseButton";
	public void prepareMouseAxisCodes(){
		addKeyWorkFull(strAxisPrefix+0+0+"_XLeft", iSpecialCodeStart--);
		addKeyWorkFull(strAxisPrefix+0+1+"_XRight", iSpecialCodeStart--);
		addKeyWorkFull(strAxisPrefix+1+0+"_YDown", iSpecialCodeStart--);
		addKeyWorkFull(strAxisPrefix+1+1+"_YUp", iSpecialCodeStart--);
		addKeyWorkFull(strAxisPrefix+2+0+"_WheelDown", iSpecialCodeStart--);
		addKeyWorkFull(strAxisPrefix+2+1+"_WheelUp", iSpecialCodeStart--);
	}
	public Key getMouseAxisKey(int iAxis, boolean bPositive){
		for(String k:tmKey.keySet()){
			if(k.startsWith(strAxisPrefix+iAxis+(bPositive?1:0))){
				return tmKey.get(k);
			}
		}
		return null;
	}
	
	public Key getMouseTriggerKey(int iButtonIndex){
		return getKeyForId(strMouseTriggerKeyPrefix+iButtonIndex);
	}
	
	public Key addMouseTriggerCode(int iButtonIndex){
		String strId=strMouseTriggerKeyPrefix+iButtonIndex;
		Key key=getKeyForId(strId);
		if(key!=null){
			throw new DetailedException("already set "+strId, key);
		}
		key=addKeyWorkFull(strId, iSpecialCodeStart--);
		return key;
	}
	
//	public Key getMouseButtonKey(int iButtonIndex){
//		return getKeyForId("mouseButton"+iButtonIndex);
//	}
	
	public void configure() {
  	addSpecialKeys();
	}
	
	public static enum EKeyMod{
		Ctrl,
		Alt,
		Shift,
		;
		
		private Key	key;
		
		public void setKey(Key key){
			DetailedException.assertNotAlreadySet(this.key, key, this);
			MessagesI.i().debugInfo(this, "setting key", this, key);
			this.key=key;
		}
		
		public Key getKey(){
			return key;
		}
		
		public String s(){return toString();}
		
		public static boolean isModKey(Key key){
			for(EKeyMod e:values()){
				if(e.key==key)return true;
				if(e.key.isMonitoredKey(key))return true;
			}
			return false;
		}
	}
	
	public KeyBind getPressedKeysAsKeyBind() {
		String strCfg = "";
		for(EKeyMod e:EKeyMod.values()){
			if(getKeyForId(e.s()).isPressed())strCfg+=e.s()+"+";
		}
		
		for(Key key:getKeyList()){
			if(key.isIgnoreKeyCode())continue; //ignore it
			if(EKeyMod.isModKey(key))continue;
			
			if(key.isPressed()){
				strCfg+=key.getSimpleId();
				return new KeyBind().setFromKeyCfg(strCfg);
			}
		}
		
		return null;
	}
	
	public ArrayList<Key> getAllPressedKeys(){
		ArrayList<Key> akey = new ArrayList<Key>();
		for(Key key:getKeyList()){
			if(key.isPressed()){
				akey.add(key);
			}
		}
		return akey;
	}
	public String getAllPressedKeysSimpleReport() {
		String str="";
		for(Key key:getAllPressedKeys()){
			if(!str.isEmpty())str+="+";
			str+=key.getSimpleId();
		}
		return str;
	}
	
	public String getKeyIdPrefixFilter() {
		return strKeyIdPrefixFilter;
	}
	public KeyCodeManagerI setKeyIdPrefixFilter(String strKeyIdPrefixFilter) {
		assert(this.strKeyIdPrefixFilter==null);
		this.strKeyIdPrefixFilter = strKeyIdPrefixFilter;
		return this;
	}
	public int getKeyCodeForEscape() {
		return iKeyCodeForEscape;
	}
	public KeyCodeManagerI setKeyCodeForEscape(int iKeyCodeForEscape) {
		this.iKeyCodeForEscape = iKeyCodeForEscape;
		return this;
	}
	public int getKeyCodeForEnter() {
		return iKeyCodeForEnter;
	}
	public KeyCodeManagerI setKeyCodeForEnter(int iKeyCodeForEnter) {
		this.iKeyCodeForEnter = iKeyCodeForEnter;
		return this;
	}
	public boolean isDebug() {
		return bDebug;
	}
	public KeyCodeManagerI setDebug(boolean bDebug) {
		this.bDebug = bDebug;
		return this; 
	}
	
}
