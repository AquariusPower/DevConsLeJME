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

/**
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class Key{
	private String strFullId;
	private String strSimpleId;
	private Integer iKeyCode = null;
	private boolean bPressed = false;
	private long lLastPressedNano=-1;
	private long lLastReleasedNano=-1;
	private ArrayList<Key> akeyMonitoredList = null;
	
//		private Key(Integer iKeyCode) {
//			this(ManageKeyCodeI.i().getKeyId(iKeyCode), iKeyCode);
//		}
	
	/**
	 * Must remain private, the scope of available keys is limited and must be managed here!
	 * @param strId
	 * @param iKeyCode
	 */
	public Key(String strId, Integer iKeyCode) {
		this(strId);
		assert(AssertionsI.i().restrictedCaller(KeyCodeManagerI.class, 1));
		
		DetailedException.assertNotNull(iKeyCode, "code", this);
		DetailedException.assertIsTrue("invalid negative keycode", iKeyCode>=0, this);
		
		this.iKeyCode=iKeyCode;
	}
	
	/**
	 * Even end user could set this.
	 * @param strId
	 * @param akeyToMonitor if any of these keys is pressed, this "key group reference" will return as pressed
	 */
	public Key(String strId, Key... akeyToMonitor) {
		this(strId);
		assert(AssertionsI.i().restrictedCaller(KeyCodeManagerI.class, 1));
		
		DetailedException.assertNotNull(akeyToMonitor, "keys to monitor", this);
		DetailedException.assertIsTrue("keys to monitor list has items", akeyToMonitor.length>0, this);
		
		addKeysToMonitor(akeyToMonitor);
	}
	
	private Key(String strId){
		DetailedException.assertNotEmpty("id", strId, this);
		
		this.strFullId=strId;
		prepareSimpleId();
		
		//will now be uniquely prefixed
//			this.strFullId=GlobalOSAppI.i().getCmdConsLibFullId()+"_"+ManageKeyCode.class.getSimpleName()+"_"+this.strFullId;
//			this.strFullId=GlobalManageKeyCodeI.i().prepareFullKeyId(this.strFullId);
	}
	
	public ArrayList<Key> getKeysToMonitorCopy(){
		return new ArrayList<Key>(akeyMonitoredList);
	}
	
	public boolean isModeKeyGroupMonitor(){
		return akeyMonitoredList!=null;
	}
	public boolean isModeKeyWithCode(){
		return iKeyCode!=null;
	}
	
	public boolean addKeysToMonitor(Key... akeyToMonitor){
		return workKeysMonitor(true,akeyToMonitor);
	}
	public boolean removeKeysFromMonitor(Key... akeyToMonitor){
		return workKeysMonitor(false,akeyToMonitor);
	}
	private boolean workKeysMonitor(boolean bAdd, Key... akeyToMonitor){
		if(iKeyCode!=null){
			throw new DetailedException("this key is NOT a group reference!", this, iKeyCode, akeyToMonitor);
		}
		
		if(this.akeyMonitoredList==null)this.akeyMonitoredList = new ArrayList<Key>();
		
		int iCount=0;
		for(Key key:akeyToMonitor.clone()){
			if(key==null)continue;
			
			if(bAdd){
				if(this.akeyMonitoredList.contains(key)){
					MessagesI.i().warnMsg(this,"already contains", key);
					continue;
				}
				
				this.akeyMonitoredList.add(key);
				iCount++;
			}else{
				this.akeyMonitoredList.remove(key);
				iCount++;
			}
		}
		
		return iCount>0;
	}
	
	public Integer getKeyCode() {
		return iKeyCode;
	}

	public boolean isPressed() {
		if(akeyMonitoredList!=null){
			for(Key key:akeyMonitoredList){
				if(key.isPressed())return true;
			}
			return false;
		}
		
		return bPressed;
	}
	protected void setPressed(boolean bPressed) {
		assert(AssertionsI.i().restrictedCaller(KeyCodeManagerI.class, 1));
		if(akeyMonitoredList!=null)throw new DetailedException("this key is a group reference, cannot be directly pressed...", this, akeyMonitoredList, bPressed);
		
		if(bPressed){
			lLastPressedNano=System.nanoTime();
		}else{
			lLastReleasedNano=System.nanoTime();
		}
		
		this.bPressed = bPressed;
	}
	
	private void prepareSimpleId(){
		this.strSimpleId = strFullId;
		
		String strPrefix=KeyCodeManagerI.i().getKeyIdPrefixFilter();
		if(strPrefix!=null){
			if(strSimpleId.startsWith(strPrefix)){
				strSimpleId=strSimpleId.substring(strPrefix.length());
			}
		}
	}
	
	public String getSimpleId(){
		return strSimpleId;
//			String strIdSimple = strFullId;
//			
//			String strPrefix=GlobalManageKeyCodeI.i().getKeyIdPrefixFilter();
//			if(strPrefix!=null){
//				if(strIdSimple.startsWith(strPrefix)){
//					strIdSimple=strIdSimple.substring(strPrefix.length());
//				}
//			}
//			
//			return strIdSimple;
	}
	
	public String getFullId() {
		return strFullId;
	}

	public boolean isMonitoredKey(Key key) {
		return akeyMonitoredList.contains(key);
	}

	public long getLastPressedNano() {
		return lLastPressedNano;
	}

//	public void setLastPressedNano(long lLastPressedNano) {
//		this.lLastPressedNano = lLastPressedNano;
//		return this;
//	}

	public long getLastReleasedNano() {
		return lLastReleasedNano;
	}

//	public void setLastReleasedNano(long lLastReleasedNano) {
//		this.lLastReleasedNano = lLastReleasedNano;
//		return this;
//	}
}
