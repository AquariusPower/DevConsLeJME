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

import com.github.devconslejme.misc.KeyCodeManagerI.KeyCodeManCompositeControl;

/**
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class Key{
	private Integer iKeyCode = null;
	private ArrayList<Key> akeyMonitoredList = null;
	
	private boolean bIgnored=false;
	private String strFullId;
	private String strSimpleId;
	private boolean bPressed = false;
	private long lLastPressedNano=-1;
	private long lLastReleasedNano=-1;
	private long	lLastPressedFrameId;
	private float	fValue;
	private ICompositeRestrictedAccessControl	ccIsSpecialExternalContextKey;
	
//		private Key(Integer iKeyCode) {
//			this(ManageKeyCodeI.i().getKeyId(iKeyCode), iKeyCode);
//		}
	
	/**
	 * The scope of available keys is limited and must be managed only by {@link KeyCodeManagerI}
	 * @param strId
	 * @param iKeyCode
	 */
	public Key(KeyCodeManCompositeControl cc, String strId, Integer iKeyCode) {
		this(strId);
		assert cc!=null;
//		assert(AssertionsI.i().restrictedCaller(KeyCodeManagerI.class, 1));
		
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
		
		if(akeyToMonitor.length>0)addKeysToMonitor(akeyToMonitor);
//		DetailedException.assertNotNull(akeyToMonitor, "keys to monitor", this);
//		DetailedException.assertIsTrue("currkeys to monitor already has items", this.akeyMonitoredList.length>0, this);
//		addKeysToMonitor(akeyToMonitor);
	}
	
	private Key(String strId){
		StringI.i().validateUId(strId);
		
		this.strFullId=strId;
		this.strSimpleId=this.strFullId;
	}
	
	public ArrayList<Key> getKeysToMonitorCopy(){
		assert iKeyCode==null : "if it is a keys-monitor, it cannot have a keycode ";
		return new ArrayList<Key>(akeyMonitoredList);
	}
	
	public boolean isKeyGroupMonitor(){
		return akeyMonitoredList!=null;
	}
	public boolean isKeyWithCode(){
		return iKeyCode!=null;
	}
	
	public boolean addKeysToMonitor(Key... akeyToMonitor){
		assert(iKeyCode==null);
		return workKeysMonitor(true,akeyToMonitor);
	}
	public boolean removeKeysFromMonitor(Key... akeyToMonitor){
		assert(iKeyCode==null);
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
		assert akeyMonitoredList==null : "if it has a keycode, it cannot have be a keys-monitor";
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
		
		if(isIgnoreKeyCode())return;
		
		if(akeyMonitoredList!=null)throw new DetailedException("this key is a group reference, cannot be directly pressed...", this, akeyMonitoredList, bPressed);
		
		/**
		 * user actions are realtime, not simulation one
		 */
		if(bPressed){
			lLastPressedNano=System.nanoTime();
			lLastPressedFrameId=HWEnvironmentI.i().getTotalFrameCount();
		}else{
			lLastReleasedNano=System.nanoTime();
		}
		
		this.bPressed = bPressed;
		
		if(!bPressed)fValue=0f;
	}
	
	/**
	 * 
	 * @param strPrefix can be null
	 * @return simpleid differs from fullid
	 */
	public boolean prepareSimpleId(String strPrefix){
		assert(this.strSimpleId==this.strFullId);
//		assert(this.strSimpleId==null);
		
//		String strPrefix=KeyCodeManagerI.i().getKeyIdPrefixFilter();
		if(strPrefix!=null){
			if(strFullId.startsWith(strPrefix)){
				strSimpleId=strFullId.substring(strPrefix.length());
				return true;
			}
		}
		
		return false;
	}
	
//	public boolean isHasSimpleId(){
//		return strSimpleId!=null;
//	}
	
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

	public long getLastReleasedNano() {
		return lLastReleasedNano;
	}
	
	public String getAsInfo(){
		return getSimpleId()+(akeyMonitoredList!=null?":"+akeyMonitoredList.toString():"");
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Key [strFullId=");
		builder.append(strFullId);
		builder.append(", strSimpleId=");
		builder.append(strSimpleId);
		builder.append(", iKeyCode=");
		builder.append(iKeyCode);
		builder.append(", bPressed=");
		builder.append(bPressed);
		builder.append(", lLastPressedNano=");
		builder.append(lLastPressedNano);
		builder.append(", lLastReleasedNano=");
		builder.append(lLastReleasedNano);
		builder.append(", akeyMonitoredList=");
		builder.append(akeyMonitoredList);
		builder.append("]");
		return builder.toString();
	}

	public long getLastPressedFrameId() {
		return lLastPressedFrameId;
	}

	public boolean isIgnoreKeyCode() {
		return bIgnored;
	}
	
	/**
	 * ignore a key code to be exclusively used/accessible outside this key binding context,
	 * so nothing will happen (in this context) while pressing it.
	 * @return
	 */
	public Key setIgnoreKeyCode() {
		assert iKeyCode!=null : "can only ignore a key code";
		this.bIgnored = true;
		return this; 
	}

	public Key setAnalogValue(float fValue) {
		this.fValue=fValue;
		return this;
	}
	
	public float getAnalogValue() {
		return fValue;
	}

	public void setPressedSpecialExternalContextKeyMode(ICompositeRestrictedAccessControl cc,boolean bPressed) {
		assert cc==ccIsSpecialExternalContextKey;
//		assert ccIsSpecialExternalContextKey;
		
//		if(ccIsSpecialExternalContextKey){
			this.bPressed=bPressed;
//		}
	}
	
	public boolean isSpecialExternalContextKeyMode(){
		return ccIsSpecialExternalContextKey!=null;
	}
	
	public void setSpecialExternalContextKeyMode(ICompositeRestrictedAccessControl cc) {
		assert this.ccIsSpecialExternalContextKey==null;
		this.ccIsSpecialExternalContextKey=cc;
	}

	public String composeCfgPrependModifiers(Key... akeyMod) {
		String str="";
		for(Key k:akeyMod){
			str+=k.getFullId()+"+";
		}
		return str+=getFullId();
	}
}
