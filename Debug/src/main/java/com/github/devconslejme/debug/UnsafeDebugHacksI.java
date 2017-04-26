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

package com.github.devconslejme.debug;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.lwjgl.opengl.XRandR;
import org.lwjgl.opengl.XRandR.Screen;

import com.github.devconslejme.misc.DetailedException.IHandleExceptions;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;

/**
 * This class helps on reading and modifying values, and calling methods of other classes unsafely.<br>
 * This can help on debugging, making specific tests and even providing temporary workarounds.<br>
 * <br>
 * Tho, to leave it working/active on a released application may be a bad idea:<br>
 * Because you may not know all the consequences such changes (calling/modifying private/protected methods/fields) may have, even after testing all flows.<br>
 * Because when updating the target library/class, new changes will be oblivious to you and possibly incompatible, and may break things unpredictably.<br>
 * <br>
 * That said... with caution... have fun! :)<br>
 * <br>
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class UnsafeDebugHacksI {
	public static UnsafeDebugHacksI i(){return GlobalManagerI.i().get(UnsafeDebugHacksI.class);}
	
	private boolean bAllowHacks = false;
	private IHandleExceptions	ihe;
	private boolean	bConfigured;
	
	public static class SimpleHandleExceptions implements IHandleExceptions{
		@Override
		public void handleExceptionThreaded(Exception e) {
			MessagesI.i().output(true, System.err, "DebugHacksException", this, e.getMessage(), e);
		}
	}
	
	/**
	 * 
	 * @param ihe can be null
	 */
	public void setHandleExceptions(IHandleExceptions ihe){
		if(bConfigured)throw new NullPointerException("already configured."); // KEEP ON TOP
		
		this.ihe=ihe!=null?ihe:new SimpleHandleExceptions();
		
		bConfigured=true;
	}

	/**
	 * 
	 * @param clazzOfObjectFrom what superclass of the object from is to be used? if null, will use owner.class()
	 * @param objFieldOwner if null, clazz must be set (will be refering to a static field then)
	 * @param strFieldName this method will break if it gets changed by lib developers...
	 * @param fldOverride use this or strFieldName
	 * @param bSetValue
	 * @param objSetNewValue
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T> T getOrSetFieldValueHK(Class<?> clazzOfObjectFrom, Object objFieldOwner, String strFieldName, Class<T> clReturnType, Field fldOverride, boolean bSetValue, Object objSetNewValue){
		if(!bAllowHacks)return null;
		
		if(clazzOfObjectFrom==null)clazzOfObjectFrom=objFieldOwner.getClass();
		
		Object objFieldValue = null;
		try{
			Field fld = fldOverride==null ? clazzOfObjectFrom.getDeclaredField(strFieldName) : fldOverride;
			
			boolean b = fld.isAccessible();
			if(!b)fld.setAccessible(true);
			
			objFieldValue = fld.get(objFieldOwner);
			
			if(bSetValue)fld.set(objFieldOwner, objSetNewValue);
			
			if(!b)fld.setAccessible(false);
		} catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
			ihe.handleExceptionThreaded(e);
		}
		
		return (T)objFieldValue;
	}
	
	/**
	 * We shouldnt access private fields/methods as things may break.
	 * Any code depending on this must ALWAYS be optional by having an alternative functionality!
	 * 
	 * @param objInvoker
	 * @param strMethodName
	 * @param aobjParams
	 */
	public Object callMethodHK(Object objInvoker, String strMethodName, Object... aobjParams) {
		if(!bAllowHacks)return null;
		
		Object objReturn = null;
		try {
			Method m = objInvoker.getClass().getDeclaredMethod(strMethodName);
			boolean bWasAccessible=m.isAccessible();
			if(!bWasAccessible)m.setAccessible(true);
			objReturn = m.invoke(objInvoker);
			if(!bWasAccessible)m.setAccessible(false);
		} catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			ihe.handleExceptionThreaded(e);
		}
		return objReturn;
	}
	
	/**
	 * In Linux, even if application is windowed, on exiting {@link XRandR} may try to restore the resolution but may do it wrongly, without considering the X viewport.
	 * This workaround works as upon restoring the screen resolution, {@link XRandR} may ignore non-standard X configuration viewportin/viewportout.
	 * A new LWJGL lib version may have it fixed already tho.
	 * 
	 * WARNING: Hacks may break when libraries get updated.
	 * 
	 * @return erased value 
	 */
	public Screen[] hackXRandRpreventResolutionRestore(){
		Screen[] a = UnsafeDebugHacksI.i().getOrSetFieldValueHK(XRandR.class, null, "savedConfiguration", Screen[].class, null, true, null);
		return a;
	}
	
	public boolean isAllowHacks() {
		return bAllowHacks;
	}

	public void setAllowHacks(boolean bAllowHacks) {
		this.bAllowHacks = bAllowHacks;
	}

}
