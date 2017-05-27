/* 
	Copyright (c) 2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
	
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
package com.github.devconslejme.misc.lemur;

import java.util.ArrayList;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.event.CursorButtonEvent;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MouseCursorButtonGlobalListenerDelegatorI {
	public static MouseCursorButtonGlobalListenerDelegatorI i(){return GlobalManagerI.i().get(MouseCursorButtonGlobalListenerDelegatorI.class);}

	/**
	 * must not be CursorListener to avoid being used elsewhere...
	 */
	public static interface IGlobalMouseCursorClickListener{
		public void clickEvent(CursorButtonEvent event, Spatial target, Spatial capture);
	}
	private ArrayList<IGlobalMouseCursorClickListener> aclxGlobal = new ArrayList<IGlobalMouseCursorClickListener>();
	/**
	 * cannot consume the event. mainly for auto focus
	 * @param clxGlobal
	 */
	public void addGlobalClickListener(IGlobalMouseCursorClickListener clxGlobal){
//		DetailedException.assertNotAlreadySet(this.clxGlobal, clxGlobal);
		if(!aclxGlobal.contains(clxGlobal))aclxGlobal.add(clxGlobal);
	}
	
	public void clickGlobalListeners(CursorButtonEvent event, Spatial target, Spatial capture) {
//		if(clxGlobal==null)return;
		for(IGlobalMouseCursorClickListener clx:aclxGlobal){
			boolean bWasConsumed=event.isConsumed();
			clx.clickEvent(event, target, capture);
			if(!bWasConsumed && event.isConsumed()){
				throw new DetailedException("must not consume the event!",clx,event,target,capture,this);
			}
		}
	}
	
}
