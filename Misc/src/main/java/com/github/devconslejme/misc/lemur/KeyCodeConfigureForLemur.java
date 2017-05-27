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
package com.github.devconslejme.misc.lemur;

import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.lemur.MouseCursorButtonGlobalListenerDelegatorI.IGlobalMouseCursorClickListener;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.event.CursorButtonEvent;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class KeyCodeConfigureForLemur {
	
	public static class GlobalListenerX implements IGlobalMouseCursorClickListener{
		@Override
		public void clickEvent(CursorButtonEvent event, Spatial target,				Spatial capture) {
			/**
			 * lemur seems to not let mouse cursor buttons to be forwared to JME other core listeners 
			 * so it has to be done here.
			 * TODO confirm that? if this could be avoided by letting the JME action listener at KeyCodeConfigureForJme do all the work? 
			 */
			KeyCodeManagerI.i().refreshMouseButtonPressedState(event.getButtonIndex(), event.isPressed());
		}
	}
	private GlobalListenerX glx = new GlobalListenerX();

	public void configure() {
		MouseCursorButtonGlobalListenerDelegatorI.i().addGlobalClickListener(glx);
	}
	
}
