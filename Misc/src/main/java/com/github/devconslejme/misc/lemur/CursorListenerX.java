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

import com.jme3.scene.Spatial;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;

/**
 * to easify
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public abstract class CursorListenerX implements CursorListener{

	private Spatial	sptPrepareToWorkWith;

	protected abstract boolean click(CursorButtonEvent event, Spatial target,				Spatial capture);
	
	@Override
	public void cursorButtonEvent(CursorButtonEvent event, Spatial target,			Spatial capture) {
		if(event.isConsumed())return;
		
		if(event.isPressed()){
			this.sptPrepareToWorkWith = capture;
		}else{
			if(sptPrepareToWorkWith!=null && sptPrepareToWorkWith==capture){
				if(!DragParentestPanelListenerI.i().isDragOverridingButtonUpClickEvent(event)){
					if(click(event, target, capture)){
						event.setConsumed();
					}
				}
			}
			
			this.sptPrepareToWorkWith=null;
		}
	}

	@Override
	public void cursorEntered(CursorMotionEvent event, Spatial target,			Spatial capture) {
	}

	@Override
	public void cursorExited(CursorMotionEvent event, Spatial target,			Spatial capture) {
	}

	@Override
	public void cursorMoved(CursorMotionEvent event, Spatial target,			Spatial capture) {
	}
	
}
