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

package com.github.devconslejme.misc;

import com.jme3.bounding.BoundingBox;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.event.AbstractCursorEvent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;
import com.simsilica.lemur.grid.GridModel;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MiscLemurI {
	public static MiscLemurI i(){return GlobalInstanceManagerI.i().get(MiscLemurI.class);}
	
	public Integer getEntryHeightPixels(ListBox lstbx){
		GridModel<Panel> gm = lstbx.getGridPanel().getModel();
		if(gm.getRowCount()==0)throw new NullPointerException("list must not be empty");
		Panel pnl = gm.getCell(0, 0, null); // create a new cell
		float fHeight = pnl.getPreferredSize().getY();
		
		return (int)FastMath.ceil(fHeight);
	}
	
//	public static class SimpleDragParentestListener implements CursorListener{
//		boolean bDragging = false;
//		private Vector3f	v3fDistToCursor;
//		private Vector3f getCursorPos(AbstractCursorEvent event){
//			return new Vector3f(event.getX(),event.getY(),0);
//		}
//		@Override
//		public void cursorButtonEvent(CursorButtonEvent event, Spatial target,				Spatial capture) {
//			if(event.getButtonIndex()==0){
//				bDragging=event.isPressed();
//				Panel pnlParentest = MiscJmeI.i().getParentest(capture, Panel.class, true);
//				v3fDistToCursor=pnlParentest.getWorldTranslation().subtract(getCursorPos(event));
//				event.setConsumed();
//			}
//		}
//		@Override
//		public void cursorEntered(CursorMotionEvent event, Spatial target,				Spatial capture) {
//		}
//		@Override
//		public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {
//		}
//		@Override
//		public void cursorMoved(CursorMotionEvent event, Spatial target,				Spatial capture) {
//			if(bDragging){
//				Panel pnlParentest = MiscJmeI.i().getParentest(capture, Panel.class, true);
//				pnlParentest.setLocalTranslation(getCursorPos(event).add(v3fDistToCursor));
//				event.setConsumed();
//			}
//		}
//	}
//	SimpleDragParentestListener sdl = new SimpleDragParentestListener();
//	public void applySimpleDragParentestListener(Panel pnl){
//		CursorEventControl.addListenersToSpatial(pnl,sdl);
//	}
}
