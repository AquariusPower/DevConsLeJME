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

import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.jme3.font.BitmapText;
import com.jme3.scene.Geometry;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;

/**
 * TODO position the carat where it is clicked
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ClickToCaratPositionListenerI implements CursorListener{
	public static ClickToCaratPositionListenerI i(){return GlobalInstanceManagerI.i().get(ClickToCaratPositionListenerI.class);}
	
	public void applyAt(Spatial spt){
		CursorEventControl.addListenersToSpatial(spt, this);
	}
	
	public void applyRecursivelyAtAllTextFieldsOf(Spatial spt){
		for(TextField tf:MiscJmeI.i().getAllChildrenRecursiveFrom(spt, TextField.class)){
			applyAt(tf);
		}
	}
	
	@Override
	public void cursorButtonEvent(CursorButtonEvent event, Spatial target,			Spatial capture) {
		TextField tf = (TextField)capture;
		
		String str=tf.getText();
		
		BitmapText bt = MiscJmeI.i().getChildRecursiveExactMatch(tf,BitmapText.class); //TODO this is just guesswork...
		
		Geometry cursor = MiscJmeI.i().getChildRecursiveExactMatch(bt,Geometry.class); //TODO this is just guesswork...
		
		// tec.textOffset is inaccessible :( 
		TextEntryComponent tec = MiscLemurI.i().getTextEntryComponentFrom(tf);
		
		// relative to the BitmapText
		float fRelativeCursorPosX = event.getX() - bt.getWorldTranslation().x;
		
		// relative to the first character shown on the TextField, just trunc
		int iRelativeCharPos = (int)(fRelativeCursorPosX/MiscLemurI.i().getFontCharWidthForStyle(tf.getStyle()));
		
		// do position carat
		int iCaratAt = tf.getDocumentModel().getCarat();
		float fCursorAtX = cursor.getWorldTranslation().x;
		
		tf.getDocumentModel().home(true);
		for(int i2=0;i2<iRelativeCharPos;i2++)tf.getDocumentModel().right();
		
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
