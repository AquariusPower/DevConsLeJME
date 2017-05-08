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

import java.util.function.Function;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.jme3.font.BitmapText;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;

/**
 * TODO position the carat where it is clicked
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ClickToPositionCaratListenerI implements CursorListener{
	public static ClickToPositionCaratListenerI i(){return GlobalManagerI.i().get(ClickToPositionCaratListenerI.class);}
	
	public void applyAt(Spatial spt){
		CursorEventControl.addListenersToSpatial(spt, this);
	}
	
	public void applyRecursivelyAtAllTextFieldsOf(Spatial spt){
		for(TextField tf:SpatialHierarchyI.i().getAllChildrenRecursiveFrom(spt, TextField.class, null)){
			applyAt(tf);
		}
	}
	
	@Override
	public void cursorButtonEvent(CursorButtonEvent event, Spatial target,			Spatial capture) {
		if(!event.isPressed())return;
		
		prepareToMoveCarat((TextField)capture, event.getLocation());
	}
	
	/**
	 * Based on TextField's text cursor geometry world translation. 
	 * @param tf
	 * @param fCursorPosX
	 */
	private void prepareToMoveCarat(TextField tf, Vector2f v2fCursorPos){
		/**
		 * Requirements that may break in the future:
		 * 1) retrieve a single BitmapText recursive child from the TextField (TextField.text<TextEntryComponent>.bitmapText<BitmapText>)
		 * 2) retrieve a single Geometry recursive child with debug name == "cursor" from the TextField
		 */
		BitmapText bt = SpatialHierarchyI.i().getChildRecursiveExactMatch(tf,BitmapText.class);
		Geometry geomCaratCursor = SpatialHierarchyI.i().getChildRecursiveExactMatch(bt,
				new Function<Spatial,Boolean>() {
					@Override
					public Boolean apply(Spatial spt) {
						if(Geometry.class.isInstance(spt) && spt.getName().equals("cursor"))return true;
						return false;
					}
				}
//				new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				Spatial spt = getValue(Spatial.class.getName()); 
//				if(Geometry.class.isInstance(spt) && spt.getName().equals("cursor"))return true;
//				return false;
//			}}
		);
		
		moveCarat(tf, 
			v2fCursorPos, 
			bt.getWorldTranslation(), 
			geomCaratCursor.getWorldTranslation(),
			MiscLemurI.i().getFontCharWidthForStyle(tf.getStyle())
		);
	}
	
	private void moveCarat(TextField tf, 
		Vector2f v2fCursorPos, 
		Vector3f v3fBitmapTextWorldTranslation, 
		Vector3f v3fGeometryCursorWorldTranslation, 
		int iMonoFontWidth
	){
		// relative to the BitmapText
		float fRelativeCursorPosX = v2fCursorPos.x 
			-v3fBitmapTextWorldTranslation.x;
		float fRelativeCaratPosX = v3fGeometryCursorWorldTranslation.x 
			-v3fBitmapTextWorldTranslation.x;
		
		// relative to the first character visibly shown on the TextField, just trunc (the whole letter will provide carate in front of it)
		int iRelativeVisibleRequestedNewCaratIndex = (int)(fRelativeCursorPosX/iMonoFontWidth);
		int iRelativeVisibleCurrentCaratIndex = (int)(fRelativeCaratPosX/iMonoFontWidth);
		int iMoveCaratAmount = iRelativeVisibleRequestedNewCaratIndex - iRelativeVisibleCurrentCaratIndex;
		
		/**
		 * converting to carat index before, works better than this: 
		float fFinalRelativePosX = fRelativeCursorPosX - fRelativeCaratPosX;
		int iMoveCaratAmount = (int)(fFinalRelativePosX/iMonoFontWidth);
		 */
		
		if(iMoveCaratAmount!=0){
			if(iMoveCaratAmount>0){
				for(int i2=0;i2<iMoveCaratAmount;i2++)tf.getDocumentModel().right();
			}else{
				for(int i2=0;i2>iMoveCaratAmount;i2--)tf.getDocumentModel().left();
			}
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
