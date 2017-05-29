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

import org.omg.CORBA.Request;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Panel;

/**
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class SizeAndLocationI {
	public static SizeAndLocationI i(){return GlobalManagerI.i().get(SizeAndLocationI.class);}
	
	private Float	fMinSizeZ = null;
	
	/**
	 * prevent messing with Z size
	 * @param pnl
	 * @param v3fSize
	 */
	public void setPreferredSize(Panel pnl, Vector3f v3fSize) {
		if (pnl instanceof ResizablePanel) {
			ResizablePanel rzp = (ResizablePanel) pnl;
			rzp.setPreferredSizeWH(v3fSize);
		}else{
			if(v3fSize.z==0)v3fSize.z=getMinSizeZ(); //fix squashed
			if(v3fSize.z!=getMinSizeZ() && v3fSize.z!=pnl.getPreferredSize().z){
				MessagesI.i().warnMsg(this, "panel size Z is not default neither current", v3fSize, pnl.getPreferredSize(), getMinSizeZ(), pnl);
			}
			pnl.setPreferredSize(v3fSize);
		}
	}
	
	/**
	 * ignores Z pos, keeping current
	 * @param pnl
	 * @param v3fPos
	 */
	public void setLocalTranslationXY(Panel pnl, Vector3f v3fPos) {
		if (pnl instanceof PanelBase) {
			PanelBase rzp = (PanelBase) pnl;
			rzp.setLocalTranslationXY(v3fPos);
		}else{
			pnl.setLocalTranslation(v3fPos.x, v3fPos.y, pnl.getLocalTranslation().z);
		}
	}

	public void setLocalTranslationZ(Panel pnl, float fZ) {
		Vector3f v3f = pnl.getLocalTranslation().clone();
		v3f.z=fZ;
		if(pnl instanceof PanelBase){
			((PanelBase)pnl).setLocalTranslationZ(fZ);
		}else{
			pnl.setLocalTranslation(v3f);
		}
	}

	public SizeAndLocationI setMinSizeZ(float fZ) {
		if(this.fMinSizeZ!=null)throw new DetailedException("already set");
		this.fMinSizeZ=fZ;
		return this;
	}

	public void maximize(PanelBase pnl) {
		maximize(pnl, 
			new Vector3f(0,HWEnvironmentJmeI.i().getDisplay().getHeight(),0), 
			new Vector3f(HWEnvironmentJmeI.i().getDisplay().getWidth(),HWEnvironmentJmeI.i().getDisplay().getHeight(),0));
	}
	public void maximize(PanelBase pnl, Vector3f v3fPosXY, Vector3f v3fSizeWH) {
		pnl.setLocalTranslationXY(v3fPosXY);
		pnl.setPreferredSizeWH(v3fSizeWH);
	}

	/**
	 * this was just a guesser,
	 * maximization scope/limits would be required,
	 * nah... use a boolean!
	 * @param pnl
	 * @return
	 */
	@Deprecated
	private boolean isMaximized(Panel pnl) {
		Vector3f v3fSize = pnl.getSize();
		Vector3f v3fPos = pnl.getLocalTranslation();
		return 
			v3fSize.x==HWEnvironmentJmeI.i().getDisplay().getWidth() && 
			v3fSize.y==HWEnvironmentJmeI.i().getDisplay().getHeight() &&
			v3fPos.x==0 &&
			v3fPos.y==HWEnvironmentJmeI.i().getDisplay().getHeight()
			;
	}

	public static enum EResizeApplyMode{
		Save,
		Restore,
		RestoreDefault,
		UpdateDefaultToCurrent,
		;
		public String s(){return toString();}
	}
	public static class SafeSize{
		public SafeSize(){};
		Vector3f v3fSafeSizeLast=null;
		Vector3f v3fSafeSizeDefault=null;
	}
//	private String strUDKeySafeSizeLast=ResizablePanel.class.getName()+"/SafeSize";
//	private String strUDKeySafeSizeDefault=ResizablePanel.class.getName()+"/SafeSizeDefault";
	/**
	 * TODO confirm: this method cannot be called every frame or may mess alignment and line wrap mode on BitmapText
	 * @param eapply
	 * @param pnl
	 */
	public void safeSizeRecursively(EResizeApplyMode eapply, Panel pnl) {
		SafeSize ss = UserDataI.i().retrieveExistingOrCreateNew(pnl, SafeSize.class);
		switch(eapply){
			case Restore:{
				Vector3f v3fSafeSize = ss.v3fSafeSizeLast;
				if(v3fSafeSize!=null)setPreferredSize(pnl,v3fSafeSize);
			}break;
			case RestoreDefault:{
				Vector3f v3fSafeSize = ss.v3fSafeSizeDefault;
				if(v3fSafeSize!=null)setPreferredSize(pnl,v3fSafeSize);
			}break;
			case Save:{
				ss.v3fSafeSizeLast=pnl.getPreferredSize().clone();
			}break;
			case UpdateDefaultToCurrent:{
				ss.v3fSafeSizeDefault=pnl.getPreferredSize().clone();
			}break;
		}
		
		for(Spatial sptChild:pnl.getChildren()){
			if (sptChild instanceof Panel) {
				safeSizeRecursively(eapply,(Panel)sptChild);
			}
		}
	}
	public void safeSizeInitialize(Panel pnl){
		SafeSize ss = UserDataI.i().retrieveExistingOrCreateNew(pnl, SafeSize.class);
		if(ss.v3fSafeSizeLast==null){ // 1st/initial safe size will be default
			safeSizeRecursively(EResizeApplyMode.UpdateDefaultToCurrent,pnl);
		}
	}

	public void moveToScreenCenterXY(Panel pnl) {
		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSizeCopy(pnl);
		setLocalTranslationXY(pnl,new Vector3f(
			HWEnvironmentJmeI.i().getDisplay().getWidth()/2f - v3fSize.x/2f, 
			HWEnvironmentJmeI.i().getDisplay().getHeight()/2f + v3fSize.y/2f, 
			0 
		));
	}

	/**
	 * see {@link #fMinSizeZ}
	 * @return
	 */
	public float getMinSizeZ() {
		return fMinSizeZ;
	}

	public Vector3f positionFullyInsideScreenLimits(Panel pnl, Vector3f v3fPos) {
		return positionFullyInsideScreenLimits(pnl,v3fPos,false);
	}
	public Vector3f positionFullyInsideScreenLimits(Panel pnl, Vector3f v3fPos, boolean bPopupMode) {
		float fYDistFromCursor=10f;
//		Vector3f v3fSize = lblPopupHelp.getSize();
		Vector3f v3fSize = pnl.getSize();
		
		float fX = v3fPos.x+(bPopupMode?-v3fSize.x/2:0);
		if(fX<0){
			fX=0;
		}else{
			float fDiff = (fX+v3fSize.x) - HWEnvironmentJmeI.i().getDisplay().getWidth();
			if(fDiff>0)fX-=fDiff;
		}
		
		float fY = v3fPos.y+(bPopupMode?v3fSize.y+fYDistFromCursor:0f);
		if(fY>HWEnvironmentJmeI.i().getDisplay().getHeight()){
			fY=HWEnvironmentJmeI.i().getDisplay().getHeight();
		}else{
			if( (fY - v3fSize.y) < 0 )fY=v3fSize.y;
		}
		
		Vector3f v3fNew = new Vector3f(fX,fY,0);
		setLocalTranslationXY(pnl, v3fNew);
		setLocalTranslationZ(pnl, v3fPos.z);
//		cntrPopupHelp.setLocalTranslation(fX,fY,MiscJmeI.i().getZAboveAllAtGuiNode());
		
		return v3fNew;
	}
}
