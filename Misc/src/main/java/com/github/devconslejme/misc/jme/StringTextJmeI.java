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
package com.github.devconslejme.misc.jme;

import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.StringI;
import com.google.common.base.Strings;
import com.jme3.app.Application;
import com.jme3.font.BitmapFont;
import com.jme3.font.BitmapText;
import com.jme3.font.LineWrapMode;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/*
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class StringTextJmeI extends StringI {
	public static StringTextJmeI i(){return GlobalManagerI.i().retrieveOverridingSupers(StringTextJmeI.class,null,StringI.class);}

	private BitmapFont	bfDefaultMonoFontOverride;
	
	public String fmtVector3f(Vector3f v3f,int iScale){
		return ""
			+StringI.i().fmtFloat(v3f.getX(),iScale)+"f,"
			+StringI.i().fmtFloat(v3f.getY(),iScale)+"f,"
			+StringI.i().fmtFloat(v3f.getZ(),iScale)+"f"
			;
	}
	
	public String fmtToDegrees(Quaternion qua,int iScale){
		float[] afAngles = qua.toAngles(null);
		return "" //max of 5 to pad is ex "-180."
			+Strings.padStart(StringI.i().fmtFloat(afAngles[0]*FastMath.RAD_TO_DEG,iScale),5+iScale,' ')+"f,"
			+Strings.padStart(StringI.i().fmtFloat(afAngles[1]*FastMath.RAD_TO_DEG,iScale),5+iScale,' ')+"f,"
			+Strings.padStart(StringI.i().fmtFloat(afAngles[2]*FastMath.RAD_TO_DEG,iScale),5+iScale,' ')+"f"
//			+String.format("%03."+iScale+"f", afAngles[0]*FastMath.RAD_TO_DEG)+"f,"
//			+StringI.i().fmtFloat(afAngles[0]*FastMath.RAD_TO_DEG,iScale)+"f,"
//			+StringI.i().fmtFloat(afAngles[1]*FastMath.RAD_TO_DEG,iScale)+"f,"
//			+StringI.i().fmtFloat(afAngles[2]*FastMath.RAD_TO_DEG,iScale)+"f"
//			+StringI.i().fmtFloat(qua.getW()*FastMath.RAD_TO_DEG,1)+""
			;
	}
	
  public BitmapFont getDefaultFont() {
  	return loadFont("Interface/Fonts/Default.fnt");
  }
  public BitmapFont getDefaultMonoFont() {
  	if(this.bfDefaultMonoFontOverride!=null)return this.bfDefaultMonoFontOverride;
  	return loadFont("Interface/Fonts/Console.fnt");
  }
  public void setDefaultMonoFontOverride(BitmapFont bf){
  	assert this.bfDefaultMonoFontOverride==null;
  	this.bfDefaultMonoFontOverride = bf;
  }
  public boolean isDefaultMonoFontOverrideSet(){
  	return this.bfDefaultMonoFontOverride!=null;
  }
  /**
   * may be cached or not
   * @param strPath
   * @return
   */
  public BitmapFont loadFont(String strPath) {
  	return AppI.i().loadFont(strPath);
//  	return GlobalManagerI.i().get(Application.class).getAssetManager().loadFont(strPath);
  }

	public void recursivelyApplyTextNoWrap(Node nodeParent) {
		/**
		 * LineWrapMode.Clip look better than NoWrap
		 */
		LineWrapMode e = LineWrapMode.Clip; //TODO could it be a clip only in the height? so it would wrap but would be clipped in the height only if overflowing downwards or outside limits 
		for(Spatial spt:nodeParent.getChildren()){
			if(spt instanceof BitmapText){
				BitmapText bt = ((BitmapText)spt);
//				System.err.println("NoWrapAt:"+((BitmapText)spt).getText());//TODO rm
				if(!bt.getLineWrapMode().equals(e)){
					bt.setLineWrapMode(e);
				}
				
				clipVertically(bt);
			}
			if(spt instanceof Node){
				recursivelyApplyTextNoWrap((Node)spt);
			}
		}
	}
	
	/**
	 * find a way to clip vertically too instead of this that is destructive:
	 * @param bt
	 */
	@Workaround
	@Bugfix //as the text should be limited to its panel all the time and never overflow neither vertically...
	private void clipVertically(BitmapText bt){
		String strText=bt.getText();
		int iNL = strText.indexOf("\n");
		if(iNL>-1){
			strText=strText.substring(0, iNL);
			bt.setText(strText);
		}
	}
	
	public BitmapText createBitmapText(BitmapFont bf,String strText, ColorRGBA color) {
		BitmapText bt = new BitmapText(bf);
		bt.setSize(12);
		bt.setText(strText);
		bt.setColor(color);
		return bt;
	}
	public BitmapText createBitmapText(String strText, ColorRGBA color) {
		return createBitmapText(getDefaultFont(),strText,color);
	}
	public BitmapText createBitmapTextMono(String strText, ColorRGBA color) {
		return createBitmapText(getDefaultMonoFont(),strText,color);
	}
}
