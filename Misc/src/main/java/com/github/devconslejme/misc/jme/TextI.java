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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.StringI;
import com.jme3.app.Application;
import com.jme3.font.BitmapFont;
import com.jme3.font.BitmapText;
import com.jme3.font.LineWrapMode;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/*
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class TextI {
	public static TextI i(){return GlobalManagerI.i().get(TextI.class);}
	
	public String fmtVector3f(Vector3f v3f,int iScale){
		return ""
			+StringI.i().fmtFloat(v3f.getX(),iScale)+"f,"
			+StringI.i().fmtFloat(v3f.getY(),iScale)+"f,"
			+StringI.i().fmtFloat(v3f.getZ(),iScale)+"f"
			;
	}
	
	public String fmtToDegrees(Quaternion qua,int iScale){
		float[] afAngles = qua.toAngles(null);
		return ""
			+StringI.i().fmtFloat(afAngles[0]*FastMath.RAD_TO_DEG,iScale)+"f,"
			+StringI.i().fmtFloat(afAngles[1]*FastMath.RAD_TO_DEG,iScale)+"f,"
			+StringI.i().fmtFloat(afAngles[2]*FastMath.RAD_TO_DEG,iScale)+"f"
//			+StringI.i().fmtFloat(qua.getW()*FastMath.RAD_TO_DEG,1)+""
			;
	}
	
  public BitmapFont loadDefaultFont() {
  	return loadFont("Interface/Fonts/Default.fnt");
  }
  public BitmapFont loadDefaultMonoFont() {
  	return loadFont("Interface/Fonts/Console.fnt");
  }
  public BitmapFont loadFont(String strPath) {
  	return GlobalManagerI.i().get(Application.class).getAssetManager().loadFont(strPath);
  }

	public void recursivelyApplyTextNoWrap(Node nodeParent) {
		/**
		 * LineWrapMode.Clip look better than NoWrap
		 */
		LineWrapMode e = LineWrapMode.Clip; //TODO could it be a clip only in the height? so it would wrap but would be clipped in the height only if overflowing downwards or outside limits 
		for(Spatial spt:nodeParent.getChildren()){
			if(spt instanceof BitmapText){
//				System.err.println("NoWrapAt:"+((BitmapText)spt).getText());//TODO rm
				if(!((BitmapText)spt).getLineWrapMode().equals(e)){
					((BitmapText)spt).setLineWrapMode(e);
				}
			}
			if(spt instanceof Node){
				recursivelyApplyTextNoWrap((Node)spt);
			}
		}
	}
}