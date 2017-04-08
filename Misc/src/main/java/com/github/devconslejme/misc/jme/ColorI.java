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

import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.jme3.math.ColorRGBA;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ColorI {
	public static ColorI i(){return GlobalInstanceManagerI.i().get(ColorI.class);}
	
	/**
	 * no overlapping
	 * @param f
	 * @return
	 */
	private float colorComponentLimit(float f){
		if(f<=0)f=0;
		if(f>=1)f=1;
		return f;
	}
	public ColorRGBA colorChangeCopy(ColorRGBA color, float fAddRGB){
		return colorChangeCopy(color,fAddRGB,color.a);
	}
	public ColorRGBA colorChangeCopy(ColorRGBA color, float fAddRGB, float fAlpha){
		color = color.clone();
		
		color.r=Math.min(color.r+fAddRGB, 0f);
		color.r=colorComponentLimit(color.r+=fAddRGB);
		color.g=colorComponentLimit(color.g+=fAddRGB);
		color.b=colorComponentLimit(color.b+=fAddRGB);
		
		color.a=fAlpha;
		return color;
	}
	
	/**
	 * neglight color by half negating or highlight RGB components
	 * @param color
	 * @return
	 */
	public ColorRGBA neglightColor(ColorRGBA color){
		color=color.clone();
		
		color.r=Math.abs((color.r+0.5f)%1.0f);
		color.g=Math.abs((color.g+0.5f)%1.0f);
		color.b=Math.abs((color.b+0.5f)%1.0f);
//		color.r=neglightColorComponent(color.r);
//		color.g=neglightColorComponent(color.g);
//		color.b=neglightColorComponent(color.b);
		
		return color;
	}
	
//	private float neglightColorComponent(float f){
//		if(f>0.5f){
//			f-=0.5f;
//		}else{
//			f+=0.5f;
//		}
//		
//		if(f<0)f=0;if(f>1)f=1; //useless??
//		
//		return f;
//	}
}
