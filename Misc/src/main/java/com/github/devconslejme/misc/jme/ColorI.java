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

import java.util.HashMap;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.TimedDelay;
import com.jme3.app.Application;
import com.jme3.material.Material;
import com.jme3.material.RenderState.BlendMode;
import com.jme3.math.ColorRGBA;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ColorI {
	public static ColorI i(){return GlobalManagerI.i().get(ColorI.class);}

	private boolean	bDebug;
	
//	public static class ColorRGBAx {
//		private ColorRGBA	color;
//
//		public ColorRGBAx(ColorRGBA color) {
//			this.color = color;
//		}
//
//		public ColorRGBA setA(float fAlpha) {
//			color.a=fAlpha;
//			return color;
//		}
//	}
	
	public static class ColorGlow{
		private ColorRGBA	color;
		
//		private TimedDelay tdColorGlow = new TimedDelay(15f);
		private TimedDelay tdColorGlow = new TimedDelay(5f);
		
		private float fColorCompMin=0.75f;
		private float fColorCompMax=1f;
		private float fAlphaMin=0f;
		private float fAlphaMax=0.50f;
		
		private float fAlphaDiff;
		private float fColorCompDiff;

		private boolean	bStartHigh;

		private boolean	bEnabled;
		
		public ColorGlow(ColorRGBA color,boolean bUseRealTime){
			this.color=color;
			tdColorGlow.setUseRealTime(bUseRealTime).setActive(true);
			
			this.fColorCompDiff=fColorCompMax-fColorCompMin;
			this.fAlphaDiff=fAlphaMax-fAlphaMin;
			
			QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
					update(getTPF());
					return true;
				}
			}).enableLoopMode();
		}
		
		public void update(float fTPF){
			int iTot=7;//MUST BE 7: r g b rg rb gb rgb!!!
			float fValOriginal = tdColorGlow.calcRemainderAsPercentualMultBy(iTot); 
			
			float fStart=0f;
			if(isStartHigh()){
				fStart+=0.5f; //will be *2 becoming 1f below
				fStart+=6f; //to use the white color initially
//				if(ColorI.i().isDebug())System.out.println(""+this+":fPercGlow="+fPercGlow);
			}
			fValOriginal+=fStart;
			
			float fPercGlow=(fValOriginal%1f);
			
			if(fPercGlow<0.5f){
				fPercGlow*=2f; //0.0 to 0.5 will become 0.0 to 1.0
			}else{
				fPercGlow=1f-fPercGlow;
				fPercGlow*=2f; //0.5 to 1.0 will become 1.0 to 0.0
			}
			
			float fColorComp=fColorCompMin+(fColorCompDiff*fPercGlow);
			fColorComp=1f;
			
			float fAlpha=fAlphaMin+(fAlphaDiff*fPercGlow);
			color.set(0,0,0,fAlpha);
			
			switch((int)fValOriginal%iTot){
				case 0:color.r=fColorComp;break;
				case 1:color.g=fColorComp;break;
				case 2:color.b=fColorComp;break;
				case 3:color.r=color.g=fColorComp;break;
				case 4:color.r=color.b=fColorComp;break;
				case 5:color.g=color.b=fColorComp;break;
				case 6:color.r=color.g=color.b=fColorComp;break;
			}
			
//			if(ColorI.i().isDebug()&&fValOriginal<1f)System.out.println(""+this+":fPercGlow="+fPercGlow+",color="+color);
			if(ColorI.i().isDebug())System.out.println(""+this+":fPercGlow="+fPercGlow+",color="+color+",fValOriginal="+fValOriginal);
		}

		public void enable() {
			tdColorGlow.resetTime().setActive(true);
			if(ColorI.i().isDebug())System.out.println(this+":RestarTimer");
			bEnabled=true;
		}

		public boolean isStartHigh() {
			return bStartHigh;
		}

		public ColorGlow setStartHigh(boolean bStartHigh) {
			this.bStartHigh = bStartHigh;
			return this; 
		}

		public void disable() {
			bEnabled=false;
		}
	}
	
//	/**
//	 * no overlapping
//	 * @param f
//	 * @return
//	 */
//	private float colorComponentLimit(float f){
//		if(f<=0)f=0;
//		if(f>=1)f=1;
//		return f;
//	}
	public ColorRGBA colorChangeCopy(ColorRGBA color, float fAddRGB){
		return colorChangeCopy(color,fAddRGB,color.a);
	}
	public ColorRGBA colorChangeCopy(ColorRGBA color, float fAddRGB, float fAlpha){
		color = color.clone();
		
		color.r+=(fAddRGB);
		color.g+=(fAddRGB);
		color.b+=(fAddRGB);
		
		color.a=fAlpha;
		
		color.clamp();
		
		return color;
	}
	
	/**
	 * neglight color by half negating or highlight RGB components
	 * @param color
	 * @return
	 */
	public ColorRGBA neglightColor(ColorRGBA color, float fIntensity){
		color=color.clone();
		
		if(fIntensity<0.1f)fIntensity=0.1f;
		if(fIntensity>1.0f)fIntensity=1.0f;
		
		color.r=Math.abs( (color.r + (0.5f*fIntensity)) % 1.0f);
		color.g=Math.abs( (color.g + (0.5f*fIntensity)) % 1.0f);
		color.b=Math.abs( (color.b + (0.5f*fIntensity)) % 1.0f);
		
		return color;
	}
	
//	/**
//	 * TODO apply it everywhere
//	 * @param color
//	 * @return
//	 */
//	public ColorRGBA fixCopy(ColorRGBA color){
//		return new ColorRGBA( //fix it
//				colorComponentLimit(color.r),
//				colorComponentLimit(color.g),
//				colorComponentLimit(color.b),
//				colorComponentLimit(color.a)
//			);
//	}
	
	public static enum EColor{
		Color,
		GlowColor,
		;
		public String s(){return toString();}
	}
	
	HashMap<Integer,Material> hmMatUnshadedColor = new HashMap<Integer,Material>();
	/**
	 * uses a cache too
	 * @param color
	 * @return
	 */
	public Material retrieveMaterialUnshadedColor(ColorRGBA color){
		color = color.clone();color.clamp(); //"fix" the color
		
		int i = color.asIntRGBA();
		Material mat = hmMatUnshadedColor.get(i);
		if(mat==null){
//			mat = new Material(AppI.i().getAssetManager(),"Common/MatDefs/Misc/Unshaded.j3md");
			mat = AppI.i().newMaterial("Common/MatDefs/Misc/Unshaded.j3md");
			mat.setColor(EColor.Color.s(), color);
			if(color.a<1f)mat.getAdditionalRenderState().setBlendMode(BlendMode.Alpha);
      //			mat.setTransparent(color.a<1f);
			hmMatUnshadedColor.put(i,mat);
		}
		return mat.clone(); //so it can be further tweaked (like wireframe) after here without messing the cache
	}
	
	public void updateColorFading(TimedDelay td, ColorRGBA color, boolean bFadeInAndOut){
		updateColorFading(td, color, bFadeInAndOut, 0.25f, 1.0f);
	}
	public void updateColorFading(TimedDelay td, ColorRGBA color, boolean bFadeInAndOut, float fMinAlpha, float fMaxAlpha){
		if(fMinAlpha<0f)fMinAlpha=0f;
		if(fMaxAlpha>1f)fMaxAlpha=1f;
		
		float fDeltaWorkAlpha = fMaxAlpha - fMinAlpha;
		
		td.setOscilateMode(bFadeInAndOut);
		
		color.a = fMinAlpha + td.calcRemainderAsPercentualMultBy(fDeltaWorkAlpha);
		
//		if(color.a<0)color.a=0;
		if(color.a>fMaxAlpha){
			color.a=fMaxAlpha;
		}else
		if(color.a < fMinAlpha){
			color.a=fMinAlpha;
		}
	}
	public boolean isDebug() {
		return bDebug;
	}
	public ColorI setDebug(boolean bDebug) {
		this.bDebug = bDebug;
		return this; 
	}
}
