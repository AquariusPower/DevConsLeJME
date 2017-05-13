/* 
Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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

import java.util.TreeMap;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.anim.Animation;
import com.simsilica.lemur.anim.SpatialTweens;
import com.simsilica.lemur.anim.Tween;
import com.simsilica.lemur.anim.TweenAnimation;
import com.simsilica.lemur.anim.Tweens;
import com.simsilica.lemur.effect.AbstractEffect;
import com.simsilica.lemur.effect.EffectInfo;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class EffectsLemurI {
	public static EffectsLemurI i(){return GlobalManagerI.i().get(EffectsLemurI.class);}
	
	public abstract class BfdEffect extends AbstractEffect<Panel>{
		protected EEffChannel	echn;
  	protected float fTime=0.250f;
		private String	strName;

		public BfdEffect(String strName, EEffChannel echn, EEffState es) {
			super(echn.s());
			this.strName=strName;
			echn.putEffect(es,this);
			this.echn = echn;
		}

		public String getName(){
			return strName;
		}
		
		public Vector3f getMoveStartAt(Panel target){
    	Vector3f v3fOrigin = target.getLocalTranslation().clone();
    	if(echn.v3fPlayPosStartOverride!=null){
      	v3fOrigin = echn.v3fPlayPosStartOverride.clone();
    	}
    	return v3fOrigin;
		}
	}
	BfdEffect efGrow;
	BfdEffect efShrink;
	
	public void configure(){
		efShrink = new BfdEffect("Shrink", EEffChannel.ChnGrowShrink, EEffState.Hide) {
	    @Override
			public Animation create( Panel target, EffectInfo existing ) {
	    	// always current position
	    	Vector3f v3fOrigin = target.getLocalTranslation().clone();
	    	
	    	/**
	    	 * it is subtract because the start pos is the lower left corner
	    	 */
	    	Vector3f v3fFrom = v3fOrigin.subtract(getRelativeCenterXYposOf(target));
	    	Vector3f v3fTo = v3fOrigin.clone();
	    	
	    	// just inverted "to" "from" related to grow one
	  		Tween twMove = SpatialTweens.move(target, v3fTo, v3fFrom, fTime);
	      Tween twScale = SpatialTweens.scale(target, 1, 0, fTime);
	      return new TweenAnimation(Tweens.smoothStep(Tweens.parallel(twMove, twScale)));
	    }
		};
		
		efGrow = new BfdEffect("Grow",EEffChannel.ChnGrowShrink, EEffState.Show) {
	    @Override
			public Animation create( Panel target, EffectInfo existing ) {
	    	Vector3f v3fOrigin = getMoveStartAt(target);
	    	
	    	/**
	    	 * it is subtract because the start pos is the lower left corner
	    	 */
	    	Vector3f v3fFrom = v3fOrigin.subtract(getRelativeCenterXYposOf(target));
	    	Vector3f v3fTo = v3fOrigin.clone();
	    	
	      Tween twMove = SpatialTweens.move(target, v3fFrom, v3fTo, fTime);
	      Tween twScale = SpatialTweens.scale(target, 0, 1, fTime);
	      return new TweenAnimation(Tweens.smoothStep(Tweens.parallel(twMove, twScale)));
	    }
		};
	}
	
	public static enum EEffState{
		Show,
		Hide,
		CallAttention,
		LoopLight,
		LoopStrong,
	}
	
	/**
	 * TODO if the effect is a factory (confirm?), this will work with multiple panels growing or shrinking simultaneously, otherwise, put all that code in a normal class, not enum
	 */
	public static enum EEffChannel{
		ChnGrowShrink,
		;
		private TreeMap<EEffState,BfdEffect> tmEf = new TreeMap<EEffState,BfdEffect>();
		private Vector3f	v3fPlayPosStartOverride;
		public void putEffect(EEffState es, BfdEffect ef){
			DetailedException.assertNotAlreadySet(tmEf.get(es), ef, "effect", this);
			tmEf.put(es,ef);
		}
		public BfdEffect getEffect(EEffState es){
			return tmEf.get(es);
		}
		public BfdEffect[] getAllEffects(){
			return tmEf.values().toArray(new BfdEffect[0]);
		}
		public void applyEffectsAt(Panel pnl){
			for(BfdEffect ef:getAllEffects()){
				pnl.addEffect(ef.getName(), ef);
			}
		}
		/**
		 * 
		 * @param es
		 * @param pnl
		 * @param v3fPosStartOverride can be null
		 */
		public void play(EEffState es, Panel pnl, Vector3f v3fPosStartOverride){
			this.v3fPlayPosStartOverride=v3fPosStartOverride;
			pnl.runEffect(tmEf.get(es).getName());
		}
		public String s(){return toString();}
	}
	
	protected Vector3f getRelativeCenterXYposOf(Panel target) {
		return target.getWorldBound().getCenter();
	}

}
