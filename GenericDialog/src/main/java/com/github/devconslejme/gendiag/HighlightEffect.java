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

package com.github.devconslejme.gendiag;

import com.github.commandsconsolegui.spAppOs.misc.PrerequisitesNotMetException;
import com.github.commandsconsolegui.spLemur.dialog.ManageLemurDialog.DummyEffect;
import com.github.commandsconsolegui.spLemur.globals.GlobalManageDialogLemurI;
import com.github.devconslejme.misc.ColorI;
import com.github.devconslejme.misc.DatailedException;
import com.jme3.math.ColorRGBA;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.anim.Animation;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.core.GuiComponent;
import com.simsilica.lemur.effect.AbstractEffect;
import com.simsilica.lemur.effect.Effect;
import com.simsilica.lemur.effect.EffectInfo;


// (tab indent=2 spaces)

/**
 * As an impossible layout exception preventer workaround, 
 * this panel will test the parentest layout and if it fails, 
 * it will grow the parentest Panel size little by little,
 * until it works again.  
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HighlightEffect {
	private Effect<Button> efHighLightBkg = new AbstractEffect<Button>("ChannelHighLight") {
		@Override
		public Animation create(final Button target, final EffectInfo existing) {
			final GuiComponent gcBgChk = target.getBackground();
			if(!QuadBackgroundComponent.class.isInstance(gcBgChk)){
				throw new DatailedException("background type not supported for this effect", gcBgChk, target, existing, this);
			}
			
			return new Animation() {
				QuadBackgroundComponent gcBg = (QuadBackgroundComponent)gcBgChk;
				ColorRGBA colorBkp = gcBg.getColor();
				boolean bApplied=false;
				@Override	public void cancel() {
					gcBg.setColor(colorBkp);
				}
				@Override	public boolean animate(double tpf) {
					if(!bApplied){
	//					if(existing!=null && existing.getAnimation()==this)return true;
						gcBg.setColor(ColorI.i().neglightColor(colorBkp));
						bApplied=true;
					}
					return true;
				}
			};
		}
	};
	
	private DummyEffect	efDummy;
	public void addMouseCursorHighlightEffects(Button btn){
		efDummy = setupSimpleEffect(btn, Button.EFFECT_ACTIVATE, efHighLightBkg, efDummy);
		btn.addEffect(Button.EFFECT_DEACTIVATE,efDummy);
	}

	/**
	 * 
	 * @param pnl
	 * @param strEffectId
	 * @param ef
	 * @param efDummy can be null initially, use a field variable
	 * @return dummy effect for re-use
	 */
	private DummyEffect setupSimpleEffect(Panel pnl, String strEffectId, Effect ef, DummyEffect efDummy){
		String strDummyId="DummyEffectUniqueId";
		if(efDummy==null)efDummy=new DummyEffect(strDummyId,ef.getChannel());
		
		if(!efDummy.getChannel().equals(ef.getChannel())){
			throw new DatailedException("both should be on the same channel", efDummy, strEffectId, ef, pnl, this);
		}
		
		if(strEffectId.equals(strDummyId)){
			throw new DatailedException("ids should differ", strDummyId, efDummy, strEffectId, ef, pnl, this);
		}
		
		pnl.addEffect(strEffectId, (Effect)ef);
		pnl.addEffect(strDummyId, efDummy);
		
		return efDummy;
	}

	/**
	 * this is just to let actual effects to end themselves with their own cancel()
	 */
	private static class DummyEffect extends AbstractEffect{
		private String	strId;
		public DummyEffect(String strId, String channel){
			super(channel);
			this.strId=strId;
		}
		
		public String getId(){
			return this.strId;
		}
		
		@Override
		public Animation create(Object target, EffectInfo existing) {
			return new Animation() {
				@Override	public void cancel() {}
				@Override	public boolean animate(double tpf) {return true;}
			};
		}
	}

}
