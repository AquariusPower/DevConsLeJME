/* 
Copyright (c) 2016-2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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

import java.util.ArrayList;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.SimulationTimeI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class EffectManagerStateI extends SimpleAppState {
	public static EffectManagerStateI i(){return GlobalManagerI.i().get(EffectManagerStateI.class);}
	
	long lLastUpdateMilis=SimulationTimeI.i().getMillis();
	ArrayList<IEffect> aEffectList = new ArrayList<IEffect>(){};
	private float	fAcumulatedTPF;
	private int	iFPStarget=15;
	
	public void configure(){
		AppI.i().attatchAppState(this);
//		GlobalManagerI.i().get(Application.class).getStateManager().attach(this);
	}
	
	public void add(IEffect i){
		if(aEffectList.contains(i)){
			MessagesI.i().debugInfo(this, "alredy contains effect", i);
			return;
		}
		
		aEffectList.add(i);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		fAcumulatedTPF+=tpf;
		if(lLastUpdateMilis+(1000/getFPStarget()) < SimulationTimeI.i().getMillis()){
			for(IEffect ie:aEffectList){//.values()){
				if(!ie.isPlaying())continue;
				ie.assertConfigIsValidAndFixIt(); //config may change during play
				if(ie.isWaitingParent())continue;
//				if(ie.getNodeParent()==null)continue;
				ie.play(fAcumulatedTPF);
			}
			fAcumulatedTPF=0f;
			lLastUpdateMilis=SimulationTimeI.i().getMillis();
		}
	}

	public int getFPStarget() {
		return iFPStarget;
	}

	public EffectManagerStateI setFPStarget(int iFPStarget) {
		this.iFPStarget = iFPStarget;
		return this;
	}
}
