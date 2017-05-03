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

import java.util.ArrayList;
import java.util.List;

import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.jme.UserDataI.IUDKey;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ClickCommandAbsorptionI {
	public static ClickCommandAbsorptionI i(){return GlobalManagerI.i().get(ClickCommandAbsorptionI.class);}
	
	private boolean	bDelegateClickCommands=true;
	
	/**
	 * In case Button click commands are not working when the button has focus.
	 * @param capture
	 */
	@Workaround
	@Bugfix
	public void delegateClickCommands(Spatial capture) {
		if(!isDelegateClickCommands())return;
		
//		if(focusman.getFocus()!=capture)return; //the ignored click commands bug happens only when the Button has focus
		
		if (capture instanceof Button) {
			Button btn = (Button) capture;
			ArrayList<Command<? super Button>> clickCommandsStored = 
				UserDataI.i().getUserDataPSH(btn, EUDClickCmds.ClickCommands.getUId());
//				List<Command<? super Button>> clickCommands = btn.getClickCommands();
//				if(clickCommands!=null){
//					for(Command<? super Button> a:clickCommands){
			if(clickCommandsStored!=null){
				for(Command<? super Button> a:clickCommandsStored){
					a.execute(btn);
				}
			}
		}
		
	}
	
	@Workaround
	@Bugfix
	@SuppressWarnings("unchecked")
	public void absorbClickCommands(Spatial capture) {
		if(!isDelegateClickCommands())return;
		
		if (capture instanceof Button) {
			Button btn = (Button) capture;
			List<Command<? super Button>> clickCommands = btn.getClickCommands();
			if(clickCommands==null)return; 
			
			if(clickCommands.size()>0){
				clickCommands = new ArrayList<Command<? super Button>>(clickCommands); //copy b4 clearing
				btn.removeClickCommands(clickCommands.toArray(new Command[0]));
				
				ArrayList<Command<? super Button>> clickCommandsStored = 
					UserDataI.i().getUserDataPSH(btn, EUDClickCmds.ClickCommands.getUId());
				if(clickCommandsStored==null){
					clickCommandsStored=new ArrayList<Command<? super Button>>();
					UserDataI.i().setUserDataPSH(btn, EUDClickCmds.ClickCommands.getUId(), clickCommandsStored);
				}
				clickCommandsStored.addAll(clickCommands);
			}
		}
	}

	private static enum EUDClickCmds implements IUDKey{
		ClickCommands,
		;
		public String s(){return toString();}

		@Override
		public Class getType() {
			return null;
		}

		@Override
		public String getUId() {
			return JavaLangI.i().enumUId(this);
		}
	}
	
	@Bugfix
	public boolean isDelegateClickCommands() {
		return bDelegateClickCommands;
	}
	
	@Bugfix
	public void setDelegateClickCommandsDisabled() {
		this.bDelegateClickCommands = false;
	}
}
