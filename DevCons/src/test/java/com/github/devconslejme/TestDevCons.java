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

package com.github.devconslejme;

import com.github.devconslejme.ConsolePluginI;
import com.github.devconslejme.JavaScriptI;
import com.github.devconslejme.misc.OSCmdI;
import com.jme3.app.SimpleApplication;
import com.jme3.system.AppSettings;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestDevCons extends SimpleApplication{

	public static void main(String[] args) {
		TestDevCons tst = new TestDevCons();
		
		AppSettings as = new AppSettings(true);
		as.setTitle(TestDevCons.class.getSimpleName());
		as.setResolution(1230,690);
		as.setResizable(true);
		as.setFrameRate(60);
		tst.setSettings(as);
		
		tst.setShowSettings(false);
		
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
		ConsolePluginI.i().configure(this,getGuiNode());
		
		JavaScriptI.i().setJSBinding(this);
		
		// Linux only: easy workaround to make strict focus policy painless
		OSCmdI.i().runOSCommand("linux 'xdotool windowactivate $(xdotool search --name \"^"+settings.getTitle()+"$\")'");
	}
	
}
