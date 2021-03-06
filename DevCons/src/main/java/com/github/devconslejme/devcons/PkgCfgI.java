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

package com.github.devconslejme.devcons;

import com.github.devconslejme.misc.Annotations.NonStandard;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallUserCustomCmd;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.PkgCfgAbs;
import com.jme3.app.Application;
import com.jme3.scene.Node;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PkgCfgI extends PkgCfgAbs{
	public static PkgCfgI i(){return GlobalManagerI.i().get(PkgCfgI.class);}

	/**
	 * 
	 * @param app (forwarded)
	 * @param nodeGui where to attach DevCons
	 * @param nodeVirtualWorld (forwarded)
	 */
	public void configure(Application app, Node nodeGui, Node nodeVirtualWorld){
		super.configure();
		StringTextDevConsI.i(); //overrider
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(app,nodeGui,nodeVirtualWorld);
		
		DevConsPluginStateI.i().configure(null,nodeGui); //first!
		
		if(KeyBindCommandManagerI.i().getCallRunUserCommand()==null){
			KeyBindCommandManagerI.i().setCallRunUserCommand(new CallUserCustomCmd() {
				@Override
				public Boolean execUserCustomCmd(String strJSCmd) {
					JavaScriptI.i().setJSBinding(this); //this allows for accessing this callable from within the script
					JavaScriptI.i().execScript(strJSCmd, false);
					return true;
				}
			});
		}
		
		initNonStandard();
		
		setConfigured();
	}
	
	@NonStandard
	private void initNonStandard(){
		/**
		 * this special configuration will start appending log to a file
		 */
		MessagesI.i().initializeLogFile();
	}
}
