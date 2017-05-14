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

import java.util.function.Function;

import com.github.devconslejme.misc.Annotations.NonStandard;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.jme3.app.Application;
import com.jme3.scene.Node;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PkgCfgI {
	public static PkgCfgI i(){return GlobalManagerI.i().get(PkgCfgI.class);}

	private boolean	bConfigured;
	
	public void configure(Application app, Node nodeGui, Node nodeVirtualWorld){
		DetailedException.assertIsFalse("configured", bConfigured, this);
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(app,nodeGui,nodeVirtualWorld);
		
//		FileI.i().configure(JmeSystem.getStorageFolder(StorageFolderType.Internal), app.getClass());
		DevConsPluginStateI.i().configure(nodeGui);
//		FileI.i().configure(DevConsPluginStateI.i().getStorageFolder());
		if(KeyBindCommandManagerI.i().getFuncRunUserCommand()==null){
			KeyBindCommandManagerI.i().setFuncRunUserCommand(new Function<String, Boolean>() {
				@Override
				public Boolean apply(String strJS) {
					JavaScriptI.i().execScript(strJS, false);
					return true;
				}
			});
		}
		
		initNonStandard();
		
		bConfigured=true;
	}
	
	@NonStandard
	private void initNonStandard(){
		/**
		 * this special configuration will start appending log to a file
		 */
		MessagesI.i().initializeLogFile();
	}
}
