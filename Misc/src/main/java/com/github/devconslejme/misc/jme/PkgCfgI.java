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

import com.github.devconslejme.misc.AssertionsI;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.input.FlyByCamera;
import com.jme3.scene.Node;
import com.jme3.system.JmeSystem;
import com.jme3.system.JmeSystem.StorageFolderType;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PkgCfgI {
	public static PkgCfgI i(){return GlobalManagerI.i().get(PkgCfgI.class);}
	
	private boolean	bConfigured;
	public void configure(Application app,Node nodeGui, Node nodeVirtualWorld){
		DetailedException.assertIsFalse("configured", bConfigured, this);
		com.github.devconslejme.misc.PkgCfgI.i().configure(
				JmeSystem.getStorageFolder(StorageFolderType.Internal), app.getClass());
		
		GlobalManagerI.i().put(Application.class, app);  //first!
		FlyByCamera flycam=null;
		if(app instanceof SimpleApplication){
			GlobalManagerI.i().put(SimpleApplication.class, (SimpleApplication)app); //code depending on this should be optional...
			flycam=G.i(SimpleApplication.class).getFlyByCamera();
		}
		
		// after first
		DebugVisualsI.i().configure();
		MiscJmeI.i().configure(nodeVirtualWorld);
		new KeyCodeConfigureForJme().configure();
		EffectManagerStateI.i().configure();
		SimulationTimeStateI.i().configure();
		QueueStateI.i().configure();
		OrthogonalCursorStateI.i().configure(nodeGui);
		AssertionsI.i().configure();
		IndicatorI.i().configure(nodeGui);
		EnvironmentJmeI.i().configure(nodeGui);
		WorldPickingI.i().configure(flycam);
		
		// non standard cfgs
//		WorldPickingI.i().addSkipType(DebugVisualsI.GeometryBVolDbg.class);
		
		bConfigured=true;
	}
}
