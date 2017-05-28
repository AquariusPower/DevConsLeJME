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

import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.PkgCfgAbs;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.jme3.app.Application;
import com.jme3.math.Vector2f;
import com.jme3.scene.Node;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.event.PickEventSession;
import com.simsilica.lemur.event.PickEventSession.RootEntry;
import com.simsilica.lemur.style.BaseStyles;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PkgCfgI extends PkgCfgAbs{
	public static PkgCfgI i(){return GlobalManagerI.i().get(PkgCfgI.class);}
	
//	private boolean	bConfigured;
//	public boolean isConfigured() {return bConfigured;}

	/**
	 * 
	 * @param app to init lemur GuiGlobals
	 * @param nodeGui to manage dialogs
	 * @param nodeVirtualWorld (forwarded)
	 */
	public void configure(Application app, Node nodeGui, Node nodeVirtualWorld){
		super.configure();
		SystemAlertLemurI.i().configure(); //this is a global overrider
		com.github.devconslejme.misc.jme.PkgCfgI.i().configure(app,nodeGui, nodeVirtualWorld);
		
		// lermur inits
		if(GuiGlobals.getInstance()==null)GuiGlobals.initialize(app); //GuiGlobals.initialize(app);
		BaseStyles.loadGlassStyle();
		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS); //this can be set again later
		
		// after lemur inits
		AbsorbClickCommandsI.i().configure();
		initNonStandardLemurPickingRayCastFrom(); //first!
		PopupHintHelpListenerI.i().configure(nodeGui);
		DragParentestPanelListenerI.i().configure(nodeGui);
		MiscLemurI.i().configure(nodeGui);
		SystemAlertLemurI.i().configure(nodeGui);
		EffectsLemurI.i().configure();
		new KeyCodeConfigureForLemur().configure();
		
		setConfigured();
	}

	/**
	 * TODO request these being exposed at least readonly
	 * 
	 * 1) lemur picking ray cast from at gui node is hardcoded at 
	 * {@link PickEventSession#getPickRay(RootEntry,Vector2f)},
	 * and it is not accessible, currently is 1000
	 * 
	 * 2) expectedly, if such default initial offset is ever updated, everywhere on lemur will also be
	 * {@link QuadBackgroundComponent#getZOffset()}
	 */
	@Workaround
	private void initNonStandardLemurPickingRayCastFrom() {
		MiscJmeI.i().setAboveAllAtGuiNode(1001);
		
		MiscLemurI.i().setPickingRayCastFromZ(1000);

		SizeAndLocationI.i().setMinSizeZ(new QuadBackgroundComponent().getZOffset());
	}

}
