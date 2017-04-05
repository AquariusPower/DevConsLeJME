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

package com.github.devconslejme.tests;

import com.github.devconslejme.gendiag.HighlightEffectI;
import com.github.devconslejme.gendiag.ResizablePanel;
import com.github.devconslejme.misc.MainThreadI;
import com.github.devconslejme.misc.MiscLibI;
import com.github.devconslejme.misc.QueueStateI;
import com.jme3.app.SimpleApplication;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.style.BaseStyles;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestResizablePanel extends SimpleApplication {
	public static void main(String[] args) {
		TestResizablePanel tst = new TestResizablePanel();
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
		GuiGlobals.initialize(this);
		BaseStyles.loadGlassStyle();
		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		
		MiscLibI.i().configure(this);
		
		int i=300;
		test(new Vector3f(100,i+100,10));
		test(new Vector3f(200,i+200,20));
		test(new Vector3f(300,i+300,30));
	}

	private void test(Vector3f pos) {
		ResizablePanel rzp = new ResizablePanel(300,200,null);
		rzp.setLocalTranslation(pos); //above DevCons
		getGuiNode().attachChild(rzp);
		
		HighlightEffectI.i().addMouseCursorHighlightEffects(rzp, (QuadBackgroundComponent)rzp.getResizableBorders());
		
		Button btn = new Button("drag borders to resize:"+pos);
//		btn.setBackground(new QuadBackgroundComponent(ColorRGBA.Red.clone()));//,5,5, 0.02f, false));
		rzp.setContents(btn);
	}
}
