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

import com.github.devconslejme.gendiag.ResizablePanel;
import com.github.devconslejme.gendiag.es.GenericDialogZayES;
import com.github.devconslejme.gendiag.es.HierarchyI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.jme3.app.SimpleApplication;
import com.jme3.math.Vector3f;
import com.simsilica.es.EntityId;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestHierarchyResizablePanel extends SimpleApplication {
	public static void main(String[] args) {
		TestHierarchyResizablePanel tst = new TestHierarchyResizablePanel();
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
//		GuiGlobals.initialize(this);
//		BaseStyles.loadGlassStyle();
//		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		
//		ConfigureTestsI.i().configure(this, getGuiNode());
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(this, getGuiNode());
		
		initTest(100);
		initTest(400);
	}

	@SuppressWarnings("unchecked")
	private void initTest(int iBaseY) {
		ResizablePanel rzpChild = test(new Vector3f(200,iBaseY+200,20), "child"+iBaseY);
//		ColorRGBA color = ColorRGBA.Yellow.clone();color.a=0.25f;QuadBackgroundComponent qbc = new QuadBackgroundComponent(color);qbc.setMargin(10,5);testChild.setBorder(qbc);
		Button btn = new Button("click to close");
		btn.addClickCommands(new Command<Button>(){
			@Override
			public void execute(Button source) {
				rzpChild.removeFromParent();
			}
		});
		rzpChild.setContents(btn);
		
		ResizablePanel rzpParent = test(new Vector3f(100,iBaseY+100,10), "parent"+iBaseY);
		btn = new Button("click to open modal");
		btn.addClickCommands(new Command<Button>(){
			@Override
			public void execute(Button source) {
//				ResizablePanel rzpParentest = MiscJmeI.i().getParentest(source, ResizablePanel.class, true);
				HierarchyI.i().showAsHierarchyModal(
					UserDataI.i().getUserDataPSH(rzpParent,EntityId.class),
					UserDataI.i().getUserDataPSH(rzpChild,EntityId.class)
				);
//				_HierarchyComponent comp = rzpParent.getComponent(_HierarchyComponent.class);
//				_HierarchyComponent compChild = rzpChild.getComponent(_HierarchyComponent.class);
//				_HierarchySystemI.i().workOn(comp).showAsHierarchyModal(compChild);
			}
		});
		rzpParent.setContents(btn);
		
		// show it all
		getGuiNode().attachChild(rzpParent);
	}

	private ResizablePanel test(Vector3f pos,String strName) {
		ResizablePanel rzp = new ResizablePanel(null);
		EntityId entid = GenericDialogZayES.i().createEntity(rzp,strName);
		UserDataI.i().setUserDataPSH(rzp, entid);
		rzp.setPreferredSize(new Vector3f(300,200,0)); //TODO z will cause trouble?
		rzp.setLocalTranslation(pos); //above DevCons
		return rzp;
	}
}
