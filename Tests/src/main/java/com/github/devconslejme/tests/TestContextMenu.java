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

import com.github.devconslejme.gendiag.ContextMenuI;
import com.github.devconslejme.gendiag.ContextMenuI.ContextButton;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu.ApplyContextChoiceCmd;
import com.github.devconslejme.gendiag.DialogHierarchyStateI;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.projman.SimpleAppStateAbs;
import com.simsilica.lemur.Button;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestContextMenu extends SimpleAppStateAbs{
	public static void main(String[] args) {
		assert(true);
		TestContextMenu test = new TestContextMenu();
		test.start();
	}

	@Override
	public void simpleInitApp() {
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(this, getGuiNode(), getRootNode(), null);
		initTest();
	}

	@Override
	public void initTest() {
		super.initTest();
		
		ResizablePanel rzp = DialogHierarchyStateI.i().createDialog(TestContextMenu.class.getSimpleName(), null);
		rzp.setContents(new Button("context click me"));
		DialogHierarchyStateI.i().showDialog(rzp);
		
		ApplyContextChoiceCmd cmd = new ApplyContextChoiceCmd() {@Override public void executeContextCommand(ContextButton source) {
			System.out.println("test");
		}};
		
		ContextMenu cm = new ContextMenu(rzp);
		cm.addNewEntry("tst", cmd);
		cm.addNewEntry("tst2", cmd);
		cm.addNewEntry("tst3", cmd);
			ContextMenu cmSub1 = cm.createSubMenu("sub1");
			cmSub1.addNewEntry("tst5", cmd);
			cmSub1.addNewEntry("tst6", cmd);
				ContextMenu cmSub3 = cmSub1.createSubMenu("sub3");
				cmSub3.addNewEntry("tst8", cmd);
				cmSub3.addNewEntry("tst9", cmd);
			ContextMenu cmSub2 = cm.createSubMenu("sub2");
			cmSub2.addNewEntry("tst7", cmd);
		cm.addNewEntry("tst4", cmd);
		
		ContextMenuI.i().applyContextMenuAtSource(rzp.getContents(), cm);
	}

	@Override
	public void update(float tpf) {}
}