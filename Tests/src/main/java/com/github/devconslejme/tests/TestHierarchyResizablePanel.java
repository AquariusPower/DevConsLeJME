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

import com.github.devconslejme.es.DialogHierarchySystemI;
import com.github.devconslejme.gendiag.ResizablePanel;
import com.github.devconslejme.gendiag.ShowDialogStateI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
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
//		AppSettings as = new AppSettings(true);
		tst.setPauseOnLostFocus(false); //good for outside monitoring
//		tst.setSettings(as);
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
//		GuiGlobals.initialize(this);
//		BaseStyles.loadGlassStyle();
//		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		
//		ConfigureTestsI.i().configure(this, getGuiNode());
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(this, getGuiNode());
		
		createParentChild(100);
		
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				createParentChild(400);
				return true;
			}
		}.setDelaySeconds(1.0f));
		
		// multi child hierarchy
		ResizablePanel rzpA = createPanel(new Vector3f(300,700,0),"MultiA",null);
		ShowDialogStateI.i().showDialog(rzpA);
		
		ResizablePanel rzpB = createPanel(new Vector3f(310,710,0),"MultiB",null);
		ShowDialogStateI.i().showDialogAsModal(
			UserDataI.i().getUserDataPSH(rzpA,EntityId.class),
			UserDataI.i().getUserDataPSH(rzpB,EntityId.class)
		);
		
		ResizablePanel rzpC = createPanel(new Vector3f(320,720,0),"MultiC",null);
		ShowDialogStateI.i().showDialogAsModal(
			UserDataI.i().getUserDataPSH(rzpA,EntityId.class),
			UserDataI.i().getUserDataPSH(rzpC,EntityId.class)
		);
		
		ResizablePanel rzpD = createPanel(new Vector3f(320,720,0),"MultiD",null);
		ShowDialogStateI.i().showDialogAsModal(
			UserDataI.i().getUserDataPSH(rzpB,EntityId.class),
			UserDataI.i().getUserDataPSH(rzpD,EntityId.class)
		);
	}

	@SuppressWarnings("unchecked")
	private void createParentChild(int iBaseY) {
		ResizablePanel rzpChild = createPanel(new Vector3f(200,iBaseY+200,20), "child"+iBaseY, "click to close");
		((Button)rzpChild.getContents()).addClickCommands(new Command<Button>(){ //FIXME this is not compatible with CursorListener!
			@Override
			public void execute(Button source) {
				rzpChild.removeFromParent(); //FIXME not being reached
			}
		});
		
		ResizablePanel rzpParent = createPanel(new Vector3f(100,iBaseY+100,10), "parent"+iBaseY, "click to open modal");
		((Button)rzpChild.getContents()).addClickCommands(new Command<Button>(){ //FIXME this is not compatible with CursorListener!
			@Override
			public void execute(Button source) { //FIXME not being reached
				EntityId entidChild = UserDataI.i().getUserDataPSH(rzpChild,EntityId.class);
				ShowDialogStateI.i().showDialogAsModal(
					UserDataI.i().getUserDataPSH(rzpParent,EntityId.class),
					entidChild
				);
			}
		});
		
		ShowDialogStateI.i().showDialog(rzpParent);
	}
	
	private ResizablePanel createPanel(Vector3f pos,String strName,String strInfo) {
		if(strInfo==null)strInfo=strName;
		
		EntityId entid = DialogHierarchySystemI.i().createEntity(strName);
		ResizablePanel rzp = new ResizablePanel(null);
		ShowDialogStateI.i().put(entid,rzp);
		UserDataI.i().setUserDataPSH(rzp, entid);
		
		ShowDialogStateI.i().setAutoMoveRelativelyToParent(entid);
		
		rzp.setPreferredSize(new Vector3f(300,200,0)); //TODO z will cause trouble?
		rzp.setLocalTranslation(pos); //above DevCons
		
		Button btn = new Button(strInfo);
		rzp.setContents(btn);
		DragParentestPanelListenerI.i().applyAt(btn);
		
		return rzp;
	}
}
