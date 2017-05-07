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

import org.lwjgl.opengl.Display;

import com.github.devconslejme.gendiag.DialogHierarchyStateI;
import com.github.devconslejme.gendiag.SimpleGenericDialog;
import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.jme3.app.SimpleApplication;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestChoiceDialog extends SimpleApplication {
	public static void main(String[] args) {
		TestChoiceDialog tst = new TestChoiceDialog();
		tst.start();
	}

//	private ResizablePanel	diag;
	private Button	btnChosenOption;
	private SimpleGenericDialog	gdc;
	
	@Override
	public void simpleInitApp() {
//		ConfigureTestsI.i().configure(this, getGuiNode());
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(this, getGuiNode());
		
		initTest();
	}
	
	public void initTest() {
		prepareDialog();
		prepareButtonThatCallsDiag();
	}

	@SuppressWarnings("unchecked")
	private void prepareButtonThatCallsDiag() {
		ResizablePanel diagParent = DialogHierarchyStateI.i().createDialog("main", null);
		
		btnChosenOption = new Button("Click to change option");
		btnChosenOption.addClickCommands(new Command<Button>(){
			@Override
			public void execute(Button source) {
				DialogHierarchyStateI.i().showDialogAsModal(diagParent, gdc.getDialog());
			}
		});
		diagParent.setContents(btnChosenOption);
		diagParent.setLocalTranslation(200, Display.getHeight()-100, 0);
		
		DialogHierarchyStateI.i().showDialog(diagParent);
		
		DragParentestPanelListenerI.i().applyAt(btnChosenOption);
//		getGuiNode().attachChild(btnChosenOption);
//		getStateManager().getState(PopupState.class).showPopup(btnChosenOption, ClickMode.Consume, null, ColorRGBA.Red);
	}

	private void prepareDialog() {
		gdc = new SimpleGenericDialog(TestChoiceDialog.class.getSimpleName());
//		gdc = new SimpleGenericDialog(DialogHierarchyStateI.i().createDialog("options", null));
		gdc.getDialog().setLocalTranslationXY(new Vector3f(100,550,0)).setLocalTranslationZ(10);
		gdc.getDialog().setPreferredSize(new Vector3f(600,500,0));
		
		gdc.setTextInfo("This could be a good info about something.\n"+
				"+1 line.\n"+
				"+1 line.\n"+
				"+1 line.\n"+
				"+1 line.\n"+
				"+1 line.\n"+
				""
		);
		/*
		gdc.setCloseOnChoiceMade(false);
		gdc.setReturnJustTheInputTextValue(true);
		*/
		
		gdc.putOption(null, "option A", 10);
		gdc.putOption(null, "option B", "This is option B");
		
		String str="option C";
		gdc.putOption(null, str, true);
		gdc.putOption(null, str, false); //test overwrite option return value
		
		OptionData od1 = gdc.putSection(null, "SubSection1");
		gdc.putOption(od1, "Option D", 100.35f);
		
		OptionData od1d1 = gdc.putSection(od1, "SubSection1.1");
		gdc.putOption(od1d1, "Option E", 43);
	}

	@Override
	public void update() {
		super.update();
		
		if(!gdc.getDialog().isOpened() && gdc.isOptionSelected()){
			Object objSelectedOption = gdc.extractSelectedOption();
			btnChosenOption.setText("Chosen="+objSelectedOption);
		}
	}
}
