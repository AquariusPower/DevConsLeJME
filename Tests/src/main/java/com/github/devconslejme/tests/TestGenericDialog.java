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

import com.github.devconslejme.devcons.DevConsGlobalsI;
import com.github.devconslejme.gendiag.SimpleGenericDialogState;
import com.github.devconslejme.gendiag.GenericDialogState.CfgParams;
import com.github.devconslejme.misc.MiscLibI;
import com.github.devconslejme.misc.QueueStateI;
import com.github.devconslejme.misc.QueueStateI.CallableX;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.style.BaseStyles;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestGenericDialog extends SimpleApplication {
	public static void main(String[] args) {
		TestGenericDialog tst = new TestGenericDialog();
		tst.start();
	}

	private SimpleGenericDialogState	diag;
	private Button	btnChosenOption;
	
	@Override
	public void simpleInitApp() {
		GuiGlobals.initialize(this);
		BaseStyles.loadGlassStyle();
		
		DevConsGlobalsI.i().put(Application.class,this);
		MiscLibI.i().configure(this);

		prepareDialog();
		prepareButtonCallsDiag();
	}
	
	@SuppressWarnings("unchecked")
	private void prepareButtonCallsDiag() {
		btnChosenOption = new Button("Click to change option",BaseStyles.GLASS);
		btnChosenOption.addClickCommands(new Command<Button>(){
			@Override
			public void execute(Button source) {
				diag.setEnabled(true);
//				getStateManager().getState(PopupState.class).showPopup(
//						diag.getMainResizablePanel(), ClickMode.Consume, null, ColorRGBA.Blue);
			}
		});
		btnChosenOption.setLocalTranslation(200, 230, 0);
		getGuiNode().attachChild(btnChosenOption);
//		getStateManager().getState(PopupState.class).showPopup(btnChosenOption, ClickMode.Consume, null, ColorRGBA.Red);
	}

	private void prepareDialog() {
		diag = new SimpleGenericDialogState(this);
		
		diag.configure(
			new CfgParams()
				.setNodeParent(getGuiNode())
				.setStyle(BaseStyles.GLASS)
				.setPos(new Vector3f(100,550,10))
				.setSize(new Vector3f(600,500,0))
		);
		
		diag.setTextInfo("This is a good info about something.\nSecond line.");
		diag.setUseInputTextValue(true);
		
		QueueStateI.i().enqueue(new CallableX(0,false) {
			@Override
			public Boolean call() {
				diag.getMainResizablePanel().setUseBumpResizableBorderMode(true);
				return true;
			}
		});
		
		diag.putOption("option A", 10);
		diag.putOption("option B", "This is option B");
		
		String str="option C";
		diag.putOption(str, true);
		diag.putOption(str, false); //test overwrite option return value
	}

	@Override
	public void update() {
		super.update();
		
//		if(btnChosenOption.getText().isEmpty()){
			if(!diag.isEnabled()){
				Object objSelectedOption = diag.collectSelectedOption();
				if(objSelectedOption!=null){
					btnChosenOption.setText("Chosen="+objSelectedOption);
				}
			}
//		}
	}
}
