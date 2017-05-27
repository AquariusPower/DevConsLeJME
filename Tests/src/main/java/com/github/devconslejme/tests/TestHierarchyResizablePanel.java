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

import com.github.devconslejme.gendiag.DialogHierarchyStateI;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.AbsorbClickCommandsI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.projman.SimpleAppStateAbs;
import com.jme3.app.SimpleApplication;
import com.jme3.math.Vector3f;
import com.simsilica.es.EntityId;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Insets3f;
import com.simsilica.lemur.event.MouseEventControl;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestHierarchyResizablePanel extends SimpleAppStateAbs {
	public static void main(String[] args) {
		TestHierarchyResizablePanel tst = new TestHierarchyResizablePanel();
		tst.setPauseOnLostFocus(false); //good for outside monitoring
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(this, getGuiNode(), getRootNode());
		initTest();
	}
	
	@Override
	public void update(float tpf) {
		for(ResizablePanel rzp:DialogHierarchyStateI.i().getAllOpenedDialogs()){
			if(rzp.getContents() instanceof Button){ //TODO this is a bad guess...
				Button btn = (Button)rzp.getContents();
				BaseTextUD btud = UserDataI.i().getMustExistOrNull(btn, BaseTextUD.class);
				DetailedException.assertNotNull(btud,btn);
				String str = btud.getBaseText();
				btn.setText(str+"\n"
					+DialogHierarchyStateI.i().getHierarchyComp(rzp).toString().replace(",", "\n")
				);
			}
		}
	}
	
	@Override
	public void initTest() {
		super.initTest();
		
		createParentChild(100);
		
		boolean bMore=true;
		if(bMore){
			QueueI.i().enqueue(new CallableX() {
				@Override
				public Boolean call() {
					createParentChild(400);
					return true;
				}
			}.setDelaySeconds(1.0f));
		}
		
		// multi child hierarchy
		if(bMore){
			ResizablePanel rzpA = createDialog(new Vector3f(300,700,0),"MultiA",null);
			DialogHierarchyStateI.i().showDialog(rzpA);
			
			ResizablePanel rzpB = createDialog(new Vector3f(310,710,0),"MultiB",null);
			DialogHierarchyStateI.i().showDialogAsModal(rzpA,rzpB);
			
			ResizablePanel rzpC = createDialog(new Vector3f(320,720,0),"MultiC",null);
			DialogHierarchyStateI.i().showDialogAsModal(rzpA,rzpC);
			
			ResizablePanel rzpD = createDialog(new Vector3f(320,720,0),"MultiD",null);
			DialogHierarchyStateI.i().showDialogAsModal(rzpB,rzpD);
		}
	}

	@SuppressWarnings("unchecked")
	private void createParentChild(int iBaseY) {
		ResizablePanel rzpChild = createDialog(new Vector3f(200,iBaseY+200,20), "child"+iBaseY, "click to close");
		AbsorbClickCommandsI.i().addClickCommands(((Button)rzpChild.getContents()),new Command<Button>(){ //FIXME this is not compatible with CursorListener!
			@Override
			public void execute(Button source) {
				rzpChild.close(); 
			}
		});
		
		ResizablePanel rzpParent = createDialog(new Vector3f(100,iBaseY+100,10), "parent"+iBaseY, "click to open modal");
		AbsorbClickCommandsI.i().addClickCommands(((Button)rzpParent.getContents()),new Command<Button>(){ //FIXME this is not compatible with CursorListener!
			@Override
			public void execute(Button source) { 
				QueueI.i().enqueue(new CallableX() {
					@Override
					public Boolean call() {
						DialogHierarchyStateI.i().showDialogAsModal(rzpParent,rzpChild);
						return true;
					}
				});
			}
		});
		
		DialogHierarchyStateI.i().showDialog(rzpParent);
	}
	
	public static class BaseTextUD{
		private String strBaseText;

		public String getBaseText() {
			return strBaseText;
		}

		public BaseTextUD setBaseText(String strBaseText) {
			this.strBaseText = strBaseText;
			return this; //for beans setter
		}
	}
//	enum EUserData{
//		keyBaseText,
//		;
//		public String s(){return toString();}
//	}
	
	private ResizablePanel createDialog(Vector3f pos,String strName,String strInfo) {
		if(strInfo==null)strInfo=strName;
		
		ResizablePanel rzp = DialogHierarchyStateI.i().prepareDialogParts(strName,null).getDialog();
		
		rzp.setPreferredSizeWH(new Vector3f(300,250,0)); //TODO z will cause trouble?
		rzp.setLocalTranslationXY(pos); //above DevCons
		
		String strBaseText=strName+"/"+strInfo;
		Button btn = new Button(strBaseText);
		UserDataI.i().putSafelyMustNotExist(btn, new BaseTextUD().setBaseText(strBaseText));
		rzp.setContents(btn);
//		btn.setInsets(new Insets3f(10, 0, 0, 0));
//		DragParentestPanelListenerI.i().applyAt(rzp);
//		/**
//		 * removing the MouseEventControl, will prevent the click commands from working,
//		 */
//		btn.removeControl(MouseEventControl.class);
		DragParentestPanelListenerI.i().applyAt(btn);
		
		return rzp;
	}
	
}
