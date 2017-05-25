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

package com.github.devconslejme.gendiag;

import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction.CmdBtnTA;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.Key;
import com.github.devconslejme.misc.KeyBind;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.BindCommand;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.TimeFormatI;
import com.simsilica.lemur.Button;

/**
 * TODO list all, allow setting/changing etc
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class KeyBindManagerDialogI {
	public static KeyBindManagerDialogI i(){return GlobalManagerI.i().get(KeyBindManagerDialogI.class);}

	private SimpleMaintenanceGenericDialog	diagBindMan;
	private boolean	bKeybindPrepend = true;
	
	public void configure(){
		diagBindMan = new SimpleMaintenanceGenericDialog(KeyBindManagerDialogI.class.getSimpleName()) {
			@Override
			public void updateMaintenanceList() {
				OptionData odCodeSection = diagBindMan.putSection(null, KeyCodeManagerI.class.getSimpleName());
				for(Key key:KeyCodeManagerI.i().getKeyListCopy()){
					OptionData od = diagBindMan.putOption(odCodeSection, key.getAsInfo(), key);
				}
				
				OptionData odCallcmdSection = diagBindMan.putSection(null, "Commands");
				for(String strCmdId:KeyBindCommandManagerI.i().getHardCommandsIdListCopy()){
					OptionData od = diagBindMan.putOption(odCallcmdSection, strCmdId, strCmdId);
					addCmdCfgCaptureBind("addBind",od,strCmdId);
				}
				
				OptionData odBindSection = diagBindMan.putSection(null, KeyBindCommandManagerI.class.getSimpleName());
				for(BindCommand bc:KeyBindCommandManagerI.i().getKeyBindListCopy()){
					String str="";
					
					str+=bc.getInfo(bKeybindPrepend);
					
					OptionData od = diagBindMan.putOption(odBindSection, str, bc);
					addCmdCfgCaptureBind("changeBind",od,bc);
					
					od.addCmdCfg(new CmdCfg("DelBind") {@Override	public void execute(Button source) {
						//TODO confirmation dialog
						KeyBindCommandManagerI.i().removeKeyBind(bc);
						diagBindMan.requestUpdateListItems();
					}});
				}
			}

			private void addCmdCfgCaptureBind(String strActionText, OptionData od,String strCmdId) {
				BindCommand bc=new BindCommand();
				KeyBindCommandManagerI.i().applyCommandAt(bc,strCmdId);
				KeyBindCommandManagerI.i().applyNewKeyBindHelperAt(bc);
				
				addCmdCfgCaptureBind(strActionText,od,bc);
			}
			private void addCmdCfgCaptureBind(String strActionText, OptionData odbc,BindCommand bc) {
				odbc.addCmdCfg(new CmdCfg(strActionText) {@Override	public void execute(Button source) {
					KeyBindCommandManagerI.i().captureAndSetKeyBindAt(bc, source);
					
					QueueI.i().enqueue(new CallableXAnon() {@Override	public Boolean call() {
						if(KeyBindCommandManagerI.i().isCapturing())return false; //wait capture end
						diagBindMan.requestUpdateListItems();
						return true;
					}});
				}});
			}
		};
		
		diagBindMan.putToolActionLater(new ToolAction("Key Bound", new CmdBtnTA() {
			@Override	public Integer executeTA(Button source) {
				diagBindMan.requestUpdateListItems();
				return (bKeybindPrepend=!bKeybindPrepend)?0:1;
			}
		}).setMultiStatusMode(bKeybindPrepend?0:1,"after","prepend"));
		
	}
	
	public void captureBind(){
		
	}
	
	public void show(){
		DialogHierarchyStateI.i().showDialog(diagBindMan.getDialog());
	}
	
//	public void testCreateBind(){
//		KeyBind kb = new KeyBind();
//		kb.setFromKeyCfg("Ctrl+L");
//		
//		BindCommand bc = new BindCommand();
//		bc.setKeyBind(kb);
//		bc.addHardCommand(new CallBoundKeyCmd(){
//				@Override
//				public Boolean callOnKeyPressed() {
//					MessagesI.i().output(false,System.out,"Info:",this,KeyBindManagerDialogI.class.getSimpleName()+":test:"+TimeFormatI.i().getRealTimeFormatted());
//					return true;
//				}
//			}
//			.holdKeyPressedForContinuousCmd()
//			.setDelaySeconds(1f)
//			.setName("simple test log entry for bind cmd")
//		);
//		
//		KeyBindCommandManagerI.i().putBindCommand(bc);
//	}
}
