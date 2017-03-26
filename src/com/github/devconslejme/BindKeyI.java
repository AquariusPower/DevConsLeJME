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

package com.github.devconslejme;

import java.util.ArrayList;

import com.jme3.input.KeyInput;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;
import com.simsilica.lemur.event.KeyModifiers;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class BindKeyI {
	private static BindKeyI instance = new BindKeyI();
	/**instance*/ public static BindKeyI i(){return instance;}
	
	private ArrayList<BindKey> abkList = new ArrayList<BindKey>();
	private KeyActionListener	kal;
	
	public BindKeyI() {
		JavaScriptI.i().setJSBinding(this);
	}
	
	public static class BindKey{
		KeyActionListener	kalAction = null;
		
		Integer iCode = null;
		int[] aiModifiers = null;
		
		String strName = null;
		String strModifiers = "";
		
		String strHelpContext = null;
		String strHelp = null;
		
		public String getHelp() {
			return strModifiers+strName+": "+strHelp;
		}
	}
	
	public void prepareKeyMappings() {
		kal = new KeyActionListener() {
			@Override
			public void keyAction(TextEntryComponent source, KeyAction key) {
				boolean bControl = key.hasModifier(KeyModifiers.CONTROL_DOWN);
				boolean bShift = key.hasModifier(KeyModifiers.SHIFT_DOWN);
				boolean bAlt = key.hasModifier(KeyModifiers.ALT_DOWN);
				
				switch(key.getKeyCode()){
					case KeyInput.KEY_C: 
						if(bControl)ClipboardI.i().copyToClipboard(LoggingI.i().getSelectedEntry());
						break;
					case KeyInput.KEY_ESCAPE: 
						ConsolePluginI.i().closeConsole();
						break;
					case KeyInput.KEY_V: 
						if(bControl)ClipboardI.i().pasteFromClipboard(true);
						break;
					case KeyInput.KEY_X: 
						if(bControl)ClipboardI.i().cutSelectedLogEntryToClipboard();
						break;
					case KeyInput.KEY_NUMPADENTER:
					case KeyInput.KEY_RETURN:
						JavaScriptI.i().submitUserCommand();
						break;
					case KeyInput.KEY_TAB:
						ConsolePluginI.i().autoComplete();
						break;
					case KeyInput.KEY_DELETE:
						if(bControl)ConsolePluginI.i().clearInput();
						break;
					case KeyInput.KEY_LEFT:
						if(bControl)ConsolePluginI.i().navigateWord(false);
						break;
					case KeyInput.KEY_RIGHT:
						if(bControl)ConsolePluginI.i().navigateWord(true);
						break;
				}
			}
		};
		
		bindKey("Close", KeyInput.KEY_ESCAPE);
		
		bindKey("copy", KeyInput.KEY_C,KeyModifiers.CONTROL_DOWN);
		bindKey("cut", KeyInput.KEY_X,KeyModifiers.CONTROL_DOWN);
		bindKey("paste", KeyInput.KEY_V,KeyModifiers.CONTROL_DOWN);
		
		bindKey("autocomplete \"starts with\"", KeyInput.KEY_TAB);
		bindKey("autocomplete \"contains\"", KeyInput.KEY_TAB, KeyModifiers.CONTROL_DOWN);
		bindKey("submit command", KeyInput.KEY_RETURN);
		bindKey("submit command", KeyInput.KEY_NUMPADENTER);
		bindKey("", KeyInput.KEY_B,KeyModifiers.CONTROL_DOWN);
		bindKey("clear input field", KeyInput.KEY_DELETE,KeyModifiers.CONTROL_DOWN);
		bindKey("\"/\" toggle input field comment", KeyInput.KEY_SLASH,KeyModifiers.CONTROL_DOWN);
		
		bindKey("navigate to previous word", KeyInput.KEY_LEFT,KeyModifiers.CONTROL_DOWN);
		bindKey("navigate to next word", KeyInput.KEY_RIGHT,KeyModifiers.CONTROL_DOWN);
		
		// cmd history select action
		KeyActionListener actCmdHistoryEntrySelectAction = new KeyActionListener() {
			@Override
			public void keyAction(TextEntryComponent source, KeyAction key) {
				ConsolePluginI.i().navigateCmdHist(key.getKeyCode());
			}
		};
		ConsolePluginI.i().putActionMapAtInputField(new KeyAction(KeyInput.KEY_UP), actCmdHistoryEntrySelectAction);
		ConsolePluginI.i().putActionMapAtInputField(new KeyAction(KeyInput.KEY_DOWN), actCmdHistoryEntrySelectAction);
		
		// scroll actions
		KeyActionListener actDumpNavigate = new KeyActionListener() {
			@Override
			public void keyAction(TextEntryComponent source, KeyAction key) {
				boolean bControl = key.hasModifier(KeyModifiers.CONTROL_DOWN); //0x1
				double dCurrent = ConsolePluginI.i().getScrollDumpAreaFlindex();
				double dAdd = 0;
				switch(key.getKeyCode()){
					case KeyInput.KEY_PGUP:
						dAdd = -ConsolePluginI.i().getShowRowsAmount();
						break;
					case KeyInput.KEY_PGDN:
						dAdd = +ConsolePluginI.i().getShowRowsAmount();
						break;
					case KeyInput.KEY_HOME:
						if(bControl)dAdd = -dCurrent;
						break;
					case KeyInput.KEY_END:
						if(bControl)dAdd = LoggingI.i().getLogEntriesSize();
						break;
					case KeyInput.KEY_UP:
						if(bControl)dAdd = -1;
						break;
					case KeyInput.KEY_DOWN:
						if(bControl)dAdd = 1;
						break;
				}
				double dSet = dCurrent + dAdd;
				if(dSet<0.0)dSet=0.0;
				ConsolePluginI.i().scrollTo(dSet);
//				ConsolePluginI.i().scrollToBottom();
			}
		};
		bindKey(actDumpNavigate,"navigate dump area to previous line",KeyInput.KEY_UP, KeyModifiers.CONTROL_DOWN);
		bindKey(actDumpNavigate,"navigate dump area to next line",KeyInput.KEY_DOWN, KeyModifiers.CONTROL_DOWN);
		bindKey(actDumpNavigate,"navigate dump area to previous page",KeyInput.KEY_PGUP);
		bindKey(actDumpNavigate,"navigate dump area to next page",KeyInput.KEY_PGDN);
		bindKey(actDumpNavigate,"navigate dump area to first entry",KeyInput.KEY_HOME, KeyModifiers.CONTROL_DOWN);
		bindKey(actDumpNavigate,"navigate dump area to last/current entry",KeyInput.KEY_END, KeyModifiers.CONTROL_DOWN);
	}

	private BindKey bindKey(String strActionPerformedHelp, int iKeyCode, int... aiKeyModifiers){
		return bindKey(kal,strActionPerformedHelp, iKeyCode, aiKeyModifiers);
	}

	/**
	 * 
	 * @param strActionPerformedHelp the key code's field name will be reflexed
	 * @param kal
	 * @param iKeyCode
	 * @param aiKeyModifiers
	 * @return better if stored into some kind of array for further line formatting 
	 */
	public BindKey bindKey(KeyActionListener kal, String strActionPerformedHelp, int iKeyCode, int... aiKeyModifiers){
		KeyAction ka = new KeyAction(iKeyCode, aiKeyModifiers);
		ConsolePluginI.i().putActionMapAtInputField(ka,kal);
		
		BindKey bk = new BindKey();
		
		bk.kalAction = kal;
		
		bk.iCode = iKeyCode;
		//TODO? bk.strName = MiscI.i().getFieldNameForValue(new DummyKeyInput(), iKeyCode, "/", "KEY_", true);
		
		bk.strHelp = strActionPerformedHelp;
		
		bk.aiModifiers = aiKeyModifiers;
		
		for(int i:aiKeyModifiers){
			if(i==KeyModifiers.CONTROL_DOWN){
				bk.strModifiers += "Ctrl+";
			}else
			if(i==KeyModifiers.SHIFT_DOWN){
				bk.strModifiers += "Shift+";
			}else
			if(i==KeyModifiers.ALT_DOWN){
				bk.strModifiers += "Alt+";
			}else{
				bk.strModifiers += "KEYMOD_"+i+"+";
			}
		}
		
		abkList.add(bk);
		
		return bk;
	}
}
