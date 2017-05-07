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

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import com.github.devconslejme.gendiag.ContextMenuI.ContextButton;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu.ApplyContextChoiceCmd;
import com.github.devconslejme.gendiag.SimpleGenericDialog.CmdCfg;
import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction.CmdBtnTA;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavadocI;
import com.github.devconslejme.misc.MethodHelp;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.StringI.EStringMatchMode;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.core.VersionedReference;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ConvertMethodsToOptions {
//	public static MethodsManagerI i(){return GlobalManagerI.i().get(MethodsManagerI.class);}
	
	private SimpleGenericDialog	diag;
	private boolean	bMatchIgnoreCase = true;
	private boolean	bShowInherited;
	private boolean	bShowOnlyEditableBeans = true;
	private boolean	bRegexFilter=true;
	private String	strUserInputTextFilter="";
	private EStringMatchMode	eStringMatchMode = EStringMatchMode.Contains;

	public ConvertMethodsToOptions(SimpleGenericDialog diag) {
		this.diag=diag;
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				init();
				return true;
			}
		});
	}
	
	private void init(){
		diag.putToolAction(new ToolAction("Methods from", new CmdBtnTA() {
			@Override	public Integer executeTA(Button source) {
				diag.requestUpdateListItems();
				return (bShowInherited=!bShowInherited)?0:1;
			}
		}).setMultiStatusMode(bShowInherited?0:1,"concrete","inherited too"));
		
		diag.putToolAction(new ToolAction("Method kind", new CmdBtnTA() {
			@Override	public Integer executeTA(Button btn) {
				diag.requestUpdateListItems();
				return (bShowOnlyEditableBeans=!bShowOnlyEditableBeans)?0:1;
			}
		}).setMultiStatusMode(bShowOnlyEditableBeans?0:1,"all","only beans"));
		
		// user filter
		ApplyContextChoiceCmd cmd = new ApplyContextChoiceCmd() {
			@Override
			public void executeContextCommand(ContextButton cbSource) {
				eStringMatchMode = (EStringMatchMode) cbSource.getStoredValue();
			}
		};
		ContextMenu cm = ContextMenuI.i().createStringRegexOptContextMenu(
				diag.getDialog(), eStringMatchMode, cmd);
		CmdBtnTA cmdbta = new CmdBtnTA() {
			@Override	public Integer executeTA(Button btn) {
				diag.requestUpdateListItems();
	//			return (bRegexFilter=!bRegexFilter)?0:1;
				return (bRegexFilter=!bRegexFilter)?1:0;
			}
		};
		ToolAction ta = new ToolAction("User filter",cmdbta)
			.setMultiStatusMode(bRegexFilter?0:1,"enable","disable")
			.setContextMenu(cm);
		diag.putToolAction(ta);
		
	}

	public int createOptionFromMethods(OptionData odParentSection, Object objToExtractMethodsFrom) {
		Method[] am = isShowInherited() ? 
			objToExtractMethodsFrom.getClass().getMethods() : 
			objToExtractMethodsFrom.getClass().getDeclaredMethods();
			
		int iValidCount=0;
		for(Method m:am){
			if(!Modifier.isPublic(m.getModifiers()))continue; //skip non public
			if(bShowOnlyEditableBeans && !JavaLangI.i().isBeanGetter(m))continue;
			
			MethodHelp mh = new MethodHelp().setObject(objToExtractMethodsFrom).setMethod(m);
			
			String strTextKey = mh.getFullHelp(true, false);
//				if(bRegexFilter && !strTextKey.matches(smd.getInputText()))continue;
			if(
					bRegexFilter
					&& 
					!StringI.i().contains(
						strTextKey, getUserInputTextFilter(), getEStringMatchMode(), isMatchIgnoreCase())
			)continue;
			
			OptionData od = diag.putOption(odParentSection,	strTextKey, mh);
			
			od.addCmdCfg(new CmdCfg("JavaDoc") {@Override	public void execute(Button source) {
					JavadocI.i().browseJavadoc(mh);
				}});
			
			od.addCmdCfg(new CmdCfg("Cp") {@Override	public void execute(Button source) {
					JavaLangI.i().copyToClipboard(mh.getFullHelp(true, true));
				}}.setHintHelp("copy to clipboard"));
			
			iValidCount++;
		}
		
		return iValidCount;
	}
	
	public boolean isShowInherited() {
		return bShowInherited;
	}
	
	public void setShowInherited(boolean bShowInherited) {
		this.bShowInherited = bShowInherited;
	}

	public boolean isShowOnlyEditableBeans() {
		return bShowOnlyEditableBeans;
	}
	
	public void setShowOnlyEditableBeans(boolean bShowOnlyEditableBeans) {
		this.bShowOnlyEditableBeans = bShowOnlyEditableBeans;
	}

	public EStringMatchMode getEStringMatchMode() {
		return eStringMatchMode;
	}
	
	public void setEStringMatchMode(EStringMatchMode eStringMatchMode) {
		this.eStringMatchMode = eStringMatchMode;
	}

	public boolean isMatchIgnoreCase() {
		return bMatchIgnoreCase;
	}

	public void setMatchIgnoreCase(boolean bMatchIgnoreCase) {
		this.bMatchIgnoreCase = bMatchIgnoreCase;
	}

	public String getUserInputTextFilter() {
		return strUserInputTextFilter;
	}

	public void setUserInputTextFilter(String strUserInputTextFilter) {
		this.strUserInputTextFilter = strUserInputTextFilter;
	}
}
