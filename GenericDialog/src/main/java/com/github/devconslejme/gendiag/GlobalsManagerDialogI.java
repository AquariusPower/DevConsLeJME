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

import java.util.Map.Entry;
import java.util.TreeMap;

import com.github.devconslejme.gendiag.SimpleGenericDialog.IUserTextInputSubmited;
import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction.CmdBtnTA;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.core.VersionedReference;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GlobalsManagerDialogI implements IUserTextInputSubmited{
	public static GlobalsManagerDialogI i(){return GlobalManagerI.i().get(GlobalsManagerDialogI.class);}

	private TreeMap<String,Object> hmSortedGlobals = new TreeMap<String,Object>(String.CASE_INSENSITIVE_ORDER);
	
	private SimpleMaintenanceGenericDialog	diagMaint;
	private boolean	bShowPackagesPrepended;
	private ConvertMethodsToOptions	metho;
	
	public GlobalsManagerDialogI(){}
	
	public void configure(){
		QueueI.i().enqueue(new CallableXAnon() {
			private VersionedReference<String>	vfNewInputTextFilterSubmitted;

			@Override
			public Boolean call() {
				if(diagMaint==null){
					diagMaint=new SimpleMaintenanceGenericDialog(GlobalsManagerDialogI.class.getSimpleName()){
						@Override public void updateMaintenanceList(){
							recreateGlobalsListForMaintenance();
						}
					};
				}
				if(!diagMaint.isInitialized())return false; //prior to new actions below
				
//				MiscJmeI.i().addToName(diagMaint.getDialog(), GlobalsManagerDialogI.class.getSimpleName(), true);
				
				diagMaint.setTitle(GlobalsManagerDialogI.class.getSimpleName());
				
//				diagMaint.setAllowAutoCfgForComplexObjects(false); //will be handled here
				metho = new ConvertMethodsToOptions(diagMaint);
				
				diagMaint.addUserInputTextSubmittedListener(GlobalsManagerDialogI.this);
				
//				diagMaint.putToolAction(new ToolAction("Methods from", new CmdBtnTA() {
//					@Override	public Integer executeTA(Button source) {
//						diagMaint.requestUpdateListItems();
//						return (bShowInherited=!bShowInherited)?0:1;
//					}
//				}).setMultiStatusMode(bShowInherited?0:1,"concrete","inherited too"));
				
				diagMaint.putToolAction(new ToolAction("Pkg info", new CmdBtnTA() {
					@Override	public Integer executeTA(Button source) {
						diagMaint.requestUpdateListItems();
						return (bShowPackagesPrepended=!bShowPackagesPrepended)?0:1;
					}
				}).setMultiStatusMode(bShowPackagesPrepended?0:1,"after","prepend"));
				
//				diagMaint.putToolAction(new ToolAction("Method kind", new CmdBtnTA() {
//					@Override	public Integer executeTA(Button btn) {
//						diagMaint.requestUpdateListItems();
//						return (bShowOnlyEditableBeans=!bShowOnlyEditableBeans)?0:1;
//					}
//				}).setMultiStatusMode(bShowOnlyEditableBeans?0:1,"all","only beans"));
				
//				// user filter
//				ApplyContextChoiceCmd cmd = new ApplyContextChoiceCmd() {
//					@Override
//					public void executeContextCommand(ContextButton cbSource) {
//						eStringMatchMode = (EStringMatchMode) cbSource.getStoredValue();
//					}
//				};
//				ContextMenu cm = ContextMenuI.i().createStringRegexOptContextMenu(
//						diagMaint.getDialog(), eStringMatchMode, cmd);
//				CmdBtnTA cmdbta = new CmdBtnTA() {
//					@Override	public Integer executeTA(Button btn) {
//						diagMaint.requestUpdateListItems();
////						return (bRegexFilter=!bRegexFilter)?0:1;
//						return (bRegexFilter=!bRegexFilter)?1:0;
//					}
//				};
//				ToolAction ta = new ToolAction("User filter",cmdbta)
//					.setMultiStatusMode(bRegexFilter?0:1,"enable","disable")
//					.setContextMenu(cm);
//				diagMaint.putToolAction(ta);
				
				vfNewInputTextFilterSubmitted = diagMaint.createInputTextSubmitedVersionedReference();
				
				return true;
			}
		});
	}
	
	public void show(){
		DialogHierarchyStateI.i().showDialog(diagMaint.getDialog());
	}
	
	protected void recreateGlobalsListForMaintenance() {
		hmSortedGlobals.clear();
		for(Object o:GlobalManagerI.i().getListCopy()){
			String strKey = o.getClass().getSimpleName();
			String strPkg = o.getClass().getPackage().getName();
			if(bShowPackagesPrepended){
				strKey=strPkg+"."+strKey;
			}else{
				strKey+=" <"+strPkg+"> ";
			}
			
			hmSortedGlobals.put(strKey, o);
		}
		
		for(Entry<String, Object> entry:hmSortedGlobals.entrySet()){
			OptionData odGlobal = diagMaint.putSection(null,entry.getKey());
			if(metho.createOptionFromMethods(odGlobal, entry.getValue())==0){
				diagMaint.remove(odGlobal);
			}
		}
	}
	
	@Override
	public void receiveSubmitedUserInputTextEvent(String str) {
		metho.setUserInputTextFilter(str);
		diagMaint.requestUpdateListItems();
	}

}
