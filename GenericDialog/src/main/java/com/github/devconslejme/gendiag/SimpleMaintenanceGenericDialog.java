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

import com.github.devconslejme.gendiag.SimpleGenericDialog.CmdCfg;
import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction.CmdBtnTA;
import com.github.devconslejme.misc.JavaLangI.LinkedHashMapX;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavadocI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.MethodHelp;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Panel;

/**
 * TODO move whatever fits at super class to there
 * TODO cell renderer to add entry hierarchy on the left (this can go to super class)
 * TODO cell renderer to add cfg buttons on the right
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public abstract class SimpleMaintenanceGenericDialog extends SimpleGenericDialog {
	public SimpleMaintenanceGenericDialog(String strTitle){
		super(strTitle);
		setCloseOnChoiceMade(false);
	}
	
	private boolean bLastRequestWasCollapsed=false;
	private boolean	bAllowAutoCfgForComplexObjects = true;
	
	@Override
	protected void initSectionTools() {
		super.initSectionTools();
		putToolAction(new ToolAction("Refresh Options", new CmdBtnTA() {@Override	public Integer executeTA(Button btn) {
				requestUpdateListItems();			return null;}}));
		
		/**
		 * two buttons because user may have shrinked or expanded sub sections
		 */
		putToolAction(new ToolAction("Collapse All", new CmdBtnTA() {@Override public Integer executeTA(Button source) {
				collapseAll();		return null;}})); 
		putToolAction(new ToolAction("Expand All", new CmdBtnTA() {@Override	public Integer executeTA(Button source) {
				expandAll();		return null;}}));
		
		requestUpdateListItems(); //1st time
	}
	
	@Override
	public void requestUpdateListItems() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				LinkedHashMapX<String, OptionData> hmBkp = bLastRequestWasCollapsed ? createOptionDataSnapshot() : null;

				SimpleMaintenanceGenericDialog.super.clearOptions();
				updateMaintenanceList();
//				LinkedHashMapX<String, OptionData> hmNewList = createDataSnapshot();
//				for(OptionData odNew:hmNewList.values()){
//					if(!odNew.isSection())continue;
//					OptionData odBkp = hmBkp.get(odNew.getTextKey());
//					if(odBkp!=null)odNew.setExpanded(odBkp.isExpanded());
//				}
				
//				SimpleMaintenanceGenericDialog.super.requestUpdateListItems();
				
				if(bLastRequestWasCollapsed){
					QueueI.i().enqueue(new CallableXAnon() {
						@Override
						public Boolean call() {
							collapseAll();
							
							LinkedHashMapX<String, OptionData> hmNewList = createOptionDataSnapshot();
							for(OptionData odNew:hmNewList.values()){
								if(!odNew.isSection())continue;
								OptionData odBkp = hmBkp.get(odNew.getTextKey());
								/**
								 * re-expand only the ones that were previously individually expanded
								 */
								if(odBkp!=null && odBkp.isExpanded())odNew.setExpanded(true);
							}
							
							SimpleMaintenanceGenericDialog.super.requestUpdateListItems();
							
							return true;
						}
					});
				}else{
					SimpleMaintenanceGenericDialog.super.requestUpdateListItems();
				}
				
				return true;
			}
		});
	}
	
	/**
	 * prefer calling {@link #requestUpdateListItems()}
	 */
	public abstract void updateMaintenanceList();
	
	@Override
	protected boolean isEnableItemConfigurator() {
		return true;
	}

	private void collapseAll() {
		int i=setExpandedAll(false);MessagesI.i().debugInfo(this, "collapsed", i);
		SimpleMaintenanceGenericDialog.super.requestUpdateListItems();
		bLastRequestWasCollapsed=true;
	}
	private void expandAll() {
		int i=setExpandedAll(true);MessagesI.i().debugInfo(this, "expanded", i);
		SimpleMaintenanceGenericDialog.super.requestUpdateListItems();
		bLastRequestWasCollapsed=false;
	}
	
	@Override
	protected Panel automaticConfiguratorCreation(OptionData od) {
		Panel pnl = super.automaticConfiguratorCreation(od);
		
		if(pnl==null){ //not user typeable simple type
			if(isAllowAutoCfgForComplexObjects()){
				automaticConfiguratorCreateAsMethodsOfValue(od);
			}
		}
		
		return pnl;
	}
	
	protected void automaticConfiguratorCreateAsMethodsOfValue(OptionData od) {
		String strKey = "CfgObj";
		if(od.isCmdCfgSet(strKey))return;
		if(od.getStoredValue()==null)return;
//		if(od.getStoredValue().getClass().equals(Object.class))return;
		
		od.addCmdCfg(new CmdCfg(strKey) {
				private SimpleMaintenanceGenericDialog	sgdCfgObj;
				private ConvertMethodsToOptions	optm;

				@Override
				public void execute(Button source) {
					if(sgdCfgObj==null){
						sgdCfgObj = new SimpleMaintenanceGenericDialog(strKey+"/"+od.getStoredValue().getClass().getName()+"/"+od.getStoredValue().hashCode()){
							@Override
							public void updateMaintenanceList() {
								optm.createOptionFromMethods(null, od.getStoredValue());
							}
						};
					}
					
					if(optm==null){
						optm = new ConvertMethodsToOptions(sgdCfgObj);
//						optm.createOptionFromMethods(null, od.getStoredValue()); //initial fill
					}
					
					DialogHierarchyStateI.i().showDialogAsModal(getDialog(), sgdCfgObj.getDialog());
				}
			}//.setText()
			 .setHintHelp("click to configure this complex object")
		);
	}

	public boolean isAllowAutoCfgForComplexObjects() {
		return bAllowAutoCfgForComplexObjects;
	}

	protected SimpleMaintenanceGenericDialog setAllowAutoCfgForComplexObjects(boolean bAllowAutoCfgForComplexObjects) {
		this.bAllowAutoCfgForComplexObjects = bAllowAutoCfgForComplexObjects;
		return this;
	}	
	
}
