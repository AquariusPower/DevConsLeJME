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
import java.util.Map.Entry;
import java.util.TreeMap;

import com.github.devconslejme.devcons.ClipboardI;
import com.github.devconslejme.gendiag.SimpleGenericDialog.CmdCfg;
import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction;
import com.github.devconslejme.gendiag.SimpleGenericDialog.ToolAction.CmdBtnTA;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavadocI;
import com.github.devconslejme.misc.MethodHelp;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.simsilica.lemur.Button;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GlobalsManagerDialogI {
	public static GlobalsManagerDialogI i(){return GlobalManagerI.i().get(GlobalsManagerDialogI.class);}

	private SimpleMaintenanceGenericDialog	smd;
	private boolean	bShowInherited;
	private boolean	bShowPackagesPrepended;
	private boolean	bShowOnlyEditableBeans = true;
//	private Comparator<Object>	cmprAtoZ = new Comparator<Object>() {
//		@Override
//		public int compare(Object o1, Object o2) {
//			return o1.getClass().;
//		}
//	};
	
	public GlobalsManagerDialogI(){}
	
	public void configure(){
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(smd==null){
					smd = new SimpleMaintenanceGenericDialog() {
						@Override
						public void updateMaintenanceList() {
							prepareGlobalsForMaintenance();
						}
					};
				}
				
				if(!smd.isInitialized())return false;
				
				smd.putToolAction(new ToolAction("Methods from", new CmdBtnTA(bShowInherited,"concrete","inherited too") {
					@Override
					public Boolean executeTA(Button source) {
//						updateStatus(bShowInherited=!bShowInherited);
						smd.requestUpdateListItems();
//						PopupHintHelpListenerI.i().setPopupHintHelp(source, 
//								bShowInherited?"only of concrete class":"show all inherited too"); //next action
						return bShowInherited=!bShowInherited;
					}
				}));
				
				smd.putToolAction(new ToolAction("Pkg info", new CmdBtnTA(bShowPackagesPrepended,"after","prepend") {
					@Override
					public Boolean executeTA(Button source) {
//						updateStatus(bShowPackagesPrepended=!bShowPackagesPrepended);
						smd.requestUpdateListItems();
//						PopupHintHelpListenerI.i().setPopupHintHelp(source, 
//								bShowPackagesPrepended?"put after":"prepend"); //next action
						return bShowPackagesPrepended=!bShowPackagesPrepended;
					}
				}));
				
				smd.putToolAction(new ToolAction("Method Kind", new CmdBtnTA(bShowOnlyEditableBeans,"all","only beans") {
					@Override
					public Boolean executeTA(Button btn) {
//						updateStatus(bShowOnlyEditableBeans=!bShowOnlyEditableBeans);
						smd.requestUpdateListItems();
						
//						updateText();
						
//						PopupHintHelpListenerI.i().setPopupHintHelp(source, 
//							bShowOnlyEditableBeans?"show all":"show only beans"); //next action
						return bShowOnlyEditableBeans=!bShowOnlyEditableBeans;
					}

//					@Override
//					protected void updateText(Button btn) {
//						btn.setText(btn.getText().split(":")[0]+": "+
//								(bShowOnlyEditableBeans?"all":"only beans") ); //next action
//					}
				}));
				
				return true;
			}
		});
	}
	
	public void show(){
		DialogHierarchyStateI.i().showDialog(smd.getDialog());
	}
	
//	private static class MethodInfo{
//		Object o;
//		Method m;
//		public MethodInfo(Object o, Method m) {
//			super();
//			this.o = o;
//			this.m = m;
//		}
//		
//	}
	
	private TreeMap<String,Object> hmSortedGlobals = new TreeMap<String,Object>(String.CASE_INSENSITIVE_ORDER);
	
	protected void prepareGlobalsForMaintenance() {
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
		
//		ArrayList<Object> aobjList = GlobalManagerI.i().getListCopy();
//		Collections.sort(aobjList,cmprAtoZ);
		
//		for(Object o:aobjList){
		for(Entry<String, Object> entry:hmSortedGlobals.entrySet()){
//			String str = entry.getKey();
//			System.out.println(entry.getKey());
			
			OptionData odGlobal = smd.putSection(null,entry.getKey());
			
			Method[] am = isShowInherited() ? 
				entry.getValue().getClass().getMethods() : 
				entry.getValue().getClass().getDeclaredMethods();
			
			int iValidCount=0;
			for(Method m:am){
				if(!Modifier.isPublic(m.getModifiers()))continue; //skip non public
				if(bShowOnlyEditableBeans && !JavaLangI.i().isBeanGetter(m))continue;
				
				MethodHelp mh = new MethodHelp().setObject(entry.getValue()).setMethod(m);
				
				OptionData od = smd.putOption(odGlobal,	mh.getFullHelp(true, false),	mh);
				
				od.addCmdCfg(new CmdCfg() {@Override	public void execute(Button source) {
						JavadocI.i().browseJavadoc(mh);
					}}.setText("JavaDoc"));
				
				od.addCmdCfg(new CmdCfg() {@Override	public void execute(Button source) {
						ClipboardI.i().copyToClipboard(mh.getFullHelp(true, true));
					}}.setText("Cp").setHintHelp("copy to clipboard"));
				
				iValidCount++;
			}
			
			if(iValidCount==0)smd.remove(odGlobal);
		}
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
	
}
