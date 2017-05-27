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

import com.github.devconslejme.gendiag.SimpleGenericDialog.IUserInteraction;
import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.StringI.EStringMatchMode;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.simsilica.lemur.core.VersionedReference;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class QueueManagerDialogI implements IUserInteraction{
	public static QueueManagerDialogI i(){return GlobalManagerI.i().get(QueueManagerDialogI.class);}
	
	private SimpleMaintenanceGenericDialog	diagMaint;
	private String	strFilter="";
	
	public void configure(){
		QueueI.i().enqueue(new CallableXAnon() {
			private VersionedReference<String>	vfNewInputTextFilterSubmitted;

			@Override
			public Boolean call() {
				if(diagMaint==null){
					diagMaint = new SimpleMaintenanceGenericDialog(QueueManagerDialogI.class.getSimpleName()) {
						@Override
						public void updateMaintenanceList() {
							recreateList();
						}
					};
				}
				diagMaint.setTitle(QueueManagerDialogI.class.getSimpleName());
				
				diagMaint.addListOptionsUserInteractionListener(QueueManagerDialogI.this);
//				MiscJmeI.i().addToName(diagMaint.getDialog(), GlobalsManagerDialogI.class.getSimpleName(), true);
				
				return true;
			}
		}.setName("configure"));
	}
	
	private void recreateList() {
		OptionData odOnce = diagMaint.putSection(null, "RunOnce");
		OptionData odLoop = diagMaint.putSection(null, "Loop");
		
		for(CallableX cx:QueueI.i().getQueueCopy()){
			String strText=cx.getInfoText();
			if(!StringI.i().contains(strText, strFilter, EStringMatchMode.Contains, true))continue;
			diagMaint.putOption(cx.isLoopMode() ? odLoop : odOnce, strText, cx);
		}
	}

	@Override
	public void receiveSubmitedUserInputTextEvent(String str) {
		setUserInputTextFilter(str);
		diagMaint.requestUpdateListItems();
	}

	private void setUserInputTextFilter(String str) {
		this.strFilter = str;
	}

	public void show(){
		DialogHierarchyStateI.i().showDialog(diagMaint.getDialog());
	}

	@Override public void receiveLastClickedItemStoredValueEvent(Object obj) {}
}

