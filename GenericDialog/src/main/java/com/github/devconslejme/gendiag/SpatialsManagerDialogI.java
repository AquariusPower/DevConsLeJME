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

import com.github.devconslejme.gendiag.DialogHierarchyStateI.DialogVisuals;
import com.github.devconslejme.gendiag.DialogHierarchyStateI.IGUIUserInteraction;
import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.app.SimpleApplication;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class SpatialsManagerDialogI implements IGUIUserInteraction{
	public static SpatialsManagerDialogI i(){return GlobalManagerI.i().get(SpatialsManagerDialogI.class);}
	
	private SimpleMaintenanceGenericDialog	diagMaint;
	private SimpleApplication	sappOpt;
	private String	strFilter="";
	
	public void configure(Node guiNode, Node rootNode) {
		sappOpt = GlobalManagerI.i().get(SimpleApplication.class);
		
		diagMaint = new SimpleMaintenanceGenericDialog(SpatialsManagerDialogI.class.getSimpleName()){
			@Override
			public void updateMaintenanceList() {
				recursiveAddSpatialsToMaintenance(null,guiNode);
				recursiveAddSpatialsToMaintenance(null,rootNode);
			}
		};
		diagMaint.setTitle(SpatialsManagerDialogI.class.getSimpleName());
//		if(!diagMaint.isInitialized())return false; //prior to new actions below
		
		diagMaint.addListOptionsUserInteractionListener(this);
		
//		DialogHierarchyStateI.i().showDialog(diagMaint.getDialog());
	}
	
	/**
	 * From(Spatial) -> To(OptionData)
	 * @param odParent
	 * @param spt
	 */
	private void recursiveAddSpatialsToMaintenance(OptionData odParent,Spatial spt) {
		String strKey = "#"+spt.hashCode()+",'"+spt.getName()+"' ("+spt.getClass().getSimpleName()+")";
		
		if(strFilter.isEmpty()){
			if(spt instanceof Node){
				/**
				 * create a section and update the parent for new childs
				 */
				odParent = diagMaint.putSection(odParent, strKey);
				strKey="#000<<<NodeSelfCfg>>> "+strKey; //#000 is to sort on top
			}
		}
		
		/**
		 * as the node is also a spatial, the key will be repeated inside of it,
		 * will look like a child, but is actually a self reference to be further
		 * used.
		 */
		if(strFilter.isEmpty() || (spt.getName()!=null && spt.getName().contains(strFilter))){
			OptionData od = diagMaint.putOption(odParent, strKey, spt);
//			od.addCmdCfg(new CmdCfg() {
//				@Override
//				public void execute(Button source) {
//					
//				}
//			});
		}
		
		if(spt instanceof Node){
			Node node = (Node)spt;
			for(Spatial sptChild:node.getChildren()){
				recursiveAddSpatialsToMaintenance(odParent,sptChild);
			}
		}
	}
	
	@Override
	public void receiveSubmitedUserInputTextEvent(DialogVisuals vs,String str) {
		this.strFilter=str;
		diagMaint.requestUpdateListItems();
	}

	public void show(){
		DialogHierarchyStateI.i().showDialog(diagMaint.getDialog());
	}

	@Override public void receiveLastClickedItemStoredValueEvent(DialogVisuals vs,Object obj) {}
	
}