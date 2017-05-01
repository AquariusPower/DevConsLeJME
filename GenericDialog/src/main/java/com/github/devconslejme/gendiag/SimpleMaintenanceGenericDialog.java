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

import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;

/**
 * TODO move whatever fits at super class to there
 * TODO cell renderer to add entry hierarchy on the left (this can go to super class)
 * TODO cell renderer to add cfg buttons on the right
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public abstract class SimpleMaintenanceGenericDialog extends SimpleGenericDialog {
	public SimpleMaintenanceGenericDialog(){
		super();
		setCloseOnChoiceMade(false);
	}
	
	@Override
	protected void initSectionTools() {
		super.initSectionTools();
		putToolAction(new ToolAction("Refresh Options", new Command<Button>() {
			@Override
			public void execute(Button source) {
//				updateMaintenanceListFull();
				requestUpdateListItems();
			}
		}));
		
		putToolAction(new ToolAction("Collapse All", new Command<Button>() {
			@Override
			public void execute(Button source) {
				setExpandedAll(false);
				requestUpdateListItems();//recreateListItems();
			}
		}));
		
		putToolAction(new ToolAction("Expand All", new Command<Button>() {
			@Override
			public void execute(Button source) {
				setExpandedAll(true);
				requestUpdateListItems();//recreateListItems();
			}
		}));
		
		requestUpdateListItems(); //1st time
	}
	
//	public void requestUpdateMaintenanceListFull(){
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
////				updateMaintenanceListFull();
//				
//				SimpleMaintenanceGenericDialog.super.clearOptions();
//				updateMaintenanceList();
//				SimpleMaintenanceGenericDialog.super.requestUpdateListItems();//recreateListItems();
//				
//				return true;
//			}
//		});
//	}
	
//	/**
//	 * prefer calling {@link #requestUpdateMaintenanceListFull()}
//	 */
//	protected void updateMaintenanceListFull(){
//		clearOptions();
//		updateMaintenanceList();
//		requestUpdateListItems();//recreateListItems();
//	}
	
//	/**
//	 * prefer calling {@link #requestUpdateMaintenanceListFull()}
//	 */
//	/**
//	 * same as {@link #requestUpdateMaintenanceListFull()}
//	 */
	@Override
	public void requestUpdateListItems() {
//		super.requestUpdateListItems();
//		requestUpdateMaintenanceListFull();
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
//				updateMaintenanceListFull();
				
				SimpleMaintenanceGenericDialog.super.clearOptions();
				updateMaintenanceList();
				SimpleMaintenanceGenericDialog.super.requestUpdateListItems();//recreateListItems();
				
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
}
