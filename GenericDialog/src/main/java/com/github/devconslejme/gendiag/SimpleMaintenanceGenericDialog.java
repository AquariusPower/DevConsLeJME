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

import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.misc.MessagesI;
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
	
	private boolean bLastRequestWasCollapsed=false;
	
	@Override
	protected void initSectionTools() {
		super.initSectionTools();
		putToolAction(new ToolAction("Refresh Options", new Command<Button>() {
			@Override
			public void execute(Button source) {
				requestUpdateListItems();
			}
		}));
		
		putToolAction(new ToolAction("Collapse All", new Command<Button>() {@Override public void execute(Button source) {
				collapseAll();	}}));
		
		putToolAction(new ToolAction("Expand All", new Command<Button>() {@Override	public void execute(Button source) {
				expandAll();	}}));
		
		requestUpdateListItems(); //1st time
	}
	
	@Override
	public void requestUpdateListItems() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				SimpleMaintenanceGenericDialog.super.clearOptions();
				updateMaintenanceList();
				SimpleMaintenanceGenericDialog.super.requestUpdateListItems();
				
				if(bLastRequestWasCollapsed){
					QueueI.i().enqueue(new CallableXAnon() {
						@Override
						public Boolean call() {
							collapseAll();
							return true;
						}
					});
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

}
