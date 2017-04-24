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

import java.util.HashMap;

import com.github.devconslejme.gendiag.ResizablePanel.EEdge;
import com.github.devconslejme.gendiag.ResizablePanel.IResizableListener;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.event.CursorEventControl;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public abstract class AbstractGenericDialog implements IResizableListener{
	private Container	cntrMain;
	private Object	objSelected;
	HashMap<ESection,ResizablePanel> hmSection = new HashMap<ESection,ResizablePanel>();
	private ResizablePanel	rzpDialog;
	
	public AbstractGenericDialog setSection(ESection e, Panel pnl) {
		hmSection.put(e,new ResizablePanel(rzpDialog.getStyle()).setContents(pnl));
		return this;
	}
	
	public AbstractGenericDialog(ResizablePanel rzpOwner) {
		this.rzpDialog=rzpOwner;
		rzpOwner.addResizableListener(this);
		preInitContentsContainer();
		initContentsContainer();
	}
	
	protected abstract void preInitContentsContainer();

	private void initContentsContainer() {
		cntrMain = new Container(new BorderLayout(), rzpDialog.getStyle());
		DetailedException.assertNotAlreadySet(getDialog().getContents(), cntrMain, this);
		
		getDialog().setContents(cntrMain);
		
		cfgSection(getSection(ESection.Info),BorderLayout.Position.North,EEdge.Bottom);
		CursorEventControl.addListenersToSpatial(
			getSection(ESection.Info).getContents(), DragParentestPanelListenerI.i());
		cfgSection(getSection(ESection.Options),BorderLayout.Position.Center,EEdge.Bottom);
		cfgSection(getSection(ESection.Input),BorderLayout.Position.South,EEdge.Top);
	}
	
	private ResizablePanel cfgSection(ResizablePanel rzp, BorderLayout.Position border, EEdge edgeResizable){
		rzp.setAllEdgesEnabled(false);
		rzp.getEdge(edgeResizable).setEnabled(true);
		cntrMain.addChild(rzp, border);
	//	PopupState ps;
		return rzp;
	}
	
	public static enum ESection{
		/**
		 * north info/description
		 */
		Info,
		
		/**
		 * center list with options to user choose, can be a tree, each entry can have buttons or even text input for more functionalities!
		 */
		Options,
		
		/**
		 * south with input to be used as options filter or typeable custom choice (like creating the player name)
		 */
		Input,
		;
	}
	
	public ResizablePanel getSection(ESection e){
		return this.hmSection.get(e);
	}
	
	/**
	 * will also reset the selected
	 * @return
	 */
	public Object extractSelectedOption() {
		Object obj=objSelected;
		objSelected=null;
		return obj;
	}
	
	/**
	 * also means there is PendingSelectedOptionExtraction
	 * @return
	 */
	public boolean isOptionSelected(){
		return objSelected!=null;
	}
	
	protected void setSelectedOptionValue(Object obj){
		this.objSelected=obj;
	}

	public ResizablePanel getDialog() {
		return rzpDialog;
	}

	@Override
	public void removedFromParentEvent(ResizablePanel rzpSource) {
	}
	@Override
	public void resizedEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {
	}
	@Override
	public void endedResizingEvent(ResizablePanel rzpSource) {
	}
	@Override
	public void resizerUpdatedLogicalStateEvent(float tpf,ResizablePanel rzp) {
	}
}
