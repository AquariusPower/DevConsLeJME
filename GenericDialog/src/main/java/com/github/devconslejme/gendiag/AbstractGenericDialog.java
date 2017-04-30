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

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.misc.lemur.ResizablePanel.EEdge;
import com.github.devconslejme.misc.lemur.ResizablePanel.IResizableListener;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.focus.DefaultFocusTraversalControl;
import com.simsilica.lemur.focus.FocusTraversal;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public abstract class AbstractGenericDialog implements IResizableListener{
	private Container	cntrMain;
	private Object	objSelected;
	HashMap<ESection,ResizablePanel> hmSection = new HashMap<ESection,ResizablePanel>();
	private ResizablePanel	rzpDialog;
//	private DefaultFocusTraversalControl ctrl = new DefaultFocusTraversalControl(){
//		@Override
//		public Spatial getDefaultFocus() {
//			return getSection(ESection.Input);
//		};
//	};
	
	public AbstractGenericDialog setSection(ESection e, Panel pnl) {
		hmSection.put(e,new ResizablePanel(rzpDialog.getStyle()).setContents(pnl));
		return this;
	}
	
	public AbstractGenericDialog(ResizablePanel rzpDialog) {
		this.rzpDialog=rzpDialog;
		MiscJmeI.i().addToName(rzpDialog, this.getClass().getSimpleName(), true);
//		rzpDialog.addControl(ctrl);
		
		bl = new BorderLayoutFT(getDialog());
		
		rzpDialog.addResizableListener(this);
		
		initPreContentsContainer();
		initContentsContainer();
	}
	
	protected abstract void initPreContentsContainer();
	
	private class BorderLayoutFT extends BorderLayout implements FocusTraversal{
		DefaultFocusTraversalControl dftc = new DefaultFocusTraversalControl();
//		FocusTraversalAdapter dftc = new FocusTraversalAdapter();
		
		public BorderLayoutFT(Spatial spt){
			super();
			DetailedException.assertNotNull(spt, this);
			dftc.setSpatial(spt);
			dftc.setFocusRoot(true);
		}
		
		@Override
		public Spatial getDefaultFocus() {
			return getSection(ESection.Input).getContents();
		}

		@Override
		public Spatial getRelativeFocus(Spatial from, TraversalDirection direction) {
			return dftc.getRelativeFocus(from, direction);
		}

		@Override
		public boolean isFocusRoot() {
			return dftc.isFocusRoot();
		}
		
	};
	private BorderLayoutFT bl;
	
	private void initContentsContainer() {
		DetailedException.assertNotNull(bl, this);
		
		DragParentestPanelListenerI.i().applyAt(rzpDialog);
		
		cntrMain = new Container(bl, rzpDialog.getStyle());
//		cntrMain = new Container(new BorderLayout(), rzpDialog.getStyle());
		DetailedException.assertNotAlreadySet(getDialog().getContents(), cntrMain, this);
		
		getDialog().setContents(cntrMain);
		
		cfgSection(getSection(ESection.Info),BorderLayout.Position.North,EEdge.Bottom);
		DragParentestPanelListenerI.i().applyAt(getSection(ESection.Info).getContents());
//		CursorEventControl.addListenersToSpatial(
//			getSection(ESection.Info).getContents(), DragParentestPanelListenerI.i());
		cfgSection(getSection(ESection.Options),BorderLayout.Position.Center,EEdge.Bottom);
		cfgSection(getSection(ESection.Input),BorderLayout.Position.South,EEdge.Top);
		
		cfgSection(getSection(ESection.Tools),BorderLayout.Position.West,EEdge.Right);
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
		
		/**
		 * West with buttons to execute related commands like updating the list of options and anything else!
		 */
		Tools,
		;
	}
	
	public ResizablePanel getSection(ESection e){
		return this.hmSection.get(e);
	}
	
	/**
	 * will also reset the selected to null
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
	
	/**
	 * 
	 * @param obj ignored if null
	 */
	protected void setChosenValue(Object obj){
		if(obj==null)return; 
		this.objSelected=obj;
	}

	public ResizablePanel getDialog() {
		return rzpDialog;
	}

	@Override	public void removedFromParentEvent(ResizablePanel rzpSource) {	}
	@Override	public void resizedEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {	}
	@Override	public void endedResizingEvent(ResizablePanel rzpSource) {	}
	@Override	public void resizerUpdatedLogicalStateEvent(float tpf,ResizablePanel rzp) {	}
	
}
