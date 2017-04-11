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
import com.github.devconslejme.gendiag.ResizablePanel.IComponent;
import com.github.devconslejme.misc.lemur.DragParentestListenerI;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.event.CursorEventControl;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public abstract class AbstractGenericDialogComposite implements IComponent{
	//private HierarchyResizablePanel	rzpMain;
	//private Application	app;
	private Container	cntrMain;
	//private CfgParams cfg;
	private Object	objSelected;
	//private Command<? super PopupState>	cmdClose;
	
	//public static class CfgParams{
	//	private Node nodeParent;
	//	private Vector3f v3fPos;
	//	private Vector3f v3fSize;
	//	private String strStyle;
		HashMap<ESection,ResizablePanel> hmSection = new HashMap<ESection,ResizablePanel>();
		private ResizablePanel	rzpOwner;
	
		
	//	public Vector3f getPos() {
	//		return v3fPos;
	//	}
	//
	//	public GenericDialogState setPos(Vector3f v3fPos) {
	//		this.v3fPos = v3fPos;
	//		return this;
	//	}
	
	//	public Vector3f getSize() {
	//		return v3fSize;
	//	}
	//
	//	public GenericDialogState setSize(Vector3f v3fSize) {
	//		this.v3fSize = v3fSize;
	//		return this;
	//	}
	
	//	public Node getNodeParent() {
	//		return nodeParent;
	//	}
	//
	//	public GenericDialogState setNodeParent(Node nodeParent) {
	//		this.nodeParent = nodeParent;
	//		return this;
	//	}
	
	//	public String getStyle() {
	//		return strStyle;
	//	}
	//
	//	public GenericDialogState setStyle(String strStyle) {
	//		this.strStyle = strStyle;
	//		return this;
	//	}
	
		public AbstractGenericDialogComposite setSection(ESection e, Panel pnl) {
			hmSection.put(e,new ResizablePanel(rzpOwner.getStyle()).setContents(pnl));
			return this;
		}
	
	//}
	
	public AbstractGenericDialogComposite(ResizablePanel rzpOwner) {
		this.rzpOwner=rzpOwner;
		preInitContentsContainer();
		initContentsContainer();
	//	this.app=GlobalInstanceManagerI.i().get(Application.class);
	//	setEnabled(false); //starts closed
	}
	
	//public void configure(CfgParams cfg){
	//	this.cfg=cfg;
	////	app.getStateManager().attach(this);
	////	getState(PopupState.class).showPopup(popup, ClickMode.Consume, closeCommand, ColorRGBA.Blue);
	//}
	//
	//public CfgParams getCfg(){
	//	return cfg;
	//}
	
	//@Override
	//protected void initialize(Application app) {
	////	rzpMain = new HierarchyResizablePanel(this.getStyle());
	////	this.setPreferredSize(this.getSize());
	////	this.setLocalTranslation(this.getPos());
	////	this.setMinSize(new Vector3f(100,100,0));
	////	rzp.setContents(this.getContents());
	//	initContentsContainer();
	//	
	////	cmdClose = new Command<PopupState>(){
	////		@Override
	////		public void execute(PopupState source) {
	////			setEnabled(false);
	////		}
	////	};
	//	
	////	this.getNodeParent().attachChild(rzpMain);
	//}
	
	protected abstract void preInitContentsContainer();

	private void initContentsContainer() {
		cntrMain = new Container(new BorderLayout(), rzpOwner.getStyle());
		getEntityOwner().setContents(cntrMain);
		
	//	this.hmSection.put(ESection.Info, 
		cfgSection(getSection(ESection.Info),BorderLayout.Position.North,EEdge.Bottom);
		CursorEventControl.addListenersToSpatial(
				getSection(ESection.Info).getContents(), DragParentestListenerI.i());
	//	MiscLemurI.i().applySimpleDragParentestListener();
	//	this.hmSection.put(ESection.Options,
		cfgSection(getSection(ESection.Options),BorderLayout.Position.Center,EEdge.Bottom);
	//	this.hmSection.put(ESection.Input,
		cfgSection(getSection(ESection.Input),BorderLayout.Position.South,EEdge.Top);
	//	cntrMain.addChild(this.getOptionsSection(),BorderLayout.Position.Center);
	//	cntrMain.addChild(this.getInputSection(),BorderLayout.Position.South);
		
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
	
	//public ResizablePanel getMainResizablePanel(){
	//	return rzpMain;
	//}
	
	public ResizablePanel getSection(ESection e){
		return this.hmSection.get(e);
	}
	
	//public Panel getSectionContents(ESection e){
	//	return this.hmSection.get(e).getContents();
	//}
	
	/**
	 * will also reset the selected
	 * @return
	 */
	public Object collectSelectedOption() {
		Object obj=objSelected;
		objSelected=null;
		return obj;
	}
	
	public boolean isOptionSelected(){
		return objSelected!=null;
	}
	
	protected void setSelectedOptionValue(Object obj){
		this.objSelected=obj;
	}

	@Override
	public void updateLogicalState(float tpf) {}

	@Override
	public ResizablePanel getEntityOwner() {
		return rzpOwner;
	}

	
	//@Override
	//protected void cleanup(Application app) {
	//}
	//
	//@Override
	//protected void enable() {
	//	getState(PopupState.class).showPopup(
	//		getMainResizablePanel(), 
	//		ClickMode.Consume, 
	//		cmdClose, 
	//		ColorI.i().colorChangeCopy(ColorRGBA.Blue, -0.75f, 0.25f) );
	//}
	//
	//@Override
	//protected void disable() {
	//	getState(PopupState.class).closePopup(getMainResizablePanel());
	//}

}
