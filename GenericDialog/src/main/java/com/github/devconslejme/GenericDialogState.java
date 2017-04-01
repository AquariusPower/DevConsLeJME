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

package com.github.devconslejme;

import com.github.devconslejme.ResizablePanel.EEdge;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GenericDialogState extends AbstractAppState{
	private ResizablePanel	rzpMain;
	private Application	app;
	private Container	cntrMain;
	private CfgParams cfg;
	private ResizablePanel	rzpInfo;
	
	public static class CfgParams{
		private Node nodeParent;
		private Vector3f v3fPos;
		private Vector3f v3fSize;
		private String strStyle;
//		private Panel	pnlContents;
		private Panel	pnlInput;
		private Panel	pnlOptions;
		private Panel	pnlInfo;

		public Vector3f getPos() {
			return v3fPos;
		}

		public CfgParams setPos(Vector3f v3fPos) {
			this.v3fPos = v3fPos;
			return this;
		}

		public Vector3f getSize() {
			return v3fSize;
		}

		public CfgParams setSize(Vector3f v3fSize) {
			this.v3fSize = v3fSize;
			return this;
		}

		public Node getNodeParent() {
			return nodeParent;
		}

		public CfgParams setNodeParent(Node nodeParent) {
			this.nodeParent = nodeParent;
			return this;
		}

		public String getStyle() {
			return strStyle;
		}

		public CfgParams setStyle(String strStyle) {
			this.strStyle = strStyle;
			return this;
		}

		public Panel getInfoSection() {
			return pnlInfo;
		}

		public Panel getOptionsSection() {
			return pnlOptions;
		}

		public Panel getInputSection() {
			return pnlInput;
		}

		public void setInputSection(Panel nodeInput) {
			this.pnlInput = nodeInput;
		}

		public void setOptionsSection(Panel nodeOptions) {
			this.pnlOptions = nodeOptions;
		}

		public void setInfoSection(Panel nodeInfo) {
			this.pnlInfo = nodeInfo;
		}

//		public Panel getContents() {
//			return pnlContents;
//		}
//
//		public CfgParams setContents(Panel pnlContents) {
//			this.pnlContents = pnlContents;
//			return this;
//		}
	}
	
	public GenericDialogState(Application app) {
		this.app=app;
	}
	
	public void configure(CfgParams cfg){
		this.cfg=cfg;
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		super.initialize(stateManager, app);

		rzpMain = new ResizablePanel(cfg.getSize(), cfg.getStyle());
		rzpMain.setLocalTranslation(cfg.getPos());
//		rzp.setContents(cfg.getContents());
		initContentsContainer();
		
		cfg.getNodeParent().attachChild(rzpMain);
	}
	
	private void initContentsContainer() {
		/**
		 * TODO north info/description
		 * TODO center list with options to user choose, can be a tree, each entry can have buttons or even text input for more functionalities!
		 * TODO south with input to be used as options filter or typeable custom choice (like creating the player name)
		 */
		cntrMain = new Container(new BorderLayout(), cfg.getStyle());
		rzpMain.setContents(cntrMain);
		
		rzpInfo = cfgSection(cfg.getInfoSection(),EEdge.Bottom,BorderLayout.Position.North);
		rzpInfo = cfgSection(cfg.getOptionsSection(),EEdge.Bottom,BorderLayout.Position.Center);
		rzpInfo = cfgSection(cfg.getInputSection(),EEdge.Top,BorderLayout.Position.South);
	}
	
	private ResizablePanel cfgSection(Panel pnlSection, EEdge edgeResizable, BorderLayout.Position border){
		ResizablePanel rzp = new ResizablePanel(pnlSection);
		rzp.setAllEdgesEnabled(false);
		rzp.getEdge(edgeResizable).setEnabled(true);
		cntrMain.addChild(rzp, border);
		return rzp;
	}
	
	public ResizablePanel getResizablePanel(){
		return rzpMain;
	}
	
}
