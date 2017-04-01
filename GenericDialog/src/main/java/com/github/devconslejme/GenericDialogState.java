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

import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.RollupPanel;
import com.simsilica.lemur.component.QuadBackgroundComponent;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GenericDialogState extends AbstractAppState{
	private ResizablePanel	rzp;
	private Application	app;
	
	public static class CfgParams{
		private Node nodeParent;
		private Vector3f v3fPos;
		private Vector3f v3fSize;
		private String strStyle;
//		private Panel	pnlContents;

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

//		public Panel getContents() {
//			return pnlContents;
//		}
//
//		public CfgParams setContents(Panel pnlContents) {
//			this.pnlContents = pnlContents;
//			return this;
//		}
	}
	CfgParams cfg;
	
	public GenericDialogState(Application app) {
		this.app=app;
	}
	
	public void configure(CfgParams cfg){
		this.cfg=cfg;
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		super.initialize(stateManager, app);

		rzp = new ResizablePanel(cfg.getSize(), cfg.getStyle());
		rzp.setLocalTranslation(cfg.getPos());
//		rzp.setContents(cfg.getContents());
		prepareContents();
		
		cfg.getNodeParent().attachChild(rzp);
	}
	
	private void prepareContents() {
		/**
		 * TODO north info/description
		 * TODO center list with options to user choose, can be a tree, each entry can have buttons or even text input for more functionalities!
		 * TODO south with input to be used as options filter or typeable custom choice (like creating the player name)
		 */
		initContentsContainer();
		
		initInfoSection();
		initOptionsSection();
		initInputSection();
	}

	private void initContentsContainer() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}
	
	private void initInputSection() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	private void initOptionsSection() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	private void initInfoSection() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	public ResizablePanel getResizablePanel(){
		return rzp;
	}
	
}
