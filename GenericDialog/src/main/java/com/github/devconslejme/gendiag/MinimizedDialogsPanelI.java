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

import java.util.ArrayList;

import javax.swing.PopupFactory;

import com.github.devconslejme.es.DialogHierarchyComp.DiagCompBean;
import com.github.devconslejme.es.DialogHierarchySystemI;
import com.github.devconslejme.gendiag.ContextMenuI.HintUpdaterPerCtxtBtn;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchyType;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.EnvironmentI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.misc.lemur.ResizablePanel.IResizableListener;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MinimizedDialogsPanelI implements IResizableListener{
	public static MinimizedDialogsPanelI i(){return GlobalManagerI.i().get(MinimizedDialogsPanelI.class);}
	
	private ResizablePanel	minimizedDiags;
	private Container	cntrMinimized;
	private DialogHierarchyStateI	dhs;
	private DialogHierarchySystemI	sys;
	private Node	nodeToMonitor;
	protected boolean	bInitialized;
	private float fMinSize=30; //TODO auto find out the current style font height
	
	public void configure(Node nodeToMonitor){
		dhs=DialogHierarchyStateI.i();
		sys=DialogHierarchySystemI.i();
		this.nodeToMonitor=nodeToMonitor;
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(!bInitialized)return false;
				
				int iMinChildren = cntrMinimized.getLayout().getChildren().size();
				if(minimizedDiags.getParent()==null){
					if(iMinChildren>0)nodeToMonitor.attachChild(minimizedDiags);
				}else{
					if(iMinChildren==0)minimizedDiags.removeFromParent();
				}
				
				return true;
			}
		}.enableLoop().setDelaySeconds(1f).setName("Show/Hide minimized dialogs' panel"));
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				minimizedDiags = dhs.createDialog("Minimized dialogs panel", null);
				minimizedDiags.addResizableListener(MinimizedDialogsPanelI.this);
				minimizedDiags.setApplyContentsBoundingBoxSize(false);
				minimizedDiags.setLocalTranslationXY(new Vector3f(0,EnvironmentI.i().getDisplay().getHeight(),Float.NaN));
				minimizedDiags.setPreferredSizeWH(new Vector3f(EnvironmentI.i().getDisplay().getWidth(),fMinSize,Float.NaN));
				sys.setHierarchyComp(dhs.getEntityId(minimizedDiags), 
						new DiagCompBean().setHierarchyType(EHierarchyType.Top));
				
				cntrMinimized=new Container();
				minimizedDiags.setContents(cntrMinimized);
				
				bInitialized=true;
				return true;
			}
		});
	}
	
	@SuppressWarnings("unchecked")
	public void minimize(AbstractGenericDialog sgd) {
		if(dhs.getHierarchyComp(sgd.getDialog()).getHierarchyParent()!=null)return;
		
		Button btn = new Button(sgd.getTitle());
		PopupHintHelpListenerI.i().setPopupHintHelp(btn, btn.getText());
		MiscJmeI.i().recursivelyApplyTextNoWrap(btn);
		//TODO rotate text 90degreess if in the vertical?
		
		addMinimized(btn, cntrMinimized.getLayout().getChildren().size());
//		cntrMinimized.addChild(btn, cntrMinimized.getLayout().getChildren().size());
		btn.addClickCommands(new Command<Button>() {
			@Override
			public void execute(Button source) {
				DialogHierarchyStateI.i().showDialog(sgd.getDialog());
				btn.removeFromParent();
				
				update();
			}

		});
		
		sgd.close();
	}
	
	private void update() {
		//read remaining
		ArrayList<Node> children = new ArrayList<Node>(cntrMinimized.getLayout().getChildren()); //remaining
		cntrMinimized.getLayout().clearChildren();
//		Vector3f v3fMaxSize = new Vector3f();
		int i=0;for(Node child:children){
			Button btnChild = (Button)child;
			addMinimized(btnChild, i++);
			Vector3f v3fPrefSize = btnChild.getPreferredSize();
//			if(v3fMaxSize.x < v3fPrefSize.x)v3fMaxSize.x=v3fPrefSize.x;
//			if(v3fMaxSize.y < v3fPrefSize.y)v3fMaxSize.y=v3fPrefSize.y;
		}
		
		Vector3f v3fSize = minimizedDiags.getSize().clone();
		if(v3fSize.x<fMinSize)v3fSize.x=fMinSize;
		if(v3fSize.y<fMinSize)v3fSize.y=fMinSize;
		minimizedDiags.setPreferredSizeWH(v3fSize);
	}
	
	private void addMinimized(Node node,int iIndex){
		Vector3f v3fSize = cntrMinimized.getSize();
		if(v3fSize.x>=v3fSize.y){
			cntrMinimized.addChild(node, 0, iIndex);
		}else{
			cntrMinimized.addChild(node, iIndex, 0);
		}
	}

	@Override
	public void resizableUpdatedLogicalStateEvent(float tpf,			ResizablePanel rzpSource) {
	}

	@Override
	public void resizableRemovedFromParentEvent(ResizablePanel rzpSource) {	}

	@Override
	public void resizableStillResizingEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {
		update();
	}

	@Override
	public void resizableEndedResizingEvent(ResizablePanel rzpSource) { }
	
}
