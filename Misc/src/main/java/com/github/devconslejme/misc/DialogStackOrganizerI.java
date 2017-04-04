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

package com.github.devconslejme.misc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.bounding.BoundingBox;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Panel;


/**
 * This will detect and organize all dialogs (Panels attached to main JME nodes),
 * based on their hierarchy.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DialogStackOrganizerI extends AbstractAppState{
	public static DialogStackOrganizerI i(){return GlobalInstanceManagerI.i().get(DialogStackOrganizerI.class);}
	
	private ArrayList<Panel> apnl = new ArrayList<Panel>();
	private Application	app;
	private Node nodeToMonitor;
	private float	fBeginOrderZ = 0f;
	private Comparator<IDialogOrganizer> cmpr = new Comparator<IDialogOrganizer>() {
		@Override
		public int compare(IDialogOrganizer o1, IDialogOrganizer o2) {
			Panel pnl1 = (Panel)o1;
			Panel pnl2 = (Panel)o2;
			
			// top only against top
			if(o1.isTopDialog() && !o2.isTopDialog())return  1;
			if(o2.isTopDialog() && !o1.isTopDialog())return -1;
			
			// parent diag below child diag
			if(o1.getParentDialog()==o2)return -1;
			if(o2.getParentDialog()==o1)return  1;
			if(o1==o2)return 0;
			
			// last focus
			return Long.compare(o1.getLastFocusTimeNano(),o2.getLastFocusTimeNano());
		}
	};
	
	public static interface IDialogOrganizer {
		public Panel getParentDialog();
		public Panel[] getChildDialogList();
		public Long getLastFocusTimeNano();
		public boolean isTopDialog();
		public boolean isModal();
	}
	
	public void configure(Application app, Node nodeToMonitor, float fBeginOrderZ){
		this.app=app;
		this.fBeginOrderZ=fBeginOrderZ;
		app.getStateManager().attach(this);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
//		nodeToMonitor.getChildren()
		organizeDialogsStack();
	}
	
	private void organizeDialogsStack() {
		ArrayList<IDialogOrganizer> aido = new ArrayList<IDialogOrganizer>();
		
		for(Spatial spt:nodeToMonitor.getChildren()){
			if(spt instanceof Panel){
				Panel pnl=(Panel)spt;
				if(pnl instanceof IDialogOrganizer){
					aido.add((IDialogOrganizer)pnl);
				}
			}
		}
		
		Collections.sort(aido,cmpr);
		
		float fOrderZ = fBeginOrderZ;
		for(IDialogOrganizer ido:aido){
			Panel pnl=(Panel)ido;
			pnl.getLocalTranslation().z=fOrderZ;
			fOrderZ += (((BoundingBox)pnl.getWorldBound()).getZExtent()*2) +1.0f; //+1 is safety
		}
	}
}
