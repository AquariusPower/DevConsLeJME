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

import java.util.ArrayList;

import com.github.devconslejme.misc.HierarchySorterI.IDialogOrganizer;
import com.github.devconslejme.misc.MiscJmeI;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.QuadBackgroundComponent;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HierarchyResizablePanel extends ResizablePanel implements IDialogOrganizer{
	/**
	 * whatchout that Lemur will not control this blocker position/size/layout relatively to the panel it is blocking!
	 */
	private Button	btnBlocker;
	
	private Panel	pnlParent;
	private ArrayList<HierarchyResizablePanel> arzdList = new ArrayList<HierarchyResizablePanel>();
	private long	lLastFocusTimeNano = -1;
	private boolean	bTopDialog;
	private boolean	bModalDialog;
	
	public HierarchyResizablePanel(String strStyle) {
		super(strStyle);
		btnBlocker = new Button("");
		btnBlocker.setBackground(
			new QuadBackgroundComponent(
				MiscJmeI.i().colorChangeCopy(ColorRGBA.Red, -0.75f, 0.25f)));
	}

	public void setEnabledBlockerLayer(boolean b){
		if(b){
			attachChild(btnBlocker);
			btnBlocker.setLocalTranslation(0, 0, MiscJmeI.i().getBoundingBoxLimits(this).z);
		}else{
			btnBlocker.removeFromParent();
		}
	}
	
	@Override
	protected void resizedTo(Vector3f v3fNewSize) {
		super.resizedTo(v3fNewSize);
		
		btnBlocker.setPreferredSize(v3fNewSize);
	}
	
	/**
	 * will prevent access to parent
	 * @param rzdChildDialog
	 */
	public void showModal(HierarchyResizablePanel rzdChildDialog){
		rzdChildDialog.setModal(true);
		getParent().attachChild(rzdChildDialog);
		arzdList.add(rzdChildDialog);
		setEnabledBlockerLayer(true);
	}
	
	/**
	 * will close if parent closes
	 * @param rzdChildDialog
	 */
	public void showModeless(HierarchyResizablePanel rzdChildDialog){
		rzdChildDialog.setModal(false);
		getParent().attachChild(rzdChildDialog);
		arzdList.add(rzdChildDialog);
	}
	
	protected void setModal(boolean b) {
		this.bModalDialog=b;
	}

	@Override
	public void updateLogicalState(float tpf) {
		super.updateLogicalState(tpf);
		
		//beware what you code here!!!
		updateSelf();
		updateChildDialogList(); 
	}
	
	private void updateSelf() {
		// close self if parent dialog closed
		if(getParentDialog()!=null){
			if(getParentDialog().getParent()==null){
				removeFromParent();
			}
		}
	}

	private void updateChildDialogList() {
		for(HierarchyResizablePanel rzd:arzdList.toArray(new HierarchyResizablePanel[0])){
			if(rzd.getParent()==null){
				arzdList.remove(rzd);
			}
		}
		
		int iModalCount=0;
		for(HierarchyResizablePanel rzd:arzdList){
			if(rzd.isModal())iModalCount++;
		}
		
		if(iModalCount==0){
			setEnabledBlockerLayer(false);
		}
	}

	@Override
	public Panel getParentDialog() {
		return pnlParent;
	}
	
	@Override
	public Panel[] getChildDialogList() {
		return arzdList.toArray(new Panel[0]);
	}

	@Override
	public Long getLastFocusTimeNano() {
		return lLastFocusTimeNano;
	}

	@Override
	public boolean isTopDialog() {
		return bTopDialog;
	}

	@Override
	public boolean isModal() {
		return bModalDialog;
	}
	
}
