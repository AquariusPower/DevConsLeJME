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

import com.github.devconslejme.gendiag.HierarchySorterI.IHierarchySorter;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.TimeConvertI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.lemur.ClickToPositionCaratListenerI;
import com.github.devconslejme.misc.lemur.DragParentestListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.jme3.app.Application;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class HierarchyResizablePanel extends ResizablePanel implements IHierarchySorter{
	private Button	btnBlocker;
	
	private long	lLastFocusAppTimeNano = -1;
	private ArrayList<HierarchyResizablePanel> arzdHierarchyChildList = new ArrayList<HierarchyResizablePanel>();
	private Panel	hrpHierarchyParent;
	private boolean	bHierarchyTop;
	private boolean	bHierarchyModal;
	private Application	app;
	
	public HierarchyResizablePanel(String strStyle) {
		super(strStyle);
		
   	setName(getName()+"/"+HierarchyResizablePanel.class.getSimpleName());
		
		initBlocker();
		
		app = GlobalInstanceManagerI.i().get(Application.class);
		
		HoverHighlightEffectI.i().applyAt(this, (QuadBackgroundComponent)getResizableBorder());
		
//    CursorEventControl.addListenersToSpatial(this, lastFocusListener);
	}
	
	private void initBlocker(){
		btnBlocker = new Button("");//!BLOCKED!");
		
		btnBlocker.setBackground(
			new QuadBackgroundComponent(//ColorRGBA.Red));
				ColorI.i().colorChangeCopy(ColorRGBA.Red, -0.75f, 0.5f)));
		
		DragParentestListenerI.i().applyAt(btnBlocker, this);
	}

	@Override
	public void updateLastFocusAppTimeNano() {
		lLastFocusAppTimeNano=TimeConvertI.i().getNanosFrom(app.getTimer());
	}

	public void setEnabledBlockerLayer(boolean b){
		if(b){
			getParent().attachChild(btnBlocker);
		}else{
			btnBlocker.removeFromParent();
		}
	}
	
	@Override
	protected void resizedTo(Vector3f v3fNewSize) {
		super.resizedTo(v3fNewSize);
	}
	
	/**
	 * will prevent access to parent
	 * @param rzdChildDialog
	 */
	public void showHierarchyModal(HierarchyResizablePanel rzdChildDialog){
		setHierarchyChild(rzdChildDialog,true);
	}
	
	/**
	 * will close if parent closes
	 * @param rzdChildDialog
	 */
	public void showHierarchyModeless(HierarchyResizablePanel rzdChildDialog){
		setHierarchyChild(rzdChildDialog,false);
	}
	
	private void setHierarchyChild(HierarchyResizablePanel rzdChildDialog, boolean bModal){
		rzdChildDialog.setHierarchyModal(bModal);
		rzdChildDialog.setHierarchyParent(this);
		getParent().attachChild(rzdChildDialog);
		arzdHierarchyChildList.add(rzdChildDialog);
		if(bModal)setEnabledBlockerLayer(true);
		rzdChildDialog.updateLastFocusAppTimeNano();
	}
	
	protected void setHierarchyModal(boolean b) {
		this.bHierarchyModal = b;
	}
	
	protected void setHierarchyParent(HierarchyResizablePanel hrpParent){
		this.hrpHierarchyParent = hrpParent;
	}

	@Override
	public void updateLogicalState(float tpf) {
		super.updateLogicalState(tpf);
		update(tpf);
	}
	protected void update(float tpf){
		/********************
		 * beware what you code here!!! 
		 ********************/
		
		// close self if parent dialog closed
		if(getHierarchyParent()!=null){
			if(getHierarchyParent().getParent()==null){
				removeFromParent();
			}
		}
		
		// remove closed childs
		for(HierarchyResizablePanel rzd:arzdHierarchyChildList.toArray(new HierarchyResizablePanel[0])){
			if(rzd.getParent()==null){ //was closed
				arzdHierarchyChildList.remove(rzd);
			}
		}
		
		// blocker work
		if(isBlocked()){
			// how many childs are modal
			int iModalCount=0;
			for(HierarchyResizablePanel rzd:arzdHierarchyChildList){
				if(rzd.isModal())iModalCount++;
			}
			
			if(iModalCount==0){
				// remove blocker
				setEnabledBlockerLayer(false);
			}else{
				Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(this);
				if(Float.compare(v3fSize.length(),0f)!=0){ //waiting top panel be updated by lemur
					Vector3f v3fPos = getLocalTranslation().clone();
					v3fPos.z += v3fSize.z;
					btnBlocker.setLocalTranslation(v3fPos);
					
					btnBlocker.setPreferredSize(getPreferredSize());
				}
			}
		}
	}
	
	public boolean isBlocked(){
		return btnBlocker.getParent()!=null;
	}
	
	@Override
	public Panel getHierarchyParent() {
		return hrpHierarchyParent;
	}
	
	@Override
	public Panel[] getHierarchyChildList() {
		return arzdHierarchyChildList.toArray(new Panel[0]);
	}

	@Override
	public Long getLastFocusAppTimeNano() {
		return lLastFocusAppTimeNano;
	}

	public HierarchyResizablePanel setTopHierarchy(boolean b) {
		this.bHierarchyTop = b;
		return this;
	}
	
	@Override
	public boolean isTopHierarchy() {
		return bHierarchyTop;
	}

	@Override
	public boolean isModal() {
		return bHierarchyModal;
	}

	@Override
	public String getReport(boolean b) {
		StringBuilder sb = new StringBuilder();
		
		sb.append("top="+bHierarchyTop);
		sb.append("/");
		
		sb.append("z="+getSize().z);
		sb.append(",");
		sb.append("Pz="+getPreferredSize().z);
		sb.append("/");
		
		sb.append("tm="+lLastFocusAppTimeNano);
		sb.append("/");
		
		sb.append("dbgnm="+getName());
		sb.append("/");
		
		return sb.toString();
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("HierarchyResizablePanel [btnBlocker=");
		builder.append(btnBlocker);
		builder.append(", lLastFocusAppTimeNano=");
		builder.append(lLastFocusAppTimeNano);
		builder.append(", arzdHierarchyChildList=");
		builder.append(arzdHierarchyChildList);
		builder.append(", hrpHierarchyParent=");
		builder.append(hrpHierarchyParent);
		builder.append(", bHierarchyTop=");
		builder.append(bHierarchyTop);
		builder.append(", bHierarchyModal=");
		builder.append(bHierarchyModal);
		builder.append(", app=");
		builder.append(app);
		builder.append("]");
		return builder.toString();
	}
	
}
