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
import com.github.devconslejme.gendiag.ResizablePanel.IComposite;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.TimeConvertI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.lemur.DragParentestListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.jme3.app.Application;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.component.QuadBackgroundComponent;


/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public final class HierarchyComposite implements IComposite,IHierarchySorter{
	private Button	btnBlocker;
	
	private long	lLastFocusAppTimeNano = -1;
	private ArrayList<HierarchyComposite> arzdHierarchyChildList = new ArrayList<HierarchyComposite>();
	private HierarchyComposite	hrpHierarchyParent;
	private boolean	bHierarchyTop;
	private boolean	bHierarchyModal;
	private Application	app;

	private ResizablePanel	rzpOwner;
	
	public HierarchyComposite(ResizablePanel rzp){
		this.rzpOwner=rzp;
//	}
//	
//	public HierarchyComposite(String strStyle) {
//		super(strStyle);
//		
//	 	setName(getName()+"/"+HierarchyComposite.class.getSimpleName());
		
		initBlocker();
		
		app = GlobalInstanceManagerI.i().get(Application.class);
		
		HoverHighlightEffectI.i().applyAt(rzp, (QuadBackgroundComponent)rzp.getResizableBorder());
		
	//  CursorEventControl.addListenersToSpatial(this, lastFocusListener);
	}
	
	private void initBlocker(){
		btnBlocker = new Button("");//!BLOCKED!");
		
		btnBlocker.setBackground(
			new QuadBackgroundComponent(//ColorRGBA.Red));
				ColorI.i().colorChangeCopy(ColorRGBA.Red, -0.75f, 0.5f)));
		
		DragParentestListenerI.i().applyAt(btnBlocker, rzpOwner);
	}
	
	@Override
	public void updateLastFocusAppTimeNano() {
		lLastFocusAppTimeNano=TimeConvertI.i().getNanosFrom(app.getTimer());
	}
	
	public void setEnabledBlockerLayer(boolean b){
		if(b){
			rzpOwner.getParent().attachChild(btnBlocker);
		}else{
			btnBlocker.removeFromParent();
		}
	}
	
//	@Override
//	protected void resizedTo(Vector3f v3fNewSize) {
//		super.resizedTo(v3fNewSize);
//	}
	
	/**
	 * will prevent access to parent
	 * @param rzdChildDialog
	 */
	public void showHierarchyModal(HierarchyComposite rzdChildDialog){
		setHierarchyChild(rzdChildDialog,true);
	}
	
	/**
	 * will close if parent closes
	 * @param rzdChildDialog
	 */
	public void showHierarchyModeless(HierarchyComposite rzdChildDialog){
		setHierarchyChild(rzdChildDialog,false);
	}
	
	private void setHierarchyChild(HierarchyComposite rzdChildDialog, boolean bModal){
		rzdChildDialog.setHierarchyModal(bModal);
		rzdChildDialog.setHierarchyParent(this);
		rzpOwner.getParent().attachChild(rzdChildDialog.rzpOwner);
		arzdHierarchyChildList.add(rzdChildDialog);
		if(bModal)setEnabledBlockerLayer(true);
		rzdChildDialog.updateLastFocusAppTimeNano();
	}
	
	protected void setHierarchyModal(boolean b) {
		this.bHierarchyModal = b;
	}
	
	protected void setHierarchyParent(HierarchyComposite hrpParent){
		this.hrpHierarchyParent = hrpParent;
	}
	
	@Override
	public void updateLogicalState(float tpf) {
		update(tpf);
	}
	protected void update(float tpf){
		/********************
		 * beware what you code here!!! 
		 ********************/
		
		// close self if parent dialog closed
		if(getHierarchyParent()!=null){
			if(getHierarchyParent().rzpOwner.getParent()==null){
				rzpOwner.removeFromParent();
			}
		}
		
		// remove closed childs
		for(HierarchyComposite rzd:arzdHierarchyChildList.toArray(new HierarchyComposite[0])){
			if(rzd.rzpOwner.getParent()==null){ //was closed
				arzdHierarchyChildList.remove(rzd);
			}
		}
		
		// blocker work
		if(isBlocked()){
			// how many childs are modal
			int iModalCount=0;
			for(HierarchyComposite rzd:arzdHierarchyChildList){
				if(rzd.isModal())iModalCount++;
			}
			
			if(iModalCount==0){
				// remove blocker
				setEnabledBlockerLayer(false);
			}else{
				Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(rzpOwner);
				if(Float.compare(v3fSize.length(),0f)!=0){ //waiting top panel be updated by lemur
					Vector3f v3fPos = rzpOwner.getLocalTranslation().clone();
					v3fPos.z += v3fSize.z;
					btnBlocker.setLocalTranslation(v3fPos);
					
					btnBlocker.setPreferredSize(rzpOwner.getPreferredSize());
				}
			}
		}
	}
	
	public boolean isBlocked(){
		return btnBlocker.getParent()!=null;
	}
	
	@Override
	public HierarchyComposite getHierarchyParent() {
		return hrpHierarchyParent;
	}
	
	@Override
	public HierarchyComposite[] getHierarchyChildList() {
		return arzdHierarchyChildList.toArray(new HierarchyComposite[0]);
	}
	
	@Override
	public Long getLastFocusAppTimeNano() {
		return lLastFocusAppTimeNano;
	}
	
	public HierarchyComposite setTopHierarchy(boolean b) {
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
		
		sb.append("z="+rzpOwner.getSize().z);
		sb.append(",");
		sb.append("Pz="+rzpOwner.getPreferredSize().z);
		sb.append("/");
		
		sb.append("tm="+lLastFocusAppTimeNano);
		sb.append("/");
		
		sb.append("dbgnm="+rzpOwner.getName());
		sb.append("/");
		
		return sb.toString();
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("HierarchyComposite [btnBlocker=");
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

	@Override
	public ResizablePanel getOwner() {
		return rzpOwner;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Class<HierarchyComposite> getCompositeType() {
		return HierarchyComposite.class;
	}

}
