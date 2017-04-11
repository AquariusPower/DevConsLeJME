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

import com.github.devconslejme.gendiag._EntitySystem.IComponent;
import com.github.devconslejme.gendiag._EntitySystem.IEntity;
import com.github.devconslejme.gendiag._EntitySystem.ISystem;
import com.simsilica.lemur.Button;


/**
 * only getters
 * unmuttable: do not extend, store things that cant be changed or references
 * TODO confirm if references is a valid unmuttable... or Ids should be used instead?
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public final class _HierarchyComponent implements IComponent{
	private Button	btnBlocker = new Button("");
	private long	lLastFocusAppTimeNano = -1;
	private _HierarchyComponent	hierarchyParent=null;
	private boolean	bHierarchyTop=false;
	private boolean	bHierarchyModal=false;
	private ResizablePanel	rzpOwner=null;
	private boolean	bInitialized=false;
	
	private void copyAllFrom(_HierarchyComponent copyFrom){
		this.btnBlocker=(copyFrom.btnBlocker);
		this.lLastFocusAppTimeNano=(copyFrom.lLastFocusAppTimeNano);
		this.hierarchyParent=(copyFrom.hierarchyParent);
		this.bHierarchyTop=(copyFrom.bHierarchyTop);
		this.bHierarchyModal=(copyFrom.bHierarchyModal);
		this.rzpOwner=(copyFrom.rzpOwner);
		this.bInitialized=(copyFrom.bInitialized);
	}
	
	public Button getBlocker() {
		return btnBlocker;
	}
	public long getLastFocusAppTimeNano() {
		return lLastFocusAppTimeNano;
	}
	public _HierarchyComponent getHierarchyParent() {
		return hierarchyParent;
	}
	public boolean isHierarchyTop() {
		return bHierarchyTop;
	}
	public boolean isHierarchyModal() {
		return bHierarchyModal;
	}
	
	@Override
	public ResizablePanel getEntityOwner() {
		return rzpOwner;
	}

//	@SuppressWarnings("unchecked")
//	@Override
//	public Class<HierarchyComponent> getCompositeType() {
//		return HierarchyComponent.class;
//	}

	public _HierarchyComponent() {} //keep this for class.newInstance()
	
	public _HierarchyComponent(_HierarchyComponent copyFrom, long lLastFocusAppTimeNano) {
		copyAllFrom(copyFrom);
		this.lLastFocusAppTimeNano = lLastFocusAppTimeNano;
	}
	
	public _HierarchyComponent(_HierarchyComponent copyFrom, _HierarchyComponent	hierarchyParent) {
		copyAllFrom(copyFrom);
		this.hierarchyParent = hierarchyParent;
	}
	
	/**
	 * 
	 * @param copyFrom
	 * @param bHierarchyTop wont set if null
	 * @param bHierarchyModal wont set if null
	 */
	public _HierarchyComponent(_HierarchyComponent copyFrom, Boolean bHierarchyTop, Boolean bHierarchyModal) {
		copyAllFrom(copyFrom);
		if(bHierarchyTop!=null)this.bHierarchyTop=bHierarchyTop;
		if(bHierarchyModal!=null)this.bHierarchyModal=bHierarchyModal;
	}
	
	public _HierarchyComponent(_HierarchyComponent copyFrom, ResizablePanel rzpOwner) {
		copyAllFrom(copyFrom);
		this.rzpOwner = rzpOwner;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public _HierarchyComponent createCloneWithNewOwner(IEntity newOwner) {
		return new _HierarchyComponent(this,(ResizablePanel)newOwner);
	}
	
	@Override
	public ISystem getSystem() {
		return _HierarchySystemI.i();
	}
	
	public boolean isInitialized() {
		return bInitialized;
	}

}
