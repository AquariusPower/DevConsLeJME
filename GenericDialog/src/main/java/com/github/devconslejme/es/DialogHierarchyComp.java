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

package com.github.devconslejme.es;

import java.util.ArrayList;

import com.github.devconslejme.misc.HierarchySorterI.EHierarchyType;
import com.simsilica.es.EntityComponent;
import com.simsilica.es.EntityId;
import com.simsilica.es.PersistentComponent;

/**
* DevSelfNote: Components: only getters; unmuttable: do not extend, store things that cant be changed or references; TODO confirm if references is a valid unmuttable... or Ids should be used instead?
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class DialogHierarchyComp implements EntityComponent, PersistentComponent{
	private DiagCompBean bean;
	
	/**
	 * why make it too simple if it can be harmlessly a bit complex? :)
	 */
	public static class DiagCompBean{
		/** to apply Setter's Calls At Parent's bean */
		private DiagCompBean beanTarget;
		private ArrayList<Runnable> arSetterCalls=new ArrayList<Runnable>();
		/** add set call */
		private void add(Runnable r){arSetterCalls.add(r);}
		
		////////// fields
		private float fBlockerZ;
		private boolean bBlocked;
		private float fBoundingHeightZ;
		private String strDebugName;
		private float fDialogZ;
		private boolean	bHierarchyModal;
		private EntityId eidHierarchyParent;
		private EHierarchyType	eHierarchyType;
		private long lLastFocusTime;
		private boolean bOpened;
		private boolean bShowLinksFromChilds;
		private boolean bVolatileModal;
		
		public DiagCompBean(){
			fBlockerZ=0f;
			bBlocked=false; //#syncFrom bBlocking
			fBoundingHeightZ=0f;
			strDebugName="";
			fDialogZ=0f;
			bHierarchyModal=false;
			eidHierarchyParent=null;
			eHierarchyType=EHierarchyType.Normal;
			lLastFocusTime=-1;
			bOpened=false;
			bShowLinksFromChilds=true;
			bVolatileModal=false;
		}
		
		private void copyFrom(DiagCompBean copyFrom){
			this.fBlockerZ=copyFrom.fBlockerZ;
			this.bBlocked=copyFrom.bBlocked;
			this.fBoundingHeightZ=copyFrom.fBoundingHeightZ;
			this.strDebugName=copyFrom.strDebugName;
			this.fDialogZ=copyFrom.fDialogZ;
			this.bHierarchyModal=copyFrom.bHierarchyModal;
			this.eidHierarchyParent=copyFrom.eidHierarchyParent;
			this.eHierarchyType=copyFrom.eHierarchyType;
			this.lLastFocusTime=copyFrom.lLastFocusTime;
			this.bOpened=copyFrom.bOpened;
			this.bShowLinksFromChilds=copyFrom.bShowLinksFromChilds;
			this.bVolatileModal=copyFrom.bVolatileModal;
		}
		
		///////////////// Getters
		public float getBlockerZ() {
			return fBlockerZ;
		}
		public boolean isBlocked() {
			return bBlocked;
		}
		public float getBoundingHeightZ() {
			return fBoundingHeightZ;
		}
		public String getDebugName() {
			return strDebugName;
		}
		public float getDialogZ() {
			return fDialogZ;
		}
		public boolean isHierarchyModal() {
			return bHierarchyModal;
		}
		public EntityId getHierarchyParent() {
			return eidHierarchyParent;
		}
		public EHierarchyType getHierarchyType() {
			return eHierarchyType;
		}
		public long getLastFocusTime() {
			return lLastFocusTime;
		}
		public boolean isOpened() {
			return bOpened;
		}
		public boolean isShowLinksFromChilds() {
			return bShowLinksFromChilds;
		}
		public boolean isVolatileModal() {
			return bVolatileModal;
		}
		
		////////////////// Monsterified Setters furtherly quickly macro formatted thru Wine+Notepad++ !
		public DiagCompBean setBlockerZ(float fBlockerZ){add(new Runnable(){@Override public void run(){
			beanTarget.fBlockerZ = fBlockerZ;}});return this;
		}

		public DiagCompBean setBlocked(boolean bBlocked){add(new Runnable(){@Override public void run(){
			beanTarget.bBlocked=bBlocked;}});return this;
		}

		public DiagCompBean setBoundingHeightZ(float fBoundingHeightZ){add(new Runnable(){@Override public void run(){
			beanTarget.fBoundingHeightZ = fBoundingHeightZ;}});return this;
		}

		public DiagCompBean setDebugName(String strDebugName){add(new Runnable(){@Override public void run(){
			beanTarget.strDebugName = strDebugName;}});return this;
		}

		public DiagCompBean setDialogZ(float fDialogZ){add(new Runnable(){@Override public void run(){
			beanTarget.fDialogZ = fDialogZ;}});return this;
		}

		public DiagCompBean setHierarchyModal(boolean bHierarchyModal){add(new Runnable(){@Override public void run(){
			beanTarget.bHierarchyModal = bHierarchyModal;}});return this;
		}

		public DiagCompBean setHierarchyParent(EntityId eidHierarchyParent){add(new Runnable(){@Override public void run(){
			beanTarget.eidHierarchyParent = eidHierarchyParent;}});return this;
		}

		public DiagCompBean setHierarchyType(EHierarchyType eHierarchyType){add(new Runnable(){@Override public void run(){
			beanTarget.eHierarchyType = eHierarchyType;}});return this;
		}

		public DiagCompBean setLastFocusTime(long lLastFocusTime){add(new Runnable(){@Override public void run(){
			beanTarget.lLastFocusTime = lLastFocusTime;}});return this;
		}

		public DiagCompBean setOpened(boolean bOpened){add(new Runnable(){@Override public void run(){
			beanTarget.bOpened = bOpened;}});return this;
		}

		public DiagCompBean setShowLinksFromChilds(boolean bShowLinksFromChilds){add(new Runnable(){@Override public void run(){
			beanTarget.bShowLinksFromChilds = bShowLinksFromChilds;}});return this;
		}

		public DiagCompBean setVolatileModal(boolean bVolatileModal){add(new Runnable(){@Override public void run(){
			beanTarget.bVolatileModal = bVolatileModal;}});return this;
		}
		
	}
	
	//////////////// delegate getters
	public float getBlockerZ() {
		return bean.getBlockerZ();
	}

	public boolean isBlocked() {
		return bean.isBlocked();
	}

	public float getBoundingHeightZ() {
		return bean.getBoundingHeightZ();
	}

	public String getDebugName() {
		return bean.getDebugName();
	}

	public float getDialogZ() {
		return bean.getDialogZ();
	}

	public boolean isHierarchyModal() {
		return bean.isHierarchyModal();
	}

	public EntityId getHierarchyParent() {
		return bean.getHierarchyParent();
	}

	public EHierarchyType getHierarchyType() {
		return bean.getHierarchyType();
	}

	public long getLastFocusTime() {
		return bean.getLastFocusTime();
	}

	public boolean isOpened() {
		return bean.isOpened();
	}

	public boolean isShowLinksFromChilds() {
		return bean.isShowLinksFromChilds();
	}
	
	public boolean isVolatileModal() {
		return bean.isVolatileModal();
	}
	
	/**
	 * The "one constructor to rule them all".
	 * @param copyFrom can be null
	 * @param beanNewValues can be null
	 */
	public DialogHierarchyComp(DialogHierarchyComp copyFrom, DiagCompBean beanNewValues){
		bean = new DiagCompBean();
		
		if(copyFrom!=null)bean.copyFrom(copyFrom.bean); //initialize with original component
		
		if(beanNewValues!=null){
			beanNewValues.beanTarget=this.bean;
			for(Runnable r:beanNewValues.arSetterCalls)r.run();
		}
	}
	
	@Override
	public DialogHierarchyComp clone(){
		return new DialogHierarchyComp(this,null);
	}
}
