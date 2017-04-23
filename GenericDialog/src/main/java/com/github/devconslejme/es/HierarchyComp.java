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

import com.github.devconslejme.misc.HierarchySorterI.EHierarchy;
import com.simsilica.es.EntityComponent;
import com.simsilica.es.EntityId;
import com.simsilica.es.PersistentComponent;

/**
* DevSelfNote: Components: only getters; unmuttable: do not extend, store things that cant be changed or references; TODO confirm if references is a valid unmuttable... or Ids should be used instead?
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class HierarchyComp implements EntityComponent, PersistentComponent{
	public static enum EField{
		fBlockerZ(Float.class),
		bBlocked(Boolean.class), //#syncTo
		fBoundingHeightZ(Float.class), 
		strDebugName(String.class),
		bHierarchyModal(Boolean.class),
		eidHierarchyParent(EntityId.class),
		eHierarchyType(EHierarchy.class),
		bInitHierarchy(Boolean.class),
//		bInitVisuals(Boolean.class),
		lLastFocusTime(Long.class),
		bOpened(Boolean.class),
		bShowLinksFromChilds(Boolean.class),
		fZ(Float.class),
		;
		Class cl;
		EField(Class cl){this.cl=cl;}
	}
	
	private float fBlockerZ=0f;
	private boolean bBlocked=false; //#syncFrom bBlocking
	private float fBoundingHeightZ=0f;
	private String strDebugName="";
	private boolean	bHierarchyModal=false;
	private EntityId eidHierarchyParent=null;
	private EHierarchy	eHierarchyType=EHierarchy.Normal;
	private boolean	bInitHierarchy=false;
//	private boolean	bInitVisuals=false;
	private long lLastFocusTime=-1;
	private boolean bOpened=false;
	private boolean bShowLinksFromChilds=true;
	private float fZ=0f;
	
	public float getBlockerZ() {return fBlockerZ;}
	public boolean isBlocked() {return bBlocked;}
	public float getBoundingHeightZ() {return fBoundingHeightZ;}
	public String getDebugName(){return strDebugName;}
	public boolean isHierarchyModal() {return bHierarchyModal;}
	public EntityId getHierarchyParent() {return eidHierarchyParent;}
	public EHierarchy getHierarchyPriority() {return eHierarchyType;}
	public boolean isInitHierarchy() {return bInitHierarchy;}
//	public boolean isInitVisuals() {return bInitVisuals;}
	public long getLastFocusTime() {return lLastFocusTime;}
	public boolean isOpened() {return bOpened;}
	public boolean isShowLinksFromChilds() {return bShowLinksFromChilds;}
	public float getZ() {return fZ;}
	
//	public HierarchyComp(Object... aobjFieldsAndValues){
//		this(null,aobjFieldsAndValues);
//	}
	public HierarchyComp(HierarchyComp copyFrom, Object... aobjFieldsAndValues){
		if(copyFrom!=null)copyFrom(copyFrom); //initialize
		
		EField e = null;
		Object objValue = null;
		for(Object obj:aobjFieldsAndValues){
			if(e!=null){objValue=obj;}
			else
			if (obj instanceof EField){e = (EField) obj; continue;}
			
			switch (e) {
//			case fBlockerZ:						this.fBlockerZ=e.cl.cast(objValue);break;
				case fBlockerZ:						this.fBlockerZ=(Float)objValue;break;
				case bBlocked:						this.bBlocked=(Boolean)objValue;break;
				case fBoundingHeightZ:		this.fBoundingHeightZ=(Float)objValue;break;
				case strDebugName:				this.strDebugName=(String)objValue;break;
				case bInitHierarchy:			this.bInitHierarchy=(Boolean)objValue;break;
//				case bInitVisuals:			this.bInitVisuals=(Boolean)objValue;break;
				case bHierarchyModal:			this.bHierarchyModal=(Boolean)objValue;break;
				case eidHierarchyParent:	this.eidHierarchyParent=(EntityId)objValue;break;
				case eHierarchyType:			this.eHierarchyType=(EHierarchy)objValue;break;
				case lLastFocusTime:			this.lLastFocusTime=(Long)objValue;break;
				case bOpened:							this.bOpened=(Boolean)objValue;break;
				case bShowLinksFromChilds:this.bShowLinksFromChilds=(Boolean)objValue;break;
				case fZ:									this.fZ=(Float)objValue;break;
			}
			
			e = null;
		}
	}
	
	private void copyFrom(HierarchyComp copyFrom) {
		this.fBlockerZ=copyFrom.fBlockerZ;
		this.bBlocked=copyFrom.bBlocked;
		this.fBoundingHeightZ=copyFrom.fBoundingHeightZ;
		this.strDebugName=copyFrom.strDebugName;
		this.bInitHierarchy=copyFrom.bInitHierarchy;
//		this.bInitVisuals=copyFrom.bInitVisuals;
		this.bHierarchyModal=copyFrom.bHierarchyModal;
		this.eidHierarchyParent=copyFrom.eidHierarchyParent;
		this.eHierarchyType=copyFrom.eHierarchyType;
		this.lLastFocusTime=copyFrom.lLastFocusTime;
		this.bOpened=copyFrom.bOpened;
		this.bShowLinksFromChilds=copyFrom.bShowLinksFromChilds;
		this.fZ=copyFrom.fZ;
	}

}
