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
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class HierarchyComp implements EntityComponent, PersistentComponent{
	public static enum EField{
		bOpened(Boolean.class), //#syncTo
		bBlocking(Boolean.class), //#syncTo
		bInitVisuals(Boolean.class), //#syncTo
		bInitHierarchy(Boolean.class),
		lLastFocusTime(Long.class),
		eidHierarchyParent(EntityId.class),
		eHierarchyType(EHierarchy.class),
		bHierarchyModal(Boolean.class),
		bShowLinkToChild(Boolean.class),
		fZ(Float.class),
		fBlockerZ(Float.class),
		fBoundingHeightZ(Float.class), 
		strDebugName(String.class),
		;
		Class cl;
		EField(Class cl){this.cl=cl;}
	}
	
	private boolean bOpened=false; //#syncFrom bBlocking
	private boolean bBlocking=false; //#syncFrom bBlocking
	private boolean	bInitVisuals=false;
	private boolean	bInitHierarchy=false;
	private long lLastFocusTime=-1;
	private EntityId eidHierarchyParent=null;
	private EHierarchy	eHierarchyType=EHierarchy.Normal;
	private boolean	bHierarchyModal=false;
	private boolean bShowLinkToChild=true;
	private float fZ=0f;
	private float fBlockerZ=0f;
	private float fBoundingHeightZ=0f;
	private String strDebugName="";
	
	public boolean isOpened() {return bOpened;}
	public boolean isBlocking() {return bBlocking;}
	public boolean isInitVisuals() {return bInitVisuals;}
	public boolean isInitHierarchy() {return bInitHierarchy;}
	public long getLastFocusTime() {return lLastFocusTime;}
	public EntityId getHierarchyParent() {return eidHierarchyParent;}
	public EHierarchy getHierarchyPriority() {return eHierarchyType;}
	public boolean isHierarchyModal() {return bHierarchyModal;}
	public boolean isShowLinkToChild() {return bShowLinkToChild;}
	public float getZ() {return fZ;}
	public float getBlockerZ() {return fBlockerZ;}
	public float getBoundingHeightZ() {return fBoundingHeightZ;}
	public String getDebugName(){return strDebugName;}
	
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
				case bOpened:						this.bOpened=(Boolean)objValue;break;
				case bBlocking:					this.bBlocking=(Boolean)objValue;break;
				case bInitHierarchy:		this.bInitHierarchy=(Boolean)objValue;break;
				case bInitVisuals:			this.bInitVisuals=(Boolean)objValue;break;
				case bHierarchyModal:		this.bHierarchyModal=(Boolean)objValue;break;
				case bShowLinkToChild:	this.bShowLinkToChild=(Boolean)objValue;break;
				case eHierarchyType:		this.eHierarchyType=(EHierarchy)objValue;break;
				case eidHierarchyParent:this.eidHierarchyParent=(EntityId)objValue;break;
				case lLastFocusTime:		this.lLastFocusTime=(Long)objValue;break;
				case fZ:								this.fZ=(Float)objValue;break;
				case fBlockerZ:					this.fBlockerZ=(Float)objValue;break;
				case fBoundingHeightZ:	this.fBoundingHeightZ=(Float)objValue;break;
				case strDebugName:			this.strDebugName=(String)objValue;break;
			}
			
			e = null;
		}
		
		System.out.println();//@DEBUG rm
	}
	
	private void copyFrom(HierarchyComp copyFrom) {
		this.bOpened=copyFrom.bOpened;
		this.bBlocking=copyFrom.bBlocking;
		this.bInitVisuals=copyFrom.bInitVisuals;
		this.bInitHierarchy=copyFrom.bInitHierarchy;
		this.bHierarchyModal=copyFrom.bHierarchyModal;
		this.bShowLinkToChild=copyFrom.bShowLinkToChild;
		this.eHierarchyType=copyFrom.eHierarchyType;
		this.eidHierarchyParent=copyFrom.eidHierarchyParent;
		this.lLastFocusTime=copyFrom.lLastFocusTime;
		this.fZ=copyFrom.fZ;
		this.fBlockerZ=copyFrom.fBlockerZ;
		this.fBoundingHeightZ=copyFrom.fBoundingHeightZ;
		this.strDebugName=copyFrom.strDebugName;
	}

}
