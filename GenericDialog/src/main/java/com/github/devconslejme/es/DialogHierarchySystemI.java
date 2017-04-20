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

import com.github.devconslejme.es.HierarchyComp.EField;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityId;
import com.simsilica.es.EntitySet;
import com.simsilica.es.Name;
import com.simsilica.es.base.DefaultEntityData;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DialogHierarchySystemI {
	public static DialogHierarchySystemI i(){return GlobalManagerI.i().get(DialogHierarchySystemI.class);}
	
	private DefaultEntityData	ed;
	private EntitySet	entsetHierarchyQuery;
	
	public void configure(){
    ed = new DefaultEntityData(); //holds all components
		entsetHierarchyQuery = ed.getEntities(HierarchyComp.class); //just to create it empty so new entities will be detected
		if(entsetHierarchyQuery.size()>0)throw new DetailedException("must begin empty so news and changes can be properly applied",entsetHierarchyQuery,ed);
	}
	
	public EntityId createEntity(String strName){
	  EntityId entid = ed.createEntity(); //to attach components to
	  
	  ed.setComponent(entid, new HierarchyComp(null)); //first time component creation
	  
	  setHierarchyComp(entid, 
//	  	EField.bInitVisuals,false,
	  	EField.strDebugName,strName
	  );
	  
	  ed.setComponent(entid, new Name(strName));
	  
	  return entid;
	}
	
	/**
	 * 
	 * @param entid
	 * @param aobj see {@link HierarchyComp#HierarchyComp(HierarchyComp, Object...)} 
	 * @return
	 */
	public void setHierarchyComp(EntityId entid, Object... aobj) {
		Entity ent = entsetHierarchyQuery.getEntity(entid);
		if(ent==null)ent=ed.getEntity(entid, HierarchyComp.class); //go the slow path if the query is not ready
		HierarchyComp hc = ent.get(HierarchyComp.class);
		ent.set(hc = new HierarchyComp(hc, aobj));
	}

	public HierarchyComp getHierarchyComp(EntityId entid) {
		Entity ent = entsetHierarchyQuery.getEntity(entid);
		if(ent==null)ent=ed.getEntity(entid, HierarchyComp.class); //go the slow path if the query is not ready
		return ent.get(HierarchyComp.class);
	}
	
}