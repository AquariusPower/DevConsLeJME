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

import com.github.devconslejme.es.HierarchyComp.EHiParm;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchyType;
import com.github.devconslejme.misc.HierarchySorterI.IHierarchy;
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
	private ArrayList<Entity>	aentSortedHierarchyDialogs = new ArrayList<Entity>();
	private long	lAppTime;
	/** to spare CPU */
	private long	lLastFullSortAppTime;
	private long	lLastSortAppTime;
	private boolean	bUpdateOnlyParentestFocusTime = false;
	
	private static class DiagHierarchyWrapper implements IHierarchy{
		private Entity	ent;
		private HierarchyComp hc;

		public DiagHierarchyWrapper(Entity ent){
			this.ent=ent;
			hc = ent.get(HierarchyComp.class);
		}
		
		public Entity getEntity(){
			return ent;
		}
		
		@Override
		public DiagHierarchyWrapper getHierarchyParent() {
			if(hc.getHierarchyParent()==null)return null;
			
			return new DiagHierarchyWrapper(
				DialogHierarchySystemI.i().entsetHierarchyQuery.getEntity(
					hc.getHierarchyParent()
				)
			);
		}

		@Override
		public EHierarchyType getHierarchyPriority() {
			return hc.getHierarchyPriority();
		}

		@Override
		public long getLastActivationNanoTime() {
			return ent.get(HierarchyComp.class).getLastFocusTime();
		}
		
		@Override
		public boolean equals(Object obj) {
			return ent.getId().equals(((DiagHierarchyWrapper)obj).getEntity().getId());
		}
	}
	
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
	  	EHiParm.strDebugName,strName
	  );
	  
	  ed.setComponent(entid, new Name(strName));
	  
	  return entid;
	}
	
	public void update(Float tpf, long lAppTime){ //TODO this is still quite dummy here...
		this.lAppTime=lAppTime;
		
		if(entsetHierarchyQuery.applyChanges()) { //contains all components required by hierarchy
			// newly matching entities
			entsetHierarchyQuery.getAddedEntities();
			
			// entities that have merely changed TODO like in have any component changed? 
			entsetHierarchyQuery.getChangedEntities();
			
			// entities that are no longer matching TODO like in one or more of the required query components went missing
			entsetHierarchyQuery.getRemovedEntities();
		}
	}
	
	/**
	 * 
	 * @param entid
	 * @param aobj see {@link HierarchyComp#HierarchyComp(HierarchyComp, Object...)} 
	 * @return
	 */
	public void setHierarchyComp(EntityId entid, Object... aobj) {
		Entity ent = getEntity(entid);
		HierarchyComp hc = ent.get(HierarchyComp.class);
		ent.set(hc = new HierarchyComp(hc, aobj));
	}

	public HierarchyComp getHierarchyComp(long lId) {
		return getHierarchyComp(new EntityId(lId));
	}
	public HierarchyComp getHierarchyComp(EntityId entid) {
		return getEntity(entid).get(HierarchyComp.class);
	}
	
	/**
	 * better deal with entity things only at the system
	 * @param entid
	 * @return
	 */
	private Entity getEntity(EntityId entid){
		Entity ent = entsetHierarchyQuery.getEntity(entid);
		if(ent==null)ent=ed.getEntity(entid, HierarchyComp.class); //go the slow path if the query is not ready
		return ent;
	}
	
	public boolean isBlocked(EntityId entid){
		return getEntity(entid).get(HierarchyComp.class).isBlocked();
	}

	public void enableBlockingLayer(EntityId entid, boolean bEnable){
		setHierarchyComp(entid, EHiParm.bBlocked, bEnable);
	}
	
	/**
	 * 
	 * @param entParentFilter if null will bring all possible
	 * @return unsorted
	 */
	public ArrayList<Entity> getAllOpenedDialogs(EntityId entidParentFilter){
		ArrayList<Entity> aent = new ArrayList<Entity>();
		
		/**
		 * TODO how to make this work?
		EntitySet entset = ed.getEntities(new FilterByHierarchyParent(ent), ShownState.class);
		 */
//		EntitySet entset = ed.getEntities(GuiLink.class,ShownState.class,LastFocusTime.class);
		
		for(Entity entChild:entsetHierarchyQuery){
			HierarchyComp hcChild = entChild.get(HierarchyComp.class);
			if(!hcChild.isOpened())continue;
			
			boolean bAdd=false;
			if(entidParentFilter==null){
				bAdd=true;
			}else{
				EntityId entidParent = hcChild.getHierarchyParent();
				if(entidParentFilter.equals(entidParent)){
					bAdd=true;
				}
			}
			
			if(bAdd)aent.add(entChild);
		}		
		
		return aent;
	}
	
	/**
	 * 
	 * @param entParentFilter if null will bring all possible
	 * @return
	 */
	public Entity[] getSortedHierarchyDialogs(){
		if(lAppTime!=lLastSortAppTime){
			aentSortedHierarchyDialogs.clear();
			
			/**
			 * the filter would make it skip many and break the sort hierarchy
			 */
			aentSortedHierarchyDialogs.addAll(getAllOpenedDialogs(null));
			sortDialogs(aentSortedHierarchyDialogs);
			
			lLastSortAppTime=lAppTime;
		}
		
		return aentSortedHierarchyDialogs.toArray(new Entity[0]);
	}

	private void sortDialogs(ArrayList<Entity> aentMainList) {
		ArrayList<DiagHierarchyWrapper> ahs = new ArrayList<DiagHierarchyWrapper>();
		for(Entity ent:aentMainList){
			ahs.add(new DiagHierarchyWrapper(ent));
		}
		
		HierarchySorterI.i().sort(ahs);//,cmprEntEquals);
		
		aentMainList.clear();
		
		for(DiagHierarchyWrapper hs:ahs){
			aentMainList.add(hs.getEntity());
		}
	}
	
	public EntityId getParentest(EntityId entid){
		EntityId entidParentest = null;
		EntityId entidParent = entid;
		while(entidParent!=null){
			entidParentest = entidParent;
			entidParent = getHierarchyComp(entidParent).getHierarchyParent();
		}
		return entidParentest;
	}
	
	public EntityId updateLastFocusAppTimeNano(EntityId entid, long lTime) {
		EntityId entidToUpdate = entid;
		
		if(bUpdateOnlyParentestFocusTime){
			entidToUpdate=getParentest(entid);
		}
		
		setHierarchyComp(entidToUpdate, EHiParm.lLastFocusTime, lTime);
		
		return entidToUpdate;
	}

	public ArrayList<EntityId> getParentList(EntityId entid) {
		ArrayList<EntityId> aentid = new ArrayList<EntityId>();
		while(entid!=null){
			aentid.add(0,entid);
			entid=getHierarchyComp(entid).getHierarchyParent();
		}
		return aentid;
	}
	
}