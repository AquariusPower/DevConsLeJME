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

package com.github.devconslejme.gendiag.es;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import com.github.devconslejme.gendiag.ResizablePanel;
import com.github.devconslejme.gendiag.es.HierarchyI.Blocker;
import com.github.devconslejme.gendiag.es.HierarchyI.LastFocusTime;
import com.github.devconslejme.gendiag.es.HierarchyI.ShownState;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityComponent;
import com.simsilica.es.EntityId;
import com.simsilica.es.EntitySet;
import com.simsilica.es.Name;
import com.simsilica.es.PersistentComponent;
import com.simsilica.es.base.DefaultEntityData;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class GenericDialogZayES extends AbstractAppState {
	public static GenericDialogZayES i(){return GlobalInstanceManagerI.i().get(GenericDialogZayES.class);}
	
	private DefaultEntityData	ed;
	private ArrayList<Class>	aclAllComponentTypes;
	
	public DefaultEntityData getEntityData(){
		return ed;
	}

	public void configure() {
    ed = new DefaultEntityData(); //holds all components
    GlobalInstanceManagerI.i().get(Application.class).getStateManager().attach(this);
    ed.getEntities(getAllComponentTypesArray()); //just to let the entset.getAddedEntities() work the 1st time
	}	
	
	public static class GuiLink implements EntityComponent, PersistentComponent{
		ResizablePanel val;
		public GuiLink(ResizablePanel val) { this.val=val; }
		public ResizablePanel getResizablePanel(){ return val; }
	}
	
	public static class Initialized implements EntityComponent, PersistentComponent{
		private boolean	bInitialized;
		public Initialized(boolean bInitialized) {
			this.bInitialized=bInitialized;
		}
		public boolean isInitialized() {
			return bInitialized;
		}
	}
	
	public EntityId createEntity(ResizablePanel rzp,String strName){
	  EntityId entid = ed.createEntity(); //to attach components to
	  ed.setComponent(entid, new GuiLink(rzp));
	  ed.setComponent(entid, new Initialized(false));
	  ed.setComponent(entid, new Name(strName));
	  return entid;
	}
	
	@Override
	public void update(float tpf) {
		EntitySet entset = ed.getEntities(getAllComponentTypesArray());
		
		boolean bWorkaroundInitializationFailure=true;
		if(bWorkaroundInitializationFailure){
			for(Entity ent:entset){
				if(!ent.get(Initialized.class).isInitialized()){
					initializeNewEntity(tpf, ent);
					ed.setComponent(ent.getId(), new Initialized(true));
				}
			}
		}
		
		if( entset.applyChanges() ) {
			// newly matching entities
			initializeNewEntities(tpf,entset.getAddedEntities()); //TODO not working?
			
			// entities that have merely changed TODO have any component changed? 
			updateChangedEntities(tpf,entset.getChangedEntities());
			
			// entities that are no longer matching TODO have any missing of the required components of the query above?
			cleanupRemovedEntities(tpf,entset.getRemovedEntities());
		}
	}
	
	private void initializeNewEntity(float tpf,Entity ent) {
		HierarchyI.i().initializeNewEntity(tpf,ent);
	}
	private void initializeNewEntities(float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			initializeNewEntity(tpf,ent);
		}
	}
	
	private void updateChangedEntity(float tpf,Entity ent) {
		HierarchyI.i().updateChangedEntity(tpf,ent.getId());
	}
	private void updateChangedEntities(float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			updateChangedEntity(tpf,ent);
		}
	}

	private void cleanupRemovedEntity(float tpf,Entity ent) {
		HierarchyI.i().cleanupRemovedEntity(tpf,ent);
	}
	private void cleanupRemovedEntities(float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			cleanupRemovedEntity(tpf,ent);
		}
	}

	public Class[] getAllComponentTypesArray() {
		return getAllComponentTypesList().toArray(new Class[0]);
	}
	public ArrayList<Class> getAllComponentTypesList() {
		if(aclAllComponentTypes==null){
			aclAllComponentTypes = new ArrayList<Class>();
			aclAllComponentTypes.add(Initialized.class);
			aclAllComponentTypes.add(GuiLink.class);
			aclAllComponentTypes.add(Name.class);
		}
		return aclAllComponentTypes;
	}
	
}
