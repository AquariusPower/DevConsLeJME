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

import java.util.Set;

import com.github.devconslejme.gendiag.ResizablePanel;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.lemur.DragParentestListenerI;
import com.jme3.math.ColorRGBA;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityId;
import com.simsilica.es.EntitySet;
import com.simsilica.es.base.DefaultEntityData;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.component.QuadBackgroundComponent;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class GenericDialogZayES {
	public static GenericDialogZayES i(){return GlobalInstanceManagerI.i().get(GenericDialogZayES.class);}
	
	private DefaultEntityData	ed;
	
	public DefaultEntityData getEntityData(){
		return ed;
	}

	public void configure() {
    ed = new DefaultEntityData(); //holds all components
	}	
	
	public EntityId createEntity(ResizablePanel rzp){
	  EntityId ent = ed.createEntity(); //to attach components to
	  ed.setComponent(ent, new HierarchyI.GuiLink(rzp));
	  return ent;
	}
	
	public void update(float tpf) {
    EntitySet entset = ed.getEntities(HierarchyI.GuiLink.class);
    while( true ) {
        if( entset.applyChanges() ) {
            // newly matching entities
            initializeNewEntities(tpf,entset.getAddedEntities());

            // entities that have merely changed TODO have any component changed? 
            updateChangedEntities(tpf,entset.getChangedEntities());
            
            // entities that are no longer matching TODO have any missing of the required components of the query above?
            cleanupRemovedEntities(tpf,entset.getRemovedEntities());
        }
    }
	}

	private void initializeNewEntities(float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			HierarchyI.i().initialize(tpf,ent);
		}
	}
	
	private void updateChangedEntities(float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			HierarchyI.i().update(tpf,ent);
		}
	}

	private void cleanupRemovedEntities(float tpf,Set<Entity> entset) {
		for(Entity ent:entset){
			HierarchyI.i().cleanup(tpf,ent);
		}
	}
}
