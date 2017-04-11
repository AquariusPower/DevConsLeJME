
package com.github.devconslejme.gendiag;

import java.util.Set;

import com.jme3.app.SimpleApplication;
import com.simsilica.es.Entity;
import com.simsilica.es.EntityData;
import com.simsilica.es.EntityId;
import com.simsilica.es.EntitySet;
import com.simsilica.es.Name;
import com.simsilica.es.base.DefaultEntityData;

/**
 * https://github.com/jMonkeyEngine-Contributions/zay-es/wiki/Getting-Started
 */
public class GenericDialogZayES {
	private DefaultEntityData	ed;

	public void init() {
    ed = new DefaultEntityData(); //holds all components
    EntityId myEntity = ed.createEntity(); //to attach components too
    ed.setComponent(myEntity, new Name("My Entity"));
	}	
	
	public void update(float tpf) {
    EntitySet namedEntities = ed.getEntities(Name.class);
    while( true ) {
        if( namedEntities.applyChanges() ) {
            // do something with newly matching entities
            addSomeStuff(namedEntities.getAddedEntities());

            // do something with entities that are no longer matching
            removeSomeStuff(namedEntities.getRemovedEntities());

            // do something with the entities that have merely changed
            changeSomeStuff(namedEntities.getChangedEntities());
        }
    }
	}

	private void changeSomeStuff(Set<Entity> changedEntities) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	private void removeSomeStuff(Set<Entity> removedEntities) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}

	private void addSomeStuff(Set<Entity> addedEntities) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}
}
