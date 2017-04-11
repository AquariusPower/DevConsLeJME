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

import java.util.HashMap;

import com.github.devconslejme.misc.GlobalInstanceManagerI;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class _EntitySystem{
	public static _EntitySystem i(){return GlobalInstanceManagerI.i().get(_EntitySystem.class);}
	
	/**
	 * just a compact class name
	 */
	public static class ES extends _EntitySystem{}
	
	private long lGlobalEntityId=0;
	private HashMap<Long,Entity> hmEntities = new HashMap<>();
	
	public interface ISystem {
		<T extends IComponent> void updateComponent(T comp, float tpf);
	}
	
	public static interface IComponent {
		IEntity getEntityOwner();
		ISystem getSystem();
		<T extends IComponent,E extends IEntity> T createCloneWithNewOwner(E newOwner);
	}
	
	public static interface IEntity{
		<T extends IComponent> T createComponent(Class<T> cl);
		<T extends IComponent> T updateComponent(T comp);
		<T extends IComponent> T getComponent(Class<T> cl);
	}
	
	public static class Entity {
		private Object objToIncrementBehavior;
		private long	lEntityId;
		private HashMap<Class<IComponent>,IComponent> hmComponents = new HashMap<>();
		
		public Entity(Object objToIncreaseBehavior) {
			this.lEntityId=_EntitySystem.i().getNextEntityId();
			this.objToIncrementBehavior=objToIncreaseBehavior;
		}
		
		@SuppressWarnings("unchecked")
		public <T> T getObj(){
			return (T)objToIncrementBehavior;
		}
		
		public long getId() {
			return lEntityId;
		}
	}
	
	public Entity getEntity(long lId){
		return hmEntities.get(lId);
	}
	
	public Entity createEntity(Object objToIncreaseBehavior){
		Entity ent =  new Entity(objToIncreaseBehavior);
		hmEntities.put(ent.getId(),ent);
		return ent;
	}

	private long getNextEntityId() {
		return ++lGlobalEntityId;
	}
}