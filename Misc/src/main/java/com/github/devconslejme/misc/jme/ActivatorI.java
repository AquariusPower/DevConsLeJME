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
package com.github.devconslejme.misc.jme;

import java.util.ArrayList;

import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ActivatorI {
	public static ActivatorI i(){return GlobalManagerI.i().get(ActivatorI.class);}
	
	/**
	 * all parent hierarchy may also be activated if each contains the listener,
	 * beware what you put there. 
	 * @param spt
	 * @param ial
	 */
	public void appllyActivetableListener(Spatial spt, Activetable ial){
		UserDataI.i().putSafelyMustNotExist(spt, ial);
	}
	
	public void configure(){
//		String strK=KeyCodeManagerI.i().getMouseTriggerKey(0).getFullId();
//		KeyBindCommandManagerI.i().putBindCommandsLater(strK,new CallBoundKeyCmd(){
//			@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
//				for(TargetGeom t:TargetI.i().getAllTargets()){
//				}
//				return true;
//			}
//		}.setName("Activate"));
	}
	
	public static abstract class Activetable{
		/**
		 * 
		 * @param sptSource
		 * @return if consumed (true) will prevent further parent's hierarchy activation
		 */
		public abstract boolean activateEvent(Spatial sptSource); 
	}
	
	protected boolean activateIfPossibleRaw(ArrayList<Spatial> aspt,Spatial spt){
		Activetable ial = UserDataI.i().getMustExistOrNull(spt,Activetable.class);
		if(ial!=null){
			aspt.add(spt);
			if(ial.activateEvent(spt)){
				return true;
			}
		}
		return false;
	}
	
	public ArrayList<Spatial> activateIfPossible(Spatial spt) {
		ArrayList<Spatial> aspt=new ArrayList<Spatial>();
		if(!activateIfPossibleRaw(aspt,spt)){
			for (Node node : SpatialHierarchyI.i().getAllParents(spt,false)) {
				if(activateIfPossibleRaw(aspt,node)){
					break;
				}
			}
		}
		
		return aspt;
	}
}
