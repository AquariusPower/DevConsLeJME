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
	public void applyActivetableListener(Spatial spt, ActivetableListenerAbs ial){
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
	
	/**
	 * @DevSelfNote must not be interface, to work as auto-key userdata
	 */
	public static abstract class ActivetableListenerAbs{
		@SuppressWarnings("unused")private boolean bDevSelfNote_KeepAsAbstract_IMeanIt;//KEEP THIS HERE
		/**
		 * 
		 * @param sptSource
		 * @return if consumed (true) will prevent further parent's hierarchy activation
		 */
		public abstract boolean activateEvent(Spatial sptSource);
		/** see {@link #activateEvent(Spatial)} */
		public boolean deactivateEvent(Spatial sptSource){return true;} 
//		public abstract boolean deactivateEvent(Spatial sptSource); 
	}
	
	public boolean isActivetable(Spatial spt){
		return getActivatableListener(spt)!=null;
	}
	
	protected boolean workIfPossibleRaw(ArrayList<Spatial> asptActivateables,Spatial spt,boolean bActivate){
		ActivetableListenerAbs ial = getActivatableListener(spt);
		if(ial!=null){
			asptActivateables.add(spt); //fill all that can work (even if they dont)
			if(bActivate?ial.activateEvent(spt):ial.deactivateEvent(spt)){
				return true;
			}
		}
		return false;
	}
	
	protected ActivetableListenerAbs getActivatableListener(Spatial spt) {
		return UserDataI.i().getMustExistOrNull(spt,ActivetableListenerAbs.class);
	}

	public ArrayList<Spatial> activateIfPossible(PhysicsData pd) {
		return workIfPossible(pd.getInitialOriginalGeometry(),true);
	}
	public ArrayList<Spatial> activateIfPossible(Spatial spt) {
		return workIfPossible(spt,true);
	}
	public ArrayList<Spatial> deactivateIfPossible(PhysicsData pd) {
		return workIfPossible(pd.getInitialOriginalGeometry(),false);
	}
	public ArrayList<Spatial> deactivateIfPossible(Spatial spt) {
		return workIfPossible(spt,false);
	}
	protected ArrayList<Spatial> workIfPossible(Spatial spt,boolean bActivate) {
		ArrayList<Spatial> asptActivatablesFound=new ArrayList<Spatial>();
		if(!workIfPossibleRaw(asptActivatablesFound,spt,bActivate)){
			for (Node node : SpatialHierarchyI.i().getAllParents(spt,false)) {
				if(workIfPossibleRaw(asptActivatablesFound,node,bActivate)){
					break;
				}
			}
		}
		
		return asptActivatablesFound;
	}
}
