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
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.google.common.collect.Lists;
import com.jme3.app.Application;
import com.jme3.collision.CollisionResult;
import com.jme3.collision.CollisionResults;
import com.jme3.input.FlyByCamera;
import com.jme3.input.MouseInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.MouseButtonTrigger;
import com.jme3.math.Ray;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class WorldPickingI {
	public static WorldPickingI i(){return GlobalManagerI.i().get(WorldPickingI.class);}
	
	private ArrayList<CollisionResult>	acrLastPickList;
	private Ray	rayLastCast;
	ArrayList<IPickListener> aplList = new ArrayList<IPickListener>();
	private boolean	bAllowConsume=true;
	
	public static interface IPickListener{
		/**
		 * 
		 * @param cr
		 * @param geom
		 * @param sptParentest
		 * @return true if consumed TODO prioritize listeners?
		 */
		boolean updatePickingEvent(ArrayList<CollisionResult> acrList, Geometry geom, Spatial sptParentest);
	}
	
	public  void addListener(IPickListener l){
		if(!aplList.contains(l))aplList.add(l);
	}
	
	public void configure(FlyByCamera flycam){
		String strPck="PickVirtualWorldThing";
		G.i(Application.class).getInputManager().addMapping(strPck, new MouseButtonTrigger(MouseInput.BUTTON_LEFT));
		G.i(Application.class).getInputManager().addListener(new ActionListener() {
				@Override
				public void onAction(String name, boolean isPressed, float tpf) {
					if(flycam!=null && flycam.isEnabled())return;
					
					if(!isPressed && name.equals(strPck)){ //on release
						WorldPickingI.i().pickWorldPiercingAtCursor(); //will call the listeners
//						WorldPickingI.i().pickWorldSpatialAtCursor();
//						Spatial spt = PickingHandI.i().pickWorldSpatialAtCursor();
//						if(spt!=null)LoggingI.i().logMarker(spt.toString());
					}
				}
			},
			strPck
		);
	}
	
	ArrayList<Class<? extends Spatial>> aclspt = new ArrayList<Class<? extends Spatial>>();
	/**
	 * TODO use skipper class at UserData? and this would be per Spatial!
	 * @param cl
	 */
	public void addSkipType(Class<? extends Spatial> cl){
		if(!aclspt.contains(cl))aclspt.add(cl);
	}
	
	/**
	 * TODO use skipper class at UserData?
	 * @param clChk
	 * @return
	 */
	public boolean isSkipType(Class<? extends Spatial> clChk){
		for(Class<? extends Spatial> cl:aclspt){
			if(cl.isAssignableFrom(clChk)){
				return true;
			}
		}
		return false;
	}
	
	public CollisionResult pickCollisionResultAtCursor(){
		ArrayList<CollisionResult> crs = pickWorldPiercingAtCursor();
		if(crs==null)return null;
		return crs.get(0);
	}
	public ArrayList<CollisionResult> pickWorldPiercingAtCursor(){
		return pickWorldPiercingAtCursor(MiscJmeI.i().getNodeVirtualWorld());
	}
	public ArrayList<CollisionResult> pickWorldPiercingAtCursor(Node nodeVirtualWorld){
		CollisionResults crs = new CollisionResults();
		
		Vector3f v3fCursorAtVirtualWorld3D = MiscJmeI.i().getApp().getCamera().getWorldCoordinates(
			EnvironmentJmeI.i().getMouse().getPos2D(), 0f);
		
		Vector3f v3fDirection = MiscJmeI.i().getApp().getCamera().getWorldCoordinates(
			EnvironmentJmeI.i().getMouse().getPos2D(), 1f);
		v3fDirection.subtractLocal(v3fCursorAtVirtualWorld3D).normalizeLocal();
		
		rayLastCast = new Ray(v3fCursorAtVirtualWorld3D, v3fDirection);
		nodeVirtualWorld.collideWith(rayLastCast, crs);
		
		ArrayList<CollisionResult> acrList=null;
		if(crs.size()==0){
			acrLastPickList=null;
		}else{
			acrList = Lists.newArrayList(crs.iterator());
			if(aclspt.size()>0){
				for(CollisionResult cr:acrList.toArray(new CollisionResult[0])){
					if(isSkipType(cr.getGeometry().getClass())){
						acrList.remove(cr);
					}
				}
			}
		}
		
		for(IPickListener l:aplList){
			if(crs!=null){
				if(l.updatePickingEvent(acrList, getLastWorldPickGeometry(), getLastWorldPickParentest())){
					if(bAllowConsume)break;
				}
			}else{
				l.updatePickingEvent(null,null,null);
			}
		}
		
		return acrList;
	}
	
	/**
	 * 
	 * @return can be a Geometry or a Node
	 */
	public Spatial getLastWorldPickParentest(){
		Geometry geom = getLastWorldPickGeometry();
		if(geom==null)return null;
		
		if(getLastWorldPickGeometry().getParent()==MiscJmeI.i().getNodeVirtualWorld()){
			return getLastWorldPickGeometry();
		}
		
		return SpatialHierarchyI.i().getParentest(getLastWorldPickGeometry(), Node.class, true, false);
//		ArrayList<Node> anode = SpatialHierarchyI.i().getAllParents(getLastWorldPick(),false);
//		return anode.get(anode.size()-1);
	}
	public Geometry getLastWorldPickGeometry(){
		if(acrLastPickList==null)return null;
//		if(crLastPick.getClosestCollision()==null)return null;
		return acrLastPickList.get(0).getGeometry();
	}
	public ArrayList<CollisionResult> getLastWorldPiercingPick(){
		return acrLastPickList;
	}
	public Ray getRayLastCast() {
		return rayLastCast;
	}

	public boolean isAllowConsume() {
		return bAllowConsume;
	}

	public WorldPickingI setAllowConsume(boolean bAllowConsume) {
		this.bAllowConsume = bAllowConsume;
		return this; //for beans setter
	}
}
