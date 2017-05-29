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

import com.github.devconslejme.devcons.LoggingI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.google.common.collect.Lists;
import com.jme3.collision.CollisionResult;
import com.jme3.collision.CollisionResults;
import com.jme3.input.FlyByCamera;
import com.jme3.math.Plane;
import com.jme3.math.Ray;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Quad;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class WorldPickingI {
	public static WorldPickingI i(){return GlobalManagerI.i().get(WorldPickingI.class);}
	
	private ArrayList<CollisionResult>	acrLastPickList = new ArrayList<CollisionResult>();
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
		boolean updatePickingEvent(int iButtonIndex, ArrayList<CollisionResult> acrList, Geometry geom, Spatial sptParentest);
	}
	
	public  void addListener(IPickListener l){
		if(!aplList.contains(l))aplList.add(l);
	}
	
	public void configure(FlyByCamera flycam){
		String strPck="PickVirtualWorldThing";
		for(int i=0;i<KeyCodeManagerI.i().getTotalMouseButtons();i++){
			int iButtonIndex=i;
			KeyBindCommandManagerI.i().putBindCommandsLater(
				KeyCodeManagerI.i().getMouseTriggerKey(i).getFullId(), 
				new CallBoundKeyCmd(){@Override public Boolean callOnKeyPressed(int iClickCountIndex) {
					if(flycam!=null && flycam.isEnabled())return true; //to ignore picking
					WorldPickingI.i().pickWorldPiercingAtCursor(iButtonIndex); //will call the world pick listeners
					return true;
				}}.setName(strPck+"MouseButton"+i)
			);
		}
		
	}
	
	public void addSkip(Spatial spt){
		UserDataI.i().retrieveExistingOrCreateNew(spt, Skip.class);
	}
	public static class Skip{}
	
	public boolean isSkip(Spatial spt){
		return UserDataI.i().contains(spt, Skip.class);
	}
	
	public CollisionResult pickCollisionResultAtCursor(int iButtonIndex){
		ArrayList<CollisionResult> crs = pickWorldPiercingAtCursor(iButtonIndex);
		if(crs==null)return null;
		return crs.get(0);
	}
	public ArrayList<CollisionResult> pickWorldPiercingAtCursor(int iButtonIndex){
		return pickWorldPiercingAtCursor(iButtonIndex,null);//MiscJmeI.i().getNodeVirtualWorld());
	}
	public ArrayList<CollisionResult> pickWorldPiercingAtCursor(int iButtonIndex,Node nodeVirtualWorld){
		acrLastPickList.clear();
		acrLastPickList.addAll(raycastPiercingAtCursor(nodeVirtualWorld));
		
		// call listeners
		Geometry geom = null; 
		Spatial spt = null;
		if(acrLastPickList.size()>0){
			geom = getLastWorldPickGeometry(); 
			spt = getLastWorldPickParentest();
		}
		for(IPickListener l:aplList){
			if(l.updatePickingEvent(iButtonIndex,acrLastPickList,geom,spt)){
				if(acrLastPickList.size()>0){
					if(bAllowConsume)break;
				}
			}
		}
		
		return acrLastPickList;
	}
	
	/**
	 * 
	 * @param nodeVirtualWorld can be null (will use default)
	 * @return
	 */
	public ArrayList<CollisionResult> raycastPiercingAtCursor(Node nodeVirtualWorld){
//		if(nodeVirtualWorld==null)nodeVirtualWorld=MiscJmeI.i().getNodeVirtualWorld();
//		
//		CollisionResults crs = new CollisionResults();
//		
//		Vector3f v3fCursorAtVirtualWorld3D = MiscJmeI.i().getApp().getCamera().getWorldCoordinates(
//			EnvironmentJmeI.i().getMouse().getPos2D(), 0f);
//		
//		Vector3f v3fDirection = MiscJmeI.i().getApp().getCamera().getWorldCoordinates(
//			EnvironmentJmeI.i().getMouse().getPos2D(), 1f);
//		v3fDirection.subtractLocal(v3fCursorAtVirtualWorld3D).normalizeLocal();
//		
//		Ray ray = new Ray(v3fCursorAtVirtualWorld3D, v3fDirection);
//		nodeVirtualWorld.collideWith(ray, crs);
//		
//		ArrayList<CollisionResult> acrList=new ArrayList<CollisionResult>();
//		if(crs.size()>0){
//			for(CollisionResult cr:Lists.newArrayList(crs.iterator())){
//				if(!isSkip(cr.getGeometry()))acrList.add(cr);
//			}
//		}
//		
//		return acrList;
		return raycastPiercingAt(nodeVirtualWorld, HWEnvironmentJmeI.i().getMouse().getPos3D());
	}
	
	public ArrayList<CollisionResult> raycastPiercingAtCenter(Node nodeVirtualWorld){
		return raycastPiercingAt(nodeVirtualWorld, HWEnvironmentJmeI.i().getDisplay().getCenter(0f)); //z will be ignored
	}
	public ArrayList<CollisionResult> raycastPiercingFromCenterTo(Node nodeVirtualWorld, Vector3f v3fDisplaceFromCenterXY){
		return raycastPiercingAt(nodeVirtualWorld, HWEnvironmentJmeI.i().getDisplay().getCenter(0f).add(v3fDisplaceFromCenterXY)); //z will be ignored
	}
	public ArrayList<CollisionResult> raycastPiercingAt(Node nodeVirtualWorld, Vector3f v3fGuiNodeXY){
		if(nodeVirtualWorld==null)nodeVirtualWorld=MiscJmeI.i().getNodeVirtualWorld();
		
		CollisionResults crs = new CollisionResults();
		
//		Vector3f v3fCursorAtVirtualWorld3D = MiscJmeI.i().getApp().getCamera().getWorldCoordinates(
//				EnvironmentJmeI.i().getMouse().getPos2D(), 0f);
		Vector2f v2f = MiscJmeI.i().toV2f(v3fGuiNodeXY);
		Vector3f v3fCursorAtVirtualWorld3D = MiscJmeI.i().getApp().getCamera().getWorldCoordinates(v2f,0f);
		
		Vector3f v3fDirection = MiscJmeI.i().getApp().getCamera().getWorldCoordinates(v2f,1f);
		v3fDirection.subtractLocal(v3fCursorAtVirtualWorld3D).normalizeLocal(); //norm just to grant it
		
		Ray ray = new Ray(v3fCursorAtVirtualWorld3D, v3fDirection);
		nodeVirtualWorld.collideWith(ray, crs);
		
		ArrayList<CollisionResult> acrList=new ArrayList<CollisionResult>();
		if(crs.size()>0){
			for(CollisionResult cr:Lists.newArrayList(crs.iterator())){
				if(!isSkip(cr.getGeometry()))acrList.add(cr);
			}
		}
		
		return acrList;
	}
	
	public Object debugTest(Object... aobj){ //keep even if emtpy!
		for(int i=0;i<6;i++){
			Plane p = MiscJmeI.i().getApp().getCamera().getWorldPlane(i);
//			Quad q = new Quad();p.get
			LoggingI.i().logEntry("CamPlane"+i+":"+p);
		}
		return null;
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
		if(acrLastPickList.size()==0)return null;
//		if(crLastPick.getClosestCollision()==null)return null;
		return acrLastPickList.get(0).getGeometry();
	}
	public ArrayList<CollisionResult> getLastWorldPiercingPickCopy(){
		return new ArrayList<CollisionResult>(acrLastPickList);
	}
//	public Ray getRayLastCast() {
//		return rayLastCast;
//	}

	public boolean isAllowConsume() {
		return bAllowConsume;
	}

	public WorldPickingI setAllowConsume(boolean bAllowConsume) {
		this.bAllowConsume = bAllowConsume;
		return this; //for beans setter
	}
}
