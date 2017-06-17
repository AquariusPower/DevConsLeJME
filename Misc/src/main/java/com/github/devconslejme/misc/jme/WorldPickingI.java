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
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.google.common.collect.Lists;
import com.jme3.collision.CollisionResult;
import com.jme3.collision.CollisionResults;
import com.jme3.input.FlyByCamera;
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
	
	private ArrayList<RayCastResultX>	acrLastPickList = new ArrayList<RayCastResultX>();
	private Ray	rayLastCast;
	private ArrayList<IPickListener> aplList = new ArrayList<IPickListener>();
	private boolean	bAllowConsume=true;
//	private Quad quadCamReversedPicking;
//	private Geometry	geomReversedPicking;
	private boolean bRayCastFromCenterUseCamPos;
	
	public static interface IPickListener{
		/**
		 * 
		 * @return true if consumed TODO prioritize listeners?
		 */
		boolean updatePickingEvent(int iButtonIndex, ArrayList<RayCastResultX> acrList, PhysicsData pdHit, Geometry geomHit, Spatial sptParentest);
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
	
	public RayCastResultX pickCollisionResultAtCursor(int iButtonIndex){
		ArrayList<RayCastResultX> crs = pickWorldPiercingAtCursor(iButtonIndex);
		if(crs==null)return null;
		return crs.get(0);
	}
	public ArrayList<RayCastResultX> pickWorldPiercingAtCursor(int iButtonIndex){
		return pickWorldPiercingAtCursor(iButtonIndex,null);//MiscJmeI.i().getNodeVirtualWorld());
	}
	public ArrayList<RayCastResultX> pickWorldPiercingAtCursor(int iButtonIndex,Node nodeVirtualWorld){
		acrLastPickList.clear();
		acrLastPickList.addAll(raycastPiercingAtCursor(nodeVirtualWorld));
		
		// call listeners
		Geometry geom = null; 
		Spatial spt = null;
		PhysicsData pd = null;
		if(acrLastPickList.size()>0){
			RayCastResultX rx = acrLastPickList.get(0);
			geom = rx.getGeom();//getLastWorldPickGeometry(); 
			spt = getLastWorldPickParentest(geom);
			pd = rx.getPD();
		}
		for(IPickListener l:aplList){
			if(l.updatePickingEvent(iButtonIndex,acrLastPickList,pd,geom,spt)){
				if(acrLastPickList.size()>0){
					if(bAllowConsume)break;
				}
			}
		}
		
		return acrLastPickList;
	}
	
	public Vector3f getProjectedLocationAtScreenOfSpatialAtWorld(Spatial sptFrom){
//		root
		return AppI.i().getScreenCoordinates(sptFrom.getWorldTranslation());
	}
	
	/**
	 * 
	 * @param nodeVirtualWorld see {@link #raycastPiercingAtXY(Node, Vector3f)}
	 * @return
	 */
	public ArrayList<RayCastResultX> raycastPiercingAtCursor(Node nodeVirtualWorld){
		return raycastPiercingAtXY(nodeVirtualWorld, HWEnvironmentJmeI.i().getMouse().getPos3D(), false);
	}
	
	/**
	 * 
	 * @param nodeVirtualWorld see {@link #raycastPiercingAtXY(Node, Vector3f)}
	 * @return
	 */
	public ArrayList<RayCastResultX> raycastPiercingAtCenter(Node nodeVirtualWorld){
		return raycastPiercingAtXY(nodeVirtualWorld, HWEnvironmentJmeI.i().getDisplay().getCenter(0f), false); //z will be ignored
	}
	/**
	 * 
	 * @param nodeVirtualWorld see {@link #raycastPiercingAtXY(Node, Vector3f)}
	 * @param v3fDisplaceFromCenterXY
	 * @return
	 */
	public ArrayList<RayCastResultX> raycastPiercingDisplFromCenter(Node nodeVirtualWorld, Vector3f v3fDisplaceFromCenterXY){
		return raycastPiercingAtXY(nodeVirtualWorld, HWEnvironmentJmeI.i().getDisplay().getCenter(0f).add(v3fDisplaceFromCenterXY), false); //z will be ignored
	}
	public ArrayList<RayCastResultX> raycastPiercingGeomsOnly(){
		return raycastPiercingAtXY(null, HWEnvironmentJmeI.i().getDisplay().getCenter(0f), true); //z will be ignored
	}
	/**
	 * if physics is not hit, will try just geometries
	 * @param nodeVirtualWorld automatic to default if null
	 * @param v3fGuiNodeXYZproj
	 * @return can be empty
	 */
	public ArrayList<RayCastResultX> raycastPiercingAtXY(Node nodeVirtualWorld, Vector3f v3fGuiNodeXYZproj, boolean bOnlyGeometries){
		if(nodeVirtualWorld==null)nodeVirtualWorld=MiscJmeI.i().getNodeVirtualWorld();
		
		CollisionResults crs = new CollisionResults();
		
		v3fGuiNodeXYZproj.z=0;
		Vector3f v3fCursorAtVirtualWorld3D = null;
		Vector3f v3fDisplayCenter = HWEnvironmentJmeI.i().getDisplay().getCenter(0f);
		boolean bIsCenter=v3fGuiNodeXYZproj.x==v3fDisplayCenter.x && v3fGuiNodeXYZproj.y==v3fDisplayCenter.y;
		if(bIsCenter && isRayCastFromCenterUseCamPos()) {
			v3fCursorAtVirtualWorld3D = AppI.i().getCamWPosCopy(v3fGuiNodeXYZproj.z);
		}else {
			v3fCursorAtVirtualWorld3D = AppI.i().getScreenPosAtWorldCoordinatesForRayCasting(v3fGuiNodeXYZproj);
		}
		
		v3fGuiNodeXYZproj.z=1;
		Vector3f v3fDirection = AppI.i().getScreenPosAtWorldCoordinatesForRayCasting(v3fGuiNodeXYZproj);
		v3fDirection.subtractLocal(v3fCursorAtVirtualWorld3D).normalizeLocal(); //norm just to grant it
			
		ArrayList<RayCastResultX> acrList = null;
		if(!bOnlyGeometries) {
			acrList=PhysicsI.i().rayCastSortNearest(
				v3fCursorAtVirtualWorld3D, v3fDirection, true, false, false);
			if(acrList.size()>0) {
				return acrList;
			}
		}else {
			acrList = new ArrayList<RayCastResultX>();
		}
		
		// if no physics found try a simple geometries ray cast 
		Ray ray = new Ray(v3fCursorAtVirtualWorld3D, v3fDirection);
		nodeVirtualWorld.collideWith(ray, crs);
		
//		ArrayList<PhysicsDataRayCastResultX> acrList=new ArrayList<PhysicsDataRayCastResultX>();
		if(crs.size()>0){
			for(CollisionResult cr:Lists.newArrayList(crs.iterator())){
				RayCastResultX resultx = new RayCastResultX(
					null, cr, PhysicsI.i().getPhysicsDataFrom(cr.getGeometry()), cr.getGeometry(), cr.getContactPoint(), cr.getContactNormal(), 
					cr.getDistance(), v3fCursorAtVirtualWorld3D, v3fDirection);
				if(!isSkip(cr.getGeometry()))acrList.add(resultx);
			}
		}
		
		return acrList;
	}
	
	public Object debugTest(Object... aobj){ //keep even if emtpy!
//		for(int i=0;i<6;i++){
//			Plane p = MiscJmeI.i().getApp().getCamera().getWorldPlane(i);
////			Quad q = new Quad();p.get
//			LoggingI.i().logEntry("CamPlane"+i+":"+p);
//		}
		return null;
	}
	
	/**
	 * 
	 * @return can be a Geometry or a Node
	 */
	public Spatial getLastWorldPickParentest(Geometry geom){
//		Geometry geom = getLastWorldPickGeometry();
		if(geom==null)return null;
		
		if(geom.getParent()==MiscJmeI.i().getNodeVirtualWorld()){
			return getLastWorldPickGeometry();
		}
		
		return SpatialHierarchyI.i().getParentestOrSelf(geom, Node.class, true, false);
//		ArrayList<Node> anode = SpatialHierarchyI.i().getAllParents(getLastWorldPick(),false);
//		return anode.get(anode.size()-1);
	}
	public Geometry getLastWorldPickGeometry(){
		if(acrLastPickList.size()==0)return null;
//		if(crLastPick.getClosestCollision()==null)return null;
		return acrLastPickList.get(0).getGeom();
	}
	public ArrayList<RayCastResultX> getLastWorldPiercingPickCopy(){
		return new ArrayList<RayCastResultX>(acrLastPickList);
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

	public boolean isRayCastFromCenterUseCamPos() {
		return bRayCastFromCenterUseCamPos;
	}

	public WorldPickingI setRayCastFromCenterUseCamPos(boolean bRayCastFromCenterUseCamPos) {
		this.bRayCastFromCenterUseCamPos = bRayCastFromCenterUseCamPos;
		return this; 
	}
}
