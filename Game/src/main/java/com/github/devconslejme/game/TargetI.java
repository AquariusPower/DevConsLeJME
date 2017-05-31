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
package com.github.devconslejme.game;

import java.util.ArrayList;
import java.util.HashMap;

import com.github.devconslejme.devcons.LoggingI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.HighlighterI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.jme.WorldPickingI;
import com.jme3.app.Application;
import com.jme3.collision.CollisionResult;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.UserData;

/**
 * TODO show target indicator on the gui layer (ray cast?)
 * TODO region select, create a box aligned to the cam rotation, extend it quite far, test each target for linesight with the raycast
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TargetI {
	public static TargetI i(){return GlobalManagerI.i().get(TargetI.class);}
	
	private TargetGeom tgtLastSingleTarget;
	private HashMap<Geometry,TargetGeom> atgtMulti = new HashMap();
	private Vector3f v3fRayCastFromXY;
	private Node	nodeWorld;
	private Application	app;
	
	public static interface IActivetableListener{
		void activateEvent(); 
	}
	
	public void configure(Node nodeWorld){
		this.app=G.i(Application.class);
		
		this.nodeWorld=nodeWorld;
		
		KeyBindCommandManagerI.i().putBindCommandsLater(KeyCodeManagerI.i().getMouseTriggerKey(0).getFullId(),new CallBoundKeyCmd(){
			@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
				for(TargetGeom t:TargetI.i().getAllTargets()){
					t.activateIfPossible();
				}
				return true;
			}
		}.setName("Activate"));
		
		String strK=KeyCodeManagerI.i().getMouseTriggerKey(1).getFullId();
		KeyBindCommandManagerI.i().putBindCommandsLater("Shift+"+strK,new CallBoundKeyCmd(){
			@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
				TargetGeom tgt = acquireNewTarget(v3fRayCastFromXY);
				if(tgt!=null)addOrRemoveAtMultiTargetList(tgt);
				return true;
			}
		}.setName("AddTargetMulti"));
    KeyBindCommandManagerI.i().putBindCommandsLater(strK,new CallBoundKeyCmd(){
    	@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
				clearLastTarget();
				tgtLastSingleTarget=acquireNewTarget(v3fRayCastFromXY);
				return true;
			}
    }.setName("SetSingleTarget"));
    
    QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				update(getTPF());
				return true;
			}
		}).enableLoopMode().setName("UpdateLastTarget");
	}
	
	protected TargetGeom acquireNewTarget(Vector3f v3f){
		TargetGeom tgt=null;
		
		ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingFromCenterTo(
			null, v3f); //rnLastConfigured.v3fMarkersCenter
		if(acr.size()>0){
			Geometry geom = acr.get(0).getGeometry();
			tgt = new TargetGeom(SpatialHierarchyI.i().getParentest(geom, Node.class, true, false));
			tgt.geomTarget=(geom);
		}
		
		if(tgt!=null){
			updateTarget(tgt, null);
			MessagesI.i().output(System.out,"Info",this,
				"Target:"
					+tgt.getRootSpatial().getName()
					+"("+tgt.getGeometryHit().getName()+")" //TODO head, arm etc
					+"@"+tgt.getDistanceStr());
		}
		
		return tgt;
	}
	
	protected void update(float tpf) {
		if(tgtLastSingleTarget!=null && tgtLastSingleTarget.getRootSpatial().getParent()==null){
			tgtLastSingleTarget=null;
		}
		
		if(tgtLastSingleTarget!=null)updateTarget(tgtLastSingleTarget,tpf);
		
		for(TargetGeom tgt:atgtMulti.values()){
			if(tgt==tgtLastSingleTarget)continue; //skip already updated
			updateTarget(tgt,tpf);
		}
	}

//	protected void updateTarget2(Target tgt, float fTPF){
//		if(tgt==tgtLast){
//			if(tgtLast!=null){
//				updateTarget(tgtLast,fTPF);
//			}else{
//				resetTarget(tgtLast);
//				tgtLast=null;
//			}
//		}
//	}
	
	public void clearMultiTargetList(){
		for(TargetGeom tgt:atgtMulti.values()){
			resetTargetIndicators(tgt);
		}
		atgtMulti.clear();
	}
	
	public ArrayList<TargetGeom> getMultiTargetListCopy(){
		return new ArrayList<TargetGeom>(atgtMulti.values());
	}
	
	/**
	 * last single + multi
	 * @return
	 */
	public ArrayList<TargetGeom> getAllTargets() {
		ArrayList<TargetGeom> at = getMultiTargetListCopy();
		if(tgtLastSingleTarget!=null)at.add(0,tgtLastSingleTarget);
		return at;
	}
	
	/**
	 * 
	 * @param tgt
	 * @param fTPF if null will be ignored
	 */
	protected void updateTarget(TargetGeom tgt, Float fTPF){
//				tgtLast = TargetI.i().getLastTarget();
//		if(tgt.getRootSpatial().hasAncestor(nodeWorld)){ //TODO is this necessary?
		tgt.fTargetDist=(app.getCamera().getLocation().distance(
			tgt.getRootSpatial().getWorldTranslation()));
		
		HighlighterI.i().applyAt(tgt.getGeometryHit());
//		}
	}
	
	protected void resetTargetIndicators(TargetGeom tgt) {
		if(!tgt.isAllowReset())return;
		if(tgt.getGeometryHit()!=null)HighlighterI.i().removeFrom(tgt.getGeometryHit());
	}
	
	public void clearLastTarget(){
		if(tgtLastSingleTarget!=null)resetTargetIndicators(tgtLastSingleTarget);
		tgtLastSingleTarget=null;
	}
	
	protected TargetGeom addOrRemoveAtMultiTargetList(TargetGeom tgt){
		if(atgtMulti.containsKey(tgt.getGeometryHit())){
			resetTargetIndicators(tgt);
			atgtMulti.remove(tgt.getGeometryHit());
		}else{
			atgtMulti.put(tgt.getGeometryHit(),tgt);
		}
		return tgt;
	}
	public TargetGeom addOrRemoveAtMultiTargetList(Spatial spt){
		TargetGeom tgt = applyAt(spt);
		return addOrRemoveAtMultiTargetList(tgt);
	}
	public TargetGeom applyAt(Spatial spt){
		return new TargetGeom(spt);
	}
	
	public TargetGeom getLastTarget() {
//		if(tgtLastSingleTarget!=null && tgtLastSingleTarget.getRootSpatial().getParent()==null)tgtLastSingleTarget=null;
		return tgtLastSingleTarget;
	}

	public Vector3f getRayCastFromXY() {
		return v3fRayCastFromXY;
	}

	public TargetI setRayCastFromXY(Vector3f v3fRayCastFromXY) {
		this.v3fRayCastFromXY = v3fRayCastFromXY;
		return this; 
	}

	public class TargetGeom {
		protected Float fTargetDist;
		private Geometry	geomTarget;
		private boolean bEnemy=false;
		private Spatial	sptRoot;
		private boolean	bAllowReset=true; //can be disabled outside here to stop glowing for ex 

		public TargetGeom(Spatial spt){
			this.sptRoot = spt;
		}

		public void activateIfPossible() {
			IActivetableListener ial = UserDataI.i().getMustExistOrNull(geomTarget,IActivetableListener.class);
			if(ial!=null)ial.activateEvent();
			if(sptRoot!=geomTarget){
				IActivetableListener ialRoot = UserDataI.i().getMustExistOrNull(sptRoot,IActivetableListener.class);
				if(ialRoot!=null)ialRoot.activateEvent();
			}
		}

		public boolean isAllowReset() {
			return bAllowReset;
		}

		public Spatial getRootSpatial() {
			return sptRoot;
		}

		public boolean isEnemy() {
			return bEnemy;
		}

		public TargetGeom setEnemy(boolean bEnemy) {
			this.bEnemy = bEnemy;
			return this; 
		}

		public Geometry getGeometryHit() {
			return geomTarget;
		}

		public String getDistanceStr() {
			return DistancesI.i().fmtDist(fTargetDist);
		}
		
		public Float getDistance() {
			return fTargetDist;
		}

		public TargetGeom setAllowReset(boolean bAllowReset) {
			this.bAllowReset = bAllowReset;
			return this; 
		}

	}

}
