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
import java.util.LinkedHashMap;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.InfoI;
import com.github.devconslejme.misc.InfoI.Info;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ActivatorI;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.ActivatorI.ActivetableListenerAbs;
import com.github.devconslejme.misc.jme.FlyByCameraX;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.HighlighterI;
import com.github.devconslejme.misc.jme.PhysicsData;
import com.github.devconslejme.misc.jme.PhysicsI;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.github.devconslejme.misc.jme.WorldPickingI;
import com.jme3.app.Application;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * Targets (and/or their root) can carry an {@link ActivetableListenerAbs}, that will be triggered when
 * they are selected.
 * 
 * TODO show target indicator on the gui layer (ray cast?)
 * TODO region select, create a box aligned to the cam rotation, extend it quite far, test each target for linesight with the raycast
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TargetI {
	public static TargetI i(){return GlobalManagerI.i().get(TargetI.class);}
	
	private TargetGeom tgtLastSingleTarget;
	private HashMap<Geometry,TargetGeom> hmGeomTgt = new HashMap();
	private Vector3f v3fRayCastFromXY;
	private Node	nodeWorld;
//	private Application	app;
	private FlyByCameraX	flycamx;
	private boolean bHighlightTargets;
	
	public void configure(Node nodeWorld, FlyByCameraX flycamx){
		this.flycamx = flycamx;
//		this.app=G.i(Application.class);
		
		this.nodeWorld=nodeWorld;
		
		KeyBindCommandManagerI.i().putBindCommandsLater(
			flycamx.prependFlyCamContextKeyMod(KeyCodeManagerI.i().getMouseTriggerKey(0).getFullId()),
			new CallBoundKeyCmd(){
				@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
					for(TargetGeom t:TargetI.i().getAllTargetsListCopy()){
						t.activateIfPossible();
					}
					return true;
				}
				@Override
				public Boolean callOnKeyReleased(int iClickCountIndex) {
					for(TargetGeom t:TargetI.i().getAllTargetsListCopy()){
						t.deactivateIfPossible();
					}
					return true;
				};
			}.setName("ActivateTarget").holdKeyPressedForContinuousCmd()
		);
		
		////////////// acquire target
		String strK=KeyCodeManagerI.i().getMouseTriggerKey(1).getFullId();
		String strFK=flycamx.prependFlyCamContextKeyMod(strK);
		String strPK=CharacterI.i().prependPossessedContextKeyMod(strK);
		//////// multi
		CallBoundKeyCmd cmdAddTgtMulti = new CallBoundKeyCmd(){
			@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
				TargetGeom tgt = acquireNewTarget(getRayCastFromXY());
				if(tgt!=null)addOrRemoveAtMultiTargetList(tgt); //toggles
				return true;
			}
		}.setName("AddTargetMulti");
		KeyBindCommandManagerI.i().putBindCommandsLater("Shift+"+strFK+","+"Shift+"+strPK,cmdAddTgtMulti);
		
		/////////// single
		CallBoundKeyCmd cmdSetTgtSg = new CallBoundKeyCmd(){
    	@Override	public Boolean callOnKeyPressed(int iClickCountIndex){
				TargetGeom tgt = acquireNewTarget(getRayCastFromXY()); //can be the same, wont toggle
				if(tgt!=null){
					clearLastSingleTarget();
					tgtLastSingleTarget=tgt;
				}
				return true;
			}
    }.setName("SetSingleTarget");
    KeyBindCommandManagerI.i().putBindCommandsLater(strFK+","+strPK,cmdSetTgtSg);
    
    ///////// tgt updates
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
		
		ArrayList<RayCastResultX> acr = WorldPickingI.i().raycastPiercingDisplFromCenter(null, v3f); //rnLastConfigured.v3fMarkersCenter
		if(acr.size()>0){
			Geometry geom = acr.get(0).getGeom();
			
			if(tgt==null){ //new one
				PhysicsData pd = acr.get(0).getPD();
//				PhysicsData pd = PhysicsI.i().getPhysicsDataFrom(geom);
				if(pd!=null && !pd.isTerrain()) {
					tgt = new TargetGeom(geom);
					tgt.geomTarget=(geom);
					tgt.setPhysicsData(pd);
				}
			}
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
		
		for(TargetGeom tgt:hmGeomTgt.values()){
			if(tgt==tgtLastSingleTarget)continue; //skip already updated
			updateTarget(tgt,tpf);
		}
		
		//////////////////////////// infos /////////////////////////////////////
		StringBuilder sb;
		
		////////////////// single tgt
		sb=new StringBuilder("");
		if(tgtLastSingleTarget!=null){
			String strSep=", ";
			
			sb.append(tgtLastSingleTarget.getGeometryHit().getName()).append(strSep);
			sb.append(tgtLastSingleTarget.getDistanceStr()).append(strSep);
			
			sb.append("pos("+StringTextJmeI.i().fmtVector3f(
				tgtLastSingleTarget.getGeometryHit().getWorldTranslation(),2)+")");
			sb.append(strSep);
			
			sb.append("rotdeg("+StringTextJmeI.i().fmtToDegrees(
				tgtLastSingleTarget.getGeometryHit().getWorldRotation(),2)+")");
			sb.append(strSep);
			
			PhysicsI.i().putPhysicsDataInfo(
				tgtLastSingleTarget.getGeometryHit(), 
				tgtLastSingleTarget.getOrCreateSubInfo("Phys")
			);
			if(tgtLastSingleTarget.hmSubInfos.size()>0){
				sb.append(
					InfoI.i().prepareFullInfoRecursive(tgtLastSingleTarget.hmSubInfos)
				);
			}
			sb.append(strSep);
			
			sb.append("ScrPos="+StringTextJmeI.i().fmtVector3f(CrossHairI.i().getTargetPosOnScreenCopy(),2));
			sb.append(strSep);

			sb.append("ScrPosRaw="+StringTextJmeI.i().fmtVector3f(CrossHairI.i().getTargetPosOnScreenRawCopy(),2));
		}
		HWEnvironmentJmeI.i().putCustomInfo("SgTgt:", tgtLastSingleTarget==null ? null : sb.toString());
		
		//////////////////// multi tgt
		ArrayList<TargetGeom> atgt = getMultiTargetListCopy();
		sb = new StringBuilder("");
//		for(TargetGeom tgt:getAllTargetsListCopy()){
		for(TargetGeom tgt:atgt){
			sb.append(tgt.getGeometryHit().getName());
			if(tgtLastSingleTarget!=null){
				if(tgt.getGeometryHit()==tgtLastSingleTarget.getGeometryHit()){
					sb.append("(Sg)");
				}
			}
			sb.append(", ");
		}
		HWEnvironmentJmeI.i().putCustomInfo("MlTgt:", atgt.size()==0 ? null : sb.toString());
	}
	
	public void clearMultiTargetList(){
		for(TargetGeom tgt:hmGeomTgt.values()){
			resetTargetIndicators(tgt);
		}
		hmGeomTgt.clear();
	}
	
	public ArrayList<TargetGeom> getMultiTargetListCopy(){
		return new ArrayList<TargetGeom>(hmGeomTgt.values());
	}
	
	/**
	 * last single + multi
	 * @return
	 */
	public ArrayList<TargetGeom> getAllTargetsListCopy() {
		ArrayList<TargetGeom> at = getMultiTargetListCopy();
		/**
		 * single has priority, will be 1st always!
		 */
		if(tgtLastSingleTarget!=null){
			TargetGeom tgtMatchingLast = hmGeomTgt.get(tgtLastSingleTarget.geomTarget);
			if(tgtMatchingLast==null){
				tgtMatchingLast=tgtLastSingleTarget;
			}else{
				at.remove(tgtMatchingLast);
			}
			at.add(0,tgtMatchingLast);
		}
		
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
		tgt.fTargetDist=AppI.i().getCamWPosCopy(0f).distance(tgt.getRootSpatial().getWorldTranslation());
		
		if(isHighlightTargets())HighlighterI.i().applyAt(tgt.getGeometryHit());
//		}
	}
	
	protected void resetTargetIndicators(TargetGeom tgt) {
		if(!tgt.isAllowReset())return;
		if(isHighlightTargets() && tgt.getGeometryHit()!=null)HighlighterI.i().removeFrom(tgt.getGeometryHit());
	}
	
	public void clearLastSingleTarget(){
		if(tgtLastSingleTarget!=null)resetTargetIndicators(tgtLastSingleTarget);
		tgtLastSingleTarget=null;
	}
	
	protected TargetGeom addOrRemoveAtMultiTargetList(TargetGeom tgt){
		if(hmGeomTgt.containsKey(tgt.getGeometryHit())){
			resetTargetIndicators(tgt);
			hmGeomTgt.remove(tgt.getGeometryHit());
		}else{
			hmGeomTgt.put(tgt.getGeometryHit(),tgt);
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
	
	public TargetGeom getLastSingleTarget() {
//		if(tgtLastSingleTarget!=null && tgtLastSingleTarget.getRootSpatial().getParent()==null)tgtLastSingleTarget=null;
		return tgtLastSingleTarget;
	}

	public Vector3f getRayCastFromXY() {
		return v3fRayCastFromXY!=null?v3fRayCastFromXY:HWEnvironmentJmeI.i().getDisplay().getCenter(0f);
	}

	public TargetI setRayCastFromXY(Vector3f v3fRayCastFromXY) {
		this.v3fRayCastFromXY = v3fRayCastFromXY;
		return this; 
	}

	public boolean isHighlightTargets() {
		return bHighlightTargets;
	}

	public TargetI setHighlightTargets(boolean bHighlightTargets) {
		this.bHighlightTargets = bHighlightTargets;
		return this; 
	}

	public class TargetGeom {
		private PhysicsData pd;
		private Float fTargetDist;
		private Geometry	geomTarget;
		private boolean bEnemy=false;
		private Spatial	sptAtRoot;
		private boolean	bAllowReset=true; //can be disabled outside here to stop glowing for ex 
//		private HashMap<String,HashMap<String,Info>> hmSubInfos = new HashMap<String,HashMap<String,Info>>();
		private LinkedHashMap<String,Info> hmSubInfos = new LinkedHashMap<String,Info>();
		
		public LinkedHashMap<String,Info> getOrCreateSubInfo(String strSubInfoKey){
			Info inf = hmSubInfos.get(strSubInfoKey);
			if(inf==null){
				LinkedHashMap<String, Info> hm=new LinkedHashMap<String, Info>(); //the linked keep the initial order!
				hmSubInfos.put(strSubInfoKey, new Info(strSubInfoKey,hm));
				return hm;
			}
			
			return inf.getValue();
		}

		public TargetGeom(Spatial sptSomeParentOrParentesOrSelf){
			this.sptAtRoot = SpatialHierarchyI.i().getParentestOrSelf(sptSomeParentOrParentesOrSelf, Spatial.class, true, false);
		}

		public void activateIfPossible() {
//			ActivatorI.i().activateIfPossible(geomTarget); //parentest spt will also be considered (least Root Node)
			ActivatorI.i().activateIfPossible(pd); //parentest spt will also be considered (least Root Node)
		}
		public void deactivateIfPossible() {
//			ActivatorI.i().deactivateIfPossible(geomTarget); //parentest spt will also be considered (least Root Node)
			ActivatorI.i().deactivateIfPossible(pd); //parentest spt will also be considered (least Root Node)
		}

		public boolean isAllowReset() {
			return bAllowReset;
		}

		public Spatial getRootSpatial() {
			return sptAtRoot;
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

		public PhysicsData getPhysicsData() {
			return pd;
		}

		public TargetGeom setPhysicsData(PhysicsData pd) {
			this.pd = pd;
			return this; 
		}

	}

}
