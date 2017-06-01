/* 
Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * TODO fix several wrongly named methods that may be duplicating or messing functionalities...
 *  
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public abstract class EffectBaseAbs<THIS extends EffectBaseAbs> implements IEffect<THIS>{
	private Vector3f	v3fFrom;
	private Vector3f	v3fTo;
	private Vector3f	v3fFollowFromDisplacement;
	private Vector3f	v3fFollowToDisplacement;
	private Spatial sptFollowFrom;
	private Spatial sptFollowTo;
	private boolean	bToMouse;
	private boolean	bDiscarded;
	private long	iHoldUntilMilis;
//	private Vector3f	v3fHoldPreviousFrom=new Vector3f();
//	private Vector3f	v3fHoldPreviousTo=new Vector3f();
//	private int	iMaxHoldMilis = 1000;
	
	private ColorRGBA	colorRefDefault=ColorI.i().colorChangeCopy(ColorRGBA.Cyan, 0f, 0.75f);
	private ColorRGBA	colorRefBase;
	private Node	nodeParent;
	private Geometry	geom;
//	private Spatial	sptOwner;
	private boolean	bPlay = false;
	private boolean	bDiscardingByOwner=true;
	private boolean	bSimplyWaitParentIfNull=true;
	private boolean	bWaitParentBeSet=false;
	private boolean	bUseFollowToPosZ = false;
	private Float	fZAboveAllAtGuiNode;
	private boolean	bBugOverrideDisablePlay;
	
//	public EffectBaseAbs(Spatial sptOwner){
//		this();
//		setOwner(sptOwner);
//	}
	
	public EffectBaseAbs() {
		this.geom = new Geometry("Geom:"+this.getClass().getSimpleName());
		Mesh mesh = new Mesh();
		mesh.setStreamed();
		this.geom.setMesh(mesh);
	}
	
	@Override
	public Vector3f getLocationFrom() {
		Vector3f v3fTargetSpot = v3fFrom==null?null:v3fFrom.clone();
		if(sptFollowFrom!=null){
			//TODO if sptFollow is a node, add a node to it and apply displacement to let rotations etc apply
			v3fTargetSpot=sptFollowFrom.getWorldTranslation().add(v3fFollowFromDisplacement);
		}
		
		if(isUseFollowToPosZ()){
			v3fTargetSpot.z = getLocationTo().z;
		}
		
		if(fZAboveAllAtGuiNode!=null)v3fTargetSpot.z = fZAboveAllAtGuiNode; //last thing
		return v3fTargetSpot;
	}
	
	@Override
	public Vector3f getLocationTo() {
		Vector3f v3fTargetSpot = v3fTo==null?null:v3fTo.clone();
		if(bToMouse){
			v3fTargetSpot=HWEnvironmentJmeI.i().getMouse().getPos3D();
			if(v3fTo!=null)v3fTargetSpot.z=v3fTo.z;
//			v3fTargetSpot.z = v3fTo==null ? MiscJmeI.i().getZAboveAllAtGuiNode() : v3fTo.z;
		}else
		if(sptFollowTo!=null){
			//TODO if sptFollow is a node, add a node to it and apply displacement to let rotations etc apply
			v3fTargetSpot=sptFollowTo.getWorldTranslation().add(v3fFollowToDisplacement);
		}
		
		if(fZAboveAllAtGuiNode!=null)v3fTargetSpot.z = fZAboveAllAtGuiNode; //last thing
		return v3fTargetSpot;
	}

//	public Vector3f getV3fFrom() {
//		return v3fFrom;
//	}
//
	public THIS setFrom(Vector3f v3fFrom) {
		this.v3fFrom = v3fFrom;
		validateDistance();
		return getThis();
	}
//
//	public Vector3f getV3fTo() {
//		return v3fTo;
//	}
//
//	public void setV3fTo(Vector3f v3fTo) {
//		this.v3fTo = v3fTo;
//	}
//
//	public Vector3f getV3fFollowFromDisplacement() {
//		return v3fFollowFromDisplacement;
//	}
//
//	public void setV3fFollowFromDisplacement(Vector3f v3fFollowFromDisplacement) {
//		this.v3fFollowFromDisplacement = v3fFollowFromDisplacement;
//	}
//
//	public Vector3f getV3fFollowToDisplacement() {
//		return v3fFollowToDisplacement;
//	}
//
//	public void setV3fFollowToDisplacement(Vector3f v3fFollowToDisplacement) {
//		this.v3fFollowToDisplacement = v3fFollowToDisplacement;
//	}
//
//	public Spatial getSptFollowFrom() {
//		return sptFollowFrom;
//	}
//
//	public void setSptFollowFrom(Spatial sptFollowFrom) {
//		this.sptFollowFrom = sptFollowFrom;
//	}
//
//	public Spatial getSptFollowTo() {
//		return sptFollowTo;
//	}
//
//	public void setSptFollowTo(Spatial sptFollowTo) {
//		this.sptFollowTo = sptFollowTo;
//	}
//
//	public boolean isbToMouse() {
//		return bToMouse;
//	}
//
//	public void setbToMouse(boolean bToMouse) {
//		this.bToMouse = bToMouse;
//	}
	
	float lMaxDist=10000;
	private void validateDistance() {
		if(getLocationFrom()!=null && getLocationTo()!=null){
			if(getLocationFrom().distance(getLocationTo())>lMaxDist){ //TODO determine a good max distance...
				//just to not break the application with like buffer overflow (ex.: when creating a big line)
				MessagesI.i().warnMsg(this, "dist > "+lMaxDist, this, getLocationFrom(), getLocationTo(), sptFollowFrom, sptFollowTo);
				bBugOverrideDisablePlay=true;
			}else{
				bBugOverrideDisablePlay=false;
			}
		}
	}

	@Override
	public void assertNotDiscarded(){
		if(bDiscarded){
			throw new DetailedException("cant work with a discarded effect");
		}
	}
	
	@Override
	public THIS setFromTo(Vector3f v3fFrom, Vector3f v3fTo){
		assertNotDiscarded();
		this.v3fFrom=DetailedException.throwIfNull(v3fFrom); 
		this.v3fTo=DetailedException.throwIfNull(v3fTo);
		validateDistance();
		return getThis();
	}
	
	@Override
	public THIS setFollowToMouse(boolean b){
		assertNotDiscarded();
		this.bToMouse=b;
		validateDistance();
		return getThis();
	}
	
	@Override
	public THIS setFollowFromTarget(Spatial spt, Vector3f v3fDisplacement){
		assertNotDiscarded();
		sptFollowFrom=spt;
		v3fFollowFromDisplacement = v3fDisplacement==null?new Vector3f():v3fDisplacement;
		validateDistance();
		return getThis();
	}
	
	@Override
	public THIS setUseFollowToPosZ() {
		bUseFollowToPosZ=true;
		return getThis();
	}
	@Override
	public boolean isUseFollowToPosZ() {
		return bUseFollowToPosZ;
	}
	
	@Override
	public THIS setZOverride(float fZAboveAllAtGuiNode) {
		this.fZAboveAllAtGuiNode=fZAboveAllAtGuiNode;
		return getThis();
	}
	
	/**
	 * @param v3fDisplacement can be null
	 */
	@Override
	public THIS setFollowToTarget(Spatial spt, Vector3f v3fDisplacement){
		assertNotDiscarded();
		sptFollowTo=spt;
		v3fFollowToDisplacement = v3fDisplacement==null?new Vector3f():v3fDisplacement;
		validateDistance();
		return getThis();
	}
	
	@Override
	public THIS setColor(ColorRGBA colorRef){
		this.colorRefBase = colorRef!=null ? colorRef : colorRefDefault;
		this.geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(colorRef).clone()); //must be a clone or will modify the line thickness of all using the same material
		return getThis();
	}
		
	public void validateParent(){
		if(nodeParent!=null && nodeParent.getParent()!=null){
			throw new DetailedException("the specified parent has parent, so it's world bounds would consider the effect",nodeParent,this);
		}
	}
	
	@Override
	public THIS setNodeParentest(Node node){
//		if(node.getParent()!=null){
////			MessagesI.i().warnMsg(this, "the specified parent has parent, so it's world bounds will consider the effect, what may be a bad thing",node,this);
//			throw new DetailedException("the specified parent has parent, so it's world bounds would consider the effect",node,this);
//		}
		this.nodeParent=node;
		validateParent();
		return getThis();
	}
	
//	@Override
//	public Object getOwner() {
//		assertNotDiscarded();
//		return sptOwner;
//	}
	
	@Override
	public THIS setPlay(boolean b) {
		this.bPlay=b;
		if(!this.bPlay){
			assertNotDiscarded();
			if(geom!=null)geom.removeFromParent();
		}
		return getThis();
	}
	
	@Override
	public THIS setAsDiscarded() {
		setPlay(false);
		if(geom!=null)geom.removeFromParent();
		bDiscarded=true;
		return getThis();
	}

	@Override
	public void assertConfigIsValidAndFixIt() {
		assertNotDiscarded();
		if(!isPlaying())return; //not redundant when called directly after adding the effect
		
		if(colorRefBase==null){
			setColor(colorRefDefault);
		}
		
		if(v3fFrom==null && sptFollowFrom==null){
			throw new DetailedException("playing and 'from' not set",this);
		}
		
		if(v3fTo==null && sptFollowTo==null && !bToMouse){
			throw new DetailedException("playing and 'to' not set",this);
		}
		
		/**
		 * expectedly gui or virtual world root nodes
		 */
		if(nodeParent==null && sptFollowFrom!=null){
			nodeParent=SpatialHierarchyI.i().getParentestOrSelf(sptFollowFrom, Node.class, false); 
		}
		if(nodeParent==null && sptFollowTo!=null){
			nodeParent=SpatialHierarchyI.i().getParentestOrSelf(sptFollowTo, Node.class, false);
		}
		
		validateParent();
		
		if(bSimplyWaitParentIfNull){
			setWaitParent(nodeParent==null);
		}else{
			DetailedException.throwIfNull(nodeParent, "parent", sptFollowFrom, sptFollowTo, this);
		}
	}
	
	@Override
	public boolean isWaitingParent() {
		return bWaitParentBeSet;
	}
	
	@Override
	public THIS setWaitParent(boolean bWaitParentBeSet) {
		this.bWaitParentBeSet=bWaitParentBeSet;
		return getThis();
	}
	
	@Override
	public boolean isPlaying() {
		return bPlay;
	}

	@Override
	public THIS setSkipDiscardingByOwner() {
		bDiscardingByOwner=false;
		return getThis();
	}
	
	@Override
	public boolean isDiscardingByOwner() {
		return bDiscardingByOwner;
	}
	
//	@Override
//	public THIS setOwner(Spatial sptOwner) {
//		this.sptOwner=sptOwner;
//		return getThis();
//	}

	@Override
	public THIS clone() {
		try {
			return (THIS)this.getClass().newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new DetailedException(e);
		}
	}

//	public Vector3f getHoldPreviousFrom() {
//		return v3fHoldPreviousFrom;
//	}
//
//	public void setHoldPreviousFrom(Vector3f v3fHoldPreviousFrom) {
//		this.v3fHoldPreviousFrom = v3fHoldPreviousFrom;
//	}
//
//	public Vector3f getHoldPreviousTo() {
//		return v3fHoldPreviousTo;
//	}
//
//	public void setHoldPreviousTo(Vector3f v3fHoldPreviousTo) {
//		this.v3fHoldPreviousTo = v3fHoldPreviousTo;
//	}
//
//	public int getMaxHoldMilis() {
//		return iMaxHoldMilis;
//	}
//
//	public void setMaxHoldMilis(int iMaxHoldMilis) {
//		this.iMaxHoldMilis = iMaxHoldMilis;
//	}

//	public void setbPlay(boolean bPlay) {
//		this.bPlay = bPlay;
//	}

	@Override
	public Node getNodeParent() {
		return nodeParent;
	}

	public Geometry getGeom() {
		return geom;
	}

	public void setGeom(Geometry geom) {
		this.geom = geom;
	}

	@Override
	public void play(float tpf) {
		assertNotDiscarded();
		
		if(bBugOverrideDisablePlay)return;
		
		if(!isPlaying())return;
		
		if(!getNodeParent().hasChild(getGeom())){
			getNodeParent().attachChild(this.getGeom());
		}
		
		playWork(tpf);
		
		getGeom().setLocalTranslation(getLocationFrom());
	}

	protected abstract void playWork(float tpf);
	
	@Override
	public boolean isDiscarded() {
		return bDiscarded;
	}
}
