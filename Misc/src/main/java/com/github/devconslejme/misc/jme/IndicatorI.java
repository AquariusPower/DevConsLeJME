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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Box;
import com.simsilica.lemur.Panel;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class IndicatorI {
	public static IndicatorI i(){return GlobalManagerI.i().get(IndicatorI.class);}
	
	public static class GeomIndicator extends Geometry{
		private Spatial	spt;
		private boolean	bQueueLoop = true;
		private boolean	bEnabled;
		private Vector3f	v3fBouncingMoveDirection;
		private Vector3f	v3fLastBouncingValidPos;
		private boolean bCanBeDestroyed=true;

		public GeomIndicator() {
			super(GeomIndicator.class.getSimpleName(),new Box(1,1,3));
		}

		public GeomIndicator setTarget(Spatial spt){
			this.spt = spt;
			
			GeomIndicator giExisting = UserDataI.i().getUserDataPSH(spt, GeomIndicator.class);
			if(giExisting!=this){
				IndicatorI.i().destroyIndicator(spt);
				UserDataI.i().setUserDataPSH(spt, this);
			}
			
			return this;
		}
		
		public Spatial getTarget(){
			return spt;
		}
		
		public void endMyQueueLoop(){
			this.bQueueLoop=false;
		}
		
		public boolean isQueueLoop(){
			return bQueueLoop;
		}
		
		public GeomIndicator setEnabled(boolean b){
			this.bEnabled =b ;
			return this;
		}
		
		public boolean isEnabled(){
			return bEnabled;
		}

		public GeomIndicator  setBouncingMoveDirection(Vector3f v3fBouncingMoveDirection) {
			this.v3fBouncingMoveDirection = v3fBouncingMoveDirection;
			return this;
		}

		public Vector3f getBouncingMoveDirection() {
			return v3fBouncingMoveDirection;
		}

		public GeomIndicator setLastBouncingValidPos(Vector3f v3f) {
			this.v3fLastBouncingValidPos=v3f.clone();
			return this;
		}

		public Vector3f getLastBouncingValidPos() {
			return v3fLastBouncingValidPos;
		}
		
		/**
		 * useful for unique indicators that will be moved around, enabled, disabled and re-targeted
		 * @return
		 */
		public GeomIndicator setDenyDestruction(){
			bCanBeDestroyed=false;
			return this;
		}
		
		public boolean isCanBeDestroyed(){
			return bCanBeDestroyed;
		}
	}
	
//	private HashMap<Spatial,GeomIndicator> hmTargetIndicators = new HashMap<Spatial,GeomIndicator>();
	
	private boolean bBouncing=false;
	
	
	public GeomIndicator createIndicator(ColorRGBA color){
		GeomIndicator gi = new GeomIndicator();
		gi.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
		
		QueueI.i().enqueue(new CallableX() {
					@Override
					public Boolean call() {
						if(gi.isEnabled()){
							if(gi.getParent()==null){
								SpatialHierarchyI.i().getParentest(gi.getTarget(),Node.class,false)
									.attachChild(gi);
							}
							
							if(isBouncing()){
								bouncingUpdateMove(gi);
							}else{
								gi.setLocalTranslation(gi.getTarget().getWorldTranslation());
							}
							
							gi.rotate(0.1f,0.1f,0.1f);
						}else{
							gi.removeFromParent();
						}
						
						if(!gi.isQueueLoop()){
							disableLoop();
							gi.removeFromParent();
						}
						
						return true;
					}
				}
				.setName("IndicatorFollowTarget")
				.setUserCanPause(true)
				.setDelaySeconds(0.1f)
				.enableLoop()
			);
		
		return gi;
	}

	protected void bouncingUpdateMove(GeomIndicator gi) {
		Spatial sptTarget = gi.getTarget();
		Vector3f v3fPosNew = gi.getLocalTranslation().clone();
//		Vector3f v3fSize = MiscJmeI.i().getBoundingBoxSize(sptTarget);
		
		if(gi.getBouncingMoveDirection()==null){
			gi.setBouncingMoveDirection(MiscJmeI.i().randomDirection());
		}
		
		Vector3f v3fDir = gi.getBouncingMoveDirection();
		v3fPosNew.x+=v3fDir.x;
		v3fPosNew.y+=v3fDir.y;
		
		if(MiscJmeI.i().isInside(sptTarget, v3fPosNew, true)){
			gi.setLocalTranslation(v3fPosNew);
			gi.setLastBouncingValidPos(gi.getLocalTranslation());
		}else{
//			Vector3f v3fCenter = MiscJmeI.i().getCenterPos(sptTarget);
			if(gi.getLastBouncingValidPos()!=null && MiscJmeI.i().isInside(sptTarget, gi.getLastBouncingValidPos(), true)){
				// restore last valid pos
				gi.setLocalTranslation(gi.getLastBouncingValidPos());
			}else{ //reset to initial valid pos
				gi.setLocalTranslation(MiscJmeI.i().getWorldCenterPosCopy(sptTarget));
				gi.setLastBouncingValidPos(gi.getLocalTranslation());
			}
//			gi.restoreLastValidPos();
//			gi.setLocalTranslation(sptTarget.getWorldTranslation());
			
			// new direction
			gi.setBouncingMoveDirection(MiscJmeI.i().randomDirection());
		}
	}
	
	public void destroyIndicator(Spatial spt){
		GeomIndicator gi = UserDataI.i().getUserDataPSH(spt, GeomIndicator.class);
		if(gi!=null && gi.isCanBeDestroyed()){
			gi.endMyQueueLoop();
		}
	}
	
	public void destroyAllIndicatorsRecursively(Spatial spt) {
		if(spt instanceof Node){
			for(Spatial sptChild:SpatialHierarchyI.i().getAllChildrenRecursiveFrom(spt, Spatial.class, null)){
				destroyIndicator(sptChild);
			}
		}else{
			destroyIndicator(spt);
		}
	}

	public boolean isBouncing() {
		return bBouncing;
	}

	public void setBouncing(boolean bBouncing) {
		this.bBouncing = bBouncing;
	}
	
}
