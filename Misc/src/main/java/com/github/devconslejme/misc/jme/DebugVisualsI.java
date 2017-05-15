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

import com.github.devconslejme.misc.Annotations.Bean;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.DebugVisualsI.ArrowGeometry.EFollowMode;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.debug.Arrow;
import com.jme3.scene.shape.Cylinder;

/**
 * These visuals are to not be integrated as child into other spatials as they would most probably 
 * modify their world bound, possibly causing troubles.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DebugVisualsI {
	public static DebugVisualsI i(){return GlobalManagerI.i().get(DebugVisualsI.class);}
	
	private boolean bVisualsEnabled=false;
	
	private float fUpdateDelay=1f;
	
	public void configure(){} //just to let the global be promptly instantiated
	
	public ArrowGeometry createArrowFollowing(Node nodeBase, Spatial sptFrom, Spatial sptTo, ColorRGBA color){
		ArrowGeometry ga = createArrow(color);
//		MiscJmeI.i().addToName(ga, DebugVisualsI.class.getSimpleName(), true);
		
		ga.setFromToCenterMode(EFollowMode.Edge, EFollowMode.Edge);
		
		ga.setControllingQueue(
			QueueI.i().enqueue(new CallableXAnon() {
//				private Spatial	sptBeingFollowed=sptTarget;
				@Override	public Boolean call() {
					if(ga.isDestroy()){
						ga.removeFromParent();
						endLoopMode();
					}else{
						if(isVisualsEnabled()){
							if(ga.getParent()==null)nodeBase.attachChild(ga);
							ga.setFromTo(sptFrom, sptTo);
						}else{
							if(ga.getParent()!=null)ga.removeFromParent();
						}
					}
					
					return true;
				}}.enableLoopMode().setDelaySeconds(getUpdateDelay()))
		);
	
		return ga;
	}
	
	@Bean
	public float getUpdateDelay() {
		return fUpdateDelay;
	}
	
	/**
	 * 
	 * @param fUpdateDelay >= 0
	 * @return
	 */
	@Bean
	public DebugVisualsI setUpdateDelay(float fUpdateDelay) {
		this.fUpdateDelay = fUpdateDelay>=0 ? fUpdateDelay : 0;
		return this;
	}

	public boolean isVisualsEnabled() {
		return bVisualsEnabled;
	}

	public void setVisualsEnabled(boolean bVisualsEnabled) {
		this.bVisualsEnabled = bVisualsEnabled;
	}
	
	public static class ArrowGeometry extends Geometry {
		private float	fScaleArrowTip= 0.025f;
		private boolean bDestroy=false;
//		private float	fScaleX = getScaleArrowTip();
//		private float	fScaleY = getScaleArrowTip();
		private CallableX	cx;
		
		private EFollowMode efmFrom;
		private EFollowMode efmTo;
		private Float	fOverrideLength;
		private Float	fOverrideZ;
		
		public static enum EFollowMode{
			Location,
			Center,
			Edge,
			;
		}
		
		public ArrowGeometry() {
			MiscJmeI.i().addToName(this, ArrowGeometry.class.getSimpleName(), true);
		}
		
		public ArrowGeometry setColor(ColorRGBA color){
			setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
			return this;
		}
		
		public ArrowGeometry setFromToCenterMode(EFollowMode eFrom, EFollowMode eTo){
			this.efmFrom=eFrom;
			this.efmTo=eTo;
			return this;
		}
		
		private Vector3f modePos(Spatial spt){
			Vector3f v3f = null;
			switch(efmFrom){
				case Edge: //TODO edge is special
				case Center:
					v3f = spt.getWorldBound().getCenter();
					break;
				case Location:
					v3f = spt.getLocalTranslation();
					break;
			}
			
			if(fOverrideZ!=null)v3f.z=fOverrideZ;
			
			return v3f;
		}
		
		public ArrowGeometry setFromTo(Spatial sptFrom, Spatial sptTo){
			Vector3f v3fFrom = modePos(sptFrom);
			Vector3f v3fTo = modePos(sptTo);
			setFromTo(v3fFrom, v3fTo);
			return this;
		}
		public ArrowGeometry setFromTo(Spatial spt, Vector2f v2fTo){
			setFromTo(spt, MiscLemurI.i().toV3f(v2fTo));
			return this;
		}
		public ArrowGeometry setFromTo(Spatial sptFrom, Vector3f v3fTo){
			Vector3f v3fFrom = modePos(sptFrom);
			setFromTo(v3fFrom, v3fTo);
			return this;
		}
		
		/**
		 * override length must be set<br>
		 * the v3fTo will be at the direction the spatial is rotated at Z column
		 * @param spt
		 * @return
		 */
		public ArrowGeometry setFrom(Spatial spt){
			DetailedException.assertNotNull(getOverrideLength());
			setFromTo(spt,
				spt.getLocalTranslation().add(
					spt.getWorldRotation().getRotationColumn(2)) //Z is to where it is looking at
			); 
			return this;
		}
		
		/**
		 * see {@link #setFromTo(Vector3f, Vector3f)}
		 * @param v3fTo
		 * @return
		 */
		public ArrowGeometry setTo(Vector3f v3fTo){
			setFromTo((Vector3f)null, v3fTo);
			return this;
		}
		/**
		 * 
		 * @param v3fFrom if null will keep its current from location
		 * @param v3fTo
		 * @return
		 */
		public ArrowGeometry setFromTo(Vector3f v3fFrom,Vector3f v3fTo){
			DetailedException.assertNotNull(v3fTo,this,v3fFrom);
			
			if(v3fFrom!=null){
				setLocalTranslation(v3fFrom);
			}else{
				v3fFrom=getLocalTranslation();
			}
			
			float fLength = getOverrideLength()!=null ? getOverrideLength() : v3fTo.distance(v3fFrom);
			
			setLocalScale(fScaleArrowTip , fScaleArrowTip, fLength);
			
			lookAt(v3fTo, Vector3f.UNIT_Y);
			
			return this;
		}

		public float getScaleArrowTip() {
			return fScaleArrowTip;
		}

		public ArrowGeometry setScaleArrowTip(float fScaleArrowTip) {
			this.fScaleArrowTip = fScaleArrowTip;
			return this;
		}

		public boolean isDestroy() {
			return bDestroy;
		}

		public void setDestroy() {
			this.bDestroy = true;
		}
		
		public CallableX getControllingQueue(){
			return cx;
		}
		public ArrowGeometry setControllingQueue(CallableX cx) {
			this.cx=cx;
			return this;
		}

		public Float getOverrideLength() {
			return fOverrideLength;
		}

		public ArrowGeometry setOverrideLength(Float fOverrideLength) {
			this.fOverrideLength = fOverrideLength;
			return this;
		}

		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("ArrowGeometry [fScaleArrowTip=");
			builder.append(fScaleArrowTip);
			builder.append(", bDestroy=");
			builder.append(bDestroy);
			builder.append(", cx=");
			builder.append(cx);
			builder.append(", fOverrideLength=");
			builder.append(fOverrideLength);
			builder.append("]");
			return builder.toString();
		}

		public ArrowGeometry setOverrideZ(float fZ) {
			this.fOverrideZ=fZ;
			return this;
		}
		
	}
	
	public ArrowGeometry createArrow(ColorRGBA color){
		ArrowGeometry geom = new ArrowGeometry();
		MiscJmeI.i().addToName(geom, DebugVisualsI.class.getSimpleName(), true);
		geom.setMesh(new Arrow(new Vector3f(0,0,1f))); //its size will be controled by z scale
		geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
		return geom;
	}

}
