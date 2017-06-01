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

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.AppI;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingSphere;
import com.jme3.bounding.BoundingVolume;
import com.jme3.bullet.BulletAppState;
import com.jme3.bullet.BulletAppState.ThreadingType;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.PhysicsTickListener;
import com.jme3.bullet.collision.shapes.BoxCollisionShape;
import com.jme3.bullet.collision.shapes.CollisionShape;
import com.jme3.bullet.collision.shapes.SphereCollisionShape;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PhysicsI implements PhysicsTickListener{
	public static PhysicsI i(){return GlobalManagerI.i().get(PhysicsI.class);}
	
	public static class Impulse{
		private Spatial	spt;
		private RigidBodyControl	rbc;
		/** @DefSelfNote dont expose it, this class is a simplifier */
		private PhysicsSpace	ps;
		
		private Vector3f	v3fForce;
		private Vector3f	v3fForceLocation;
		private Vector3f	v3fRelPos;
		private Vector3f	v3fImpulse;
		private Vector3f	v3fTorque;
		private Vector3f	v3fTorqueImpulse;
		
		public Spatial getSpt() {
			return spt;
		}
		public Impulse setSpt(Spatial spt) {
			this.spt = spt;
			return this;
		}
		public RigidBodyControl getRBC() {
			return rbc;
		}
		public Impulse setRBC(RigidBodyControl rbc) {
			this.rbc = rbc;
			return this;
		}
		
		public PhysicsSpace getPs() {
			return ps;
		}
		
		public Vector3f getForce() {
			return v3fForce;
		}
		public Impulse setForce(Vector3f v3fForce) {
			this.v3fForce = v3fForce;
			return this;
		}
		public Vector3f getForceLocation() {
			return v3fForceLocation;
		}
		public Impulse setForceLocation(Vector3f v3fForceLocation) {
			this.v3fForceLocation = v3fForceLocation;
			return this;
		}
		public Vector3f getRelPos() {
			return v3fRelPos;
		}
		public Impulse setRelPos(Vector3f v3fRelPos) {
			this.v3fRelPos = v3fRelPos;
			return this;
		}
		public Vector3f getImpulse() {
			return v3fImpulse;
		}
		public Impulse setImpulse(Vector3f v3fImpulse) {
			this.v3fImpulse = v3fImpulse;
			return this;
		}
		public Vector3f getV3fTorque() {
			return v3fTorque;
		}
		public Impulse setTorque(Vector3f v3fTorque) {
			this.v3fTorque = v3fTorque;
			return this;
		}
		public Vector3f getTorqueImpulse() {
			return v3fTorqueImpulse;
		}
		public Impulse setTorqueImpulse(Vector3f v3fTorqueImpulse) {
			this.v3fTorqueImpulse = v3fTorqueImpulse;
			return this;
		}
		
	}

	private BulletAppState	bullet;
	private PhysicsSpace	ps;
	
	public void configure(){
		bullet = new BulletAppState();
		bullet.setThreadingType(ThreadingType.PARALLEL);
		AppI.i().attatchAppState(bullet);
		
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
				ps = bullet.getPhysicsSpace();
				ps.addTickListener(PhysicsI.this);
//				return true;
//			}
//		});
	}
	
	private ArrayList<Impulse> arbcQueue = new ArrayList();
	
	/**
	 * 
	 * @param spt
	 * @return dynamic: mass 1f
	 */
	public RigidBodyControl imbueFromWBounds(Spatial spt){
		BoundingVolume bv = spt.getWorldBound();
		RigidBodyControl rbc=null;
		CollisionShape cs=null;
		if (bv instanceof BoundingBox) {
			BoundingBox bb = (BoundingBox) bv;
			cs = new BoxCollisionShape(bb.getExtent(null));
		}else
		if (bv instanceof BoundingSphere) {
			BoundingSphere bb = (BoundingSphere) bv;
			cs = new SphereCollisionShape(bb.getRadius());
		}else{
			throw new DetailedException("unsupported "+bv.getClass(),spt);
		}
		
		cs.setScale(spt.getWorldScale());
		rbc= new RigidBodyControl(cs);
		rbc.setMass(1f);
		
		spt.addControl(rbc);
		
		ps.add(spt);
		
		return rbc;
	}
	
	public void add(Spatial spt){
		ps.add(spt);
	}
	
	public void enqueue(Spatial spt, Impulse imp){
//		if(obj instanceof Spatial){
			imp.spt = spt;//(Spatial)obj;
			imp.rbc = imp.spt.getControl(RigidBodyControl.class);
//		}else
//		if(obj instanceof RigidBodyControl){
//			imp.rbc=(RigidBodyControl)obj;
//		}else{
//			throw new DetailedException("unsupported type "+obj.getClass().getName(),obj,imp);
//		}
		imp.ps=ps;
		
		synchronized (arbcQueue) {
			arbcQueue.add(imp);
		}
	}

	@Override
	public void prePhysicsTick(PhysicsSpace ps, float tpf) {
		synchronized(arbcQueue){
			for(Impulse imp:arbcQueue){
				assert imp.ps==ps;
				
				if(imp.v3fForce!=null){
					if(imp.v3fForceLocation==null){
						imp.rbc.applyCentralForce(imp.v3fForce);
					}else{
						imp.rbc.applyForce(imp.v3fForce, imp.v3fForceLocation);
					}
				}
				
				if(imp.v3fImpulse!=null && imp.v3fRelPos!=null)
					imp.rbc.applyImpulse(imp.v3fImpulse, imp.v3fRelPos);
				
				if(imp.v3fTorque!=null)
					imp.rbc.applyTorque(imp.v3fTorque);
				
				if(imp.v3fTorqueImpulse!=null)
					imp.rbc.applyTorque(imp.v3fTorqueImpulse);
				
			}
			
			arbcQueue.clear();
		}
	}

	@Override
	public void physicsTick(PhysicsSpace space, float tpf) {
		//TODO write current forces to spatials for easy access?
	}
}
