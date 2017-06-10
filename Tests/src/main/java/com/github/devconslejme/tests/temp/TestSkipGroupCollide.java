package com.github.devconslejme.tests.temp;

import com.jme3.app.SimpleApplication;
import com.jme3.bullet.BulletAppState;
import com.jme3.bullet.BulletAppState.ThreadingType;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.PhysicsTickListener;
import com.jme3.bullet.collision.PhysicsCollisionGroupListener;
import com.jme3.bullet.collision.PhysicsCollisionObject;
import com.jme3.bullet.collision.shapes.BoxCollisionShape;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.bullet.objects.PhysicsRigidBody;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;

public class TestSkipGroupCollide extends SimpleApplication implements PhysicsTickListener, PhysicsCollisionGroupListener {
	public static void main(String[] args) {
		TestSkipGroupCollide test = new TestSkipGroupCollide();
		test.start();
	}

	private BulletAppState	bullet;
	private PhysicsSpace	ps;
	private long	lWait;
	private RigidBodyControl	rbcLast;
	private BoxCollisionShape	csProjectile;

	@Override
	public void simpleInitApp() {
		bullet = new BulletAppState();
		bullet.setThreadingType(ThreadingType.PARALLEL);
		getStateManager().attach(bullet);
		ps = bullet.getPhysicsSpace();
		
		bullet.setDebugEnabled(true);
		ps.addTickListener(this);
		
		ps.addCollisionGroupListener(this,PhysicsCollisionObject.COLLISION_GROUP_01);
		
		getCamera().setLocation(new Vector3f(0,-4,70));
		
		//for(int i=0;i<5;i++)
		{
		BoxCollisionShape csfloor = new BoxCollisionShape(new Vector3f(25,0.05f,25));
		RigidBodyControl rbc = new RigidBodyControl(csfloor);
		rbc.setPhysicsLocation(new Vector3f(0,5-(i*5),0));
		rbc.setMass(0f);
		ps.add(rbc);
		}
		
		csProjectile = new BoxCollisionShape(new Vector3f(0.05f,0.05f,0.05f));
	}
	
	@Override
	public void simpleUpdate(float tpf) {
		super.simpleUpdate(tpf);
		getFlyByCamera().setEnabled(true);
		org.lwjgl.input.Mouse.setGrabbed(true);//getInputManager().setCursorVisible(false);
		getFlyByCamera().setMoveSpeed(10f);
		
		if(System.currentTimeMillis() > (lWait+1000)){
			RigidBodyControl rbc = new RigidBodyControl(csProjectile);
			rbc.setPhysicsLocation(new Vector3f(FastMath.nextRandomFloat()*5,10,FastMath.nextRandomFloat()*5));
			rbc.setMass(1f);
			rbc.setCcdMotionThreshold(0.05f);
			rbc.setCcdSweptSphereRadius(0.05f);
			ps.add(rbc);
			
			rbcLast=rbc;
			
			lWait=System.currentTimeMillis();
		}
	}

	@Override
	public void prePhysicsTick(PhysicsSpace space, float tpf) {
		if(rbcLast==null)return;
		rbcLast.applyTorqueImpulse(new Vector3f(FastMath.nextRandomFloat(),FastMath.nextRandomFloat(),FastMath.nextRandomFloat()));
		rbcLast.applyImpulse(
			new Vector3f(FastMath.nextRandomFloat()-0.5f,-1f,FastMath.nextRandomFloat()-0.5f)
				.mult(50f), 
			Vector3f.ZERO);
		rbcLast=null;
	}

	@Override
	public void physicsTick(PhysicsSpace space, float tpf) {
		for(PhysicsRigidBody r:space.getRigidBodyList()){
			if(r.getMass()>0){
				if(r.getPhysicsLocation().y < -100){
					Vector3f v3f = r.getPhysicsLocation();
					v3f.y=20;
					r.setLinearVelocity(Vector3f.ZERO);
					r.setPhysicsLocation(v3f);
//					r.setMass(0f); //freeze
				}
			}
		}
	}

	@Override
	public boolean collide(PhysicsCollisionObject nodeA,PhysicsCollisionObject nodeB) {
		return false;
	}
}
