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
		
		ps.addCollisionGroupListener(this,PhysicsCollisionObject.COLLISION_GROUP_02);
		
		getCamera().setLocation(new Vector3f(0,0,20));
		
		for(int i=0;i<5;i++)
		{
		BoxCollisionShape csfloor = new BoxCollisionShape(new Vector3f(5,0.05f,5));
		RigidBodyControl rbc = new RigidBodyControl(csfloor);
		rbc.setPhysicsLocation(new Vector3f(0,2-i,0));
		rbc.setMass(0f);
		rbc.setCollisionGroup(PhysicsCollisionObject.COLLISION_GROUP_02);
		rbc.addCollideWithGroup(PhysicsCollisionObject.COLLISION_GROUP_02);
		ps.add(rbc);
		}
		
		csProjectile = new BoxCollisionShape(new Vector3f(0.05f,0.05f,0.05f));
		
		RigidBodyControl rbc = new RigidBodyControl(csProjectile);
		rbc.setPhysicsLocation(new Vector3f(0,3,0));
		rbc.setMass(1f);
		rbc.setCcdMotionThreshold(0.05f);
		rbc.setCcdSweptSphereRadius(0.05f);
		rbc.setCollisionGroup(PhysicsCollisionObject.COLLISION_GROUP_02);
		rbc.addCollideWithGroup(PhysicsCollisionObject.COLLISION_GROUP_02);
		ps.add(rbc);
		
		rbcLast=rbc;
		
		lWait=System.currentTimeMillis();

	}
	
	@Override
	public void simpleUpdate(float tpf) {
		super.simpleUpdate(tpf);
		getFlyByCamera().setEnabled(true);
		org.lwjgl.input.Mouse.setGrabbed(true);//getInputManager().setCursorVisible(false);
		getFlyByCamera().setMoveSpeed(10f);
	}

	@Override
	public void prePhysicsTick(PhysicsSpace space, float tpf) {
		System.out.println("pre");
	}

	@Override
	public void physicsTick(PhysicsSpace space, float tpf) {
		System.out.println("tick");
		for(PhysicsRigidBody r:space.getRigidBodyList()){
			if(r.getMass()>0){
				if(r.getPhysicsLocation().y < -3){
					r.setGravity(new Vector3f(0,10,0));
				}else 
				if(r.getPhysicsLocation().y > 3){
					r.setGravity(new Vector3f(0,-10,0));
				}
			}
		}
	}

	@Override
	public boolean collide(PhysicsCollisionObject nodeA,PhysicsCollisionObject nodeB) {
		System.out.println(System.currentTimeMillis()+","+((PhysicsRigidBody)nodeA).getLinearVelocity()+","+((PhysicsRigidBody)nodeB).getLinearVelocity());
		return false; //why the 1st group collide cant be skipped/ignored?
	}
}
