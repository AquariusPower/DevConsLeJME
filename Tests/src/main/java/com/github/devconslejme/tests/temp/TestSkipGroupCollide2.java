package com.github.devconslejme.tests.temp;

import com.jme3.app.SimpleApplication;
import com.jme3.bullet.BulletAppState;
import com.jme3.bullet.BulletAppState.ThreadingType;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.collision.PhysicsCollisionGroupListener;
import com.jme3.bullet.collision.PhysicsCollisionObject;
import com.jme3.bullet.collision.shapes.BoxCollisionShape;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.bullet.objects.PhysicsGhostObject;
import com.jme3.bullet.objects.PhysicsRigidBody;
import com.jme3.math.Vector3f;

/**
 * this uses a ghost that would just generate events without interfering with collisions forces
 */
public class TestSkipGroupCollide2 extends SimpleApplication implements PhysicsCollisionGroupListener {
	public static void main(String[] args) {
		TestSkipGroupCollide2 test = new TestSkipGroupCollide2();
		test.start();
	}

	private BulletAppState bullet;
	private PhysicsSpace ps;
	private BoxCollisionShape csProjectile;
	private RigidBodyControl rbc;

	@Override
	public void simpleInitApp() {

		getCamera().setLocation(new Vector3f(0, 0, 20));
		getFlyByCamera().setEnabled(true);
		getFlyByCamera().setMoveSpeed(10f);

		bullet = new BulletAppState();
		bullet.setThreadingType(ThreadingType.PARALLEL);
		bullet.setDebugEnabled(true);

		getStateManager().attach(bullet);

		ps = bullet.getPhysicsSpace();
		ps.addCollisionGroupListener(this, PhysicsCollisionObject.COLLISION_GROUP_01);

		for (int i = 0; i < 5; i++) {
			PhysicsGhostObject csfloor = new PhysicsGhostObject(new BoxCollisionShape(new Vector3f(5, 0.06f, 5)));
			csfloor.setPhysicsLocation(new Vector3f(0, 2 - i, 0));
			ps.add(csfloor);
		}

		csProjectile = new BoxCollisionShape(new Vector3f(0.05f, 0.05f, 0.05f));

		rbc = new RigidBodyControl(csProjectile);
		rbc.setPhysicsLocation(new Vector3f(0, 10, 0));
		rbc.setCcdMotionThreshold(0.05f);
		rbc.setCcdSweptSphereRadius(0.05f);
		rbc.setMass(1f);
		ps.add(rbc);

	}

	@Override
	public boolean collide(PhysicsCollisionObject nodeA, PhysicsCollisionObject nodeB) {

		System.out.println(System.currentTimeMillis() + "," + ((PhysicsRigidBody) nodeB).getLinearVelocity());

		return false;
	}

	@Override
	public void simpleUpdate(float tpf) {
		if (rbc.getPhysicsLocation().y < -3) {
			rbc.setGravity(new Vector3f(0, 10, 0));
		} else if (rbc.getPhysicsLocation().y > 3) {
			rbc.setGravity(new Vector3f(0, -10, 0));
		}
	}
}
