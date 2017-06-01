package com.github.devconslejme.tests.temp;

import com.jme3.app.SimpleApplication;
import com.jme3.bullet.BulletAppState;
import com.jme3.bullet.BulletAppState.ThreadingType;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.PhysicsTickListener;
import com.jme3.bullet.collision.shapes.BoxCollisionShape;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;

public class TestCaseCcdFailOnScaledCollisionShape extends SimpleApplication implements PhysicsTickListener {
	public static void main(String[] args) {
		TestCaseCcdFailOnScaledCollisionShape test = new TestCaseCcdFailOnScaledCollisionShape();
		test.start();
	}

	private BulletAppState	bullet;
	private PhysicsSpace	ps;
	private long	lWait;
	private RigidBodyControl	rbcLast;

	@Override
	public void simpleInitApp() {
		bullet = new BulletAppState();
		bullet.setThreadingType(ThreadingType.PARALLEL);
		getStateManager().attach(bullet);
		ps = bullet.getPhysicsSpace();
		
		bullet.setDebugEnabled(true);
		ps.addTickListener(this);
		
		getCamera().setLocation(new Vector3f(0,0,50));
		
		{
		BoxCollisionShape csfloor = new BoxCollisionShape(new Vector3f(25,0.05f,25));
		RigidBodyControl rbc = new RigidBodyControl(csfloor);
		rbc.setPhysicsLocation(new Vector3f(0,-10,0));
		rbc.setMass(0f);
		ps.add(rbc);
		}
		
		{
		BoxCollisionShape csfloor2 = new BoxCollisionShape(new Vector3f(0.5f,0.5f,0.5f));
		csfloor2.setScale(new Vector3f(7.15f,0.30f,7.15f)); //TODO why couldnt I use 50,0.1f,50?
		RigidBodyControl rbc2 = new RigidBodyControl(csfloor2);
		rbc2.setMass(0f);
		ps.add(rbc2);
		}
		
	}
	
	@Override
	public void simpleUpdate(float tpf) {
		super.simpleUpdate(tpf);
		getFlyByCamera().setEnabled(true);
		getFlyByCamera().setMoveSpeed(10f);
		
		if(System.currentTimeMillis() > (lWait+1000)){
			BoxCollisionShape cs = new BoxCollisionShape(new Vector3f(0.05f,0.05f,0.05f));
			RigidBodyControl rbc = new RigidBodyControl(cs);
			rbc.setPhysicsLocation(new Vector3f(FastMath.nextRandomFloat()*5,5,FastMath.nextRandomFloat()*5));
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
				.mult(10f), 
			Vector3f.ZERO);
		rbcLast=null;
	}

	@Override
	public void physicsTick(PhysicsSpace space, float tpf) {
	}
}



