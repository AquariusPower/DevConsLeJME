package com.github.devconslejme.tests.temp;

import com.jme3.app.SimpleApplication;
import com.jme3.bullet.BulletAppState;
import com.jme3.bullet.BulletAppState.ThreadingType;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.PhysicsTickListener;
import com.jme3.bullet.collision.shapes.BoxCollisionShape;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.bullet.objects.PhysicsRigidBody;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;

/**
 * TEST CASE FAILED: the problem was between the chair and the keyboard! :>
 */
@Deprecated
public class TestCaseCcdFailOnScaledCollisionShape extends SimpleApplication implements PhysicsTickListener {
	public static void main(String[] args) {
		TestCaseCcdFailOnScaledCollisionShape test = new TestCaseCcdFailOnScaledCollisionShape();
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
		
		getCamera().setLocation(new Vector3f(0,-4,70));
		
		{
		BoxCollisionShape csfloor = new BoxCollisionShape(new Vector3f(25,0.05f,25));
		RigidBodyControl rbc = new RigidBodyControl(csfloor);
		rbc.setPhysicsLocation(new Vector3f(0,-10,0));
		rbc.setMass(0f);
		ps.add(rbc);
		}
		
		{
		BoxCollisionShape csfloor = new BoxCollisionShape(new Vector3f(0.5f,0.5f,0.5f));
		csfloor.setScale(new Vector3f(50,0.1f,50)); //TODO the debug visuals dont match?
		RigidBodyControl rbc = new RigidBodyControl(csfloor);
		rbc.setPhysicsLocation(new Vector3f(0,-5,0));
		rbc.setMass(0f);
		ps.add(rbc);
		}
		
		{
		BoxCollisionShape csfloor = new BoxCollisionShape(new Vector3f(0.5f,0.5f,0.5f));
		csfloor.setScale(new Vector3f(7.15f,0.30f,7.15f)); //TODO why couldnt I use 50,0.1f,50? is the native bullet debug visuals actually broken?
		RigidBodyControl rbc = new RigidBodyControl(csfloor);
		rbc.setPhysicsLocation(new Vector3f(0,0,0));
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
		
		if(System.currentTimeMillis() > (lWait+100)){
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
				if(r.getPhysicsLocation().y < -20){
					r.setMass(0f); //freeze
				}
			}
		}
	}
}



