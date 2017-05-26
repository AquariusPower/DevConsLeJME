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
package com.github.devconslejme.tests;

import com.github.devconslejme.projman.SimpleAppStateAbs;
import com.jme3.bullet.BulletAppState;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.PhysicsTickListener;
import com.jme3.bullet.collision.shapes.CapsuleCollisionShape;
import com.jme3.bullet.control.CharacterControl;
import com.jme3.bullet.objects.PhysicsCharacter;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class TestPhysicsPicker extends SimpleAppStateAbs implements PhysicsTickListener {
	private BulletAppState	bullet;
	private PhysicsSpace	physicsSpace;
	private PhysicsCharacter	pchPlayer;

	public static void main(String[] args) {
		TestPhysicsPicker test = new TestPhysicsPicker();
		test.start();
	}
	
	@Override
	public void simpleInitApp() {
		com.github.devconslejme.misc.jme.PkgCfgI.i().configure(this,getGuiNode(),getRootNode());
		initTest();
	}
	
	@Override
	public void update(float tpf) {
	}
	
	/**
	 * public so can be called from devcons user cmds
	 */
	@Override
	public void initTest() {
		super.initTest();
		
		bullet = new BulletAppState();
		
		getStateManager().attach(bullet);
		
		physicsSpace = bullet.getPhysicsSpace();
		physicsSpace.addTickListener(this);
		
		pchPlayer = new PhysicsCharacter(new CapsuleCollisionShape(0.25f, 1.7f, 1), 0.5f);
		physicsSpace.add(pchPlayer);
		
		new CharacterControl();
	}
	
//	public static class BulletAppStateX extends BulletAppState{}

	@Override
	public void prePhysicsTick(PhysicsSpace space, float tpf) {
	}

	@Override
	public void physicsTick(PhysicsSpace space, float tpf) {
	}
}
