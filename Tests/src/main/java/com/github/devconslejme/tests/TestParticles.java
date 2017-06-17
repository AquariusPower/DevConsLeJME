package com.github.devconslejme.tests;

import com.github.devconslejme.projman.SimpleApplicationAndStateAbs;

public class TestParticles extends SimpleApplicationAndStateAbs {
	public static void main(String[] args) {
		TestParticles test = new TestParticles();
		test.start();
	}

	@Override
	public void simpleInitApp() {
		//TODO com.github.devconslejme.misc.TODO.PkgCfgI.i().configure();
		initTest();
	}
	
	@Override
	public void update(float tpf) {
//		throw new UnsupportedOperationException("method not implemented");
	}
	
	/**
	 * public so can be called from devcons user cmds
	 */
	@Override
	public void initTest() {
		super.initTest();
	}
}
