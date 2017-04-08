package com.github.devconslejme.tests.temp;

import com.jme3.app.SimpleApplication;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.style.BaseStyles;

public class TestBaseCopyFrom extends SimpleApplication{
	public static void main(String[] args) {
		TestBaseCopyFrom test = new TestBaseCopyFrom();
		test.start();
	}

	@Override
	public void simpleInitApp() {
		GuiGlobals.initialize(this);
		BaseStyles.loadGlassStyle();
		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		
		initTest();
	}

	private void initTest() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("method not implemented yet");
	}
}