package com.github.devconslejme.tests.temp;

import com.jme3.app.SimpleApplication;
import com.jme3.input.event.MouseButtonEvent;
import com.jme3.input.event.MouseMotionEvent;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorMotionEvent;
import com.simsilica.lemur.event.MouseEventControl;
import com.simsilica.lemur.style.BaseStyles;
import com.simsilica.lemur.event.MouseListener;
import com.simsilica.lemur.event.CursorListener;

public class TestcaseMouseAndCursorListeners extends SimpleApplication{
	public static void main(String[] args) {
		TestcaseMouseAndCursorListeners test = new TestcaseMouseAndCursorListeners();
		test.start();
	}

	private Button	btn;

	@Override
	public void simpleInitApp() {
		GuiGlobals.initialize(this);
		BaseStyles.loadGlassStyle();
		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		
		initTest();
	}

	private void initTest() {
		btn = new Button("test");
		btn.setLocalTranslation(300, 300, 0);
		guiNode.attachChild(btn);
		
		MouseEventControl.addListenersToSpatial(btn, new TestMouseListener());
		CursorEventControl.addListenersToSpatial(btn, new TestCursorListener());
	}
	
	class TestMouseListener implements MouseListener{
		@Override
		public void mouseButtonEvent(MouseButtonEvent event, Spatial target,				Spatial capture) {
			dumpEvent(event.isConsumed(),event);
		}
		@Override		public void mouseEntered(MouseMotionEvent event, Spatial target,				Spatial capture) {		}
		@Override		public void mouseExited(MouseMotionEvent event, Spatial target,				Spatial capture) {		}
		@Override		public void mouseMoved(MouseMotionEvent event, Spatial target,				Spatial capture) {		}
	}
	class TestCursorListener implements CursorListener{
		@Override
		public void cursorButtonEvent(CursorButtonEvent event, Spatial target,				Spatial capture) {
			dumpEvent(event.isConsumed(),event);
		}
		@Override		public void cursorEntered(CursorMotionEvent event, Spatial target,				Spatial capture) {		}
		@Override		public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {		}
		@Override		public void cursorMoved(CursorMotionEvent event, Spatial target,		Spatial capture) {		}
	}
	
	public void dumpEvent(boolean b,Object event){
		System.out.println(event.toString()+"\nconsumed="+b+"\nhash="+event.hashCode());
		System.out.println();
	}
}
