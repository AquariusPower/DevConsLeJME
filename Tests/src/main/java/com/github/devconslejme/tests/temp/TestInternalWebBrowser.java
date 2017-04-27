package com.github.devconslejme.tests.temp;

import java.awt.Dimension;
import java.net.URI;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.web.WebView;

import javax.swing.JFrame;

import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.system.JmeCanvasContext;
import com.jme3.system.SystemListener;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.style.BaseStyles;

public class TestInternalWebBrowser extends SimpleApplication{
	public static void main(String[] args) {
		TestInternalWebBrowser test = new TestInternalWebBrowser();
		test.start();
	}

	@Override
	public void simpleInitApp() {
		GuiGlobals.initialize(this);
		BaseStyles.loadGlassStyle();
		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		
		initTest(null);
	}

	private void initTest(URI uri) {
			//internal web viewer possible? this is not working...
		
			// this is where the internal web browser (below) could be set into
			Application app=GlobalManagerI.i().get(Application.class);
			JmeCanvasContext jcc = (JmeCanvasContext)app.getContext(); //FIXME how make it be LwjglCanvas, right?
			jcc.setSystemListener((SystemListener)app);
			JFrame jf = new JFrame("tst");
			jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
			jf.add(jcc.getCanvas());
			Dimension dim = new Dimension(640, 480);
			jcc.getCanvas().setPreferredSize(dim);
			jf.pack();
			jf.setVisible(true);
		
		// this would be the internal web browser 
//			javafx.embed.swing.JFXPanel p;
//			javafx.scene.web.WebView w;
			JFXPanel p = new JFXPanel(); //jcomponent
			Platform.runLater(()->{
				WebView w = new WebView();
				w.getEngine().load(uri.toString());
				p.setScene(new Scene(w));
			});
	}
}
