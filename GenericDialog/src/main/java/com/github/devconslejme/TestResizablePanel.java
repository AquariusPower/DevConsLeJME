package com.github.devconslejme;

import com.jme3.math.ColorRGBA;
import com.jme3.scene.Node;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.component.QuadBackgroundComponent;

public class TestResizablePanel {
	private ResizablePanel	rzp;
	private Node	nodeParent;
	
	public TestResizablePanel(Node nodeParent){
		this.nodeParent=nodeParent;
	}
	
	public void test(){
//		rzp.setBorderSize(3);
		if(rzp!=null)return;
		
		rzp = new ResizablePanel(300,200,DevConsPluginStateI.i().getStyle());
		rzp.setLocalTranslation(100,350,1);
		nodeParent.attachChild(rzp);
		Button btn = new Button("hellow");
		btn.setBackground(new QuadBackgroundComponent(ColorRGBA.Red.clone()));//,5,5, 0.02f, false));
		rzp.setContents(btn);
	}
}
