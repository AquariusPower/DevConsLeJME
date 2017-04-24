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

package com.github.devconslejme.misc.jme;

import org.lwjgl.opengl.Display;

import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.input.InputManager;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.debug.Grid;
import com.jme3.scene.shape.Line;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class OrthogonalCursorStateI extends AbstractAppState{
	public static OrthogonalCursorStateI i(){return GlobalManagerI.i().get(OrthogonalCursorStateI.class);}

	private Geometry	geom;
	private Application	app;
	private Node	nodeParent;
	private InputManager	inputman;
	private int	fSize;
	
	public void configure(Node nodeParent){
		app = GlobalManagerI.i().get(Application.class);
		app.getStateManager().attach(this);
		inputman=app.getInputManager();
		this.nodeParent=nodeParent;
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		super.initialize(stateManager, app);
		geom=new Geometry(OrthogonalCursorStateI.class.getSimpleName());
		fSize=Math.max(Display.getWidth(),Display.getHeight())+10; //a bit more to hide the borders
		geom.setMesh(new Grid(3,3,fSize));
//		geom.lookAt(new Vector3f(v2f.x,v2f.y-1000000,0), Vector3f.UNIT_Z.mult(1000));
		geom.lookAt(new Vector3f(0,-1000000,0), Vector3f.UNIT_Z.mult(1000));
		geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(
			ColorI.i().colorChangeCopy(ColorRGBA.Yellow, 0f, 0.1f)));
		nodeParent.attachChild(geom);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		if(inputman.isCursorVisible()){
			Vector2f v2f = inputman.getCursorPosition();
			geom.setLocalTranslation(v2f.x-fSize, v2f.y+fSize, 1001);
		}
	}
	
	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		
		if(enabled){
			nodeParent.attachChild(geom);
		}else{
			geom.removeFromParent();
		}
		
	}
}
