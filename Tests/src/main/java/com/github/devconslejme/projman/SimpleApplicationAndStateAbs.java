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
package com.github.devconslejme.projman;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.FlyByCameraX;
import com.github.devconslejme.tests.TestDevConsFull;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.state.AppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.input.FlyByCamera;
import com.jme3.renderer.Camera;
import com.jme3.renderer.RenderManager;
import com.jme3.scene.Node;

/**
 * Two behaviors, can also be called from another application and behave as a app state.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public abstract class SimpleApplicationAndStateAbs extends SimpleApplication implements AppState{
  private boolean bInit = false;
  private boolean bEnabled = false;
//  private Application	app;
//  private SimpleApplication	sapp;
  
  private boolean bUseFullTestPipe=true; //this prevents the specific test being run directly
  
  @Override
  public void start() {
  	if(bUseFullTestPipe) {
  		TestDevConsFull.main(null); 
  	}else {
  		super.start();
  	}
  }
  
	/**
	 * This 1 line code below can only be used when overriding this method!   
	 * com.github.devconslejme.TODO.PkgCfgI.i().configure();
	 * 
	 * you can also use {@link #initTest()} for the specific test inits
	 */
	@Override
	public abstract void simpleInitApp();
	
	/**
	 * use the update from the app state
	 */
	@Deprecated	@Override	public void simpleUpdate(float tpf) {}
	
  @Override
	public void initialize(AppStateManager stateManager, Application app) {
//  	initTest();
		bInit = true; //this is almost dummy considering initTest() ...
  }

//  protected abstract void initTest();
  protected void initTest(){
		bEnabled=true;
//		app=(G.i(Application.class));
//		sapp=G.i(SimpleApplication.class);
//		app.getStateManager().attach(this); //can be another applicaiton using this as a app state only
		AppI.i().attatchAppState(this); //can be another applicaiton using this as a app state only
  }
  
  //TODO add all other alt-app methods
  
  @Override
  public FlyByCamera getFlyByCamera() {
//  	return sapp!=null&&sapp!=this ? sapp.getFlyByCamera():super.getFlyByCamera();
  	return G.i(FlyByCameraX.class);
  }
//  public boolean isFlyByCameraX(){
//  	return (getFlyByCamera() instanceof FlyByCameraX);
//  }
//  public FlyByCameraX getFlyByCameraX() {
//  	return (FlyByCameraX)getFlyByCamera();
//  }
  
  @Deprecated
  @Override
  public Camera getCamera() {
//  	return sapp!=null&&sapp!=this ? sapp.getCamera():super.getCamera();
  	throw new DetailedException("forbidden, use "+AppI.class);
  }
  
  @Override
  public Node getRootNode() {
//  	return sapp!=null&&sapp!=this ? sapp.getRootNode():super.getRootNode();
  	return AppI.i().getRootNode();
  }
  
  @Override
  public Node getGuiNode() {
//  	return sapp!=null&&sapp!=this ? sapp.getGuiNode():super.getGuiNode();
  	return AppI.i().getGuiNode();
  }
  
	@Override
	public boolean isInitialized() {
      return bInit;
  }

  @Override
	public void setEnabled(boolean enabled) {
      this.bEnabled = enabled;
  }
  
  @Override
	public boolean isEnabled() {
      return bEnabled;
  }

  @Override	public void stateAttached(AppStateManager stateManager) {}
  @Override	public void stateDetached(AppStateManager stateManager) {}
  @Override	public void render(RenderManager rm) {}
  @Override	public void postRender(){}

  @Override
	public void cleanup() {
      bInit = false;
  }

//	public Application getApp() {
//		return app;
//	}
//
//	public SimpleApplication getSApp() {
//		return sapp;
//	}
  
}
