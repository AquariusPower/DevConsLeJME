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

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.state.AppState;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * to avoid direcly exposing things like the Camera object preventing configuring it from anywhere
 * TODO remove the global app
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class AppI {
	public static AppI i(){return GlobalManagerI.i().get(AppI.class);}

	private Application	app;
	private SimpleApplication	sappOpt;
	private Spatial	sptCamFollowMove;
	
	public void configure(Application app){
		this.app=app;
		this.sappOpt=(app instanceof SimpleApplication)?(SimpleApplication)app:null;
		
		initUpdateCamera();
	}
	
	private void initUpdateCamera() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				updateCamera(getTPF());
				return true;
			}
		}).enableLoopMode();
	}

	protected void updateCamera(float tpf) {
		if(sptCamFollowMove!=null){
			app.getCamera().setLocation(sptCamFollowMove.getWorldTranslation());
		}
	}

	public Vector3f getScreenCoordinates(Vector3f worldPos){
		return app.getCamera().getScreenCoordinates(worldPos);
	}
	
	/**
	 * 
	 * @param v3fScreenPos x,y= screen pos, z =projection pos
	 * @return
	 */
	public Vector3f getScreenPosAtWorldCoordinatesForRayCasting(Vector3f v3fScreenPos) {
		return app.getCamera().getWorldCoordinates(new Vector2f(v3fScreenPos.x,v3fScreenPos.y), v3fScreenPos.z);
	}
	
	public Vector3f getCamWPos(float fInFrontDistZ){
		return app.getCamera().getLocation().add(app.getCamera().getDirection().mult(fInFrontDistZ));
	}
	
//	public Vector3f placeAtWCoordCamDirCenter(Spatial spt,float fDistCamZ,boolean bLookAtDir) {
//		return placeAtWCoordCamDirXY(spt, HWEnvironmentJmeI.i().getDisplay().getCenter(fDistCamZ), bLookAtDir);
//	}
	/**
	 * 
	 * @param spt
	 * @param v3fScreenPos z is distance from camera
	 * @param bLookAtDir
	 * @return 
	 * @return
	 */
	public Vector3f placeAtCamWPos(Spatial spt,float fInFrontDistZ,boolean bLookAtDir) {
		spt.setLocalTranslation(getCamWPos(fInFrontDistZ));
		if(bLookAtDir){
			spt.lookAt(getCamWPos(fInFrontDistZ*2f),Vector3f.UNIT_Y); //if z dist is negative will work too
		}
		return spt.getWorldTranslation();
	}

	public AppI attatchAppState(AppState as) {
		if(!app.getStateManager().hasState(as))app.getStateManager().attach(as);
		return this;
	}
	
	public boolean isSApp(){
		return sappOpt!=null;
	}
	
	public Node getRootNode() {
		return sappOpt.getRootNode();
	}

	public Vector3f getCamLookingAtDir() {
		return app.getCamera().getDirection().clone();
	}
	
	public Vector3f getCamLeftDir() {
		return app.getCamera().getLeft().clone();
	}

	public AppI setCamFollow(Spatial spt) {
		this.sptCamFollowMove=spt;
		return this;
	}
	
}
