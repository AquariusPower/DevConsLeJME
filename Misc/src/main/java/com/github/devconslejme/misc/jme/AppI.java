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
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.TimeFormatI;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.state.AppState;
import com.jme3.font.BitmapFont;
import com.jme3.input.controls.InputListener;
import com.jme3.input.controls.Trigger;
import com.jme3.material.Material;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.post.FilterPostProcessor;
import com.jme3.post.filters.BloomFilter;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.texture.Texture;

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
	private FilterPostProcessor	fpp;
	private BloomFilter	bloomFilter;
	private boolean bLimitScreenCoordinates;
	private float fMaxFrustum=1000;
	
	public void configure(Application app){
		this.app=app;
		this.sappOpt=(app instanceof SimpleApplication)?(SimpleApplication)app:null;
		
		initUpdateCamera();
		initFilters();
	}
	
	public void setBloomFilterEnabled(boolean enabled) {
		bloomFilter.setEnabled(enabled);
	}
	
	/**
	 * frustum far will be near * maxFrustum to avoid glitches TODO test case, transparencies? shadows? with frustum diff > 10000?
	 * @param fFrustumNearBase
	 */
	public void setCameraFrustum(float fFrustumNearBase) {
		app.getCamera().setFrustumNear(fFrustumNearBase);
		app.getCamera().setFrustumFar(fFrustumNearBase*getMaxFrustum());
	}
	
	private void initFilters() {
    fpp = new FilterPostProcessor(app.getAssetManager());
    bloomFilter = new BloomFilter(BloomFilter.GlowMode.Objects);
    fpp.addFilter(bloomFilter);
    app.getViewPort().addProcessor(fpp);
	}

	private void initUpdateCamera() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				updateInfo(getTPF());
				updateCamera(getTPF());
				return true;
			}
		}).enableLoopMode();
	}

	protected void updateInfo(float tpf) {
		HWEnvironmentJmeI.i().putCustomInfo("Times", 
			"Real:"+TimeFormatI.i().getRealTimeFormatted()
			+",AppElps:"+TimeFormatI.i().formatElapsed(app.getTimer().getResolution(), app.getTimer().getTime())
			+",SmltElps:"+TimeFormatI.i().formatElapsed(1000, SimulationTimeI.i().getMillis())
		);
	}

	protected void updateCamera(float tpf) {
		if(sptCamFollowMove!=null){
			app.getCamera().setLocation(sptCamFollowMove.getWorldTranslation());
		}
	}

	public Vector3f getScreenCoordinates(Vector3f worldPos){
		Vector3f v3f = getScreenCoordinatesRaw(worldPos);
		if(isLimitScreenCoordinates()) {
			v3f.x=FastMath.clamp(v3f.x, 0, HWEnvironmentJmeI.i().getDisplay().getWidth());
			v3f.y=FastMath.clamp(v3f.y, 0, HWEnvironmentJmeI.i().getDisplay().getHeight());
			if(v3f.z>1f) {
				v3f.x=FastMath.clamp(v3f.x, 0, HWEnvironmentJmeI.i().getDisplay().getWidth());
				v3f.y=FastMath.clamp(v3f.y, 0, HWEnvironmentJmeI.i().getDisplay().getHeight());
				v3f.x=HWEnvironmentJmeI.i().getDisplay().getWidth()-v3f.x;
				v3f.y=(app.getCamera().getLocation().y > worldPos.y ? 0 : HWEnvironmentJmeI.i().getDisplay().getHeight());
			}
		}
		return v3f;
	}
	public Vector3f getScreenCoordinatesRaw(Vector3f worldPos){
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
	
	public Vector3f getCamWPosCopy(Vector3f v3fDisplacementInFront){
		return app.getCamera().getLocation()
			.add(app.getCamera().getDirection().mult(v3fDisplacementInFront.z))
			.add(app.getCamera().getLeft().mult(v3fDisplacementInFront.x))
			.add(app.getCamera().getUp().mult(v3fDisplacementInFront.y))
		;
	}
	public Vector3f getCamWPosCopy(float fInFrontDistZ){
		return app.getCamera().getLocation().add(app.getCamera().getDirection().mult(fInFrontDistZ));
	}
	
	public Vector3f placeAtCamWPos(PhysicsData pd,float fInFrontDistZ,boolean bLookAtDir) {
		Vector3f v3f = placeAtCamWPos(pd.getSpatialWithPhysics(), fInFrontDistZ,bLookAtDir);
		PhysicsI.i().syncPhysTransfFromSpt(pd, true, true);
		return v3f;
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
		spt.setLocalTranslation(getCamWPosCopy(fInFrontDistZ));
		if(bLookAtDir){
			spt.lookAt(getCamWPosCopy(fInFrontDistZ*2f),Vector3f.UNIT_Y); //if z dist is negative will work too
		}
		return spt.getWorldTranslation();
	}
	public Vector3f placeAtCamWPos(Spatial spt,Vector3f v3fDisplacementInFront,boolean bLookAtDir) {
		spt.setLocalTranslation(getCamWPosCopy(v3fDisplacementInFront));
		if(bLookAtDir){
			spt.lookAt(getCamWPosCopy(v3fDisplacementInFront.z*2f),Vector3f.UNIT_Y); //if z dist is negative will work too
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
		if(sappOpt==null)return null;
		return sappOpt.getRootNode();
	}

	public Vector3f getCamLookingAtDirCopy() {
		return app.getCamera().getDirection().clone();
	}
	
	public Vector3f getCamLeftDirCopy() {
		return app.getCamera().getLeft().clone();
	}

	public AppI setCamFollow(Spatial spt) {
		this.sptCamFollowMove=spt;
		return this;
	}
	
	public Spatial getCamFollow() {
		return sptCamFollowMove;
	}

	public Node getGuiNode() {
		if(sappOpt==null)return null;
		return sappOpt.getGuiNode();
	}

	public boolean isLimitScreenCoordinates() {
		return bLimitScreenCoordinates;
	}

	public AppI setLimitScreenCoordinates(boolean bLimitScreenCoordinates) {
		this.bLimitScreenCoordinates = bLimitScreenCoordinates;
		return this; 
	}

	public float getMaxFrustum() {
		return fMaxFrustum;
	}

	public AppI setMaxFrustum(float fMaxFrustum) {
		this.fMaxFrustum = fMaxFrustum;
		return this; 
	}

//	public AssetManager getAssetManager() {
//		return app.getAssetManager();
//	}

	public Material newMaterial(String string) {
		return new Material(app.getAssetManager(),string);
	}

	public boolean removeMapping(String str) {
		if(app.getInputManager().hasMapping(str)){
			app.getInputManager().deleteMapping(str);
			return true;
		}
		return false;
	}

	public void addKeyMappingAndListener(String strMapping, Trigger tg, InputListener acl) {
		if(!app.getInputManager().hasMapping(strMapping)){
			app.getInputManager().addMapping(strMapping, tg);
		}
		
		/**
		 * if the "keycode id" mapping already existed, it will just add a listener to it!
		 */
		app.getInputManager().addListener(acl, strMapping);
	}

	public long getTimerResolution() {
		return app.getTimer().getResolution();
	}

	public long getTime() {
		return app.getTimer().getTime();
	}

	public Quaternion getCamRotCopy() {
		return app.getCamera().getRotation().clone();
	}

	public BitmapFont loadFont(String strPath) {
		return app.getAssetManager().loadFont(strPath);
	}

	public long getTimeNano() {
		assert app.getTimer().getResolution() == 1000000000;
		return getTime();
	}

	public <T extends AppState> T getState(Class<T> cl) {
		return app.getStateManager().getState(cl);
	}

	public float getTimeInSeconds() {
		return app.getTimer().getTimeInSeconds();
	}

	@Workaround
	public void setCursorVisible(boolean bEnable) {
		/**
		 * Inversed request first!
		 * this trick is required to grant the cursor visibility state will
		 * actually be modified/applied/changed, otherwise it would be ignored as 
		 * the internal boolean would not have changed!
		 */
		app.getInputManager().setCursorVisible(!bEnable);
		
		// apply requested state
		app.getInputManager().setCursorVisible(bEnable);
	}

	public boolean isInputManagerReady() {
		return app.getInputManager()!=null;
	}

	public Texture loadTexture(String string) {
		return app.getAssetManager().loadTexture(string);
	}
	
}
