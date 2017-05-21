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

import java.util.ArrayList;

import com.github.devconslejme.debug.DebugTrackProblemsJME;
import com.github.devconslejme.debug.UnsafeDebugHacksI;
import com.github.devconslejme.devcons.DevConsPluginStateI;
import com.github.devconslejme.devcons.LoggingI;
import com.github.devconslejme.extras.DynamicFPSLimiter;
import com.github.devconslejme.extras.OSCmd;
import com.github.devconslejme.extras.SingleAppInstance;
import com.github.devconslejme.extras.SingleAppInstance.CallChkProblemsAbs;
import com.github.devconslejme.gendiag.GlobalsManagerDialogI;
import com.github.devconslejme.gendiag.KeyBindManagerDialogI;
import com.github.devconslejme.gendiag.QueueManagerDialogI;
import com.github.devconslejme.gendiag.SpatialsManagerI;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.CheckProblemsI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.TimedDelay;
import com.github.devconslejme.misc.jme.EnvironmentJmeI;
import com.github.devconslejme.misc.jme.EnvironmentJmeI.IEnvironmentListener;
import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.IndicatorI.GeomIndicator;
import com.github.devconslejme.misc.jme.FlyByCameraX;
import com.github.devconslejme.misc.jme.MeshI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.OriginDevice;
import com.github.devconslejme.misc.jme.RotateI;
import com.github.devconslejme.misc.jme.WorldPickingI.IPickListener;
import com.github.devconslejme.misc.lemur.SystemAlertLemurI;
import com.github.devconslejme.projman.SimpleAppStateAbs;
import com.github.devconslejme.tests.TestOriginDeviceGame.OriginDeviceMonster;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppState;
import com.jme3.audio.AudioListenerState;
import com.jme3.collision.CollisionResult;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Spatial;
import com.jme3.system.AppSettings;
import com.jme3.system.JmeSystem;
import com.jme3.system.JmeSystem.StorageFolderType;
import com.jme3.system.lwjgl.LwjglAbstractDisplay;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;

/**
 * this {@link GlobalManagerI} global will be auto set as {@link Application} thru the package configuration.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestDevCons extends SimpleApplication implements IEnvironmentListener, IPickListener{
	public class GeometryVolDbg extends Geometry{
	}

	public static void main(String[] args) {
		if(bEnableOpt)opt_initSingleAppInstanceAtMain();
		
		TestDevCons tst = new TestDevCons();
		
		/**
		 * this is mainly to disable default key bindings and features,
		 * to also easify replacing them 
		 */
		if(bEnableOpt)tst = new TestDevCons(new AudioListenerState()); //the above will just be ignored
		
		if(bEnableOpt)opt_initWindow(tst);
		
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
		if(bEnableOpt)opt_initBasics();
		
		com.github.devconslejme.devcons.PkgCfgI.i().configure(this,getGuiNode(), getRootNode());
		
		/**
		 * to remove {@link JavaScriptI} auto global access to some class/object, ex.: 
		JavaScriptI.i().addForbidClassAccessJS(TestDevCons.class);
		 */
		
		if(bEnableOpt)opt_initAll();
	}
	
	
	
	
	
	
	
	/******************************************************************************************
	 * OPTIONALS BELOW
	 * you can just detele it all from this line below til the end.
	 * or bEnableOpt=false just for a simpler test.
	 ******************************************************************************************/
	private static boolean	bEnableOpt = true;
	
//	ArrayList<SimpleAppStateAbs> aoUpdOpts = new ArrayList<SimpleAppStateAbs>();
	private FlyByCameraX	flycam;

	private OriginDevice	orde;
	public OriginDevice getOriginDevice() {
		return orde;
	}

	private float	fSpeedBkp;

//	private Geometry	torX;
//	private Geometry	torY;
//	private Geometry	torZ;
//	private EffectElectricity	ef;
//	private TimedDelay	tdEffectRetarget;
//	private float	fRetargetDefaultDelay=3;
	
	@Override
	public FlyByCameraX getFlyByCamera() {
		return flycam;
	}
	
	public TestDevCons() {super();}
	public TestDevCons(AppState... initialStates) {super(initialStates);}

	private void opt_initBasics() {
    flycam = new FlyByCameraX(getCamera());//.setAllowMove(true);
    flycam.registerWithInput(getInputManager());
		
		opt_disableSomeSimpleAppThings();
	}
	
	private void opt_initAll() {
		orde = new OriginDevice().setEnabled(true);
		GlobalManagerI.i().put(OriginDevice.class,orde);
		getRootNode().attachChild(orde);
		// good position related to these objects
		getCamera().setLocation(new Vector3f(9.787677f, 6.957723f, 11.003839f)); //taken from devcons
		getCamera().setRotation(new Quaternion(-0.068618454f, 0.91919893f, -0.18511744f, -0.34072912f)); //taken from devcons
		
		opt_initExtras();
		
		opt_initShowFPS();
		
		opt_initOtherStuff();
		
		opt_initIntegrateAllOtherTests();
		
		opt_initAlertConsoleKey();
	}
	
	private void opt_initAlertConsoleKey() {
		QueueI.i().enqueue(new CallableXAnon() {
			TimedDelay td = new TimedDelay(3f, "wait user read it");
			private StackTraceElement[]	aste;
			@Override
			public Boolean call() {
				if(!SystemAlertLemurI.i().isShowingAlert()){
					aste=SystemAlertLemurI.i().showSystemAlert("hit F10 to open console", null);
					SystemAlertLemurI.i().setAlertStayOnCenter(true);
					td.setActive(true);
				}else{
					if(td.isReady()){
						SystemAlertLemurI.i().hideSystemAlert(aste);
						return true;//end
					}
				}
				
				return false;
			}
		});
	}

	private void opt_initShowFPS() {
		EnvironmentJmeI.i().setShowFPS(true).setShowCamPos(true).setShowCamRot(true);
	}

	public TestDevCons setSpeed(float f){
		if(f<0){
			MessagesI.i().warnMsg(this, "positive only", f, speed);
		}else{
			super.speed=f;
		}
		return this;
	}
	
	public void togglePause(){
		if(speed==0){
			assert(fSpeedBkp>0);
			setSpeed(fSpeedBkp);
		}else{
			fSpeedBkp=speed;
			setSpeed(0);
		}
	}
	
	@Override
	public boolean updatePickingEvent(ArrayList<CollisionResult> acrList, Geometry geom, Spatial sptParentest) {
		return false;
	}
	
	private void opt_disableSomeSimpleAppThings() {
		MiscJmeI.i().enqueueUnregisterKeyMappings(
//			DebugKeysAppState.INPUT_MAPPING_MEMORY,
//			DebugKeysAppState.INPUT_MAPPING_CAMERA_POS,
//			SimpleApplication.INPUT_MAPPING_HIDE_STATS,
			SimpleApplication.INPUT_MAPPING_EXIT //this is important to let ESC be used for more things
		);
//		stateManager.getState(StatsAppState.class).setDisplayStatView(false);
	}
	
	@SuppressWarnings("unused")
	private static void opt_initWindow(TestDevCons tdc) {
		EnvironmentJmeI.i().getDisplay().setResizable(true);
		EnvironmentJmeI.i().addListener(tdc);
		
		AppSettings as = new AppSettings(true);
		as.setTitle(TestDevCons.class.getSimpleName());
		as.setResolution(1230,690);
		as.setResizable(true);
		if(false)as.setFrameRate(60); //using dynamic fps limiter
		tdc.setSettings(as);
		
		tdc.setShowSettings(false);
		
	}
	
	private static void opt_initSingleAppInstanceAtMain() {
		GlobalManagerI.i().get(SingleAppInstance.class).configureOptionalAtMainMethod(
			JmeSystem.getStorageFolder(StorageFolderType.Internal)); // this is optional
	}
	
	private void opt_initExtras() {
		//// SingleAppInstance
		GlobalManagerI.i().get(SingleAppInstance.class).configureRequiredAtApplicationInitialization(null);
		GlobalManagerI.i().get(SingleAppInstance.class).addCheckProblemsCall(
			new CallChkProblemsAbs(){
				@Override
				public Integer call() throws Exception {
					return CheckProblemsI.i().checkProblems(getExitErrorCause());
				}
			}
		);
		
		//// DynamicFPSLimiter
		getStateManager().attach(new AbstractAppState(){
			@Override
			public void update(float tpf) {
				super.update(tpf);
				GlobalManagerI.i().get(DynamicFPSLimiter.class).update(tpf);
			}
		});
		
		//// Queue manager dialog
		QueueManagerDialogI.i().configure();
		DevConsPluginStateI.i().putButtonLater("QueueManager", "open queued tasks manager", 
				new Command<Button>() {@Override public void execute(Button source) {
					QueueManagerDialogI.i().show();
				}}, null
			);
		
		//// Globals manager dialog
		GlobalsManagerDialogI.i().configure();
		DevConsPluginStateI.i().putButtonLater("GlobalsManager", "open global instances manager", 
			new Command<Button>() {@Override public void execute(Button source) {
				GlobalsManagerDialogI.i().show();
			}}, null
		);
		
		//// KeyBinding manager dialog
		DevConsPluginStateI.i().putButtonLater("KeyBindingManager", "open key bindings manager", 
			new Command<Button>() {@Override public void execute(Button source) {
				KeyBindManagerDialogI.i().show();
			}}, null
		);
		
		//// Spatials manager dialog
		SpatialsManagerI.i().configure();
		DevConsPluginStateI.i().putButtonLater("SpatialsManager", "open Spatials hierarchy manager", 
			new Command<Button>() {@Override public void execute(Button source) {
				SpatialsManagerI.i().show();
			}}, null
		);
	}

	private void opt_initOtherStuff() {
		//// Debug Track Problems
		DebugTrackProblemsJME.i().configure(getGuiNode(), getRootNode());
		CheckProblemsI.i().addProblemsChecker(DebugTrackProblemsJME.i());
		
		//// linux only
		if(System.getProperty("os.name").equalsIgnoreCase("linux")){
			//// Workarounds
			raiseAppWindowAtLinux();
		
			//// Unsafe Hacks
			UnsafeDebugHacksI.i().setAllowHacks(true);
			UnsafeDebugHacksI.i().hackXRandRpreventResolutionRestore();
		}
	}
	
	private void addTest(Class<? extends SimpleAppStateAbs> cl){
		LoggingI.i().logSubEntry(
			GlobalManagerI.i().get(cl) //will be auto instanced
				.getClass().getSimpleName()+".initTest()");
	}
	
	/**
	 * so thru devcons user commands can instantiate the other tests
	 */
	private void opt_initIntegrateAllOtherTests() {
		LoggingI.i().logMarker("Can init these tests:");
		addTest(TestContextMenu.class);
		addTest(TestChoiceDialog.class);
		addTest(TestMultiChildDialog.class);
		addTest(TestHierarchyResizablePanel.class);
		addTest(TestMaintenanceDialog.class);
		addTest(TestResizablePanel.class);
		addTest(TestOriginDeviceGame.class);
//		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestContextMenu()));
//		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestChoiceDialog()));
//		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestMultiChildDialog()));
//		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestHierarchyResizablePanel()));
//		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestMaintenanceDialog()));
//		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestResizablePanel()));
//		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestOriginDeviceGame()));
	}
	
	@Override
	public void simpleUpdate(float fTPF) {
		super.simpleUpdate(fTPF);
		
		if(bEnableOpt){
//			for(SimpleApplication obj:aoUpdOpts){
//				obj.simpleUpdate(fTPF);
//			}
			
			orde.update(fTPF);
			
			updateDebugTest(fTPF);
		}
		
	}
	
	/**
	 * Linux only: raise application window as easy workaround to make strict focus policy painless
	 */
	@Workaround
	private void raiseAppWindowAtLinux() {
		GlobalManagerI.i().get(OSCmd.class).runLinuxCmd(
			"xdotool windowactivate $(xdotool search --name \"^"+settings.getTitle()+"$\")");
	}

	/**
	 * this is called for uncaugth exceptions! from {@link LwjglAbstractDisplay}
	 */
	@Override
	public void handleError(String errMsg, Throwable t) {
		GlobalManagerI.i().get(SingleAppInstance.class).setExitRequestCause(t);
		super.handleError(errMsg,t); //seems ok after the above
	}

	@Override
	public void displayResizedEvent(int iW, int iH) {
		reshape( Math.max(iW,1), Math.max(iH,1) );
	}
	
	/** @DevSelfNote keep even if emtpy */ Object[] aobjDebugTest;
	/** @DevSelfNote keep even if emtpy */ 
	public Object debugTest(Object... aobj){
		Geometry geom = GeometryI.i().create(MeshI.i().cone(1f), ColorRGBA.Blue);
		aobjDebugTest=new Object[]{geom};
		getRootNode().attachChild(geom);
		return null;
	}
	/** @DevSelfNote keep even if emtpy */ 
	public void updateDebugTest(float fTPF){
		if(aobjDebugTest==null)return;
		Geometry geom = ((Geometry)aobjDebugTest[0]);
		if(false)
			geom.setLocalTranslation(3,1,0);
		RotateI.i().rotateAroundPivot(geom, orde, 0.01f, true);
	}
}
