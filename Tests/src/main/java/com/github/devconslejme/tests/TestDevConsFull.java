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
import java.util.function.Function;

import com.github.devconslejme.debug.DebugTrackProblemsJME;
import com.github.devconslejme.debug.UnsafeDebugHacksI;
import com.github.devconslejme.devcons.DevConsPluginStateI;
import com.github.devconslejme.devcons.JavaScriptI;
import com.github.devconslejme.devcons.LoggingI;
import com.github.devconslejme.extras.DynamicFPSLimiter;
import com.github.devconslejme.extras.OSCmd;
import com.github.devconslejme.extras.SingleAppInstance;
import com.github.devconslejme.extras.SingleAppInstance.CallChkProblemsAbs;
import com.github.devconslejme.gendiag.GlobalsManagerDialogI;
import com.github.devconslejme.gendiag.KeyBindManagerDialogI;
import com.github.devconslejme.gendiag.QueueManagerDialogI;
import com.github.devconslejme.gendiag.SpatialsManagerDialogI;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.CheckProblemsI;
import com.github.devconslejme.misc.CommandLineParser;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.DetailedException.IHandleExitListener;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.SystemAlertI;
import com.github.devconslejme.misc.jme.FlyByCameraX;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI.IEnvironmentListener;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.NodeX;
import com.github.devconslejme.misc.jme.OriginDevice;
import com.github.devconslejme.misc.jme.PhysicsData;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.github.devconslejme.misc.jme.SimpleAppState;
import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.github.devconslejme.misc.jme.WorldPickingI.IPickListener;
import com.github.devconslejme.projman.SimpleApplicationAndStateAbs;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.state.AppState;
import com.jme3.audio.AudioListenerState;
import com.jme3.font.BitmapText;
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
 * this will integrate everything possible, even access to all other tests from here will be available.
 * for a raw, simplest basic console plugin, see {@link TestDevConsSimple} instead.
 * 
 * this {@link GlobalManagerI} global will be auto set as {@link Application} thru the package configuration.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestDevConsFull extends SimpleApplication implements IEnvironmentListener, IPickListener, IHandleExitListener{

	public static void main(String[] args) {
		if(bEnableOpt)opt_initSingleAppInstanceAtMain();
		
		TestDevConsFull tst = new TestDevConsFull();
		if(bEnableOpt)DetailedException.setHandleExitListener(tst);
		
		/**
		 * this is mainly to disable default key bindings and other features like flycam,
		 * to also easify replacing them because of the way the legacy simple app works.
		 */
		if(bEnableOpt)tst = new TestDevConsFull(new AudioListenerState()); //the above will just be ignored
		
		if(bEnableOpt)opt_initWindow(tst);
		
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
		/** this should be the 1st thing configured */
		com.github.devconslejme.devcons.PkgCfgI.i().configure(this, getGuiNode(), getRootNode());
		if(bEnableOpt)com.github.devconslejme.game.PkgCfgI.i().configure(this, getGuiNode(), getRootNode(), flycamx);
		
		if(bEnableOpt)opt_initAppBasics();
		
		/**
		 * if you want to remove {@link JavaScriptI} auto global access to some class/object ex.: 
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
	private FlyByCameraX	flycamx;
//	private OriginDevice	orde;
	private float	fSpeedBkp;
	
//	public OriginDevice getOriginDevice() {
//		return orde;
//	}
	
	/**
	 * To flycamX override work, at {@link #TestDevCons(AppState...)} the fly cam state must be NOT set.
	 * Such state at {@link SimpleApplication#initialize()} would instance the default fly cam, and we
	 * need to prevent that.
	 */
	@Override
	public FlyByCameraX getFlyByCamera() {
    if(flycamx==null){
    	flycamx = new FlyByCameraX(getCamera());//.setAllowMove(true);
    	flycamx.registerWithInput(getInputManager());
    }
    
		return flycamx;
	}
	
	@Override
	public void initialize(){
		super.rootNode=new NodeX(super.rootNode.getName());
		super.guiNode=new NodeX(super.guiNode.getName());
		
		super.initialize();
	}
	public TestDevConsFull(){super();}
	public TestDevConsFull(AppState... initialStates) {super(initialStates);}

	private void opt_initAppBasics() {
		opt_disableSomeSimpleAppThings();
	}
	
	private void opt_initAll() {
//		orde = new OriginDevice().setEnabled(true);
		GlobalManagerI.i().putGlobal(OriginDevice.class,new OriginDevice());
//		getRootNode().attachChild(orde);
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
		SystemAlertI.i().showTemporarySystemAlert("hit F10 to open console", 3f);
	}

	private void opt_initShowFPS() {
		HWEnvironmentJmeI.i()
			.setShowFPS(true)
			.setShowCamPos(true)
			.setShowCamRot(true)
			.setShowMouseCursorPos(true);
	}

	public TestDevConsFull setSpeed(float f){
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
	public boolean updatePickingEvent(int iButtonIndex, ArrayList<RayCastResultX> acrList, PhysicsData pd, Geometry geom, Spatial sptParentest) {
		return false;
	}
	
	private void opt_disableSomeSimpleAppThings() {
		MiscJmeI.i().enqueueUnregisterKeyMappings(
			SimpleApplication.INPUT_MAPPING_EXIT //this is important to let ESC be used for more things
		);
	}
	
	@SuppressWarnings("unused")
	private static void opt_initWindow(TestDevConsFull tdc) {
		HWEnvironmentJmeI.i().getDisplay().setResizable(true);
		HWEnvironmentJmeI.i().addListener(tdc);
		
		AppSettings as = new AppSettings(true);
		as.setTitle(TestDevConsFull.class.getSimpleName()); //important at least for linux xdotool
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
		getStateManager().attach(new SimpleAppState(){
			@Override
			public void update(float tpf) {
				super.update(tpf);
				GlobalManagerI.i().get(DynamicFPSLimiter.class).update(tpf);
			}
		});
		
		//// Queue manager dialog
		DevConsPluginStateI.i().putButtonLater("MsgReport", "list all reviewable messages", 
			new Command<Button>() {@Override public void execute(Button source) {
//				MessagesI.i().getMessagesUId();JavaScriptI.i().
				JavaScriptI.i().showMsgReport(null);
//				for(String str:MessagesI.i().getMessagesReport(null)){
//					LoggingI.i().logSubEntry(JavaScriptI.i().prepareCmd(EBaseCommand.msgRep,str));
//				}
			}}, null);
		
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
		SpatialsManagerDialogI.i().configure(getGuiNode(),getRootNode());
		DevConsPluginStateI.i().putButtonLater("SpatialsManager", "open Spatials hierarchy manager", 
			new Command<Button>() {@Override public void execute(Button source) {
				SpatialsManagerDialogI.i().show();
			}}, null
		);
		
		//// OS Cmds
		G.i(OSCmd.class).configure(new Function<String, ArrayList<String>>() {
			@Override
			public ArrayList<String> apply(String strLine) {
				return new CommandLineParser(strLine).getAllPartsStrListCopy();
			}
		});
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
	
	private void addTest(Class<? extends SimpleApplicationAndStateAbs> cl){
		LoggingI.i().logSubEntry(
			GlobalManagerI.i().get(cl) //will be auto instanced
				.getClass().getSimpleName()+".initTest()");
	}
	
	/**
	 * so thru devcons user commands can instantiate the other tests
	 */
	private void opt_initIntegrateAllOtherTests() {
		LoggingI.i().logMarker("Can init these tests:");
		//TODO find the classes and auto add them based on the super class too
		addTest(TestContextMenu.class);
		addTest(TestChoiceDialog.class);
		addTest(TestMultiChildDialog.class);
		addTest(TestHierarchyResizablePanel.class);
		addTest(TestMaintenanceDialog.class);
		addTest(TestResizablePanel.class);
		addTest(TestOriginDeviceGame.class);
		addTest(TestZoomDistances.class);
		addTest(TestProjectiles.class);
		
	}
	
	@Override
	public void simpleUpdate(float fTPF) {
		super.simpleUpdate(fTPF);
		
		if(bEnableOpt){
			if(DetailedException.isExitRequested())return; //suspend all processing avoiding more possible/derived problems
//			if(DetailedException.isExitRequested()){
//				GlobalManagerI.i().get(SingleAppInstance.class).setExitRequestCause(DetailedException.getExitRequestCause());
//				return; //suspend all processing
//			}
			
//			orde.update(fTPF);
			
			updateDebugTest(fTPF);
		}
		
	}
	
	/**
	 * Linux only: raise application window as easy workaround to make strict focus policy painless
	 */
	@Workaround
	private void raiseAppWindowAtLinux() {
		GlobalManagerI.i().get(OSCmd.class).runLinuxCmd(
			"xdotool windowactivate $(xdotool search --sync --name \"^"+settings.getTitle()+"$\")");
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
		BitmapText bt = StringTextJmeI.i().createBitmapTextMono(">>>>>>>>>>>>>>>>>>>> "+Character.toString((char)176)+" <<<<<<<<<<<<<<<<<<<<<<",ColorRGBA.White);
		getGuiNode().attachChild(bt);
		bt.setLocalTranslation(300,290,500);
		return null;
	}
	/** @DevSelfNote keep even if emtpy */ 
	public void updateDebugTest(float fTPF){}

	@Override
	public void requestExitEvent(Exception exExitRequestCause) {
		HWEnvironmentJmeI.i().getMouse().forceUngrab();
		GlobalManagerI.i().get(SingleAppInstance.class).setExitRequestCause(exExitRequestCause);
//			DetailedException.getExitRequestCause());
	}
}
