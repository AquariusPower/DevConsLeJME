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
import com.github.devconslejme.extras.DynamicFPSLimiter;
import com.github.devconslejme.extras.OSCmd;
import com.github.devconslejme.extras.SingleAppInstance;
import com.github.devconslejme.extras.SingleAppInstance.CallChkProblemsAbs;
import com.github.devconslejme.gendiag.GlobalsManagerDialogI;
import com.github.devconslejme.gendiag.QueueManagerDialogI;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.CheckProblemsI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.LwjglI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.tests.temp.TestVisualizeOtherWindowContents;
import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.StatsAppState;
import com.jme3.app.state.AbstractAppState;
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
public class TestDevCons extends SimpleApplication{
	public static void main(String[] args) {
		if(bEnableOpt)opt_initSingleAppInstanceAtMain();
		TestDevCons tst = new TestDevCons();
		if(bEnableOpt)opt_initWindow(tst);
		tst.start();
	}
	
	@Override
	public void simpleInitApp() {
		com.github.devconslejme.devcons.PkgCfgI.i().configure(this,getGuiNode());
		
		/**
		 * to remove {@link JavaScriptI} auto global access to some class/object, ex.: 
		JavaScriptI.i().addForbidClassAccessJS(TestDevCons.class);
		 */
		
		if(bEnableOpt){
			opt_disableSomeSimpleAppThings();
			opt_initOptionalExtras();
			opt_initOptionalOtherStuff();
			opt_initOptionalIntegrateAllOtherTests();
		}
	}
	
	
	
	
	
	
	
	/******************************************************************************************
	 * OPTIONALS BELOW
	 * you can just detele them all.
	 ******************************************************************************************/
	
	/** */
	ArrayList<SimpleApplication> aoUpdOpts = new ArrayList<SimpleApplication>();
	private static boolean	bEnableOpt = true;
	
	private void opt_disableSomeSimpleAppThings() {
		// disable some mappings to let the console manage it too.
		getInputManager().deleteMapping(SimpleApplication.INPUT_MAPPING_CAMERA_POS); //TODO there is no super code for it?
		getInputManager().deleteMapping(SimpleApplication.INPUT_MAPPING_MEMORY); //TODO there is no super code for it?
		getInputManager().deleteMapping(SimpleApplication.INPUT_MAPPING_HIDE_STATS);
		getInputManager().deleteMapping(SimpleApplication.INPUT_MAPPING_EXIT); //this is important to let ESC be used for more things
		stateManager.getState(StatsAppState.class).setDisplayStatView(false);
	}
	
	private static void opt_initWindow(TestDevCons tst) {
		LwjglI.i().getDisplay().setResizable(true);
		
		AppSettings as = new AppSettings(true);
		as.setTitle(TestDevCons.class.getSimpleName());
		as.setResolution(1230,690);
		as.setResizable(true);
		as.setFrameRate(60);
		tst.setSettings(as);
		
		tst.setShowSettings(false);
	}
	
	private static void opt_initSingleAppInstanceAtMain() {
		GlobalManagerI.i().get(SingleAppInstance.class).configureOptionalAtMainMethod(
				JmeSystem.getStorageFolder(StorageFolderType.Internal)); // this is optional
	}
	
	private void opt_initOptionalExtras() {
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
	}

	private void opt_initOptionalOtherStuff() {
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

	/**
	 * so thru devcons user commands can instantiate the other tests
	 */
	private void opt_initOptionalIntegrateAllOtherTests() {
		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestContextMenu()));
		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestChoiceDialog()));
		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestMultiChildDialog()));
		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestHierarchyResizablePanel()));
		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestMaintenanceDialog()));
		aoUpdOpts.add(GlobalManagerI.i().putConcrete(new TestResizablePanel()));
	}
	
	@Override
	public void simpleUpdate(float tpf) {
		super.simpleUpdate(tpf);
		for(SimpleApplication obj:aoUpdOpts){
			obj.simpleUpdate(tpf);
		}
		
		if(LwjglI.i().getDisplay().wasResized()){
			reshape(
				Math.max(LwjglI.i().getDisplay().getWidth(),1),
				Math.max(LwjglI.i().getDisplay().getHeight(),1)
			);
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
}
