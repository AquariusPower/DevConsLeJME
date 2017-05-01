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

package com.github.devconslejme.devcons;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.lwjgl.opengl.Display;

import com.github.devconslejme.es.DialogHierarchySystemI;
import com.github.devconslejme.es.HierarchyComp.EField;
import com.github.devconslejme.gendiag.ContextMenuI;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenuOwnerListenerI;
import com.github.devconslejme.gendiag.ContextMenuI.HintUpdater;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenuAnon;
import com.github.devconslejme.gendiag.DialogHierarchyStateI;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchy;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.lemur.ClickToPositionCaratListenerI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI.EPopup;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.misc.lemur.ResizablePanel.EEdge;
import com.github.devconslejme.misc.lemur.VersionedVector3f;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.font.BitmapFont;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.system.JmeSystem;
import com.jme3.system.JmeSystem.StorageFolderType;
import com.simsilica.es.EntityId;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.HAlignment;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.core.GuiComponent;
import com.simsilica.lemur.core.VersionedList;
import com.simsilica.lemur.core.VersionedReference;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.DefaultCursorListener;
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;
import com.simsilica.lemur.style.Attributes;
import com.simsilica.lemur.style.Styles;
import com.simsilica.lemur.text.DocumentModel;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DevConsPluginStateI extends AbstractAppState {//implements IResizableListener{
	public static DevConsPluginStateI i(){return GlobalManagerI.i().get(DevConsPluginStateI.class);}
	
//	private Vector3f	v3fApplicationWindowSize;
	private float	fLemurPreferredThickness = 1f;
//	private Vector3f	v3fConsoleSize;
	private float	fConsoleHeightPercDefault = 0.5f;
	private float	fConsoleHeightPerc = fConsoleHeightPercDefault;
	private String strStyle = "console";
	private ListBox<String>	lstbxLoggingSection;
//	private VersionedContainer	cntrMain;
	private Container	cntrMain;
	private Node	nodeParent;
	private BitmapFont	font;
	private ColorRGBA	colorConsoleStyleBackground;
	private Container	cntrStatus;
	private Label	lblTitle;
	private Button	btnClipboardShow;
	private ButtonClickListener	btnclk;
	private TextField	tfInput;
	private int	iKeyCodeToggleConsole = KeyInput.KEY_F10;
	private String	strInputMappingToggleDeveloperConsole = "ToggleDeveloperConsole";
	private File	flStorageFolder;
	private HashMap<String,VarMon> hmVarMon = new HashMap<String,VarMon>();
	private ListBox<String> lstbxVarMonitorBar = new ListBox<String>();
	private CallableXScrollTo cxScrollTo;
	private boolean	bKeepScrollAtBottom;
	private VersionedReference<List<String>>	vrListBoxChangedToAutoScrollToBottom;
	private VersionedReference<Double>	vrSliderChangedToSuspendAutoScrollBottom;
	private ResizablePanel	rzpMain;
	private Application	app;
	private ResizablePanel	rzpVarBar;
	private Button	btnShowVarMon;
	private boolean	bRequestUpdateNoWrap;
	private Button	btnRestoreSize;
	private VersionedReference<Set<Integer>>	vrSelectionChangedToShowVarHelp;
//	private VarMon	vmAppTime;
//	private VarMon	vmCursorPos;
//	private VarMon	vmVisibleRows;
//	private VarMon	vmSlider;
	private String	strBaseTitle = "DevCons";
	private Vector3f	v3fDefaultPos = new Vector3f(0, getWindowSize().y-20, 0);
	private Vector3f	v3fBkpLastNonDefaultPos;
	private Vector3f	v3fDefaultBarSize = new Vector3f(100,100,0);
	private float	fDistToToggleRestorePosSize = 5f;
	private VersionedStatus vlstrVarMonitorEntries = new VersionedStatus();
	//private Vector3f	v3fBkpLastNonDefaultBarSize;
	private Float	fBkpLastNonDefaultBarWidthX;
	protected Vector3f	v3fDefaultSize;
	private Vector3f	v3fBkpLastNonDefaultSize;
	private ContextMenuAnon	cmVarMon;
	private boolean	bAutoUpdateWrapAt=true;
	private VersionedReference<Set<Integer>>	vrSelectionChangedToUpdateInputText;
//	VersionedReference<Vector3f> vrLoggingSectionSize;
	
	private Comparator<VarMon>	cmprStat = new Comparator<VarMon>() {
		@Override
		public int compare(VarMon o1, VarMon o2) {
			int i = o1.esp.compareTo(o2.esp);
			if(i==0) i = o1.strKey.compareTo(o2.strKey);
			return i;
		}
	};
	private static class VersionedStatus extends VersionedList<String>{
		private HashMap<String,Integer> hmKV = new HashMap<String,Integer>();
		public void put(String strKey, String strValue){
			Integer i = hmKV.get(strKey);
			strValue="="+strValue;
			if(i==null){
				add(strKey);
				add(strValue);
				hmKV.put(strKey, size()-2); //the index of the key
			}else{
				set(i+1,strValue);
			}
		}
	}
	
//	private VersionedList<String>	vlstrVarMonitorEntries = new VersionedList<String>(){
//		private HashMap<String,Integer> hmKV = new HashMap<String,Integer>();
//		public void put(String strKey, String strValue){
//			Integer i = hmKV.get(strKey);
//			strValue="="+strValue;
//			if(i==null){
//				add(strKey);
//				add(strValue);
//				hmKV.put(strKey, size()-2); //the index of the key
//			}else{
//				set(i+1,strValue);
//			}
//		}
//	};
	
	class CallableXScrollTo extends CallableX{
		public CallableXScrollTo(){
			super();
			setDelaySeconds(0.25f);
			enableLoop();
			setUserCanPause(true);
		}
		
		@Override
		public Boolean call() {
			if(bKeepScrollAtBottom && vrListBoxChangedToAutoScrollToBottom.update()){
				lstbxLoggingSection.getSlider().getModel().setValue(0);
			}
			
			if(vrSliderChangedToSuspendAutoScrollBottom.update()){
				bKeepScrollAtBottom=false;
			}
			
			// if sliter hits the bottom will stick on it
			if(Double.compare(lstbxLoggingSection.getSlider().getModel().getValue(), 0)==0){
				bKeepScrollAtBottom=true;
			}
			
			return true;
		}

	};
	
	public static enum EStatPriority{ //this order matters!
		Title, //will show at title too!
		Top,
		High,
		Normal,
		Low,
		Bottom,
		Hidden,
		;
		public String s(){return toString();}
	}
	
	public static class VarMon{
		private EStatPriority esp;
		private String strKey;
		private String strValue;
		private String strHelp;
		private boolean bShow=true;
		private CallableX	cx;
		
		public void set(String strValue) {
			this.strValue = strValue;
			
			DevConsPluginStateI.i().updateVarMonValueAtList(this,false);
		}
		public void set(EStatPriority esp) {
			this.esp = esp;
		}
		public void set(EStatPriority esp, String strKey, String strHelp, String strValue,CallableX cx) {
			this.strKey = strKey;
			this.strHelp=strHelp;
			this.cx=cx;
			set(esp);
			set(strValue);
		}
		
		public boolean toggleShow(){
			return bShow=!bShow;
		}
		
		@Override
		public String toString() {
			return strKey+"='"+strValue+"';";
		}
		
//		public void setUpdateCall(CallableX cx){
//			this.cx=cx;
//		}
		
		public void update(){
			cx.call();
		}
		
		public CallableX getUpdateCall(){
			return cx;
		}
		
		public String getQueueName() {
			return VarMon.class.getSimpleName()+":"+strKey;
		}
		public String getValueToList() {
			return "="+strValue;
		}
	}
	
	public void configure(Node nodeParent) {
		this.app=GlobalManagerI.i().get(Application.class);
		
		this.nodeParent = nodeParent;
		
		app.getStateManager().attach(this);
		
		flStorageFolder = new File(
			JmeSystem.getStorageFolder(StorageFolderType.Internal),
			app.getClass().getPackage().getName().replace(".",File.separator) //package of Application class
				+File.separator+app.getClass().getSimpleName() //Application class
				+File.separator+DevConsPluginStateI.class.getSimpleName() //DevCons plugin
		);
		
		JavaScriptI.i().configure(); //before all others
		LoggingI.i().configure();
	}
	
	public VarMon createVarMon(EStatPriority esp, String strKey, String strHelp, CallableX cx){
		VarMon vm=hmVarMon.get(strKey);
		if(vm==null){
			vm=new VarMon();
			hmVarMon.put(strKey, vm);
		}
		vm.set(esp,strKey,strHelp,"",cx);
		
		cx.putKeyClassValue(vm);
		QueueI.i().enqueue(cx.setName(vm.getQueueName()).enableLoop().setDelaySeconds(1f));
		
		enqueueUpdateVarMonList();
//		updateVarMonList();
		
		return vm;
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		if(isInitialized()){return;}
		
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				DevConsPluginStateI.this.setEnabled(false);
				return true;
			}
		}.setName("the console starts closed"));
		
		// jme
		cxScrollTo = new CallableXScrollTo();
		QueueI.i().enqueue(cxScrollTo);
		
		ActionListener al = new ActionListener(){
      @Override
			public void onAction(String name, boolean value, float tpf) {
				if(!value)return;
				
				if (name.equals(strInputMappingToggleDeveloperConsole)) {
					setEnabled(!isEnabled());
				}
			}
		};
		app.getInputManager().addMapping(strInputMappingToggleDeveloperConsole, 
			new KeyTrigger(iKeyCodeToggleConsole));
		app.getInputManager().addListener(al, strInputMappingToggleDeveloperConsole);
		
		// js
		JavaScriptI.i().init();
//		JavaScriptI.i().setJSBinding(this);
		
		// gui
		font = app.getAssetManager().loadFont("Interface/Fonts/DroidSansMono.fnt");
		
		initStyle();
		
//		initSize();
		
		initMainContainer();
		
		initStatusSection();
		initLoggingSection();
		initInputSection();
		
		initVarMonitorWestBar();
		
		toggleDefaultPosSize(true);
		
		ClickToPositionCaratListenerI.i().applyRecursivelyAtAllTextFieldsOf(cntrMain);

//		GuiGlobals.getInstance().requestFocus(tfInput);
		
		MiscLemurI.i().createListBoxVisibleItemsUpdater(lstbxLoggingSection);
		MiscLemurI.i().createListBoxVisibleItemsUpdater(lstbxVarMonitorBar);
		
		QueueI.i().enqueue(new CallableX(){
				private VersionedReference<Vector3f> vrv3fLoggingSize = 
						new VersionedVector3f(lstbxLoggingSection.getSize()).createReference();
				@Override
				public Boolean call() {
					if(!vrv3fLoggingSize.update())return true;
					
					if(bAutoUpdateWrapAt)updateLoggingWrapAt();
					bRequestUpdateNoWrap=true; //apply NoWrap to bitmaptexts also
					
					return true;
				}
			}
			.setName("UpdateLogNoWrap")
			.setDelaySeconds(0.25f)
			.enableLoop()
		);
		bRequestUpdateNoWrap=true;
		
//		SimpleDragParentestListenerI.i().applyAt(panelMain);
		
		super.initialize(stateManager, app);
	}
	
	public void toggleDefaultPosSize(boolean bForceDefault) {
		boolean bRestoreDefault = false;
		
		Vector3f v3fCurrentPos = rzpMain.getLocalTranslation().clone();
		if(bForceDefault || v3fCurrentPos.distance(v3fDefaultPos)>fDistToToggleRestorePosSize){
			bRestoreDefault=true;
		}
		
		Vector3f v3fCurrentSize = rzpMain.getSize().clone();
		if(bForceDefault || v3fCurrentSize.distance(v3fDefaultSize)>fDistToToggleRestorePosSize){
			bRestoreDefault=true;
		}
		
		Float fCurrentBarWidthX = rzpVarBar.getSize().x; //inner pannel reads not from preferred size
		if(bForceDefault || Math.abs(fCurrentBarWidthX - v3fDefaultBarSize.x) > fDistToToggleRestorePosSize){
			bRestoreDefault=true;
		}
		
		if(bRestoreDefault){
			rzpMain.setLocalTranslationXY(v3fDefaultPos);
			setConsoleSizeByHeightPerc(fConsoleHeightPerc);
			rzpVarBar.getPreferredSize().x=v3fDefaultBarSize.x;
			if(rzpMain.isUpdateLogicalStateSuccess()){
				v3fBkpLastNonDefaultPos = v3fCurrentPos.clone();
				fBkpLastNonDefaultBarWidthX = fCurrentBarWidthX;
				v3fBkpLastNonDefaultSize = rzpMain.getSize().clone();
			}
			LoggingI.i().logEntry("DevCons: restoring default pos size");
		}else{
			if(v3fBkpLastNonDefaultPos!=null){
				rzpMain.setLocalTranslationXY(v3fBkpLastNonDefaultPos);
			}
			if(v3fBkpLastNonDefaultSize!=null){
				rzpMain.setPreferredSize(v3fBkpLastNonDefaultSize);
			}
			if(fBkpLastNonDefaultBarWidthX!=null){
				rzpVarBar.getPreferredSize().x=fBkpLastNonDefaultBarWidthX;
			}
			LoggingI.i().logEntry("DevCons: applying last non-default pos size");
		}
		
//		updateLoggingWrapAt();
//		updateVisibleLogItems();
//		setConsoleSizeByHeightPerc(fConsoleHeightPercDefault);
//		updateConsoleHeight(fConsoleHeightPercDefault);
	}

	private void initVarMonitorWestBar() {
		lstbxVarMonitorBar = new ListBox<String>(null, getStyle());
		lstbxVarMonitorBar.setModel(vlstrVarMonitorEntries);
		
		rzpVarBar = new ResizablePanel(null);
		rzpVarBar.setContents(lstbxVarMonitorBar);
		rzpVarBar.setName(rzpVarBar.getName()+"/VarMonitorBar");
//		rzpVarBar.addResizableListener(this);
		
		rzpVarBar.setMinSize(new Vector3f(50,50,0));
		
		rzpVarBar.setAllEdgesEnabled(false);
		rzpVarBar.getEdge(EEdge.Right).setEnabled(true);
		
		GuiComponent gc = rzpVarBar.getResizableBorder();
		if (gc instanceof QuadBackgroundComponent) {
			HoverHighlightEffectI.i().applyAt(rzpVarBar, (QuadBackgroundComponent) gc);
		}
		
		toggleVarMonitorBar(true);
		
		initVarMonValues();
		
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				updateVarMonHelp();
//				updateVarMonList();
				return true;
			}
		}
		.setName("VarMonHelp")
		.setDelaySeconds(1f)
		.enableLoop()
		.setUserCanPause(true));
		
		initVarMonitorContextMenu();
	}
	
	private void initVarMonitorContextMenu() {
		cmVarMon = new ContextMenuAnon(rzpMain);
//		cmVarMon.setHierarchyParent(rzpMain);
		
		Command<Button> cmd = new Command<Button>(){
			@Override
			public void execute(Button btnSourcePriorityChoice) {
				EStatPriority e = null;
				
				String str = btnSourcePriorityChoice.getText();
				if(str.startsWith(EStatPriority.class.getSimpleName())){
					str=str.substring(EStatPriority.class.getSimpleName().length()+1);
					try{e=EStatPriority.valueOf(str);}catch(IllegalArgumentException ex){}
				}
				
				if(e!=null){
					Button btn = (Button)cmVarMon.getContextSource();
					VarMon vm = hmVarMon.get(btn.getText());
					if(vm!=null)vm.set(e);
				}else{
					//other possible commands not related to priority
				}
				
				enqueueUpdateVarMonList();
//				updateVarMonList();
				
				grantContextMenuAtVarMonList();
			}
		};
		
		// prepare the context menu entries
//		for(EStatPriority e:EStatPriority.values()){
		cmVarMon.setSingleChoiceMode(true);
		for(int i=0;i<EStatPriority.values().length;i++){
			EStatPriority esp = EStatPriority.values()[i];
			String str=EStatPriority.class.getSimpleName()+":"+esp.s();
			cmVarMon.addNewEntry(str, cmd, new HintUpdater() {
				@Override
				public Boolean call() {
					Button btnSource = (Button)cmVarMon.getContextSource();
					VarMon vm = hmVarMon.get(btnSource.getText());
					if(vm!=null && esp.equals(vm.esp)){ 
						setPopupHintHelp("Current choice");
						return true; //valid if the text is the key
					}
					
					return false; //invalid if the text does not match a monitoring key
				}
			});
		}
	}
	
//	class CallableX1 extends CallableX<CallableX1>{
//		@Override
//		public Boolean call() {
//			getValue(VarMon.class).set( String.format("%d", lstbxLoggingSection.getVisibleItems()) );
//			return true;
//		}
//	};
	
	private void initVarMonValues() {
		createVarMon(EStatPriority.Bottom, "Slider", "DevCons Logging area Slider Value",new CallableXAnon() {
//			public void a(){}
			@Override
			public Boolean call() {
//				setDelaySeconds(1).a();
				getValue(VarMon.class).set(
					String.format("%.0f/%.0f(%.0f)", 
						lstbxLoggingSection.getSlider().getModel().getValue(),
						lstbxLoggingSection.getSlider().getModel().getMaximum(),
						LoggingI.i().getLogEntriesSize()
					)
				);
				return true;
			}
		});
		
		createVarMon(EStatPriority.Bottom, "VisibleRows", "DevCons Logging area Visible Rows",new CallableXAnon() {
			@Override
			public Boolean call() {
				getValue(VarMon.class).set( String.format("%d", lstbxLoggingSection.getVisibleItems()) );
				return true;
			}
		});
		
		VarMon vmCursorPos = createVarMon(EStatPriority.Normal, "CursorPos", "Mouse Cursor Position on the application",new CallableXAnon() {
			@Override
			public Boolean call() {
				Vector2f v2fCursor = app.getInputManager().getCursorPosition();
				getValue(VarMon.class).set( String.format("%.0f,%.0f", v2fCursor.x, v2fCursor.y) );
				return true;
			}
		});
		vmCursorPos.getUpdateCall().setDelaySeconds(0.25f);
		
		createVarMon(EStatPriority.Normal, "AppTime", "Application Elapsed Time from its start time",new CallableXAnon() {
			@Override
			public Boolean call() {
				getValue(VarMon.class).set( String.format("%.3f", app.getTimer().getTimeInSeconds()) );
				return true;
			}
		});
		
		enqueueUpdateVarMonList();
//		QueueI.i().enqueue(new CallableX() {
//			@Override
//			public Boolean call() {
//				if(!updateVarMonList())return false; //1st valid update
//				return true;
//			}
//		});
	}

	/**
	 * 
	 * @param b null to toggle, or set if true or remove if false
	 */
	private void toggleVarMonitorBar(Boolean b) {
		boolean bVisible = b!=null ? bVisible=b : !cntrMain.hasChild(rzpVarBar);
		if(bVisible){
			cntrMain.addChild(rzpVarBar, BorderLayout.Position.West);
		}else{
//			cntrMain.addChild(new Panel(), BorderLayout.Position.West);
			cntrMain.removeChild(rzpVarBar);
		}
		
		bRequestUpdateNoWrap=true; 
	}

	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		if(bRequestUpdateNoWrap){
			MiscJmeI.i().recursivelyApplyTextNoWrap(rzpMain);
			bRequestUpdateNoWrap=false;
		}
	}
	
	private void updateVarMonHelp(){
//		for(VarMon vm:hmVarMon.values()){
//			vm.update();
//		}
		
//		vmSlider.set(
//			String.format("%.0f/%.0f(%.0f)", 
//				lstbxLoggingSection.getSlider().getModel().getValue(),
//				lstbxLoggingSection.getSlider().getModel().getMaximum(),
//				LoggingI.i().getLogEntriesSize()
//			)
//		);
//		
//		vmVisibleRows.set(
//			String.format("%d", lstbxLoggingSection.getVisibleItems()) );
//		
//		Vector2f v2fCursor = app.getInputManager().getCursorPosition();
//		vmCursorPos.set(
//			String.format("%.0f,%.0f", v2fCursor.x, v2fCursor.y) );
//		
//		vmAppTime.set(
//				String.format("%.3f", app.getTimer().getTimeInSeconds()) );
		
		// show specific status help
		if(vrSelectionChangedToShowVarHelp!=null){
			if(vrSelectionChangedToShowVarHelp.update()){
				Integer iSel = lstbxVarMonitorBar.getSelectionModel().getSelection();
				if(iSel!=null){
					VarMon vm = hmVarMon.get(vlstrVarMonitorEntries.get(iSel));
					if(vm!=null){
						LoggingI.i().logEntry("VarHelp:"+vm.strHelp);
					}
					grantContextMenuAtVarMonList();
				}
			}
		}
	}
	
	/**
	 * if there is a selected item, the var mon context menu will not work,
	 * because the selection Panel highlighter will receive the mouse cursor raycast hit on click (as it is positioned above the listbox item),
	 * instead of the listbox item (that is below it)...
	 */
	@Workaround
	private void grantContextMenuAtVarMonList(){
		lstbxVarMonitorBar.getSelectionModel().setSelection(-1);
	}
	
	private void updateVarMonValueAtList(VarMon vm,boolean bRefreshingJustCreate){
		if(bRefreshingJustCreate){
			vlstrVarMonitorEntries.add(vm.strKey);
			vlstrVarMonitorEntries.add(vm.getValueToList());
		}else{
			int i = vlstrVarMonitorEntries.indexOf(vm.strKey);
			if(i==-1)return;
	//		assert(i>-1);
	//		i++;
	//		vlstrVarMonitorEntries.remove(i);
	//		vlstrVarMonitorEntries.add(i, "="+vm.strValue);
			vlstrVarMonitorEntries.set(++i, vm.getValueToList());
		}
	}
	
	private void enqueueUpdateVarMonList(){
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
//				if(!updateVarMonList())return false; //1st valid update
//				private boolean updateVarMonList() {
//				if(lstbxLoggingSection==null)return false;
				
				ArrayList<VarMon> astList = new ArrayList<VarMon>(hmVarMon.values());
				Collections.sort(astList,cmprStat);
//				String str="";
				vlstrVarMonitorEntries.clear();
				String strAddToTitle="";
				labelLoop:for(VarMon vm:astList){
					switch(vm.esp){
						case Hidden:
							if(bAllowHiddenStats){
								continue labelLoop; //skips current from being added
							}
							break;
						case Title:
							strAddToTitle+=vm.strKey+"="+vm.strValue+";";
							break;
					}
					
					updateVarMonValueAtList(vm,true);
//					str+=st;
//					vlstrVarMonitorEntries.add(st.strKey);
//					vlstrVarMonitorEntries.add("="+st.strValue);
//					vlstrVarMonitorEntries.put();
				}
				
				if(!strAddToTitle.isEmpty())lblTitle.setText(strBaseTitle+":"+strAddToTitle);
				
//				lblStats.setText(str);
//				lstbxVarMonitorBar.setVisibleItems(lstbxLoggingSection.getVisibleItems());
				
				vrSelectionChangedToShowVarHelp = lstbxVarMonitorBar.getSelectionModel().createReference();

//				lstbxVarMonitorBar.getGridPanel().getCell(0, 0);
				
				QueueI.i().enqueue(new CallableX() {
					@Override
					public Boolean call() {
						if(lstbxVarMonitorBar.getGridPanel().getModel().getRowCount()==0)return false;
						
						ContextMenuI.i().applyContextMenuAtListBoxItems(lstbxVarMonitorBar,cmVarMon);
						for(Panel pnl:MiscLemurI.i().getAllListBoxItems(lstbxVarMonitorBar, false)){
							Button btn=(Button)pnl;
							if(btn.getText().startsWith("=")){ //is the value
								CursorEventControl.removeListenersFromSpatial(btn,ContextMenuOwnerListenerI.i());
							}
						}
						
						return true;
					}
				}.setName("ContextMenuAtListBoxAfterPopulated"));
				
				bRequestUpdateNoWrap=true;
//				MiscJmeI.i().recursivelyApplyTextNoWrap(lstbxVarMonitorBar);
				
//				return true;
//			}
				return true;
			}
		}); 
	}
	
	@Override
	public void setEnabled(boolean enabled) {
		if(!isInitialized())throw new DetailedException("not initialized");
		
		if(enabled){
			DialogHierarchyStateI.i().showDialog(rzpMain);
//			nodeParent.attachChild(hrpMain);
		}else{
			rzpMain.close();
		}
		
		super.setEnabled(enabled);
	}
	
	private void initInputSection() {
		tfInput = new TextField("",getStyle());
		cntrMain.addChild(tfInput, BorderLayout.Position.South);
		
		DragParentestPanelListenerI.i().applyAt(tfInput);
		
		BindKeyI.i().prepareKeyMappings();
		
		DialogHierarchyStateI.i().addRequestAutoFocus(tfInput);
		
//		QueueI.i().enqueue(new CallableX() { //TODO this delay still has a chance of typing something at other input field? like when holding for long a key?
//			@Override
//			public Boolean call() {
//				if(!DialogHierarchyStateI.i().getHierarchyComp(rzpMain).isBlocked()){
//					GuiGlobals.getInstance().requestFocus(tfInput);
//				}
//				return true;
//			}
//		}.setName("FocusAtDevConsInput").setDelaySeconds(0.25f).setLoop(true));
		
	}
	
	public void scrollKeepAtBottom() {
		bKeepScrollAtBottom=true;
//		scrollTo(-1);
	}
	
	enum ECallableXKey{
		dIndexIn,
		;
		public String s(){return toString();}
	}
	public void scrollTo(final double dIndex) {
		lstbxLoggingSection.getSlider().getModel().setValue(dIndex);
		
		bKeepScrollAtBottom=false;
	}

	public int getShowRowsAmount() {
		return lstbxLoggingSection.getVisibleItems();
	}

	public double getScrollMaxIndex() {
		return lstbxLoggingSection.getSlider().getModel().getMaximum();
	}
	
	public double getScrollIndex() {
		return lstbxLoggingSection.getSlider().getModel().getValue();
	}

	public void navigateWord(boolean bForward) {
		Integer iCurPos = tfInput.getDocumentModel().getCarat();
		String strText = getInputText(); //strText.length()
		Integer iNewPos = null;
		
		int i=iCurPos;
		
		if(bForward){
			if(i==strText.length())return;
		}else{
			if(i==0)return;
			i--;
		}
		
		boolean bLetter = Character.isLetter(strText.charAt(i));
		while(true){
			i+=bForward?1:-1;
			if(i==0 || i==strText.length())break;
			
			if(bForward){
				if(!bLetter){
					if(!Character.isLetter(strText.charAt(i))){
						continue;
					}
					break; //found letter
				}else{
					if(!Character.isLetter(strText.charAt(i))){
						bLetter=false;
					}
					continue;
				}
			}else{
				if(!bLetter){
					if(Character.isLetter(strText.charAt(i))){
						bLetter=true;
					}
					continue;
				}else{
					if(Character.isLetter(strText.charAt(i))){
						continue;
					}
					i++; //this will skip the blank to the next char.
					break;
				}
			}
		}
		iNewPos=i;
		
		setInputTextCaratAt(iNewPos);
	}

	protected void clearInput() {
		tfInput.setText("");
	}

	protected void closeConsole() {
		LoggingI.i().logExceptionEntry(new UnsupportedOperationException("method not implemented yet"), null);
	}
	
	boolean bAllowHiddenStats=true; //TODO shouldnt it init as false? :)
//	Command<Button> cmdToggleHiddenStats = new Command<Button>(){
//		@Override
//		public void execute(Button source) {
//			bAllowHiddenStats=!bAllowHiddenStats;
//			enqueueUpdateVarMonList();
//		}
//	};
	
	private void initStatusSection() {
		cntrStatus = new Container(getStyle());
		cntrMain.addChild(cntrStatus, BorderLayout.Position.North);
//		MiscLemurI.i().applySimpleDragParentestListener(cntrStatus);
		
		// buttons
		ArrayList<Panel> apnl = new ArrayList<Panel>();
		int iButtonIndex=0;
		
		btnRestoreSize = new Button("DefaultPosSize",getStyle());
		PopupHintHelpListenerI.i().setPopupHintHelp(btnRestoreSize, "Restore DevCons defaults Size and Position");
		apnl.add(btnRestoreSize);
		
		btnShowVarMon = new Button("VarMonBar:Toggle",getStyle());
		PopupHintHelpListenerI.i().setPopupHintHelp(btnShowVarMon, "Show Variables Monitor Bar");
		ContextMenu cm = new ContextMenu(rzpMain);
		cm.addNewEntry(
			"ToggleHiddenStats", 
			new Command<Button>(){
				@Override
				public void execute(Button source) {
					bAllowHiddenStats=!bAllowHiddenStats;
					enqueueUpdateVarMonList();
				}
			},
			new HintUpdater() {
				@Override
				public Boolean call() {
					setPopupHintHelp("("+(bAllowHiddenStats?"show":"hide")+")"); //say the next action on clicking
					return true;
				}
			}
		);
		ContextMenuI.i().applyContextMenuAt(btnShowVarMon,cm);
		apnl.add(btnShowVarMon);
		
		btnClipboardShow = new Button("Clipboard:Show",getStyle());
		PopupHintHelpListenerI.i().setPopupHintHelp(btnClipboardShow, "Show Clipboard Contents");
		apnl.add(btnClipboardShow);
		
		lblTitle = new Label(strBaseTitle ,getStyle());
//		SimpleDragParentestListenerI.i().applyAt(lblStats);
		lblTitle.setColor(new ColorRGBA(1,1,0.5f,1));
		lblTitle.setTextHAlignment(HAlignment.Right);
		apnl.add(lblTitle);
//		cntrStatus.addChild(lblStats,0,0);
		
		btnclk = new ButtonClickListener();
		for(Panel pnl:apnl){
			if (pnl instanceof Button) {
				Button btn = (Button) pnl;
				btn.setTextHAlignment(HAlignment.Center);
				CursorEventControl.addListenersToSpatial(btn, btnclk);
//				btn.addClickCommands(btnclk);
			}
			DragParentestPanelListenerI.i().applyAt(pnl);
//			else
//			if (pnl instanceof Label) {
//				Label lbl = (Label)pnl;
//				lbl.setTextHAlignment(HAlignment.Left);
//			}
//			MiscJmeI.i().recursivelyApplyTextNoWrap(pnl);
//			pnl.setPreferredSize(new Vector3f(10,10,0));
			cntrStatus.addChild(pnl,0,iButtonIndex++);
		}
		
//		initStatusValues();
		
	}
	
	private class ButtonClickListener extends DefaultCursorListener{
		@Override
		protected void click(CursorButtonEvent event, Spatial target,				Spatial capture) {
			super.click(event, target, capture);
			
			if(event.getButtonIndex()!=0)return;
			
			if(capture.equals(btnClipboardShow)){
				ClipboardI.i().showClipboard();
			}else
			if(capture.equals(btnShowVarMon)){
				toggleVarMonitorBar(null);
			}else
			if(capture.equals(btnRestoreSize)){
				toggleDefaultPosSize(false);
			}else{
				MessagesI.i().warnMsg(this, "missing event mapping for "+capture, target);
			}
		}
		
//		@Override
//		public void cursorButtonEvent(CursorButtonEvent event, Spatial target,				Spatial capture) {
////			if(event.isConsumed())return;
//			
//			if(event.getButtonIndex()==0){
//				if(!event.isPressed()){
//					if(capture.equals(btnClipboardShow)){
//						ClipboardI.i().showClipboard();
//					}else
//					if(capture.equals(btnShowVarMon)){
//						toggleVarMonitorBar(null);
//					}else
//					if(capture.equals(btnRestoreSize)){
//						applyDefaultPosSize();
//					}else{
//						MessagesI.i().warnMsg(this, "missing event mapping for "+capture, target);
//					}
//				}
//				
//				event.setConsumed();
//			}
//		}

	}
	
//	private class ButtonClick implements Command<Button>{
//		@Override
//		public void execute(Button source) {
//			if(source.equals(btnClipboardShow)){
//				ClipboardI.i().showClipboard();
//			}else
//			if(source.equals(btnShowVarMon)){
//				toggleVarMonitorBar(null);
//			}else
//			if(source.equals(btnRestoreSize)){
//				applyDefaultPosSize();
//			}
//		}
//	}
	
	public static enum EAttribute{
		/** elemet text color */
		color, 
		
		fontSize,
		
		/** color */
		background,
		
		resizableBorders,
		
		/** name/id */
		font,
		;
		public String s(){return toString();}
	}
	
	public static enum DialogStyleElementId{
		SystemAlert,
		;
		
		public String s(){return this.toString();}
		public String str(){return this.toString();}
	}
	
	private void initStyle() {
		colorConsoleStyleBackground = ColorI.i().colorChangeCopy(ColorRGBA.Blue, -0.75f);
		
//		if(GuiGlobals.getInstance()==null)GuiGlobals.initialize(app);
		
		Styles styles = GuiGlobals.getInstance().getStyles();
		Attributes attrs = styles.getSelector(getStyle()); // this also creates the style
		
		ColorRGBA clBg;
		
		attrs.set(EAttribute.fontSize.s(), 14);
		attrs.set(EAttribute.color.s(), ColorI.i().colorChangeCopy(ColorRGBA.White,-0.25f)); //console default text
		clBg = colorConsoleStyleBackground;
		attrs.set(EAttribute.background.s(), new QuadBackgroundComponent(clBg));
		attrs.set(EAttribute.font.s(), font);
		
		attrs = styles.getSelector(Button.ELEMENT_ID, getStyle());
		attrs.set(EAttribute.color.s(), ColorI.i().colorChangeCopy(ColorRGBA.Cyan,-0.10f)); 
		clBg = colorConsoleStyleBackground.mult(1.1f); //new ColorRGBA(0,0.25f,0,0.75f);
		attrs.set(Button.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
		attrs = styles.getSelector(EPopup.DialogStyleElementIdPopupHelp.uId(), getStyle());
		attrs.set(EAttribute.color.s(), ColorRGBA.Blue.clone());
		clBg = ColorRGBA.Cyan.clone();
		attrs.set(Button.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
//		attrs = styles.getSelector("resizablePanel", getStyle());
//		attrs.set(EAttribute.color.s(), ColorRGBA.Yellow.clone()); //TODO REMOVE IF USELESS
//		clBg = ColorRGBA.Cyan.clone();
//		attrs.set(ResizablePanel.LAYER_RESIZABLE_BORDERS, new QuadBackgroundComponent(clBg));
		
		// INPUT
		attrs = styles.getSelector(TextField.ELEMENT_ID, getStyle());
		attrs.set(EAttribute.color.s(), new ColorRGBA(0.75f,0.75f,0.25f,1));
		clBg = new ColorRGBA(0.15f, 0, 0.15f, 1);
		attrs.set(TextField.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
		attrs = styles.getSelector(ListBox.ELEMENT_ID, ListBox.SELECTOR_ID, getStyle());
		clBg = ColorI.i().colorChangeCopy(ColorRGBA.Yellow,0,0.25f);
		attrs.set(ListBox.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
	}
	
	private void initMainContainer() {
		rzpMain = DialogHierarchyStateI.i().createDialog(DevConsPluginStateI.class.getSimpleName(), strStyle);
		EntityId entid = DialogHierarchyStateI.i().getEntityId(rzpMain); //DialogHierarchySystemI.i().createEntity(ContextMenuI.class.getSimpleName());
		DialogHierarchySystemI.i().setHierarchyComp(entid, EField.eHierarchyType, EHierarchy.Top);
		rzpMain.setApplyContentsBoundingBoxSize(false);
//		rzpMain.addResizableListener(this);
		
		cntrMain = new Container(new BorderLayout(), getStyle());
		rzpMain.setContents(cntrMain);
		
		setConsoleSizeByHeightPerc(fConsoleHeightPerc); //just to init default value
	}

	private void initLoggingSection() {
		lstbxLoggingSection = new ListBox<String>(null, getStyle());
		lstbxLoggingSection.setName(lstbxLoggingSection.getName()+"/Logging");
		LoggingI.i().setModelAt(lstbxLoggingSection);
		
		cntrMain.addChild(lstbxLoggingSection, BorderLayout.Position.Center);
		
//		SimpleDragParentestListenerI.i().applyAt(lstbxLoggingSection);
		
		vrSelectionChangedToUpdateInputText = lstbxLoggingSection.getSelectionModel().createReference();
		vrListBoxChangedToAutoScrollToBottom = lstbxLoggingSection.getModel().createReference();
		vrSliderChangedToSuspendAutoScrollBottom=lstbxLoggingSection.getSlider().getModel().createReference();
		
//		VersionedVector3f voLoggingSize = new VersionedVector3f(lstbxLoggingSection.getSize());
//		vrLoggingSectionSize = new VersionedReference<Vector3f>(voLoggingSize);
	}
//	public static class VersionedVector3f implements VersionedObject<Vector3f>{
//		private Vector3f	v3fPrevious;
//		private Vector3f	v3f;
//		private long	lVersion;
//
//		public VersionedVector3f(Vector3f v3f) {
//			this.v3f=v3f;
//			this.v3fPrevious=v3f.clone();
//		}
//
//		@Override
//		public long getVersion() {
//			if(!v3f.equals(v3fPrevious))lVersion++;
//			return lVersion;
//		}
//
//		@Override
//		public Vector3f getObject() {
//			return v3f;
//		}
//
//		@Override
//		public VersionedReference<Vector3f> createReference() {
//			return new VersionedReference<Vector3f>(this);
//		}
//	}

//	private void initSize() {
//		v3fApplicationWindowSize = new Vector3f(
//			Display.getWidth(),
//			Display.getHeight(),
//			fLemurPreferredThickness );
//	}

	public float getConsoleHeightPerc() {
		return fConsoleHeightPerc;
	}

	public void setConsoleSizeByHeightPerc(float fConsoleHeightPerc2) {
		this.fConsoleHeightPerc = fConsoleHeightPerc2;
		
		if(fConsoleHeightPerc>1.0f)fConsoleHeightPerc=1.0f;
		if(fConsoleHeightPerc<0f)fConsoleHeightPerc=0f; //will be further fixed below
		
		int iHeight = (int) (getWindowSize().y * fConsoleHeightPerc);
		
		updateConsoleSize(iHeight);
	}

	private void updateConsoleSize(int iHeightPixels) {
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
				Vector3f v3f = new Vector3f(
						getWindowSize().x,
						iHeightPixels,
						fLemurPreferredThickness);
				
				/**
				 * PUTS A NEW SIZE REQUEST
				 */
				rzpMain.setPreferredSize(v3f);
//				updateVisibleLogItems();
				
				QueueI.i().enqueue(new CallableX() {
					@Override
					public Boolean call() {
						if(!rzpMain.isUpdateLogicalStateSuccess())return false;
						/**
						 * WAITS FOR THE NEW SIZE REQUEST SUCCEED
						 */
						v3fDefaultSize=rzpMain.getSize().clone();
						return true;
					}
				}.setName("DevCons: updates default size"));
				
				return true;
			}
		}.setName("DevCons: request new size"));
		
	}
	
	public Vector3f getWindowSize(){
		return new Vector3f(Display.getWidth(),Display.getHeight(),0);
	}
	
//	public void updateVisibleLogItems(){
//		QueueI.i().enqueue(new CallableX(){
//			@Override
//			public Boolean call() {
////			float fHeight = cntrMain.getPreferredSize().y;
//				float fHeight = cntrMain.getSize().y; //inner container needs some time to be setup by lemur?
//				DevConsPluginStateI.this.lstbxLoggingSection.getPreferredSize();
//				DevConsPluginStateI.this.lstbxLoggingSection.getSize();
//				if(Float.compare(fHeight,0f)==0)return false;
//				
//				int iLines = (int) (fHeight/MiscLemurI.i().getEntryHeightPixels(lstbxLoggingSection));
//				iLines--; //to void the text being too close to each other 
//				lstbxLoggingSection.setVisibleItems(iLines);
////				lstbxLoggingSection.getCellRenderer();
//				
//				bUpdateNoWrap=true;
////				MiscJmeI.i().recursivelyApplyTextNoWrap(lstbxLoggingSection);
//				
//				return true;
//			}
//		}.setDelaySeconds(0.1f));
//	}

	public String getStyle() {
		return strStyle;
	}

	public void setStyle(String strStyle) {
		this.strStyle = strStyle;
		updateStyle();
	}

	private void updateStyle() {
		LoggingI.i().logExceptionEntry(new UnsupportedOperationException("method not implemented yet"), null);
	}

	public String getInputText() {
		return tfInput.getText();
	}

	public void putActionMapAtInputField(KeyAction ka, KeyActionListener kal) {
		tfInput.getActionMap().put(ka, kal);
	}

	public Integer getSelectedIndex(){
		return lstbxLoggingSection.getSelectionModel().getSelection();
	}

	public void setInputText(String str) {
		tfInput.setText(str);
	}

	public File getStorageFolder() {
		return flStorageFolder;
	}

	public void insertAtInputTextCaratPos(String str, Integer iDeleteBeforeIt) {
		if(iDeleteBeforeIt!=null){
			for(int i=0;i<iDeleteBeforeIt;i++){
				tfInput.getDocumentModel().left();
				tfInput.getDocumentModel().delete();
			}
		}
		
		tfInput.getDocumentModel().insert(str);
	}
	
	public void copyLogSelectionToInputText(){
//		if(!vrSelectionChangedToUpdateInputText.update())return;
		
		String str = getInputText();
		str=str.trim();
		if(str.startsWith("//"))str=str.substring(2);
		if(!str.isEmpty())JavaScriptI.i().addCmdToHistory("//"+str); //just to backup anything that is there even if incomplete
		
		String strText = LoggingI.i().getSelectedEntry();
		if(strText==null)return;
		
		strText=strText.trim();
		int iMoveTo = strText.indexOf('(');
		
		// remove useless type text
		String strAfter = strText.substring(iMoveTo+1);
		boolean bFoundType=false;
		for(String strType:JavaLangI.i().getPrimitivesAndStringStrArray(false)){
			if(strAfter.startsWith(strType)){
				strText=strText.substring(0, iMoveTo+1);
				if(strType.equals(String.class.getSimpleName())){
					strText+="\"\"";
					iMoveTo+=2;
				}else{
					iMoveTo++;
				}
				strText+=strAfter.substring(strType.length());
				bFoundType = true;
				break;
			}
		}
		
		if(iMoveTo>-1 && !bFoundType){
			iMoveTo++;
		}
		
		setInputText(strText);
		setInputTextCaratAt(iMoveTo);
	}
	
	public void setInputTextCaratAt(int iMoveTo){
		if(iMoveTo>-1){
			DocumentModel dm = tfInput.getDocumentModel();
			dm.end(true);
			for(int i=dm.getCarat(); i>iMoveTo; i--)dm.left();
		}
	}

	public String getInputLettersBeforeCarat() {
		return getInputTextBeforeCarat("[a-zA-Z]");
	}
	/**
	 * 
	 * @param strRegexToMatchEachValidChar can be null
	 * @return
	 */
	public String getInputTextBeforeCarat(String strRegexToMatchEachValidChar) {
		String str=getInputText();
		str=str.substring(0,tfInput.getDocumentModel().getCarat());
		
		if(strRegexToMatchEachValidChar!=null){
			Integer iNotAlpha=0;
			for(int i=str.length()-1;i>=0;i--){
				if( !(str.charAt(i)+"").matches(strRegexToMatchEachValidChar) ){
					iNotAlpha=i+1;
					break;
				}
			}
			str=str.substring(iNotAlpha);
		}
		
		return str;
	}

	public void showQueue(){
		for(CallableX cx:QueueI.i().getQueueCopy()){
			LoggingI.i().logSubEntry(cx.toString());
		}
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("DevConsPluginStateI [fLemurPreferredThickness=");
		builder.append(fLemurPreferredThickness);
		builder.append(", fConsoleHeightPercDefault=");
		builder.append(fConsoleHeightPercDefault);
		builder.append(", fConsoleHeightPerc=");
		builder.append(fConsoleHeightPerc);
		builder.append(", strStyle=");
		builder.append(strStyle);
		builder.append(", lstbxLoggingSection=");
		builder.append(lstbxLoggingSection);
		builder.append(", cntrMain=");
		builder.append(cntrMain);
		builder.append(", nodeParent=");
		builder.append(nodeParent);
		builder.append(", font=");
		builder.append(font);
		builder.append(", colorConsoleStyleBackground=");
		builder.append(colorConsoleStyleBackground);
		builder.append(", cntrStatus=");
		builder.append(cntrStatus);
		builder.append(", lblTitle=");
		builder.append(lblTitle);
		builder.append(", btnClipboardShow=");
		builder.append(btnClipboardShow);
		builder.append(", btnclk=");
		builder.append(btnclk);
		builder.append(", tfInput=");
		builder.append(tfInput);
		builder.append(", iKeyCodeToggleConsole=");
		builder.append(iKeyCodeToggleConsole);
		builder.append(", strInputMappingToggleDeveloperConsole=");
		builder.append(strInputMappingToggleDeveloperConsole);
		builder.append(", flStorageFolder=");
		builder.append(flStorageFolder);
		builder.append(", hmVarMon=");
		builder.append(hmVarMon);
		builder.append(", lstbxVarMonitorBar=");
		builder.append(lstbxVarMonitorBar);
		builder.append(", cxScrollTo=");
		builder.append(cxScrollTo);
		builder.append(", cmprStat=");
		builder.append(cmprStat);
		builder.append(", vrSelectionChangedToUpdateInputText=");
		builder.append(vrSelectionChangedToUpdateInputText);
		builder.append(", bKeepScrollAtBottom=");
		builder.append(bKeepScrollAtBottom);
		builder.append(", vrListBoxChangedToAutoScrollToBottom=");
		builder.append(vrListBoxChangedToAutoScrollToBottom);
		builder.append(", vrSliderChangedToSuspendAutoScrollBottom=");
		builder.append(vrSliderChangedToSuspendAutoScrollBottom);
		builder.append(", rzpMain=");
		builder.append(rzpMain);
		builder.append(", app=");
		builder.append(app);
		builder.append(", rzpVarBar=");
		builder.append(rzpVarBar);
		builder.append(", btnShowVarMon=");
		builder.append(btnShowVarMon);
		builder.append(", bUpdateNoWrap=");
		builder.append(bRequestUpdateNoWrap);
		builder.append(", btnRestoreSize=");
		builder.append(btnRestoreSize);
		builder.append(", vrSelectionChangedToShowVarHelp=");
		builder.append(vrSelectionChangedToShowVarHelp);
		builder.append(", strBaseTitle=");
		builder.append(strBaseTitle);
		builder.append(", v3fDefaultPos=");
		builder.append(v3fDefaultPos);
		builder.append(", v3fBkpLastNonDefaultPos=");
		builder.append(v3fBkpLastNonDefaultPos);
		builder.append(", v3fDefaultBarSize=");
		builder.append(v3fDefaultBarSize);
		builder.append(", fDistToToggleRestorePosSize=");
		builder.append(fDistToToggleRestorePosSize);
		builder.append(", vlstrVarMonitorEntries=");
		builder.append(vlstrVarMonitorEntries);
		builder.append(", fBkpLastNonDefaultBarWidthX=");
		builder.append(fBkpLastNonDefaultBarWidthX);
		builder.append(", v3fDefaultSize=");
		builder.append(v3fDefaultSize);
		builder.append(", v3fBkpLastNonDefaultSize=");
		builder.append(v3fBkpLastNonDefaultSize);
		builder.append(", cmVarMon=");
		builder.append(cmVarMon);
		builder.append("]");
		return builder.toString();
	}

//	@Override
//	public void resizerUpdatedLogicalStateEvent(float tpf,			ResizablePanel rzpSource) {
//	}
//	@Override
//	public void removedFromParentEvent(ResizablePanel rzpSource) {
//	}
//	@Override
//	public void resizedEvent(ResizablePanel rzpSource, Vector3f v3fNewSize) {
//	}
//	@Override
//	public void endedResizingEvent(ResizablePanel rzpSource) {
//		updateLoggingWrapAt();
//	}

	private void updateLoggingWrapAt() {
		QueueI.i().enqueue(new CallableX() {
			@Override
			public Boolean call() {
//				ArrayList<Button> abtn = MiscJmeI.i().getAllChildrenRecursiveFrom(lstbxLoggingSection, Button.class, null);
				ArrayList<Panel> abtn = MiscLemurI.i().getAllListBoxItems(lstbxLoggingSection,false);
//				Panel pnl = lstbxLoggingSection.getGridPanel().getCell(0,0);
//				Panel pnl = lstbxLoggingSection.getGridPanel().getModel().getCell(0,0,null);
				if(abtn.isEmpty())return false; //wait something show up
				
				Panel btn = abtn.get(0);
				Vector3f v3f = btn.getSize();
				if(v3f.length()==0)return false; //wait it be ready
				
				int iFontWidth = MiscLemurI.i().getFontCharWidthForStyle(btn.getStyle());
				int iWrapAt = (int) (v3f.x/iFontWidth);
				iWrapAt-=2; //safety margin to look good
				if(iWrapAt<10){
					iWrapAt=10;
				}
//				if(iWrapAt)
				LoggingI.i().setWrapAtColumn(iWrapAt);
				
				return true;
			}
		});
	}

	public boolean isAutoUpdateWrapAt() {
		return bAutoUpdateWrapAt;
	}

	public void setAutoUpdateWrapAt(boolean bAutoUpdateWrapAt) {
		this.bAutoUpdateWrapAt = bAutoUpdateWrapAt;
	}
	
}
