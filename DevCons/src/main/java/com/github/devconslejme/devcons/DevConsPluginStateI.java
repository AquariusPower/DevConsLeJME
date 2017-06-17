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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import com.github.devconslejme.es.DialogHierarchyComp.DiagCompBean;
import com.github.devconslejme.es.DialogHierarchySystemI;
import com.github.devconslejme.gendiag.ContextMenuI;
import com.github.devconslejme.gendiag.ContextMenuI.ContextButton;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu.ApplyContextChoiceCmd;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenuAnon;
import com.github.devconslejme.gendiag.ContextMenuI.HintUpdaterPerCtxtBtn;
import com.github.devconslejme.gendiag.DialogHierarchyStateI.IDialogHierarchyListener;
import com.github.devconslejme.gendiag.DialogHierarchyStateI;
import com.github.devconslejme.gendiag.DialogHierarchyStateI.DialogVisuals;
import com.github.devconslejme.gendiag.DialogHierarchyStateI.IGUIUserInteraction;
import com.github.devconslejme.gendiag.ManagerHelperI;
import com.github.devconslejme.gendiag.ManagerHelperI.RetVal;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.HierarchySorterI.EHierarchyType;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavaLangI.FuncIn;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableX;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.SimpleAppState;
import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.github.devconslejme.misc.lemur.CaratAutoPositionListenerI;
import com.github.devconslejme.misc.lemur.CursorListenerX;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.HoverHighlightEffectI;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI.EPopup;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.misc.lemur.ResizablePanel.EEdge;
import com.github.devconslejme.misc.lemur.VersionedVector3f;
import com.jme3.app.Application;
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
import com.simsilica.es.EntityId;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.HAlignment;
import com.simsilica.lemur.Insets3f;
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
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;
import com.simsilica.lemur.style.Attributes;
import com.simsilica.lemur.style.Styles;
import com.simsilica.lemur.text.DocumentModel;

/**
 * By default hit F10 to open it. 
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DevConsPluginStateI extends SimpleAppState implements IDialogHierarchyListener, IGUIUserInteraction {
	public static DevConsPluginStateI i(){return GlobalManagerI.i().get(DevConsPluginStateI.class);}
	
//	private Vector3f	v3fApplicationWindowSize;
//	private float	fLemurPreferredThickness = 1f;
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
	private BtnConsoleAction btnTitle;
//	private ButtonWithCmd btnClipboardShow;
	private BtnConsoleActionCursorListenerX	bclk;
	private TextField	tfInput;
	private Integer	iKeyCodeToggleConsole;
	private String	strInputMappingToggleDeveloperConsole = "ToggleDeveloperConsole";
//	private File	flStorageFolder;
	private HashMap<String,VarMon> hmVarMon = new HashMap<String,VarMon>();
	private ListBox<String> lstbxVarMonitorBar = new ListBox<String>();
	private CallableXScrollTo cxScrollTo;
	private boolean	bKeepScrollAtBottom=true;
	private VersionedReference<List<String>>	vrListBoxChangedToAutoScrollToBottom;
	private VersionedReference<Double>	vrSliderChangedToSuspendAutoScrollBottom;
	private ResizablePanel	rzpMain;
//	private Application	app;
	private ResizablePanel	rzpVarBar;
//	private Button	btnShowVarMon;
	private boolean	bRequestUpdateNoWrap;
//	private Button	btnRestoreSize;
	private VersionedReference<Set<Integer>>	vrSelectionChangedToShowVarHelp;
	private String	strBaseTitle = "DevCons";
	private Vector3f	v3fDefaultPos = new Vector3f(0, getWindowSize().y-20, 0);
	private Vector3f	v3fBkpLastNonDefaultPos;
	private Vector3f	v3fDefaultBarSize = new Vector3f(100,100,0);
	private float	fDistToToggleRestorePosSize = 5f;
	private VersionedStatus vlstrVarMonitorEntries = new VersionedStatus();
	private Float	fBkpLastNonDefaultBarWidthX;
	private  Vector3f	v3fDefaultSize;
	private Vector3f	v3fBkpLastNonDefaultSize;
	private ContextMenuAnon	cmVarMon;
	private boolean	bAutoUpdateWrapAt=true;
	private VersionedReference<Set<Integer>>	vrSelectionChangedToUpdateInputText;
	private boolean bAllowHiddenStats=true; //TODO shouldnt it init as false? :)
	private LinkedHashMap<String,Button>	hmButtons = new LinkedHashMap<String,Button>();
	private CallableXAnon cxRecreateButtons;
	
	private Comparator<VarMon>	cmprStat = new Comparator<VarMon>() {
		@Override
		public int compare(VarMon o1, VarMon o2) {
			int i = o1.eStatPriority.compareTo(o2.eStatPriority);
			if(i==0) i = o1.strKey.compareTo(o2.strKey);
			return i;
		}
	};
	private ActionListener	alToggleConsole;
	private DialogVisuals	vs;
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
			super(1);
			setDelaySeconds(0.25f);
			enableLoopMode();
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
		private EStatPriority eStatPriority=EStatPriority.Normal;
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
			this.eStatPriority = esp;
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
		
		public CallableX getCallableUpdater(){
			return cx;
		}
		
		public String getQueueName() {
			return VarMon.class.getSimpleName()+":"+strKey;
		}
		public String getValueToList() {
			return "="+strValue;
		}
	}
	
	public void configure(Integer iOpenDevConsKeyCode, Node nodeParent) {
//		this.app=GlobalManagerI.i().get(Application.class);
		
//		font = StringTextJmeI.i().loadFont("Interface/Fonts/DroidSansMono.fnt"); 
		font = StringTextDevConsI.i().getDefaultMonoFont(); 
//		StringTextJmeI.i().setDefaultMonoFontOverride(font);
		
		this.iKeyCodeToggleConsole=iOpenDevConsKeyCode;
		if(this.iKeyCodeToggleConsole==null)this.iKeyCodeToggleConsole=KeyInput.KEY_F10;
//		setKeyCodeToggleConsole(iOpenDevConsKeyCode);
		
		this.nodeParent = nodeParent;
		
		AppI.i().attatchAppState(this);
//		app.getStateManager().attach(this);
		
		LoggingI.i().configure();
		JavaScriptI.i().configure(); //before all others TODO why? it is already a globals listener...
		
		ManagerHelperI.i().setHandleCallRetVal(new FuncIn<RetVal>(){@Override public void applyIn(RetVal rv){
			LoggingI.i().logEntry(rv.getDescription());
			JavaScriptI.i().showValue(rv.getRetVal());
		}});
		LoggingI.i().logEntry("Handling calls return values, will be shown here thru: "+ManagerHelperI.class.getName());
		
		DialogHierarchyStateI.i().addDialogHierarchyListener(this);
		DialogHierarchyStateI.i().addGlobalListOptionsUserInteractionListener(this);
	}
	
	public static abstract class CallableVarMonX extends CallableX<CallableVarMonX>{
		private VarMon vm;
		
		@Override
		protected CallableVarMonX getThis() {
			return this;
		}

		public VarMon getVarMon() {
			return vm;
		}
		
	}
	
	public VarMon createVarMon(EStatPriority esp, String strKey, String strHelp, CallableVarMonX cx){
		cx.vm=(hmVarMon.get(strKey));
		if(cx.getVarMon()==null){
			cx.vm=(new VarMon());
			hmVarMon.put(strKey, cx.getVarMon());
		}
		cx.getVarMon().set(esp,strKey,strHelp,"",cx);
		
		QueueI.i().enqueue(cx.setName(cx.getVarMon().getQueueName()).enableLoopMode().setDelaySeconds(1f));
		
		enqueueUpdateVarMonList();
//		updateVarMonList();
		
		return cx.getVarMon();
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
		
		alToggleConsole = new ActionListener(){
      @Override
			public void onAction(String name, boolean value, float tpf) {
				if(!value)return;
				
				if (name.equals(strInputMappingToggleDeveloperConsole)) {
					setEnabled(!isEnabled());
				}
			}
		};
		setKeyCodeToggleConsole(iKeyCodeToggleConsole);
		
		// js
		JavaScriptI.i().init();
//		JavaScriptI.i().setJSBinding(this);
		
		// gui
//		font = TextStringI.i().loadFont("Interface/Fonts/DroidSansMono.fnt");
//		TextStringI.i().setDefaultMonoFont(font);
		
		initStyle();
		
//		initSize();
		
		initMainContainer();
		
		initStatusSection();
		initLoggingSection();
		initInputSection();
		
		initVarMonitorWestBar();
		
		toggleDefaultPosSize(true);
		
		CaratAutoPositionListenerI.i().applyRecursivelyAtAllTextFieldsOf(cntrMain);

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
			.enableLoopMode()
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
			LoggingI.i().logEntry(strBaseTitle+": restoring default pos size");
		}else{
			if(v3fBkpLastNonDefaultPos!=null){
				rzpMain.setLocalTranslationXY(v3fBkpLastNonDefaultPos);
			}
			if(v3fBkpLastNonDefaultSize!=null){
				rzpMain.setPreferredSizeWH(v3fBkpLastNonDefaultSize);
			}
			if(fBkpLastNonDefaultBarWidthX!=null){
				rzpVarBar.getPreferredSize().x=fBkpLastNonDefaultBarWidthX;
			}
			LoggingI.i().logEntry(strBaseTitle+": applying last non-default pos size");
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
		
		QueueI.i().enqueue(
			new CallableX() {@Override public Boolean call() {
				updateVarMonHelp(); 
				return true;
			}}.setName("VarMonHelp")
				.setDelaySeconds(1f)
				.enableLoopMode()
				.setUserCanPause(true)
		);
		
		initVarMonitorContextMenu();
		
		QueueI.i().enqueue(new CallableX() {
				@Override
				public Boolean call() {
					//the row count is the 
					if(lstbxVarMonitorBar.getGridPanel().getModel().getRowCount()==0)return false;
					if(vlstrVarMonitorEntries.size()!=lstbxVarMonitorBar.getGridPanel().getModel().getRowCount())return false; //TODO this will never happen right?
					if(lstbxVarMonitorBar.getVisibleItems()<=0)return false; //should never be < 0 tho, extra care may be?
					
					/**
					 * correct amount of shown items must be ready
					 * TODO pointless?
					 */
					int iShown = lstbxVarMonitorBar.getVisibleItems();
					if(iShown > vlstrVarMonitorEntries.size()){
						iShown = vlstrVarMonitorEntries.size();
					}
					
//					int iShown = vlstrVarMonitorEntries.size();
//					if(vlstrVarMonitorEntries.size() < lstbxVarMonitorBar.getVisibleItems()){
//						iShown = lstbxVarMonitorBar.getVisibleItems();
//					}
//					
//					int iShown = lstbxVarMonitorBar.getVisibleItems();
//					if(vlstrVarMonitorEntries.size()<iShown)iShown = vlstrVarMonitorEntries.size();
//					if(lstbxVarMonitorBar.getGridPanel().getModel().getRowCount()!=iShown)return false;
	//				if(MiscLemurI.i().getAllListBoxItems(lstbxVarMonitorBar,false).size()!=iShown)return false;
					
					if(ContextMenuI.i().applyContextMenuAtListBoxItems(lstbxVarMonitorBar,cmVarMon)>0){
						// keep only at the var id
						for(Panel pnl:MiscLemurI.i().getAllListBoxItems(lstbxVarMonitorBar, false)){
							Button btn=(Button)pnl;
							if(btn.getText().startsWith("=")){ //remove from the displayed value
								ContextMenuI.i().removeContextMenuOf(btn);
							}
						}
					}
					
					return true;
				}
			}.setName("GrantApplyContextMenuAtVarMonListBox")
			 .enableLoopMode()
			 .setDelaySeconds(1f)
		);

	}
	
	private void initVarMonitorContextMenu() {
		cmVarMon = new ContextMenuAnon(rzpMain);
//		cmVarMon.setHierarchyParent(rzpMain);
		
		ApplyContextChoiceCmd cmd = new ApplyContextChoiceCmd() {
			@Override public void executeContextCommand(ContextButton btnSourcePriorityChoice) {
				EStatPriority e = null;
				
				String str = btnSourcePriorityChoice.getText();
				if(str.startsWith(EStatPriority.class.getSimpleName())){
					str=str.substring(EStatPriority.class.getSimpleName().length()+1);
					try{e=EStatPriority.valueOf(str);}catch(IllegalArgumentException ex){}
				}
				
				if(e!=null){
					Button btn = cmVarMon.getContextSource();
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
		cmVarMon.setSingleChoiceMode(true);
		for(int i=0;i<EStatPriority.values().length;i++){
			EStatPriority esp = EStatPriority.values()[i];
			String str=EStatPriority.class.getSimpleName()+":"+esp.s();
			cmVarMon.addNewEntry(str, esp, cmd, new HintUpdaterPerCtxtBtn() {
				@Override
				public Boolean call() {
					Button btnSource = cmVarMon.getContextSource();
					VarMon vm = hmVarMon.get(btnSource.getText());
					return (vm!=null && vm.eStatPriority.equals(getStoredValueFromContextButton())); 
				}
			});
		}
	}
	
	private void initVarMonValues() {
		createVarMon(EStatPriority.Bottom, "Slider", strBaseTitle+" Logging area Slider Value",new CallableVarMonX() {
			@Override
			public Boolean call() {
				getVarMon().set(
					String.format("%.0f/%.0f(%.0f)", 
						lstbxLoggingSection.getSlider().getModel().getValue(),
						lstbxLoggingSection.getSlider().getModel().getMaximum(),
						LoggingI.i().getLogEntriesSize()
					)
				);
				return true;
			}
		});
		
		createVarMon(EStatPriority.Bottom, "VisibleRows", strBaseTitle+" Logging area Visible Rows",new CallableVarMonX() {
			@Override
			public Boolean call() {
				getVarMon().set( String.format("%d", lstbxLoggingSection.getVisibleItems()) );
				return true;
			}
		});
		
		VarMon vmCursorPos = createVarMon(EStatPriority.Normal, "CursorPos", "Mouse Cursor Position on the application",new CallableVarMonX() {
			@Override
			public Boolean call() {
//				Vector2f v2fCursor = app.getInputManager().getCursorPosition();
				Vector2f v2fCursor = HWEnvironmentJmeI.i().getMouse().getPos2D();//app.getInputManager().getCursorPosition();
				getVarMon().set( String.format("%.0f,%.0f", v2fCursor.x, v2fCursor.y) );
				return true;
			}
		});
		vmCursorPos.getCallableUpdater().setDelaySeconds(0.25f);
		
		createVarMon(EStatPriority.Normal, "AppTime", "Application Elapsed Time from its start time",new CallableVarMonX() {
			@Override
			public Boolean call() {
//				getVarMon().set( String.format("%.3f", app.getTimer().getTimeInSeconds()) );
				getVarMon().set( String.format("%.3f", AppI.i().getTimeInSeconds()) );
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
//			cntrMain.addChild(new Button("tstOverride"), BorderLayout.Position.West);
		}else{
			cntrMain.removeChild(rzpVarBar);
		}
		
		bRequestUpdateNoWrap=true; 
	}

	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		if(bRequestUpdateNoWrap){
			StringTextJmeI.i().recursivelyApplyTextNoWrap(rzpMain);
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
				ArrayList<VarMon> astList = new ArrayList<VarMon>(hmVarMon.values());
				Collections.sort(astList,cmprStat);
				vlstrVarMonitorEntries.clear();
				String strAddToTitle="";
				labelLoop:for(VarMon vm:astList){
					switch(vm.eStatPriority){
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
				}
				
				if(!strAddToTitle.isEmpty())btnTitle.setText(strBaseTitle+":"+strAddToTitle);
				
				vrSelectionChangedToShowVarHelp = lstbxVarMonitorBar.getSelectionModel().createReference();

//				QueueI.i().enqueue(new CallableX() {
//					@Override
//					public Boolean call() {
//						if(lstbxVarMonitorBar.getGridPanel().getModel().getRowCount()==0)return false;
//						
//						/**
//						 * correct amount of shown items must be ready
//						 */
//						int iShown = lstbxVarMonitorBar.getVisibleItems();
//						if(vlstrVarMonitorEntries.size()<iShown)iShown = vlstrVarMonitorEntries.size();
//						if(lstbxVarMonitorBar.getGridPanel().getModel().getRowCount()!=iShown)return false;
////						if(MiscLemurI.i().getAllListBoxItems(lstbxVarMonitorBar,false).size()!=iShown)return false;
//						
//						ContextMenuI.i().applyContextMenuAtListBoxItems(lstbxVarMonitorBar,cmVarMon);
//						// keep only at the var id
//						for(Panel pnl:MiscLemurI.i().getAllListBoxItems(lstbxVarMonitorBar, false)){
//							Button btn=(Button)pnl;
//							if(btn.getText().startsWith("=")){ //remove from the displayed value
//								ContextMenuI.i().removeContextMenuOf(btn);
//							}
//						}
//						
//						return true;
//					}
//				}.setName("ContextMenuAtListBoxAfterPopulated"));
				
				bRequestUpdateNoWrap=true;
				return true;
			}
		}); 
	}
	
	@Override
	public void setEnabled(boolean enabled) {
		if(!isInitialized())throw new DetailedException("not initialized");
		
		if(enabled){
			DialogHierarchyStateI.i().showDialog(rzpMain);
			enqueueUpdateVarMonList();
//			nodeParent.attachChild(hrpMain);
		}else{
			rzpMain.close();
		}
		
		super.setEnabled(enabled);
	}
	
	private void initInputSection() {
		tfInput = new TextField("",getStyle());
		cntrMain.addChild(tfInput, BorderLayout.Position.South);
		
//		DragParentestPanelListenerI.i().applyAt(tfInput);
		
		BindKeyLemurI.i().prepareKeyMappings();
		
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
	
	public static abstract class CallButtonPostCfg extends CallableX<CallButtonPostCfg>{
		private BtnConsoleAction	btnc;

		private void setButton(BtnConsoleAction btnc) {
			this.btnc=btnc;
		}
		public BtnConsoleAction getButton() {
			return btnc;
		}
	}
	
	private void initStatusSection() {
		cntrStatus = new Container(getStyle());
		cntrMain.addChild(cntrStatus, BorderLayout.Position.North);
		
		// buttons
		putButtonLater("DefaultPosSize", "Restore DevCons defaults Size and Position", new Command<Button>() {
				@Override
				public void execute(Button source) {
					toggleDefaultPosSize(false);
				}
			}, 
			null
		);
		
		putButtonLater("VarMonBar:Toggle", "Show Variables Monitor Bar", 
			new Command<Button>() {
				@Override
				public void execute(Button source) {
					toggleVarMonitorBar(null);
				}
			}, 
			new CallButtonPostCfg() {
				@Override
				public Boolean call() {
					ContextMenu cm = new ContextMenu(rzpMain);
					cm.addNewEntry(
						"ToggleHiddenStats", 
						new ApplyContextChoiceCmd() {@Override public void executeContextCommand(ContextButton source) {
								bAllowHiddenStats=!bAllowHiddenStats;
								enqueueUpdateVarMonList();
							}
						},
						new HintUpdaterPerCtxtBtn() {
							@Override
							public Boolean call() {
								setPopupHintHelp("(click to "+(bAllowHiddenStats?"show":"hide")+")"); //say the next action on clicking
								return true;
							}
						}
					);
					ContextMenuI.i().applyContextMenuAtSource(getButton(),cm);
					
					return true;
				}
			}
		);
		
		putButtonLater("Clipboard:Show", "Show Clipboard Contents", new Command<Button>(){@Override	public void execute(Button source) {
			ClipboardI.i().showClipboard();	}},null);
		
		// special "useless" (w/o commands) button
		btnTitle = new BtnConsoleAction(strBaseTitle,getStyle());
		btnTitle.setColor(new ColorRGBA(1,1,0.5f,1));
		btnTitle.setTextHAlignment(HAlignment.Right);
		hmButtons.put(btnTitle.getText(),btnTitle);
		
		bclk = new BtnConsoleActionCursorListenerX();
		
		requestRecreateButtons();
	}
	
	public static class BtnConsoleAction extends Button{
		private BtnConsoleAction(String s, String style) {
			super(s, style);
		}

		public Command<? super Button> getCmd() {
			return cmd;
		}

		private void setCmd(Command<? super Button> cmd) {
			DetailedException.assertNotAlreadySet(this.cmd, cmd, this);
			this.cmd = cmd;
		}

		private Command<? super Button> cmd;
	}
	
	/**
	 * uses queue
	 * @param strTextKey
	 * @param strPopupHelp
	 * @param cmd
	 */
	public void putButtonLater(String strTextKey, String strPopupHelp, Command<? super Button> cmd, CallButtonPostCfg cbpc){
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(!isInitialized())return false;
				
				BtnConsoleAction btnc = putButton(strTextKey, strPopupHelp, cmd);
				
				if(cbpc!=null){
					cbpc.setButton(btnc);
					QueueI.i().enqueue(cbpc);
				}
				
				return true;
			}
		});
	}
	
	/**
	 * 
	 * @param strTextKey
	 * @param strPopupHelp can be null
	 * @param cmd
	 * @return
	 */
	private BtnConsoleAction putButton(String strTextKey, String strPopupHelp, Command<? super Button> cmd){
		DetailedException.assertIsInitialized(isInitialized(), this, strTextKey, strPopupHelp, cmd);
		
		BtnConsoleAction btnc = new BtnConsoleAction(strTextKey,getStyle());
		btnc.setInsets(new Insets3f(0,0,0,10));
		if(strPopupHelp!=null)PopupHintHelpListenerI.i().setPopupHintHelp(btnc, strPopupHelp);
		btnc.setCmd(cmd);
		hmButtons.put(btnc.getText(),btnc);
		requestRecreateButtons();
		
		return btnc;
	}
	
	public void requestRecreateButtons() {
		if(cxRecreateButtons==null)cxRecreateButtons = new CallableXAnon() {
				@Override
				public Boolean call() {
//					recreateButtons();
					
					cntrStatus.clearChildren();
					
					ArrayList<Button> abtn = new ArrayList<Button>(hmButtons.values());
					abtn.remove(btnTitle);
					abtn.add(btnTitle); //last
					
					int iButtonIndex=0;
					for(Button btn:abtn){
//						if (pnl instanceof Button) {
//							Button btn = (Button) pnl;
							btn.setTextHAlignment(HAlignment.Center);
							CursorEventControl.addListenersToSpatial(btn, bclk);
//						}
						DragParentestPanelListenerI.i().applyAt(btn);
						cntrStatus.addChild(btn,0,iButtonIndex++);
					}
					
					return true;
				}
			};
		
		QueueI.i().enqueue(cxRecreateButtons);
	}
	
//	/**
//	 * prefer using {@link #requestRecreateButtons()}
//	 */
//	private void recreateButtons(){
//		int iButtonIndex=0;
//		cntrStatus.clearChildren();
//		for(Button btn:hmButtons.values()){
////			if (pnl instanceof Button) {
////				Button btn = (Button) pnl;
//				btn.setTextHAlignment(HAlignment.Center);
//				CursorEventControl.addListenersToSpatial(btn, btnclk);
////			}
//			DragParentestPanelListenerI.i().applyAt(btn);
//			cntrStatus.addChild(btn,0,iButtonIndex++);
//		}
//	}
	
	private class BtnConsoleActionCursorListenerX extends CursorListenerX{
		@Override
		protected boolean click(CursorButtonEvent event, Spatial target,				Spatial capture) {
			if(event.getButtonIndex()!=0)return false; //only left clicks allowed
			
			BtnConsoleAction btn = (BtnConsoleAction) capture;
			if(btn.cmd!=null){
				btn.cmd.execute(btn);
				return true;
			}else{
				MessagesI.i().warnMsg(this, "missing command for", capture, target);
			}
			
			return false;
		}
	}
	
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
		
		//TODO this would require recreating the popup help label every time to apply current target's style...
		attrs = styles.getSelector(EPopup.DialogStyleElementIdPopupHelp.getUId(), getStyle());
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
		vs = DialogHierarchyStateI.i().prepareDialogParts(DevConsPluginStateI.class.getSimpleName(), getStyle());
		rzpMain = vs.getDialog(); 
//		EntityId entid = DialogHierarchyStateI.i().getEntityId(rzpMain); //DialogHierarchySystemI.i().createEntity(ContextMenuI.class.getSimpleName());
		DialogHierarchySystemI.i().setHierarchyComp(vs.getEntityId(), new DiagCompBean().setHierarchyType(EHierarchyType.Top));
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
						0);//fLemurPreferredThickness);
				
				/**
				 * PUTS A NEW SIZE REQUEST
				 */
				rzpMain.setPreferredSizeWH(v3f);
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
				}.setName(strBaseTitle+": updates default size"));
				
				return true;
			}
		}.setName(strBaseTitle+": request new size"));
		
	}
	
	public Vector3f getWindowSize(){
		return new Vector3f(HWEnvironmentJmeI.i().getDisplay().getWidth(),HWEnvironmentJmeI.i().getDisplay().getHeight(),0);
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

//	public File getStorageFolder() {
//		return flStorageFolder;
//	}

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
		builder.append(btnTitle);
		builder.append(", tfInput=");
		builder.append(tfInput);
		builder.append(", iKeyCodeToggleConsole=");
		builder.append(iKeyCodeToggleConsole);
		builder.append(", strInputMappingToggleDeveloperConsole=");
		builder.append(strInputMappingToggleDeveloperConsole);
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
//		builder.append(", app=");
//		builder.append(app);
		builder.append(", rzpVarBar=");
		builder.append(rzpVarBar);
		builder.append(", bUpdateNoWrap=");
		builder.append(bRequestUpdateNoWrap);
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

	public int getKeyCodeToggleConsole() {
		return iKeyCodeToggleConsole;
	}

//	public DevConsPluginStateI setKeyCodeToggleConsole(String strKeyToggleConsoleNoMods) {
//		setKeyCodeToggleConsole(new KeyBind().setFromKeyCfg(strKeyToggleConsoleNoMods).getActionKey().getKeyCode());
//		return this;
//	}
	
	private DevConsPluginStateI setKeyCodeToggleConsole(int iKeyCodeToggleConsole) {
		this.iKeyCodeToggleConsole = iKeyCodeToggleConsole;
		
//		if(alToggleConsole!=null){ //otherwise will be called again during initialize
		AppI.i().removeMapping(strInputMappingToggleDeveloperConsole);
//			if(app.getInputManager().hasMapping(strInputMappingToggleDeveloperConsole)){
//				app.getInputManager().deleteMapping(strInputMappingToggleDeveloperConsole);
//			}
		AppI.i().addKeyMappingAndListener(strInputMappingToggleDeveloperConsole, new KeyTrigger(iKeyCodeToggleConsole), alToggleConsole);
//			app.getInputManager().addMapping(strInputMappingToggleDeveloperConsole, 
//				new KeyTrigger(iKeyCodeToggleConsole));
//			
//			app.getInputManager().addListener(alToggleConsole, strInputMappingToggleDeveloperConsole);
//		}
		
		String strKey = KeyCodeManagerI.i().getKeyIdFromCode(iKeyCodeToggleConsole);
		KeyCodeManagerI.i().getKeyForId(strKey).setIgnoreKeyCode();
		
		return this;
	}

	@Override
	public void dialogCreatedEvent(DialogVisuals vs) {
		LoggingI.i().logEntry("DialogCreated:"+vs.getDialog().getName());
	}

	@Override
	public void dialogMadeVisibleEvent(DialogVisuals vs) {
		LoggingI.i().logEntry("DialogOpened:"+vs.getDialog().getName());
	}

	@Override
	public void dialogClosedEvent(DialogVisuals vs) {
		LoggingI.i().logEntry("DialogClosed:"+vs.getDialog().getName());
	}

	@Override
	public void receiveSubmitedUserInputTextEvent(DialogVisuals vs, String str) {
		LoggingI.i().logEntry("DialogInputSubmitted@("+vs.getDialog().getName()+"): '"+str+"'");
	}

	@Override
	public void receiveLastClickedItemStoredValueEvent(DialogVisuals vs, Object obj) {
		LoggingI.i().logEntry("DialogSelectedListItemValue@("+vs.getDialog().getName()+"):");
		JavaScriptI.i().showValue(obj);
	}
	
}
