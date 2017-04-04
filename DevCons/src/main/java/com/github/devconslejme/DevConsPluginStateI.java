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

package com.github.devconslejme;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.lwjgl.opengl.Display;

import com.github.devconslejme.ResizablePanel.IResizableListener;
import com.github.devconslejme.misc.DialogStackOrganizerI;
import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.github.devconslejme.misc.MiscJmeI;
import com.github.devconslejme.misc.MiscLemurI;
import com.github.devconslejme.misc.QueueStateI;
import com.github.devconslejme.misc.SimpleDragParentestListenerI;
import com.github.devconslejme.misc.QueueStateI.CallableX;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.font.BitmapFont;
import com.jme3.font.BitmapText;
import com.jme3.font.LineWrapMode;
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
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.HAlignment;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.core.VersionedReference;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;
import com.simsilica.lemur.style.Attributes;
import com.simsilica.lemur.style.Styles;
import com.simsilica.lemur.text.DocumentModel;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DevConsPluginStateI extends AbstractAppState implements IResizableListener{
	public static DevConsPluginStateI i(){return GlobalInstanceManagerI.i().get(DevConsPluginStateI.class);}
	
	private Vector3f	v3fApplicationWindowSize;
	private float	fLemurPreferredThickness = 1f;
//	private Vector3f	v3fConsoleSize;
	private float	fConsoleHeightPerc = 0.5f;
	private String strStyle = "console";
	private ListBox<String>	lstbxLoggingSection;
//	private VersionedContainer	cntrMain;
	private Container	cntrMain;
	private Node	nodeParent;
	private BitmapFont	font;
	private ColorRGBA	colorConsoleStyleBackground;
	private Container	cntrStatus;
	private Label	lblStats;
	private Button	btnClipboardShow;
	private ButtonClick	btnclk;
	private TextField	tfInput;
	private int	iKeyCodeToggleConsole = KeyInput.KEY_F10;
	private String	strInputMappingToggleDeveloperConsole = "ToggleDeveloperConsole";
	private File	flStorageFolder;
	private HashMap<String,Stat> hmStatusIdValue = new HashMap<String,Stat>();
	class CallableXScrollTo extends CallableX{
		public CallableXScrollTo(){
			super(0.25f,true);
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
//			double dIndex = getValue(ECallableXKey.dIndexIn.s());
//			
////			double dMax = lstbxLoggingSection.getSlider().getModel().getMaximum();
////			double dMax = lstbxLoggingSection.getSlider().getModel().getMaximum() + lstbxLoggingSection.getVisibleItems();
//			double dMax = LoggingI.i().getLogEntriesSize();
//			if(dIndex==-1)dIndex=dMax; //-1 is a trick to reach the max
//			if(dIndex>dMax)dIndex=dMax;
//			/**
//			 * the index is actually inverted
//			 */
//			double dIndexFixed = dMax-dIndex;
////			double dPerc = dIndexFixed/dMax;
//			
//			if(Double.compare(dIndexFixed, lstbxLoggingSection.getSlider().getModel().getValue())==0){
//				return true;
//			}
//			
////			lstbxLoggingSection.getSlider().getModel().setPercent(dPerc);
//			lstbxLoggingSection.getSlider().getModel().setValue(dIndexFixed); //TODO is this redundant?
//			
//			return false; // so the next call here will let it be verified
		}

	};
	private CallableXScrollTo cxScrollTo;
	
	private Comparator<Stat>	cmprStat = new Comparator<Stat>() {
		@Override
		public int compare(Stat o1, Stat o2) {
			int i = o1.esp.compareTo(o2.esp);
			if(i==0) i = o1.strKey.compareTo(o2.strKey);
			return i;
		}
	};
	private VersionedReference<Set<Integer>>	vrSelectionChangedToUpdateInputText;
	private String[]	astrType = new String[]{
			String.class.getSimpleName(),
			
			boolean.class.getSimpleName(),
			
			float.class.getSimpleName(),
			double.class.getSimpleName(),
			
			int.class.getSimpleName(),
			long.class.getSimpleName(),
	};
	private boolean	bKeepScrollAtBottom;
	private VersionedReference<List<String>>	vrListBoxChangedToAutoScrollToBottom;
	private VersionedReference<Double>	vrSliderChangedToSuspendAutoScrollBottom;
//	private IResizableListener	irl = new IResizableListener(){
//		@Override
//		public void resizedTo(Vector3f v3fNewSize) {
//			updateVisibleLogItems(cntrMain.getPreferredSize().y);
//			System.out.println(v3fNewSize);
//		}
//	};
//	private PanelResizableEnhancer	panelResizableEnhancer;
//	private float	fMinHeight=100;
//	private float	fMinWidth=500;
	private ResizablePanel	panelMain;
//	private VersionedReference<Container>	vrMainSize;
	
//	private VersionedObject<Container>	voMainSize = new VersionedObject<Container>(){
//		@Override
//		public long getVersion() {
//			return 0;
//		}
//
//		@Override
//		public Container getObject() {
//			return cntrMain;
//		}
//
//		@Override
//		public VersionedReference<Container> createReference() {
//			return null;
//		}
//		
//	};
	
	public static enum EStatPriority{
		Top,
		High,
		Normal,
		Low,
		Bottom,
		;
		public String s(){return toString();}
	}
	
	public static class Stat{
		String strKey;
		String strValue;
		EStatPriority esp;
		String strHelp;
		
		public void set(EStatPriority esp, String strKey, String strHelp, String strValue) {
			this.strKey = strKey;
			this.strValue = strValue;
			this.strHelp=strHelp;
			this.esp = esp;
		}
		
		@Override
		public String toString() {
			return strKey+"='"+strValue+"';";
		}
	}
	
	public void configure(Application app, Node nodeParent) {
		DevConsGlobalsI.i().put(Application.class,app);
		
		this.nodeParent = nodeParent;
		DevConsGlobalsI.i().app().getStateManager().attach(this);
		
		flStorageFolder = new File(
			JmeSystem.getStorageFolder(StorageFolderType.Internal),
			DevConsGlobalsI.i().app().getClass().getPackage().getName().replace(".",File.separator) //package of Application class
				+File.separator+DevConsGlobalsI.i().app().getClass().getSimpleName() //Application class
				+File.separator+DevConsPluginStateI.class.getSimpleName() //DevCons plugin
		);
		
		JavaScriptI.i().configure(); //before all others
		
		JavaScriptI.i().setJSBinding(this);
		JavaScriptI.i().setJSBinding(BindKeyI.i());
		JavaScriptI.i().setJSBinding(ClipboardI.i());
		JavaScriptI.i().setJSBinding(FileI.i());
		
		LoggingI.i().configure();
		JavaScriptI.i().setJSBinding(LoggingI.i());
		
		QueueStateI.i().configure(DevConsGlobalsI.i().app());
		JavaScriptI.i().setJSBinding(QueueStateI.i());
	}
	
	public void putStatus(EStatPriority esp, String strKey, String strHelp, String strValue){
		Stat st=hmStatusIdValue.get(strKey);
		if(st==null){
			st=new Stat();
			hmStatusIdValue.put(strKey, st);
		}
		st.set(esp,strKey,strHelp,strValue);
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		if(isInitialized()){return;}
		
		QueueStateI.i().enqueue(new CallableX("the console starts closed",0,false) {
			@Override
			public Boolean call() {
				DevConsPluginStateI.this.setEnabled(false);
				return true;
			}
		});
		
		// jme
		cxScrollTo = new CallableXScrollTo();
		QueueStateI.i().enqueue(cxScrollTo);
		
		ActionListener al = new ActionListener(){
      @Override
			public void onAction(String name, boolean value, float tpf) {
				if(!value)return;
				
				if (name.equals(strInputMappingToggleDeveloperConsole)) {
					setEnabled(!isEnabled());
				}
			}
		};
		app.getInputManager().addMapping(strInputMappingToggleDeveloperConsole , new KeyTrigger(iKeyCodeToggleConsole ));
		app.getInputManager().addListener(al, strInputMappingToggleDeveloperConsole);
		
		// js
		JavaScriptI.i().init();
//		JavaScriptI.i().setJSBinding(this);
		
		// gui
		font = app.getAssetManager().loadFont("Interface/Fonts/DroidSansMono.fnt");
		
		initStyle();
		
		initSize();
		
		initMainContainer();
		
		initStatusSection();
		initLoggingSection();
		initInputSection();
		
		panelMain.setLocalTranslation(0, Display.getHeight(), 0);
		
//		GuiGlobals.getInstance().requestFocus(tfInput);
		
		super.initialize(stateManager, app);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		if(isEnabled()){
			JavaScriptI.i().update();
			
//			if(vrMainSize.update()){
//				updateVisibleLogItems();
//			}
		}
	}
	
	private void updateStatusValues(){
		putStatus(EStatPriority.Normal, "Sld", "DevCons Slider Value",
			String.format("%.0f/%.0f(%.0f)", 
				lstbxLoggingSection.getSlider().getModel().getValue(),
				lstbxLoggingSection.getSlider().getModel().getMaximum(),
				LoggingI.i().getLogEntriesSize()
			)
		);
		
		putStatus(EStatPriority.Normal, "VsR", "DevCons Logging area Visible Rows",
			String.format("%d", lstbxLoggingSection.getVisibleItems()) );
		
		Vector2f v2fCursor = DevConsGlobalsI.i().app().getInputManager().getCursorPosition();
		putStatus(EStatPriority.Bottom, "Cur", "Cursor Position",
			String.format("%.0f,%.0f", v2fCursor.x, v2fCursor.y) );
		
		putStatus(EStatPriority.Bottom, "Tmr", "Application Time",
				String.format("%d", DevConsGlobalsI.i().app().getTimer().getTime()) );
	}
	
	private void updateStatus() {
		ArrayList<Stat> astList = new ArrayList<Stat>(hmStatusIdValue.values());
		Collections.sort(astList,cmprStat);
		String str="";
		for(Stat st:astList){
			str+=st;
		}
		lblStats.setText(str);
	}

	@Override
	public void setEnabled(boolean enabled) {
		if(!isInitialized())throw new NullPointerException("not initialized");
		
		if(enabled){
			nodeParent.attachChild(panelMain);
		}else{
			panelMain.removeFromParent();
		}
		
		super.setEnabled(enabled);
	}
	
	private void initInputSection() {
		tfInput = new TextField("",getStyle());
		cntrMain.addChild(tfInput, BorderLayout.Position.South);
		
		BindKeyI.i().prepareKeyMappings();
		
//		QueueStateI.i().enqueue(new CallableX("copyLogSelectionToInputText",0.25f,true) {
//			@Override
//			public Boolean call() {
//				copyLogSelectionToInputText();
//				return true;
//			}
//		}.setUserCanPause(true));
		
		QueueStateI.i().enqueue(new CallableX("FocusAtDevConsInput",0.25f,true) { //TODO this delay still has a chance of typing something at other input field? like when holding for long a key?
			@Override
			public Boolean call() {
				GuiGlobals.getInstance().requestFocus(tfInput);
				return true;
			}
		});
		
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
//	QueueI.i().enqueue(
//	cxScrollTo.putKeyValue(ECallableXKey.dIndexIn.s(), dIndexIn)
//);
//		double dIndex = dIndexIn;
//			
//		double dMax = LoggingI.i().getLogEntriesSize();
//		if(dIndex==-1)dIndex=dMax; //-1 is a trick to reach the max
//		if(dIndex>dMax)dIndex=dMax;
//		/**
//		 * the index is actually inverted
//		 */
//		double dIndexFixed = dMax-dIndex;
//		
//		lstbxLoggingSection.getSlider().getModel().setValue(dIndexFixed);
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
//		return lstbxLoggingSection.getSlider().getModel().getMaximum()
//			-lstbxLoggingSection.getSlider().getModel().getValue();
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

	public static enum EUserDataMiscJme{
		strPopupHelp,
		;
		
		public String s(){return this.toString();}
	}
	
	private void initStatusSection() {
		cntrStatus = new Container(getStyle());
		cntrMain.addChild(cntrStatus, BorderLayout.Position.North);
//		MiscLemurI.i().applySimpleDragParentestListener(cntrStatus);
		
		// console stats
		lblStats = new Label("Initializing Console status.",getStyle());
		CursorEventControl.addListenersToSpatial(lblStats, SimpleDragParentestListenerI.i());
		lblStats.setColor(new ColorRGBA(1,1,0.5f,1));
		cntrStatus.addChild(lblStats,0,0);
		
		// buttons
		ArrayList<Button> abtn = new ArrayList<Button>();
		int iButtonIndex=0;
		btnClipboardShow = new Button("S",getStyle());
		setPopupHelp(btnClipboardShow, "Show Clipboard Contents");
		abtn.add(btnClipboardShow);
		
		btnclk = new ButtonClick();
		for(Button btn:abtn){
			btn.setTextHAlignment(HAlignment.Center);
			btn.addClickCommands(btnclk);
			cntrStatus.addChild(btn,0,++iButtonIndex);
		}
		
		QueueStateI.i().enqueue(new CallableX("DevConsUpdateStatus",0.5f,true) {
			@Override
			public Boolean call() {
				updateStatusValues();
				updateStatus();
				return true;
			}
		}.setUserCanPause(true));
		
	}
	
	private void setPopupHelp(Spatial spt, String strHelp){
		spt.setUserData(EUserDataMiscJme.strPopupHelp.s(), strHelp);
	}
	
	private class ButtonClick implements Command<Button>{
		@Override
		public void execute(Button source) {
			if(source.equals(btnClipboardShow)){
				ClipboardI.i().showClipboard();
			}
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
		PopupHelp, 
		SystemAlert,
		;
		
		public String s(){return this.toString();}
		public String str(){return this.toString();}
	}
	
	private void initStyle() {
		colorConsoleStyleBackground = MiscJmeI.i().colorChangeCopy(ColorRGBA.Blue, -0.75f);
		
		if(GuiGlobals.getInstance()==null)GuiGlobals.initialize(DevConsGlobalsI.i().app());
		
		Styles styles = GuiGlobals.getInstance().getStyles();
		Attributes attrs = styles.getSelector(getStyle()); // this also creates the style
		
		ColorRGBA clBg;
		
		attrs.set(EAttribute.fontSize.s(), 14);
		attrs.set(EAttribute.color.s(), MiscJmeI.i().colorChangeCopy(ColorRGBA.White,-0.25f)); //console default text
		clBg = colorConsoleStyleBackground;
		attrs.set(EAttribute.background.s(), new QuadBackgroundComponent(clBg));
		attrs.set(EAttribute.font.s(), font);
		
		attrs = styles.getSelector(Button.ELEMENT_ID, getStyle());
		attrs.set(EAttribute.color.s(), MiscJmeI.i().colorChangeCopy(ColorRGBA.Cyan,-0.10f)); 
		clBg = colorConsoleStyleBackground.mult(1.1f); //new ColorRGBA(0,0.25f,0,0.75f);
		attrs.set(Button.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
		attrs = styles.getSelector(DialogStyleElementId.PopupHelp.s(), getStyle());
		attrs.set(EAttribute.color.s(), ColorRGBA.Blue.clone());
		clBg = ColorRGBA.Cyan.clone();
		attrs.set(Button.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
		attrs = styles.getSelector("resizablePanel", getStyle());
		attrs.set(EAttribute.color.s(), ColorRGBA.Yellow.clone()); //TODO REMOVE IF USELESS
		clBg = ColorRGBA.Cyan.clone();
		attrs.set(ResizablePanel.LAYER_RESIZABLE_BORDERS, new QuadBackgroundComponent(clBg));
		
		// INPUT
		attrs = styles.getSelector(TextField.ELEMENT_ID, getStyle());
		attrs.set(EAttribute.color.s(), new ColorRGBA(0.75f,0.75f,0.25f,1));
		clBg = new ColorRGBA(0.15f, 0, 0.15f, 1);
		attrs.set(TextField.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
		attrs = styles.getSelector(ListBox.ELEMENT_ID, ListBox.SELECTOR_ID, getStyle());
		clBg = MiscJmeI.i().colorChangeCopy(ColorRGBA.Yellow,0,0.25f);
		attrs.set(ListBox.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
	}
	
//	private static class VersionedContainer extends Container implements VersionedObject<Container>{
//		private long	lVersion;
//
//		public VersionedContainer(GuiLayout layout, String style) {
//			super(layout, style);
//		}
//		
//		@Override
//		public void setPreferredSize(Vector3f size) {
//			super.setPreferredSize(size);
//			incVersion();
//		}
//		
//		@Override
//		public void setSize(Vector3f size) {
//			super.setSize(size);
//			incVersion();
//		}
//		
//		private void incVersion() {
//			lVersion++;
//		}
//
//		@Override
//		public long getVersion() {
//			return lVersion;
//		}
//
//		@Override
//		public Container getObject() {
//			return this;
//		}
//
//		@Override
//		public VersionedReference<Container> createReference() {
//      return new VersionedReference<Container>(this);
//		}
//
//	}
	
	private void initMainContainer() {
		cntrMain = new Container(new BorderLayout(), getStyle());
		
		panelMain = new ResizablePanel(cntrMain);
		panelMain.addResizableListener(this);
//		panelMain.setMinSize(new Vector3f(fMinWidth ,fMinHeight,0));
		
		setConsoleHeightPerc(fConsoleHeightPerc); //just to init default value
	}

	private void initLoggingSection() {
		lstbxLoggingSection = new ListBox<String>(null, getStyle());
		LoggingI.i().setModelAt(lstbxLoggingSection);
		
		cntrMain.addChild(lstbxLoggingSection, BorderLayout.Position.Center);
		
//		lstbxLoggingSection.addCommands(ListAction.Down, cmdClickLogRow);
		
		
		vrSelectionChangedToUpdateInputText = lstbxLoggingSection.getSelectionModel().createReference();
		vrListBoxChangedToAutoScrollToBottom = lstbxLoggingSection.getModel().createReference();
		vrSliderChangedToSuspendAutoScrollBottom=lstbxLoggingSection.getSlider().getModel().createReference();
		
//		updateVisibleLogItems();
	}

	private void initSize() {
		v3fApplicationWindowSize = new Vector3f(
			Display.getWidth(),
			Display.getHeight(),
			fLemurPreferredThickness );
	}

	public float getConsoleHeightPerc() {
		return fConsoleHeightPerc;
	}

	public void setConsoleHeightPerc(float fConsoleHeightPerc2) {
		this.fConsoleHeightPerc = fConsoleHeightPerc2;
		
		if(fConsoleHeightPerc>1.0f)fConsoleHeightPerc=1.0f;
		if(fConsoleHeightPerc<0f)fConsoleHeightPerc=0f; //will be further fixed below
		
		float fHeight = (v3fApplicationWindowSize.y * fConsoleHeightPerc);
		
//		if(fHeight<fMinHeight){
//			fHeight=fMinHeight; //TODO this is guess work... areas: info+list+input with 20 each + safety margin
//			fConsoleHeightPerc=fHeight/v3fApplicationWindowSize.y;
//		}
		
		updateConsoleHeight(fHeight);
	}

	private void updateConsoleHeight(float fHeight) {
		QueueStateI.i().enqueue(new CallableX(0,false) {
			@Override
			public Boolean call() {
//				v3fConsoleSize
				Vector3f v3f = new Vector3f(
						v3fApplicationWindowSize.x,
						fHeight,
						fLemurPreferredThickness);
					
				panelMain.setPreferredSize(v3f);
//				cntrMain.setPreferredSize(v3fConsoleSize);
				
				updateVisibleLogItems();
				
				return true;
			}
		});
	}
	
	public void updateVisibleLogItems(){
		QueueStateI.i().enqueue(new CallableX(0.1f,false){
			@Override
			public Boolean call() {
//			float fHeight = cntrMain.getPreferredSize().y;
				float fHeight = cntrMain.getSize().y; //inner container needs some time to be setup by lemur?
				if(Float.compare(fHeight,0f)==0)return false;
				
				int iLines = (int) (fHeight/MiscLemurI.i().getEntryHeightPixels(lstbxLoggingSection));
				iLines--; //to void the text being too close to each other 
				lstbxLoggingSection.setVisibleItems(iLines);
//				lstbxLoggingSection.getCellRenderer();
				
				recursivelyApplyNoWrap(lstbxLoggingSection);
				
				return true;
			}
		});
	}

	private void recursivelyApplyNoWrap(Node nodeParent) {
		for(Spatial spt:nodeParent.getChildren()){
			if(spt instanceof BitmapText){
				((BitmapText)spt).setLineWrapMode(LineWrapMode.NoWrap);
			}
			if(spt instanceof Node){
				recursivelyApplyNoWrap((Node)spt);
			}
		}
	}

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

	public void insertAtInputTextCaratPos(String str) {
		tfInput.getDocumentModel().insert(str);
	}
//	public void insertInputTextAtCaratPosition(String str) {
//		tfInput.getDocumentModel().insert(str);
//	}

//	public Application getApp() {
//		return app;
//	}
	
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
		for(String strType:astrType){
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
		String str=getInputText();
		str=str.substring(0,tfInput.getDocumentModel().getCarat());
		Integer iNotAlpha=0;
		for(int i=str.length()-1;i>=0;i--){
			if( !(str.charAt(i)+"").matches("[a-zA-Z]") ){
				iNotAlpha=i+1;
				break;
			}
		}
		return str.substring(iNotAlpha);
	}

	@Override
	public void resizedTo(Vector3f v3fNewSize) {
		updateVisibleLogItems();
	}

	public void showQueue(){
		for(CallableX cx:QueueStateI.i().getQueueCopy()){
			LoggingI.i().logSubEntry(cx.toString());
		}
	}
}
