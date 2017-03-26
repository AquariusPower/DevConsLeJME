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

import java.util.ArrayList;

import org.lwjgl.opengl.Display;

import com.github.devconslejme.misc.AutoCompleteI.AutoCompleteResult;
import com.github.devconslejme.misc.MiscJmeI;
import com.github.devconslejme.misc.MiscLemurI;
import com.jme3.app.Application;
import com.jme3.font.BitmapFont;
import com.jme3.input.KeyInput;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
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
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;
import com.simsilica.lemur.style.Attributes;
import com.simsilica.lemur.style.Styles;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ConsolePluginI {
	private static ConsolePluginI instance = new ConsolePluginI();
	/*instance*/ public static ConsolePluginI i(){return instance;}
	
	private Vector3f	v3fApplicationWindowSize;
	private float	fLemurPreferredThickness = 1f;
	private Vector3f	v3fConsoleSize;
	private float	fConsoleHeightPerc = 0.5f;
	private boolean bInitialized = false;
	private String strStyle = "console";
	private ListBox<String>	lstbxLoggingSection;
	private Container	cntrMain;
	private Node	nodeParent;
	private BitmapFont	font;
	private ColorRGBA	colorConsoleStyleBackground;
	private Application	app;
	private Container	cntrStatus;
	private Label	lblStats;
	private Button	btnClipboardShow;
	private ButtonClick	btnclk;
	private TextField	tfInput;
	private ArrayList<String> astrCmdHistory = new ArrayList<String>();
	private int iNavigateCmdHistoryIndex = 0;
	
	public ConsolePluginI(){
		if(instance==null)return;
		if(instance!=this)throw new NullPointerException("use the single instance");
	}
	
	public void init(Application app, Node nodeParent) {
		if(bInitialized){return;}
		
		astrCmdHistory.add(""); //just to avoid empty list when adding new cmd to it
		
		JavaScriptI.i().setJSBinding(this);
		
		// jme
		this.app=app;
		
		// gui
		this.nodeParent = nodeParent;
		
		font = app.getAssetManager().loadFont("Interface/Fonts/DroidSansMono.fnt");
		
		initStyle();
		
		initSize();
		
		initMainContainer();
		
		initStatusSection();
		initLoggingSection();
		initInputSection();
		
		cntrMain.setLocalTranslation(0, Display.getHeight(), 0);
		
		GuiGlobals.getInstance().requestFocus(tfInput);
		
		bInitialized=true;
	}
	
	private void initInputSection() {
		tfInput = new TextField("",getStyle());
		cntrMain.addChild(tfInput, BorderLayout.Position.South);
		
		BindKeyI.i().prepareKeyMappings();
	}
	
	
	protected void navigateCmdHist(int keyCode) {
		switch(keyCode){
			case KeyInput.KEY_UP:
				iNavigateCmdHistoryIndex--;
				break;
			case KeyInput.KEY_DOWN:
				iNavigateCmdHistoryIndex++;
				break;
		}
		if(iNavigateCmdHistoryIndex<0)iNavigateCmdHistoryIndex=0;
		if(iNavigateCmdHistoryIndex> (astrCmdHistory.size()-1)  )iNavigateCmdHistoryIndex=astrCmdHistory.size()-1;
		
		tfInput.setText(astrCmdHistory.get(iNavigateCmdHistoryIndex));
	}

	protected void scrollToBottom() {
		scrollTo(-1);
	}

	protected void scrollTo(final double dIndexIN) {
		app.enqueue(new Runnable() {
			@Override
			public void run() {
				double dIndex = dIndexIN;
				
				/**
				 * the index is actually inverted
				 */
				double dMax = lstbxLoggingSection.getSlider().getModel().getMaximum();
				if(dIndex==-1)dIndex=dMax;
				dIndex = dMax-dIndex;
				double dPerc = dIndex/dMax;
				
				lstbxLoggingSection.getSlider().getModel().setPercent(dPerc);
				lstbxLoggingSection.getSlider().getModel().setValue(dIndex); //TODO is this redundant?
			}
		});
	}

	protected int getShowRowsAmount() {
		return lstbxLoggingSection.getVisibleItems();
	}

	protected double getScrollDumpAreaFlindex() {
		return lstbxLoggingSection.getSlider().getModel().getMaximum()
			-lstbxLoggingSection.getSlider().getModel().getValue();
	}

	protected void navigateWord(boolean b) {
		LoggingI.i().logExceptionEntry(new UnsupportedOperationException("method not implemented yet"), null);
	}

	protected void clearInput() {
		tfInput.setText("");
	}

	protected void autoComplete() {
		AutoCompleteResult ar = JavaScriptI.i().showHelp(tfInput.getText());
		tfInput.setText(ar.getImprovedPart());
		scrollToBottom();
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
		
		// console stats
		lblStats = new Label("Initializing Console status.",getStyle());
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
	}
	
	private void setPopupHelp(Spatial spt, String strHelp){
		spt.setUserData(EUserDataMiscJme.strPopupHelp.s(), strHelp);
	}
	
	private class ButtonClick implements Command<Button>{
		@Override
		public void execute(Button source) {
			if(source.equals(btnClipboardShow)){
				showClipboard();
			}
		}
	}
	
	private void showClipboard() {
		LoggingI.i().logExceptionEntry(new UnsupportedOperationException("method not implemented yet"), null);
	}
	
	public static enum EAttribute{
		/** elemet text color */
		color, 
		
		fontSize,
		
		/** color */
		background,
		
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
		
		if(GuiGlobals.getInstance()==null)GuiGlobals.initialize(app);
		
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
		
		// INPUT
		attrs = styles.getSelector(TextField.ELEMENT_ID, getStyle());
		attrs.set(EAttribute.color.s(), new ColorRGBA(0.75f,0.75f,0.25f,1));
		clBg = new ColorRGBA(0.15f, 0, 0.15f, 1);
		attrs.set(TextField.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
		attrs = styles.getSelector(ListBox.ELEMENT_ID, ListBox.SELECTOR_ID, getStyle());
		clBg = MiscJmeI.i().colorChangeCopy(ColorRGBA.Yellow,0,0.25f);
		attrs.set(ListBox.LAYER_BACKGROUND, new QuadBackgroundComponent(clBg));
		
	}

	private void initMainContainer() {
		cntrMain = new Container(new BorderLayout(), getStyle());
		cntrMain.setPreferredSize(v3fConsoleSize);
		nodeParent.attachChild(cntrMain);
	}

	private void initLoggingSection() {
		lstbxLoggingSection = new ListBox<String>(null, getStyle());
		LoggingI.i().setModelAt(lstbxLoggingSection);
		lstbxLoggingSection.setVisibleItems(MiscLemurI.i().getEntryHeightPixels(lstbxLoggingSection));
		
		cntrMain.addChild(lstbxLoggingSection, BorderLayout.Position.Center);
	}

	private void initSize() {
		v3fApplicationWindowSize = new Vector3f(
			Display.getWidth(),
			Display.getHeight(),
			fLemurPreferredThickness );
			
		v3fConsoleSize = new Vector3f(
			v3fApplicationWindowSize.x,
			(v3fApplicationWindowSize.y * fConsoleHeightPerc),
			fLemurPreferredThickness);
	}

	public float getConsoleHeightPerc() {
		return fConsoleHeightPerc;
	}

	public void setConsoleHeightPerc(float fConsoleHeightPerc) {
		this.fConsoleHeightPerc = fConsoleHeightPerc;
		updateConsoleHeight();
	}

	private void updateConsoleHeight() {
		LoggingI.i().logExceptionEntry(new UnsupportedOperationException("method not implemented yet"), null);
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

	public void addCmdToHistory(String strJS) {
		// ignores equals to last cmd
		boolean b = astrCmdHistory.get(astrCmdHistory.size()-1).equals(strJS);
		if(!b)astrCmdHistory.add(strJS);
		
		// reset navigator index
		iNavigateCmdHistoryIndex=astrCmdHistory.size();
	}
	
	public ArrayList<String> getCmdHistory(){
		return astrCmdHistory;
	}
	
	public Integer getSelectedIndex(){
		return lstbxLoggingSection.getSelectionModel().getSelection();
	}

	public void setInputText(String str) {
		tfInput.getDocumentModel().insert(str);
	}
	
}
