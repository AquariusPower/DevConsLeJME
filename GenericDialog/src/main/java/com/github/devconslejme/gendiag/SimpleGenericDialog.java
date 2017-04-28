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

package com.github.devconslejme.gendiag;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.Set;

import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI;
import com.google.common.base.Function;
import com.google.common.base.Strings;
import com.jme3.input.KeyInput;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.core.VersionedList;
import com.simsilica.lemur.core.VersionedReference;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.DefaultCursorListener;
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;
import com.simsilica.lemur.list.DefaultCellRenderer;


/**
 * A text based generic dialog.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public final class SimpleGenericDialog extends AbstractGenericDialog {
	private Label	btnInfoText;
	private ListBox<OptionData>	lstbxOptions;
	private VersionedList<OptionData>	vlodOptions;
	private LinkedHashMap<String,OptionData> hmOptionsRoot = new LinkedHashMap<String,OptionData>();
	private TextField	tfInput;
	private boolean	bReturnJustTheInputTextValue;
	private boolean	bUserSubmitedInputValue;
	private KeyActionListener	kal;
	private boolean	bUpdateListItems = true;
	private static class SectionIndicator{}
	private SectionIndicator sectionIndicator = new SectionIndicator();
	private Function<OptionData, String>	trOptions;
	private boolean bToggleExpandedOnce=false;
	protected Command<? super Button>	cmdOption;
//	private VersionedReference<Double>	vrSlider;
//	private Command<? super Button>	cmdToggleExpand = new Command<Button>() {
//		@Override
//		public void execute(Button source) {
//			/**
//			 * the click will select it
//			 */
//			bToggleExpandedOnce=true;
//		}
//	};
//	private CursorListener	clToggleExpand = new DefaultCursorListener(){
//		@Override
//		protected void click(CursorButtonEvent event, Spatial target, Spatial capture) {
//			bToggleExpandedOnce=true;
//		}
//	};
	private DefaultCellRenderer<OptionData>	crOptions;
	private VersionedReference<Set<Integer>>	vrSelection;
	protected boolean	bUpdateOptionSelected;
	private boolean	bCloseOnChoiceMade=true;
	private Container	cntrInfo;
	private String	strName;
	private String	strTitle;
	private Button	btnTitleText;
	private Container	cntrTitle;
	private Button	btnMinimize;
	private Button	btnMaximizeRestore;
	private Button	btnClose;
	private ArrayList<Button>	abtnInfoSection;
	private Command<? super Button>	cmdInfoSectionButtons;
	private Container	cntrDiagControls;
//	private Button	btnRestoreIniSize;
//	private Button	btnUpdateDefaultSize;
	private int	iDiagControlColumnInitIndex;
	protected String	strUDKeyPosBeforeMaximize = SimpleGenericDialog.class+"/PosBeforeMaximize";
	protected boolean	bKeepMaximized;
	
	public static class OptionData{
		private String strTextKey;
		private OptionData odParent;
		private Object objValue;
		private boolean bExpanded=true;
		private LinkedHashMap<String,OptionData> hmOptions = new LinkedHashMap<String,OptionData>();
		
		public void toggleExpanded(){
			bExpanded=!bExpanded;
		}
		private void setTextKey(String strTextKey) {
			this.strTextKey = strTextKey;
		}
		private void setSectionParent(OptionData odParent) {
			this.odParent = odParent;
		}
		private void setValue(Object objValue) {
			this.objValue = objValue;
		}
		public String getTextKey() {
			return strTextKey;
		}
		public OptionData getSectionParent() {
			return odParent;
		}
		public Object getValue() {
			return objValue;
		}
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("OptionData [strTextKey=");
			builder.append(strTextKey);
			builder.append(", odParent=");
			builder.append(odParent==null?null:odParent.getTextKey()); //!!!!!!!!!!!!!!! CUSTOM !!!!!!!!!!!!!!
			builder.append(", objValue=");
			builder.append(objValue);
			builder.append(", bExpanded=");
			builder.append(bExpanded);
			builder.append(", hmOptions=");
			builder.append(hmOptions);
			builder.append("]");
			return builder.toString();
		}
		public boolean isExpanded() {
			return bExpanded;
		}
		private void setExpanded(boolean bExpanded) {
			this.bExpanded = bExpanded;
		}
		public String getVisibleText() {
			int iDepth=0;
			OptionData odParent=this;while((odParent=odParent.getSectionParent())!=null)iDepth++;
			
			String str=strTextKey;
			
			if(getValue() instanceof SectionIndicator){
				str="["+(isExpanded()?"-":"+")+"] "+str+(!isExpanded()?" {"+hmOptions.size()+"}":"");
			}
			
			str=" "+str;
			str=Strings.padStart(str, str.length()+iDepth, '>');
			
			return str;
		}
	}
	
	public SimpleGenericDialog(ResizablePanel rzpOwner) {
		super(rzpOwner);
//		configureDefaults();
	}

	public SimpleGenericDialog() {
		this(DialogHierarchyStateI.i().createDialog(SimpleGenericDialog.class.getSimpleName(), null));
	}
	
	private void buttonClicked(Button btn){
		bUpdateOptionSelected=true;
		OptionData od = UserDataI.i().getUserDataPSH(btn, OptionData.class);
		lstbxOptions.getSelectionModel().setSelection(vlodOptions.indexOf(od));
//		updateOptionSelected();
////		tfInput.setText(getSelectedOptionVisibleText());
//		OptionData od = getSelectedOptionData();
//		if(SectionIndicator.class.isInstance(od.getValue())){
//			bToggleExpandedOnce=true;
//		}else{
//			tfInput.setText(od.getTextKey());
//		}
	}
	
	private Button createInfoButton(String strText,String strHintPopup){
		Button btn = new Button(strText,getDialog().getStyle());
		abtnInfoSection.add(btn);
		if(strHintPopup!=null)PopupHintHelpListenerI.i().setPopupHelp(btn,strHintPopup);
//		btn.setPreferredSize(new Vector3f(1,1,0.1f));
//		btn.setSize(new Vector3f(1,1,0.1f));
		return btn;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initPreContentsContainer() {
		ESection es;
		
		es=ESection.Info;
		if(getSection(es)==null){
			abtnInfoSection = new ArrayList<Button>();
			
			cmdInfoSectionButtons = new Command<Button>() {
				@Override
				public void execute(Button source) {
					if(source==btnMaximizeRestore){ //toggle
						if(MiscLemurI.i().isMaximized(getDialog())){
							/**
							 * restore
							 */
							getDialog().restoreDefaultSafeSize();
							
							Vector3f v3fPosBM = (Vector3f)getDialog().getUserData(strUDKeyPosBeforeMaximize);
							v3fPosBM.z=getDialog().getLocalTranslation().z; //do not mess with z!!!
							getDialog().setLocalTranslation(v3fPosBM);
							
							bKeepMaximized=false;
						}else{
							/**
							 * maximize
							 */
							getDialog().setUserData(strUDKeyPosBeforeMaximize,getDialog().getLocalTranslation());
//							MiscLemurI.i().maximize(getDialog());
							
							bKeepMaximized=true;
						}
					}else
//					if(source==btnRestoreIniSize){
//						getDialog().restoreDefaultSafeSize();
//					}else
//					if(source==btnUpdateDefaultSize){
//						getDialog().applyCurrentSafeSizeAsDefault();
//					}else
					if(source==btnClose){
						getDialog().close();
					}else
					{
						MessagesI.i().warnMsg(SimpleGenericDialog.this, "cmd not supported yet", source);
					}
				}
			};
			
			strTitle="(no title)";
			
			// title row
			cntrDiagControls = new Container();
			iDiagControlColumnInitIndex=0;
//			cntrDiagControls.addChild(btnRestoreIniSize = createInfoButton("r","Restore to default/initial size"), iDiagControlColumnInitIndex++);
//			cntrDiagControls.addChild(btnUpdateDefaultSize = createInfoButton("u","Update default size to current"), iDiagControlColumnInitIndex++);
//			cntrDiagControls.addChild(btnMinimize = createInfoButton("-","Minimize"), iDiagControlColumnInitIndex++);
//			cntrDiagControls.addChild(btnMaximizeRestore = createInfoButton("M","Maximize/Restore"), iDiagControlColumnInitIndex++);
//			cntrDiagControls.addChild(btnClose = createInfoButton("X","Close"), iDiagControlColumnInitIndex++);
//			btnRestoreIniSize=appendNewDiagControl("r","Restore to default/initial size");
//			btnUpdateDefaultSize=appendNewDiagControl("u","Update default size to current");
			btnMinimize=appendNewDiagControl("-","Minimize");
			btnMaximizeRestore=appendNewDiagControl("M","Maximize/Restore");
			btnClose=appendNewDiagControl("X","Close");
			MiscLemurI.i().changeBackgroundColor(btnClose, ColorI.i().colorChangeCopy(ColorRGBA.Red,0f,0.25f), true); //TODO use a lemur style instead
			
			// title row put it all
			cntrTitle = new Container(new BorderLayout());
			
			ContextMenu cm = new ContextMenu(getDialog());
			cm.addNewEntry("Restore to default/initial size", new Command<Button>() {@Override public void execute(Button source) {
				getDialog().restoreDefaultSafeSize();}});
			cm.addNewEntry("Update default size to current", new Command<Button>() {@Override public void execute(Button source) {
				getDialog().applyCurrentSafeSizeAsDefault();}});
			
			btnTitleText = createInfoButton(strTitle,null);
			MiscLemurI.i().changeBackgroundColor(btnTitleText, ColorI.i().colorChangeCopy(ColorRGBA.Blue,0f,0.25f), true); //TODO use a lemur style instead
			DragParentestPanelListenerI.i().applyAt(btnTitleText);
			ContextMenuI.i().attachContextMenuAt(btnTitleText, cm);
			
//			cntrTitle.setPreferredSize(new Vector3f(1,1,0.1f));
			cntrTitle.addChild(btnTitleText, BorderLayout.Position.Center);
			cntrTitle.addChild(cntrDiagControls, BorderLayout.Position.East);
			
			// text info row
			/**
			 * IMPORTANT: Button works MUCH better than Label when clicking to drag for ex.
			 * as the label will require aiming at the label's text...
			 */
			btnInfoText = createInfoButton("(No Info)",null);
			
			// cfg all buttons
			for(Button btn:abtnInfoSection){
				btn.addClickCommands(cmdInfoSectionButtons);
			}
			
			// info section
			cntrInfo = new Container(new BorderLayout());
			cntrInfo.addChild(cntrTitle, BorderLayout.Position.North);
			cntrInfo.addChild(btnInfoText, BorderLayout.Position.Center);
			
			setSection(es,cntrInfo);
		}
		
		es=ESection.Options;
		if(getSection(es)==null){
			trOptions = new Function<SimpleGenericDialog.OptionData, String>() {
				@Override
				public String apply(OptionData input) {
					return input.getVisibleText();
				}
			};
			
			vlodOptions = new VersionedList<OptionData>();
			lstbxOptions = new ListBox<OptionData>(vlodOptions, getDialog().getStyle());
			
			cmdOption = new Command<Button>() {
				@Override
				public void execute(Button source) {
					buttonClicked(source);
				}
			};
			
			/**
			 * FIXME: BUGFIX
			 * for some reason, some of the buttons on the listbox will not work with the
			 * Button.addClickCommands(), to workaround that, I am using together the 
			 * CursorListener.
			 */
			DefaultCursorListener clOption = new DefaultCursorListener(){
				@Override
				protected void click(CursorButtonEvent event, Spatial target, Spatial capture) {
					buttonClicked((Button)capture);
				};
			};
			
			crOptions = new DefaultCellRenderer<OptionData>(getDialog().getStyle()){
				@Override
				public Panel getView(OptionData value, boolean selected, Panel existing) {
					Button btn = (Button)super.getView(value, selected, existing);
					btn.addClickCommands(cmdOption);
					UserDataI.i().setUserDataPSH(btn, value);
					CursorEventControl.addListenersToSpatial(btn, clOption);
					return btn;
				}
			};			
			crOptions.setTransform(trOptions);
			lstbxOptions.setCellRenderer(crOptions);
			
			vrSelection = lstbxOptions.getSelectionModel().createReference();
			
//			((DefaultCellRenderer<OptionData>)lstbxOptions.getCellRenderer()).setTransform(trOptions);
//			lstbxOptions.addClickCommands(new Command<ListBox>(){
//				@Override
//				public void execute(ListBox source) {
//					bToggleExpandedOnce=true;
////					tfInput.setText(getSelectedOptionVisibleText());
//					OptionData od = getSelectedOptionData();
//					if(!SectionIndicator.class.isInstance(od.getValue())){
//						tfInput.setText(od.getTextKey());
//					}
//				}
//			});
			setSection(es,lstbxOptions);
			lstbxOptions.setVisibleItems(10); //TODO make automatic
//			getSection(es).setMinSize(new Vector3f(100,getEntryHeight(),0));
			
//			vrSlider = lstbxOptions.getSlider().getModel().createReference();
			
//			applyListenerToListBoxItems();
		}
		
		es=ESection.Input;
		if(getSection(es)==null){
			kal = new KeyActionListener() {
				@Override
				public void keyAction(TextEntryComponent source, KeyAction key) {
					switch(key.getKeyCode()){
						case KeyInput.KEY_RETURN:
						case KeyInput.KEY_NUMPADENTER:
							bUserSubmitedInputValue=true;
							break;
					}
				}
			};
			
			tfInput = new TextField("", getDialog().getStyle());
			
			tfInput.getActionMap().put(new KeyAction(KeyInput.KEY_NUMPADENTER),kal); 
			tfInput.getActionMap().put(new KeyAction(KeyInput.KEY_RETURN),kal);
			//tfInput.getActionMap().entrySet()
			
			setSection(es,tfInput);
		}
		
	}
	
//	public int getEntryHeight(){
//		boolean bWasEmpty=vlsOptions.isEmpty();
//		if(bWasEmpty)vlsOptions.add("W");
//		int i = MiscLemurI.i().getEntryHeightPixels(lstbxOptions);
//		if(bWasEmpty)vlsOptions.clear();
//		return i;
//	}
	
	private Button appendNewDiagControl(String strText, String strHint) {
		Button btn = createInfoButton(strText,strHint);
		MiscJmeI.i().addToName(btn, strText, true);
		cntrDiagControls.addChild(btn, iDiagControlColumnInitIndex++);
		return btn;
	}

	public void setTextInfo(String strInfo){
		btnInfoText.setText(strInfo);
	}
	
	public OptionData putSection(OptionData odParent, String strNewSectionKey){
		OptionData od = new OptionData();
		od.setSectionParent(odParent);
		od.setTextKey(strNewSectionKey);
		od.setValue(sectionIndicator);
		
		put(odParent,strNewSectionKey,od);
//		hmOptionsRoot.put(strNewSectionKey, od);
		
		return od;
	}
	
	/**
	 * 
	 * @param strSectionParentKey if null, will be root/topmost on the hierarchy
	 * @param strTextOptionKey also is the displayed unique text per section
	 * @param objValue
	 */
	public void putOption(OptionData odParent, String strTextOptionKey, Object objValue){
		OptionData od = new OptionData();
		od.setSectionParent(odParent);
		od.setTextKey(strTextOptionKey);
		od.setValue(objValue);
		
		put(odParent,strTextOptionKey,od);
	}
	
	private void put(OptionData odParent, String strTextOptionKey, OptionData od){
		HashMap<String,OptionData> hmOpt = hmOptionsRoot;
		if(odParent!=null){
//			OptionData odParent = findSectionRecursively(hmOpt, strSectionParentKey);
//			DetailedException.assertNotNull(odParent, "the parent section must be set before being referenced/requested/used!", odParent);
			hmOpt=odParent.hmOptions;
		}
		
		OptionData odPrevious = hmOpt.put(strTextOptionKey, od);
		if(odPrevious!=null)MessagesI.i().warnMsg(this, "option was already set", odPrevious.toString(), od.toString());
	}
	
	private OptionData findSectionRecursively(HashMap<String,OptionData> hmOpt, String strSectionKey){
		OptionData odFound= hmOpt.get(strSectionKey);
		if(odFound!=null)return odFound;
		
		//look for sub-sections
		for(OptionData od:hmOpt.values()){
			if(od.getValue() instanceof SectionIndicator){
				odFound = findSectionRecursively(od.hmOptions,strSectionKey);
				if(odFound!=null)return odFound;
			}
		}
		
		return null;
	}
	
	private void recreateListItems(){
		vlodOptions.clear();
		recreateListItemsRecursively(hmOptionsRoot,0);
	}
	
	@SuppressWarnings("unchecked")
	private void recreateListItemsRecursively(HashMap<String, OptionData> hmOpt, int iDepth){
		for(Entry<String, OptionData> entry:hmOpt.entrySet()){
			OptionData od = entry.getValue();
//			String strTextKey = od.getTextKey();
//			vlsOptions.remove(strTextKey); //to be like replace
//			strTextKey=Strings.padStart(" "+strTextKey, strTextKey.length()+iDepth, '>');
			vlodOptions.add(od);
			if(od.getValue() instanceof SectionIndicator){
				if(od.isExpanded()){
					recreateListItemsRecursively(od.hmOptions,++iDepth);
				}
			}
		}
		
//		for(Panel pnl:MiscLemurI.i().getAllListBoxItems(lstbxOptions)){
//			Button btn=(Button)pnl;
//			CursorEventControl.addListenersToSpatial(btn, clToggleExpand);
////			btn.addClickCommands(cmdToggleExpand);
////			UserDataI.i().setUserDataPSH(btn, obj)
////			btn.
//		}
	}
	
	public Integer getSelectedOptionIndex(){
		return lstbxOptions.getSelectionModel().getSelection();
	}
	
	public OptionData getSelectedOptionData(){
		return vlodOptions.get(getSelectedOptionIndex());
	}
	
	public String getSelectedOptionVisibleText(){
		return vlodOptions.get(getSelectedOptionIndex()).getVisibleText();
	}
	
	public Object getSelectedOptionValue(){
		Object obj = vlodOptions.get(getSelectedOptionIndex()).getValue();
		if (obj instanceof SectionIndicator)return null;
		return obj;
//		return hmOptionsRoot.get(getSelectedOptionText()).getValue();
	}
	
//	private void applyListenerToListBoxItems(){
//		for(Panel pnl:MiscLemurI.i().getAllListBoxItems(lstbxOptions)){
//			Button btn=(Button)pnl;
//			CursorEventControl.addListenersToSpatial(btn, clToggleExpand);
//		}
//	}
	
	public void update(float tpf) {
		if(bKeepMaximized){
			MiscLemurI.i().maximize(getDialog());
		}
		
		if(bToggleExpandedOnce){
			getSelectedOptionData().toggleExpanded();
			
//			applyListenerToListBoxItems();
			
			bUpdateListItems = true;
			
			bToggleExpandedOnce=false;
		}
		
		if(bUpdateListItems){
			recreateListItems();
			bUpdateListItems=false;
		}
		
		/**
		 * the selection seems to only change after a mouse cursor click
		 */
//		if(vrSelection.update() || bUpdateOptionSelected){
		if(bUpdateOptionSelected){
			updateOptionSelected();
			bUpdateOptionSelected=false;
		}
		
		if(bReturnJustTheInputTextValue){
			if(bUserSubmitedInputValue){
				setChosenValue(getInputText());
				bUserSubmitedInputValue=false;
			}
		}else{ // set as soon an option is selected
			Integer i=getSelectedOptionIndex();
			if(i!=null){
				setChosenValue(getSelectedOptionValue());
			}
		}
		
		if(isOptionSelected()){
			if(bCloseOnChoiceMade){
				getDialog().close();
			}
		}
	}
	
	private void updateOptionSelected() {
		OptionData od = getSelectedOptionData();
		if(SectionIndicator.class.isInstance(od.getValue())){
			bToggleExpandedOnce=true;
		}else{
			tfInput.setText(od.getTextKey());
		}
	}

	@Override
	public Object extractSelectedOption() {
		lstbxOptions.getSelectionModel().setSelection(-1);
		return super.extractSelectedOption();
	}
	
	public String getInputText(){
		return tfInput.getText();
	}
	
	/**
	 * options text will be used to fill the input text and be returned as the value instead of the custom objects
	 * @param b
	 */
	public void setReturnJustTheInputTextValue(boolean b){
		this.bReturnJustTheInputTextValue=b;
	}
	
	public boolean isUseInputTextValue(){
		return bReturnJustTheInputTextValue;
	}
	
	@Override
	public void resizerUpdatedLogicalStateEvent(float tpf,ResizablePanel rzp) {
		update(tpf);
	}

	public void setCloseOnChoiceMade(boolean bCloseOnSelect) {
		this.bCloseOnChoiceMade=bCloseOnSelect;
	}

}
