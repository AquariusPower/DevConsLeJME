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

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.google.common.base.Function;
import com.google.common.base.Strings;
import com.jme3.input.KeyInput;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.core.VersionedList;
import com.simsilica.lemur.core.VersionedReference;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
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
	private Label	btnInfo;
	private ListBox<OptionData>	lstbxOptions;
	private VersionedList<OptionData>	vlsOptions;
	private LinkedHashMap<String,OptionData> hmOptionsRoot = new LinkedHashMap<String,OptionData>();
	private TextField	tfInput;
	private boolean	bUseInputTextValue;
	private boolean	bUserSubmitInputValue;
	private KeyActionListener	kal;
	private boolean	bUpdateListItems = true;
	private static class SectionIndicator{}
	private SectionIndicator sectionIndicator = new SectionIndicator();
	private Function<OptionData, String>	transform;
	private boolean bToggleExpandedOnce=false;
	private VersionedReference<Double>	vrSlider;
//	private Command<? super Button>	cmdToggleExpand = new Command<Button>() {
//		@Override
//		public void execute(Button source) {
//			/**
//			 * the click will select it
//			 */
//			bToggleExpandedOnce=true;
//		}
//	};
	private CursorListener	clToggleExpand = new DefaultCursorListener(){
		@Override
		protected void click(CursorButtonEvent event, Spatial target, Spatial capture) {
			bToggleExpandedOnce=true;
		}
	};
	
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
				str="["+(isExpanded()?"-":"+")+"] "+str+"["+hmOptions.size()+"]";
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

	@SuppressWarnings("unchecked")
	@Override
	protected void preInitContentsContainer() {
		ESection es;
		
		es=ESection.Info;
		if(getSection(es)==null){
			/**
			 * IMPORTANT: Button works MUCH better than Label when clicking to drag for ex.
			 */
			btnInfo = new Button("(No Info)", getDialog().getStyle());
			setSection(es,btnInfo);
		}
		
		es=ESection.Options;
		if(getSection(es)==null){
			transform = new Function<SimpleGenericDialog.OptionData, String>() {
				@Override
				public String apply(OptionData input) {
					return input.getVisibleText();
				}
			};
			
			vlsOptions = new VersionedList<OptionData>();
			lstbxOptions = new ListBox<OptionData>(vlsOptions, getDialog().getStyle());
			((DefaultCellRenderer<OptionData>)lstbxOptions.getCellRenderer()).setTransform(transform);
			lstbxOptions.addClickCommands(new Command<ListBox>(){
				@Override
				public void execute(ListBox source) {
					tfInput.setText(getSelectedOptionVisibleText());
				}
			});
			setSection(es,lstbxOptions);
//			getSection(es).setMinSize(new Vector3f(100,getEntryHeight(),0));
			
			vrSlider = lstbxOptions.getSlider().getModel().createReference();
			
			applyListenerToListBoxItems();
		}
		
		es=ESection.Input;
		if(getSection(es)==null){
			kal = new KeyActionListener() {
				@Override
				public void keyAction(TextEntryComponent source, KeyAction key) {
					switch(key.getKeyCode()){
						case KeyInput.KEY_RETURN:
						case KeyInput.KEY_NUMPADENTER:
							bUserSubmitInputValue=true;
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
	
	public void setTextInfo(String strInfo){
		btnInfo.setText(strInfo);
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
		vlsOptions.clear();
		recreateListItemsRecursively(hmOptionsRoot,0);
	}
	
	@SuppressWarnings("unchecked")
	private void recreateListItemsRecursively(HashMap<String, OptionData> hmOpt, int iDepth){
		for(Entry<String, OptionData> entry:hmOpt.entrySet()){
			OptionData od = entry.getValue();
//			String strTextKey = od.getTextKey();
//			vlsOptions.remove(strTextKey); //to be like replace
//			strTextKey=Strings.padStart(" "+strTextKey, strTextKey.length()+iDepth, '>');
			vlsOptions.add(od);
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
		return vlsOptions.get(getSelectedOptionIndex());
	}
	
	public String getSelectedOptionVisibleText(){
		return vlsOptions.get(getSelectedOptionIndex()).getVisibleText();
	}
	
	public Object getSelectedOptionValue(){
		Object obj = vlsOptions.get(getSelectedOptionIndex()).getValue();
		if (obj instanceof SectionIndicator)return null;
		return obj;
//		return hmOptionsRoot.get(getSelectedOptionText()).getValue();
	}
	
	private void applyListenerToListBoxItems(){
		for(Panel pnl:MiscLemurI.i().getAllListBoxItems(lstbxOptions)){
			Button btn=(Button)pnl;
			CursorEventControl.addListenersToSpatial(btn, clToggleExpand);
		}
	}
	
	public void update(float tpf) {
		if(bToggleExpandedOnce){
			getSelectedOptionData().toggleExpanded();
			
			applyListenerToListBoxItems();
			
			bUpdateListItems = true;
			
			bToggleExpandedOnce=false;
		}
		
		if(bUpdateListItems){
			recreateListItems();
			bUpdateListItems=false;
		}
		
		if(vrSlider.update()){
			applyListenerToListBoxItems();
		}
		
		if(bUseInputTextValue){
//			GuiGlobals.getInstance().requestFocus(tfInput);
//			GuiGlobals.getInstance().requestFocus(getDialog()); //will traverse to the text input
			if(bUserSubmitInputValue){
				setSelectedOptionValue(getInputText());
				bUserSubmitInputValue=false;
			}
		}else{ // set as soon an option is selected
			Integer i=getSelectedOptionIndex();
			if(i!=null){
				setSelectedOptionValue(getSelectedOptionValue());
			}
		}
		
		if(isOptionSelected()){
			getDialog().close();
		}
	}
	
//	private boolean updateUserSubmitInputValue() {
//		return false;
//	}

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
	public void setUseInputTextValue(boolean b){
		this.bUseInputTextValue=b;
	}
	
	public boolean isUseInputTextValue(){
		return bUseInputTextValue;
	}
	
	@Override
	public void resizerUpdatedLogicalStateEvent(float tpf,ResizablePanel rzp) {
		update(tpf);
	}

}
