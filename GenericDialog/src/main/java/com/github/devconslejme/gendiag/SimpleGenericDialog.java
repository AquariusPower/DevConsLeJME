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

import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.jme3.app.Application;
import com.jme3.input.KeyInput;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.core.VersionedList;
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;


/**
 * A text based generic dialog.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public final class SimpleGenericDialog extends AbstractGenericDialog {
	public SimpleGenericDialog(ResizablePanel rzpOwner) {
		super(rzpOwner);
//		configureDefaults();
	}

	public SimpleGenericDialog() {
		this(DialogHierarchyStateI.i().createDialog(SimpleGenericDialog.class.getSimpleName(), null));
	}

	private Label	btnInfo;
	
	private ListBox<String>	lstbxOptions;
	private VersionedList<String>	vlsOptions;
	HashMap<String,Object> hmOptions = new HashMap<String, Object>();
	
	private TextField	tfInput;

	private boolean	bUseInputTextValue;

	private boolean	bUserSubmitInputValue;

	private KeyActionListener	kal;

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
			vlsOptions = new VersionedList<String>();
			lstbxOptions = new ListBox<String>(vlsOptions, getDialog().getStyle());
			lstbxOptions.addClickCommands(new Command<ListBox>(){
				@Override
				public void execute(ListBox source) {
					tfInput.setText(getSelectedOptionText());
				}
			});
			setSection(es,lstbxOptions);
//			getSection(es).setMinSize(new Vector3f(100,getEntryHeight(),0));
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
	
	public int getEntryHeight(){
		boolean bWasEmpty=vlsOptions.isEmpty();
		if(bWasEmpty)vlsOptions.add("W");
		int i = MiscLemurI.i().getEntryHeightPixels(lstbxOptions);
		if(bWasEmpty)vlsOptions.clear();
		return i;
	}
	
	public void setTextInfo(String strInfo){
		btnInfo.setText(strInfo);
	}
	
	public void putOption(String strTextKey, Object objValue){
		hmOptions.put(strTextKey, objValue);
		vlsOptions.remove(strTextKey);
		vlsOptions.add(strTextKey);
	}
	
	public Integer getSelectedOptionIndex(){
		return lstbxOptions.getSelectionModel().getSelection();
	}
	
	public String getSelectedOptionText(){
		return vlsOptions.get(getSelectedOptionIndex());
	}
	
	public Object getSelectedOptionValue(){
		return hmOptions.get(getSelectedOptionText());
	}
	
	public void update(float tpf) {
		if(bUseInputTextValue){
			GuiGlobals.getInstance().requestFocus(tfInput);
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
//			setEnabled(false);
			getDialog().removeFromParent();
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
