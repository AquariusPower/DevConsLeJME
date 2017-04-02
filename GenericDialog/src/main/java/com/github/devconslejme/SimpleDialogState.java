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

import java.util.HashMap;

import com.github.devconslejme.misc.MiscLemurI;
import com.jme3.app.Application;
import com.jme3.app.state.AppStateManager;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.core.VersionedList;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SimpleDialogState extends GenericDialogState {
	private Label	lblInfo;
	
	private ListBox<String>	lstbxOptions;
	private VersionedList<String>	vlsOptions;
	HashMap<String,Object> hmOptions = new HashMap<String, Object>();
	
	private TextField	tfInput;

	public SimpleDialogState(Application app) {
		super(app);
	}
	
//	@Override
//	public void initialize(AppStateManager stateManager, Application app) {
//		super.initialize(stateManager, app);
//		
//		GuiGlobals.getInstance().requestFocus(tfInput);
//	}
	
	@Override
	public void configure(CfgParams cfg) {
		super.configure(cfg);
		
		configureDefaults();
	}

	private void configureDefaults() {
		ESection es;
		
		es=ESection.Info;
		if(getSection(es)==null){
			lblInfo = new Label("(No Info)", getCfg().getStyle());
			getCfg().setSection(es,lblInfo);
		}
		
		es=ESection.Options;
		if(getSection(es)==null){
			vlsOptions = new VersionedList<String>();
			lstbxOptions = new ListBox<String>(vlsOptions, getCfg().getStyle());
			getCfg().setSection(es,lstbxOptions);
//			getSection(es).setMinSize(new Vector3f(100,getEntryHeight(),0));
		}
		
		es=ESection.Input;
		if(getSection(es)==null){
			tfInput = new TextField("", getCfg().getStyle());
			getCfg().setSection(es,tfInput);
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
		lblInfo.setText(strInfo);
	}
	
	public void putOption(String strTextKey, Object objValue){
		hmOptions.put(strTextKey, objValue);
		vlsOptions.remove(strTextKey);
		vlsOptions.add(strTextKey);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		Integer i=lstbxOptions.getSelectionModel().getSelection();
		if(i!=null){
			setSelectedOptionValue(hmOptions.get(vlsOptions.get(i)));
			setEnabled(false);
		}
	}
	
	@Override
	public Object collectSelectedOption() {
		lstbxOptions.getSelectionModel().setSelection(-1);
		return super.collectSelectedOption();
	}
}
