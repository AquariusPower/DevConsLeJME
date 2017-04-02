package com.github.devconslejme;

import com.jme3.app.Application;
import com.jme3.app.state.AppStateManager;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.TextField;

public class SimpleDialogState extends GenericDialogState {
	private float	iRowHeight=30; //TODO use dynamic value

	public SimpleDialogState(Application app) {
		super(app);
	}
	
	@Override
	public void initialize(AppStateManager stateManager, Application app) {
		super.initialize(stateManager, app);
	}
	
	@Override
	public void configure(CfgParams cfg) {
		super.configure(cfg);
		
		ESection es;
		
		es=ESection.Info;
		if(getSection(es)==null){
			Label lblInfo = new Label("(No Info)",cfg.getStyle());
			cfg.setSection(es,lblInfo);
		}
		
		es=ESection.Options;
		if(getSection(es)==null){
			ListBox lstbx = new ListBox();
			cfg.setSection(es,lstbx);
			getSection(es).setMinSize(new Vector3f(100,iRowHeight,0));
		}
		
		es=ESection.Input;
		if(getSection(es)==null){
			TextField tf = new TextField("",cfg.getStyle());
			cfg.setSection(es,tf);
		}
		
	}
}
