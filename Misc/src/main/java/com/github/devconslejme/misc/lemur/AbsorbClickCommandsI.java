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
package com.github.devconslejme.misc.lemur;

import java.util.ArrayList;
import java.util.List;

import com.github.devconslejme.gendiag.DialogHierarchyStateI;
import com.github.devconslejme.gendiag.DialogHierarchyStateI.Visuals;
import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.GlobalManagerI.G;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.jme3.app.SimpleApplication;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorMotionEvent;

/**
 * So the addClickCommands() can still be used,  but will just be handled here instead.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class AbsorbClickCommandsI extends CursorListenerX{
	public static AbsorbClickCommandsI i(){return GlobalManagerI.i().get(AbsorbClickCommandsI.class);}
	
	private boolean	bDelegateClickCommands=true;
	
	/**
	 * In case Button click commands are not working when the button has focus.
	 * @param capture
	 */
	@Workaround
	@Bugfix
	private int delegateClickCommands(Spatial capture) {
		if(!isDelegateClickCommands())return 0;
		
//		if(focusman.getFocus()!=capture)return; //the ignored click commands bug happens only when the Button has focus
		
		int iExecutedClickCmds=0;
		if (capture instanceof Button) {
			Button btn = (Button) capture;
//			ArrayList<Command<? super Button>> clickCommandsStored =
//				UserDataI.i().retrieve(btn, EUDClickCmds.ClickCommands.getUId(), new Function<Void, ArrayList<Command<? super Button>>>() {@Override	public ArrayList<Command<? super Button>> apply(Void input) {return new ArrayList<Command<? super Button>>();}});
//			ClickCommandsStore ccs = UserDataI.i().getUserDataPSH(btn, ClickCommandsStore.class);
//			ArrayList<Command<? super Button>> clickCommandsStored = 
//				UserDataI.i().getUserDataPSH(btn, EUDClickCmds.ClickCommands.getUId());
//				List<Command<? super Button>> clickCommands = btn.getClickCommands();
//				if(clickCommands!=null){
//					for(Command<? super Button> a:clickCommands){
			ClickCommandsStore ccs = UserDataI.i().retrieve(btn, ClickCommandsStore.class, false);
			if(ccs!=null){
//			if(UserDataI.i().isUserDataSet(btn, ClickCommandsStore.class)){
				for(Command<? super Button> a:ccs.clickCommandsStored){
					a.execute(btn);
					iExecutedClickCmds++;
				}
			}
		}
		
		return iExecutedClickCmds;
	}
	
	@Workaround
	@Bugfix
	@SuppressWarnings("unchecked")
	public void absorbClickCommands(Spatial capture) {
		if(!isDelegateClickCommands())return;
		
		if (capture instanceof Button) {
			Button btn = (Button) capture;
			List<Command<? super Button>> clickCommands = btn.getClickCommands();
			if(clickCommands==null)return; 
			
			if(clickCommands.size()>0){
				clickCommands = new ArrayList<Command<? super Button>>(clickCommands); //copy b4 clearing
				btn.removeClickCommands(clickCommands.toArray(new Command[0]));
				
//				ArrayList<Command<? super Button>> clickCommandsStored =
//						UserDataI.i().retrieve(btn, EUDClickCmds.ClickCommands.getUId(), new Function<Void, ArrayList<Command<? super Button>>>() {@Override	public ArrayList<Command<? super Button>> apply(Void input) {return new ArrayList<Command<? super Button>>();}});
				ClickCommandsStore ccs = UserDataI.i().retrieve(btn, ClickCommandsStore.class, true);
				if(ccs.clickCommandsStored.size()==0)CursorEventControl.addListenersToSpatial(btn, this); //1st time
//				ArrayList<Command<? super Button>> clickCommandsStored = 
//					UserDataI.i().getUserDataPSH(btn, EUDClickCmds.ClickCommands.getUId());
//				if(clickCommandsStored==null){
//					clickCommandsStored=new ArrayList<Command<? super Button>>();
//					UserDataI.i().setUserDataPSHSafely(btn, EUDClickCmds.ClickCommands.getUId(), clickCommandsStored);
//					CursorEventControl.addListenersToSpatial(btn, this);
//				}
				ccs.clickCommandsStored.addAll(clickCommands);
			}
			
		}
	}
	
	public static class ClickCommandsStore{
		ArrayList<Command<? super Button>> clickCommandsStored =
			new ArrayList<Command<? super Button>>();
	}

//	private static enum EUDClickCmds implements IUDKey{
//		ClickCommands,
//		;
//		public String s(){return toString();}
//
//		@Override
//		public Class getType() {
//			return null;
//		}
//
//		@Override
//		public String getUId() {
//			return JavaLangI.i().enumUId(this);
//		}
//	}
	
	@Bugfix
	public boolean isDelegateClickCommands() {
		return bDelegateClickCommands;
	}
	
	@Bugfix
	public void setDelegateClickCommandsDisabled() {
		this.bDelegateClickCommands = false;
	}

//	@Override
//	public void cursorButtonEvent(CursorButtonEvent event, Spatial target,			Spatial capture) {
//		Visuals vs = DialogHierarchyStateI.i().getVisuals(SpatialHierarchyI.i().getParentest(
//			capture, ResizablePanel.class, true));
//		if(vs!=null)DialogHierarchyStateI.i().setFocusRecursively(vs.getEntityId()); //TODO useless?
//		
//		if(event.isConsumed())return; // ex.: by drag parentest listener
//		if(event.getButtonIndex()!=0)return; //left mouse button
//		if(!event.isPressed()){ //button up
//			if(delegateClickCommands(capture)>0)event.setConsumed();
//		}
//	}
	@Override
	protected boolean click(CursorButtonEvent event, Spatial target, Spatial capture) {
		if(event.isConsumed())return false; // ex.: by drag parentest listener
		if(event.getButtonIndex()!=0)return false; //left mouse button
//		MiscLemurI.i().clickGlobalListeners(event,target,capture);
		if(delegateClickCommands(capture)>0){
			event.setConsumed();
			return true;
		}
		return false;
	}
	

//	@Override
//	public void cursorEntered(CursorMotionEvent event, Spatial target,			Spatial capture) {
//	}
//
//	@Override
//	public void cursorExited(CursorMotionEvent event, Spatial target,			Spatial capture) {
//	}
//
//	@Override
//	public void cursorMoved(CursorMotionEvent event, Spatial target,			Spatial capture) {
//	}
	
	public void addClickCommands(Button btn, Command<? super Button>... acmd) {
		btn.addClickCommands(acmd);
		absorbClickCommands(btn);
	}
	
	public ArrayList<Button> containsClickCommandsRecursively(Node nodeParentest, boolean bAbsorb){
		if(nodeParentest==null){
			SimpleApplication sapp = G.i(SimpleApplication.class);
			if(sapp!=null)nodeParentest=sapp.getGuiNode();
		}
		
		ArrayList<Button> abtn = new ArrayList<Button>();
		for(Button btn:SpatialHierarchyI.i().getAllChildrenOfTypeRecursiveFrom(nodeParentest, Button.class, null)){
			if(btn.getClickCommands()!=null && btn.getClickCommands().size()>0){
				abtn.add(btn);
				if(bAbsorb){
					absorbClickCommands(btn);
				}
			}
		}
		
		return abtn;
	}

}
