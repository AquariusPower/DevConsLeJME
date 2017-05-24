package com.github.devconslejme.tests.temp;

import com.github.devconslejme.gendiag.DialogHierarchyStateI;
import com.github.devconslejme.misc.jme.EnvironmentJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.AbsorbClickCommandsI;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.tests.TestHierarchyResizablePanel.BaseTextUD;
import com.jme3.app.SimpleApplication;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.focus.FocusManagerState;
import com.simsilica.lemur.style.BaseStyles;

/**
 * TEST CASE FAILED: the click actually worked here!
 */
@Deprecated
public class TestcaseLemurClickImmuneButton extends SimpleApplication{
	public static void main(String[] args) {
		TestcaseLemurClickImmuneButton test = new TestcaseLemurClickImmuneButton();
		test.start();
	}

	@Override
	public void simpleInitApp() {
		GuiGlobals.initialize(this);
		BaseStyles.loadGlassStyle();
		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		
		com.github.devconslejme.gendiag.PkgCfgI.i().configure(this, getGuiNode(), getRootNode());
		initTest();
	}

	private void initTest() {
//		addButton(null,500);
		addButton(addButton(null,100),300);
		
//		createParentChild(100);
		createParentChild(400);
	}
	
	@SuppressWarnings("unchecked")
	private void createParentChild(int iBaseY) {
		ResizablePanel rzpChild = createDialog(new Vector3f(200,iBaseY+200,20), "child"+iBaseY, "click to close");
		AbsorbClickCommandsI.i().addClickCommands(((Button)rzpChild.getContents()),new Command<Button>(){
			@Override
			public void execute(Button source) {
				rzpChild.close(); 
			}
		});
		
		ResizablePanel rzpParent = createDialog(new Vector3f(100,iBaseY+100,10), "parent"+iBaseY, "click to open modal");
		AbsorbClickCommandsI.i().addClickCommands(((Button)rzpParent.getContents()),new Command<Button>(){
			@Override
			public void execute(Button source) { 
				DialogHierarchyStateI.i().showDialogAsModal(rzpParent,rzpChild);
			}
		});
		
		DialogHierarchyStateI.i().showDialog(rzpParent);
	}
	
	public static class BaseTextUD{
		private String strBaseText;

		public String getBaseText() {
			return strBaseText;
		}

		public BaseTextUD setBaseText(String strBaseText) {
			this.strBaseText = strBaseText;
			return this; //for beans setter
		}
	}

//	enum EUserData{
//		keyBaseText,
//		;
//		public String s(){return toString();}
//	}
	private ResizablePanel createDialog(Vector3f pos,String strName,String strInfo) {
		if(strInfo==null)strInfo=strName;
		
		ResizablePanel rzp = DialogHierarchyStateI.i().createDialog(strName,null);
		rzp.setPreferredSizeWH(new Vector3f(300,250,0));
		rzp.setLocalTranslationXY(pos); //above DevCons
		
		String strBaseText=strName+"/"+strInfo;
		Button btn = new Button(strBaseText);
		UserDataI.i().putSafelyMustNotExist(btn, new BaseTextUD().setBaseText(strBaseText));
		rzp.setContents(btn);
//		DragParentestPanelListenerI.i().applyAt(btn);
		
		return rzp;
	}
	
	@SuppressWarnings("unchecked")
	private ResizablePanel addButton(ResizablePanel parent, float fX) {
		Button btn = new Button("MouseCursor Click Immune");
		
		ResizablePanel rzp = DialogHierarchyStateI.i().createDialog("test",null);
//		ResizablePanel rzp = new ResizablePanel(null);
		rzp.setContents(btn);
		
//		btn.setLocalTranslation(fX, Display.getHeight()/2f, 0);
//	getGuiNode().attachChild(btn);
		if(parent!=null){
//			btn.addClickCommands(new Command<Button>(){
			AbsorbClickCommandsI.i().addClickCommands(btn,new Command<Button>(){
				@Override
				public void execute(Button source) {
//					source.removeFromParent();
					rzp.close();
				}
			});
			
			AbsorbClickCommandsI.i().addClickCommands(((Button)parent.getContents()),new Command<Button>(){
				@Override
				public void execute(Button source) {
					DialogHierarchyStateI.i().showDialogAsModal(parent, rzp);
				}
			});
		}else{
			DialogHierarchyStateI.i().showDialog(rzp);
		}
//		getGuiNode().attachChild(rzp);
		rzp.setLocalTranslationXY(new Vector3f(fX, EnvironmentJmeI.i().getDisplay().getHeight()/2f, 0));
		
		getStateManager().getState(FocusManagerState.class).setFocus(btn);
		
		return rzp;
	}
}