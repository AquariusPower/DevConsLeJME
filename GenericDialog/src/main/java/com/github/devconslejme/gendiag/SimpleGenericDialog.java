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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import com.github.devconslejme.gendiag.ContextMenuI.ContextButton;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu;
import com.github.devconslejme.gendiag.ContextMenuI.ContextMenu.ApplyContextChoiceCmd;
import com.github.devconslejme.gendiag.ContextMenuI.HintUpdaterPerCtxtBtn;
import com.github.devconslejme.gendiag.DialogHierarchyStateI.DialogVisuals;
import com.github.devconslejme.gendiag.DialogHierarchyStateI.IGUIUserInteraction;
import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.SimpleVarReadOnly;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.ESimpleType;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.JavaLangI.LinkedHashMapX;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.MethodX;
import com.github.devconslejme.misc.MultiClickI.CallMultiClickUpdate;
import com.github.devconslejme.misc.MultiClickI.MultiClick;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.MiscJmeI;
import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.github.devconslejme.misc.jme.UserDataI;
import com.github.devconslejme.misc.lemur.AbsorbClickCommandsI;
import com.github.devconslejme.misc.lemur.CursorListenerX;
import com.github.devconslejme.misc.lemur.DragParentestPanelListenerI;
import com.github.devconslejme.misc.lemur.MiscLemurI;
import com.github.devconslejme.misc.lemur.PopupHintHelpListenerI;
import com.github.devconslejme.misc.lemur.ResizablePanel;
import com.github.devconslejme.misc.lemur.SizeAndLocationI;
import com.google.common.base.Function;
import com.jme3.input.KeyInput;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.Button;
import com.simsilica.lemur.Command;
import com.simsilica.lemur.Container;
import com.simsilica.lemur.Insets3f;
import com.simsilica.lemur.Label;
import com.simsilica.lemur.ListBox;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.TextField;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.BorderLayout.Position;
import com.simsilica.lemur.component.TextEntryComponent;
import com.simsilica.lemur.core.VersionedHolder;
import com.simsilica.lemur.core.VersionedList;
import com.simsilica.lemur.core.VersionedReference;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.KeyAction;
import com.simsilica.lemur.event.KeyActionListener;
import com.simsilica.lemur.focus.FocusManagerState;
import com.simsilica.lemur.list.DefaultCellRenderer;
import com.simsilica.lemur.style.ElementId;


/**
 * A text based generic dialog.
 * TODO move whatever fits at super class to there
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SimpleGenericDialog extends AbstractGenericDialog {
	private Label	btnInfoText;
	private ListBox<OptionData>	lstbxOptions;
	private VersionedList<OptionData>	vlodOptions;
	/** a list of options can never be empty or the dialog will make no sense at all */
	private LinkedHashMapX<String,OptionData> hmOptionsRoot;
	private VersionedList<ToolAction>	vlodTools;
	private TextField	tfInput;
	private boolean	bReturnJustTheInputTextValue;
	private boolean	bRequestUserSubmitedInputValueApply;
	private KeyActionListener	kalSectionInput;
	private boolean	bRequestUpdateListItems;
	private SectionIndicator sectionIndicator;
	private Function<IVisibleText, String>	funcVisibleText;
	private boolean bRequestSelectedToggleExpandedOnce;
	private Command<? super Button>	cmdOption;
	private DefaultCellRenderer<IVisibleText>	crVisibleText;
	private VersionedReference<Set<Integer>>	vrSelection;
	private boolean	bRequestUpdateOptionSelected;
	private boolean	bCloseOnChoiceMade;
	private Container	cntrInfo;
	private String	strName;
//	private String	strTitle;
	private Button	btnTitleText;
	private Container	cntrTitle;
	private Button	btnMinimize;
	private Button	btnMaximizeRestore;
	private Button	btnClose;
	private ArrayList<Button>	abtnInfoSection;
	private Command<? super Button>	cmdInfoSectionTitleButtons;
	private Container	cntrDiagControls;
	private int	iDiagControlColumnInitIndex;
	private VersionedHolder<String> vhInputTextSubmitted = new VersionedHolder<>();
	VersionedReference<String> vrInputTextSubmittedApplyAsTypedTextChoice = vhInputTextSubmitted.createReference();
	private boolean	bKeepMaximized;
//@FloatLimits(min=-1f,max=1f) float f;
	private ListBox<ToolAction>	lstbxTools;
	/**
	 * for some reason, some of the buttons on the listbox will not work with the
	 * Button.addClickCommands(). To force it to work, I am using together the 
	 * CursorListener.
	 */
	@Workaround @Bugfix
	private CursorListenerX	curlisExtraClickCmd;
	private ContextMenu	cmIST;
	private ContextMenu	cmSubBorderSize;
	private int	iNestingStepDistance=10;
	private FocusManagerState	focusman;
	protected boolean	bLockPosSize;
//	private boolean	bMethodCallUnsafely;
	
	private static class SectionIndicator{}
	
	public static class ToolAction implements IVisibleText{
		private String strTextKey;
		private CmdBtnTA cmdAction;
//		private String	strVisibleText;
		private Integer	iStatusAutoUpdateText;
		private String[]	astrStatus;
		private String	strVisibleTextBtn;
//		private Integer	iInitStatus;
		private ContextMenu	cm;
		
		public abstract static class CmdBtnTA implements Command<Button>{
			private ToolAction ta;
			
			@Deprecated
			@Override
			public void execute(Button source) {
				ta.updateStatus(executeTA(source));
				ta.updateTextWork(source);
			}
			
			/**
			 * 
			 * @param btn
			 * @return this value will also auto update the status text
			 */
			protected abstract Integer executeTA(Button btn);
			
		}
		
		public ToolAction(String strTextKey, CmdBtnTA cmdAction) {
			this.strTextKey = strTextKey;
			
			this.cmdAction = cmdAction;
			this.cmdAction.ta=this;
		}
		
		public ToolAction setMultiStatusMode(Integer iInitStatus, String... astrStatus) {
//			this.iInitStatus = iInitStatus;
			this.astrStatus = astrStatus;
			
			setStatusArray(astrStatus);
			this.iStatusAutoUpdateText=iInitStatus;
			updateTextWork(null);
			
			return this;
		}
		
		/**
		 * 
		 * @param source if null, and auto update mode, will just auto store on a field
		 */
		private String updateTextWork(Button source) {
			if(iStatusAutoUpdateText!=null){
				return updateTextAuto(source);
			}else{
				return updateTextCustom(source);
			}
		}
		
		protected String updateTextCustom(Button btn){return "";}
		
		protected void updateStatus(Integer i){
			if(i!=null)this.iStatusAutoUpdateText=i;
		}
		
		protected String updateTextAuto(Button btn){
			strVisibleTextBtn=(strTextKey+": "+getNextStatus());
			
			if(btn!=null){
				btn.setText(strVisibleTextBtn);
				updateHintHelp(btn);
			}
			
			return strVisibleTextBtn;
		}

		private String getNextStatus() {
			int i=iStatusAutoUpdateText;
			i++;
			if(i==astrStatus.length)i=0;
			return astrStatus[i];
		}

		private String getCurrentStatus() {
			return astrStatus[iStatusAutoUpdateText];
		}

		private void updateHintHelp(Button btn) {
			if(iStatusAutoUpdateText!=null){
				PopupHintHelpListenerI.i().setPopupHintHelp(btn, "Current: "+getCurrentStatus());
			}
		}

//		private String getStatusText(int i) {
//			// overlaps
//			i = i>=astrStatus.length?i=0:i;
//			i = i<0?astrStatus.length-1:i;
//			return astrStatus[i];
//		}

		protected String[] getStatusArray() {
			return astrStatus;
		}

		protected void setStatusArray(String[] astrStatus) {
			this.astrStatus = astrStatus;
		}
		
		@Override
		public String getVisibleText() {
//			return strTextKey;
//			return cmdAction.strVisibleTextBtn;
			if(iStatusAutoUpdateText!=null){
				updateTextWork(null);
				return strVisibleTextBtn;
			}
			
			return strTextKey;
//			return strVisibleText;
		}

		public ToolAction setContextMenu(ContextMenu cm) {
			this.cm=cm;
			return this;
		}

		public ContextMenu getContextMenu() {
			return cm;
		}
		
	}
	
	public static class OptionDataDummy extends OptionData{
		public OptionDataDummy(){
			String str="(TempDummy)";
			setTextKey(str+OptionDataDummy.class.getName());
			setStoredValue(str);
		}
	}
	
	public static class OptionData implements IVisibleText{
		private String strTextKey;
		private OptionData odParent;
		private Object objStoredValue;
		private boolean bExpanded;
		private LinkedHashMapX<String,OptionData> hmNestedChildrenSubOptions;
		private ArrayList<CmdCfg> acmdcfgList = new ArrayList<CmdCfg>();
		private boolean	bHasBean;
		
		public OptionData(){
			bExpanded=true;
			hmNestedChildrenSubOptions = new LinkedHashMapX<String,OptionData>();
		}
		
		protected OptionData setTextKey(String strTextKey) {
			this.strTextKey = strTextKey;
			return this; 
		}
		protected OptionData setStoredValue(Object objValue) {
			this.objStoredValue = objValue;
			return this; 
		}
		private OptionData setSectionParent(OptionData odParent) {
			assert(odParent==null || odParent.isSection());
			this.odParent = odParent;
			return this; 
		}
		public String getTextKey() {
			return strTextKey;
		}
		public OptionData getSectionParent() {
			assert(odParent==null || odParent.isSection());
			return odParent;
		}
		public Object getStoredValue() {
			if(isSection())return null;
			return objStoredValue;
		}
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("OptionData [strTextKey=");
			builder.append(strTextKey);
			builder.append(", odParent=");
			builder.append(odParent==null?null:odParent.getTextKey()); //!!!!!!!!!!!!!!! CUSTOM !!!!!!!!!!!!!!
			builder.append(", objValue=");
			builder.append(objStoredValue);
			builder.append(", bExpanded=");
			builder.append(bExpanded);
			builder.append(", hmOptions=");
			builder.append(hmNestedChildrenSubOptions);
			builder.append("]");
			return builder.toString();
		}
		public boolean isExpanded() {
			return bExpanded;
		}
		protected OptionData setExpanded(boolean bExpanded) {
			this.bExpanded = bExpanded;
			return this; 
		}
		public boolean toggleExpanded(){
			setExpanded(!isExpanded());
//			bExpanded=!bExpanded;
			return bExpanded;
		}
		/**
		 * 
		 * @param aodStore will be created if null
		 * @return
		 */
		public ArrayList<OptionData> getAllChildrenRecursively(ArrayList<OptionData> aodStore){
			if(aodStore==null)aodStore = new ArrayList<OptionData>();
			for(OptionData odChild:hmNestedChildrenSubOptions.values()){
				aodStore.add(odChild);
				odChild.getAllChildrenRecursively(aodStore);
//				if(odChild.hmNestedChildrenSubOptions.size()>0){
//					getAllChildrenRecursively(aodStore);
//				}
			}
			return aodStore;
		}
		@Override
		public String getVisibleText() {
			int iDepth=getNestingDepth();
			
			String str=strTextKey;
			
			if(isSection()){
//				str="["+(isExpanded()?"-":"+")+"] "
				str=""
					+str
					+(
//						!isExpanded()
//						? 
								" {p"+iDepth+",c"+hmNestedChildrenSubOptions.size()+",ac"+getAllChildrenRecursively(null).size()+"}"
//						: ""
					)
				;
			}
			
//			str=" "+str;
//			str=Strings.padStart(str, str.length()+iDepth, '>');
			
			return str;
		}

		public CmdCfg[] getCmdCfgList() {
			return acmdcfgList.toArray(new CmdCfg[0]);
		}
		
		public boolean isCmdCfgSet(String strKey){
			return get(strKey)!=null;
		}
		
		public CmdCfg get(String strKey){
			for(CmdCfg ccChk:acmdcfgList){
				if(ccChk.getTextUniqueKey().equals(strKey))return ccChk;
			}
			return null;
		}
		
		public void addCmdCfg(CmdCfg cc) {
//			DetailedException.assertNotAlreadySet(this.cmdCfg, cmdCfg, this);
			CmdCfg ccFound=get(cc.getTextUniqueKey());
			if(ccFound!=null){
				MessagesI.i().warnMsg(this, "already set", cc.getTextUniqueKey(), cc, ccFound);
			}else{
				acmdcfgList.add(cc);
			}
		}

		public int getNestingDepth() {
			int iDepth=0;
			OptionData odParent=this;
			while((odParent=odParent.getSectionParent())!=null)iDepth++;
			return iDepth;
		}

		/**
		 * if this one is a getter and has a setter
		 */
		public void setHasBean() {
			bHasBean=true;
		}
		public boolean isHasBean() {
			return bHasBean;
		}

		public boolean isSection() {
			return SectionIndicator.class.isInstance(objStoredValue);
		}
		
	}
	
	public static abstract class CmdCfg implements Command<Button>{
//		private boolean	bIsTheApplyUserCustomValueCommand;
//		public CmdCfg(boolean bIsTheApplyUserCustomValueCommand){
//			this.bIsTheApplyUserCustomValueCommand = bIsTheApplyUserCustomValueCommand;
//			
//		}
		
		public CmdCfg(String strTextUniqueKey){
			this.strTextUniqueKey=strTextUniqueKey;
		}
		
		private String	strTextUniqueKey;
		private String	strHintHelp;

		public String getTextUniqueKey() {
			return strTextUniqueKey;
		}

//		public CmdCfg setText(String strText) {
//			this.strText = strText;
//			return this;
//		}

		public CmdCfg setHintHelp(String string) {
			this.strHintHelp=string;
			return this;
		}

		public String getHintHelp() {
			return strHintHelp;
		}

	}
	
//	public SimpleGenericDialog(String strTitle, ResizablePanel rzpOwner) {
//		super(rzpOwner);
//		setTitle(strTitle);
////		configureDefaults();
//	}

	public SimpleGenericDialog(String strTitle) {
		setTitle(strTitle);
		
		setDialogVisuals(
			DialogHierarchyStateI.i().prepareDialogParts(
				SimpleGenericDialog.this.getClass().getSimpleName(),
				null
			)
		);
		
		addListOptionsUserInteractionListener(DialogHierarchyStateI.i().getGlobalUserInteractionListener());
		
//		this(strTitle, DialogHierarchyStateI.i().prepareDialogParts(SimpleGenericDialog.class.getSimpleName(), null, this));
//		addListOptionsUserInteractionListener(DialogHierarchyStateI.i());
		
		hmOptionsRoot = new LinkedHashMapX<String,OptionData>();
		bRequestUpdateListItems = true;
		sectionIndicator = new SectionIndicator();
		bCloseOnChoiceMade=true;
		
		// text info row
		abtnInfoSection = new ArrayList<Button>();
		/**
		 * IMPORTANT: Button works MUCH better than Label when clicking to drag for ex.
		 * as the label will require aiming at the label's text...
		 */
		btnInfoText = createInfoButton("(No Info)",null);
		
		
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				DialogHierarchyStateI.i().prepareDialogParts(
////						SimpleGenericDialog.class.getSimpleName(), null, SimpleGenericDialog.this);
//					SimpleGenericDialog.this.getClass().getSimpleName(), null, SimpleGenericDialog.this);
//				return true;
//			}
//		});
	}
	
	/**
	 * TODO this may be called twice as a workaround, how to avoid it?
	 * @param btn
	 */
	private boolean buttonClicked(ButtonCell btn){
		OptionData od = btn.od;
		if(od!=null){
			bRequestUpdateOptionSelected=true;
			lstbxOptions.getSelectionModel().setSelection(vlodOptions.indexOf(od));
			setLastSelectedOptionStoredValue(od.getStoredValue());
			return true;
		}
		
		ToolAction ta = btn.ta;
		if(ta!=null){
			//TODO just ignore???
			return true;
		}
		
		return false;
	}
	
	/**
	 * this method is quite simple, can be called at the constructor
	 * @param strText
	 * @param strHintPopup
	 * @return
	 */
	private Button createInfoButton(String strText,String strHintPopup){
		Button btn = new Button(strText,getDialog().getStyle());
		abtnInfoSection.add(btn);
		if(strHintPopup!=null)PopupHintHelpListenerI.i().setPopupHintHelp(btn,strHintPopup);
		return btn;
	}
	
	@SuppressWarnings("unchecked")
	private void initSectionInfo(){
		ESection es=ESection.Info;
		if(getSection(es)==null){
			initSectionInfoTitle();
			
			// cfg all buttons
			for(Button btn:abtnInfoSection){
//				btn.addClickCommands(cmdInfoSectionTitleButtons);
				AbsorbClickCommandsI.i().addClickCommands(btn,cmdInfoSectionTitleButtons);
			}
			
			// info section
			cntrInfo = new Container(new BorderLayout());
			cntrInfo.addChild(cntrTitle, BorderLayout.Position.North);
			cntrInfo.addChild(btnInfoText, BorderLayout.Position.Center);
			
			setSection(es,cntrInfo);
		}
	}
	
	public static class MaximizeUD{
		private Vector3f v3fPosBeforeMaximize;

		public Vector3f getPosBeforeMaximize() {
			return v3fPosBeforeMaximize;
		}

		public MaximizeUD setPosBeforeMaximize(Vector3f v3fPosBeforeMaximize) {
			this.v3fPosBeforeMaximize = v3fPosBeforeMaximize;
			return this; //for beans setter
		}
	}
	
	MultiClick mcTitle = new MultiClick(0,2,new CallMultiClickUpdate() {
		@Override
		public void applyMultiClick(int totalClicks) {
			if(totalClicks==2){
				toggleMaximize();
			}
		}
	});
	private Button	btnOtherDialogs;
	
	ContextMenu cmOtherDiags;
	
	private void initSectionInfoTitle() {

		cmdInfoSectionTitleButtons = new Command<Button>() {
			@Override
			public void execute(Button source) {
				if(source==btnMaximizeRestore){ //toggle
					toggleMaximize();
				}else
				if(source==btnClose){
					getDialog().close();
				}else
				if(source==btnMinimize){
					MinimizedDialogsPanelI.i().minimize(SimpleGenericDialog.this);
				}else
				if(source==btnOtherDialogs){
					//the context menu button click will be used
				}else
				if(source==btnTitleText){
					mcTitle.updateIncClicks();
				}else
				{
					MessagesI.i().warnMsg(SimpleGenericDialog.this, "cmd not supported yet", source);
				}
			}
		};
		
		// title row
		cntrDiagControls = new Container();
		iDiagControlColumnInitIndex=0;
		
		btnOtherDialogs=appendNewDiagControl("O","other root dialogs");
		cmOtherDiags = new ContextMenu(getDialog()){
			@Override
			public void recreateEntries() {
				super.recreateEntries();
				
//				boolean bHas=false;
				/**
				 * there will always have at least this self dialog
				 */
				for(ResizablePanel diag:DialogHierarchyStateI.i().getAllOpenedAndMinimizedDialogs()){
//					bHas=true;
					DialogVisuals vs = DialogHierarchyStateI.i().getVisuals(diag);
					addNewEntry(vs.getGenDiagOpt().getTitle(), new ApplyContextChoiceCmd() {
						@Override
						public void executeContextCommand(ContextButton cbSource) {
							cmOtherDiags.close();
							DialogHierarchyStateI.i().raiseDialogLater(vs);
						}
					});
				}
			}
		};
		ContextMenuI.i().applyContextMenuAtSource(btnOtherDialogs,cmOtherDiags);
		
		btnMinimize=appendNewDiagControl("_","Minimize");
		btnMaximizeRestore=appendNewDiagControl("[ ]","Maximize/Restore");
		btnClose=appendNewDiagControl("X","Close");
//		MiscLemurI.i().changeBackgroundColor(btnClose, ColorI.i().colorChangeCopy(ColorRGBA.Red,0f,0.25f), true); //TODO use a lemur style instead
		MiscLemurI.i().changeBackgroundColor(btnClose, ColorRGBA.Red, true); //TODO use a lemur style instead
		
		// title row put it all
		cntrTitle = new Container(new BorderLayout());
		
		initInfoSectionTitleContextMenu();
		
		btnTitleText = createInfoButton("(no title)",null);
//		MiscLemurI.i().changeBackgroundColor(btnTitleText, ColorI.i().colorChangeCopy(ColorRGBA.Blue,0f,0.25f), true); //TODO use a lemur style instead
		MiscLemurI.i().changeBackgroundColor(btnTitleText, ColorRGBA.Blue, true); //TODO use a lemur style instead
		DragParentestPanelListenerI.i().applyAt(btnTitleText);
		ContextMenuI.i().applyContextMenuAtSource(btnTitleText, cmIST);
		
//		cntrTitle.setPreferredSize(new Vector3f(1,1,0.1f));
		cntrTitle.addChild(btnTitleText, BorderLayout.Position.Center);
		cntrTitle.addChild(cntrDiagControls, BorderLayout.Position.East);
		
	}

	protected void toggleMaximize() {
		if(bKeepMaximized){							/**							 * restore							 */
			getDialog().restoreDefaultSafeSize();
			
			MaximizeUD mud = UserDataI.i().getMustExistOrNull(getDialog(), MaximizeUD.class);
			DetailedException.assertNotNull(mud,getDialog(),this);
			Vector3f v3fPosBeforeMaximize = mud.getPosBeforeMaximize();
			getDialog().setLocalTranslationXY(v3fPosBeforeMaximize);
			
			bKeepMaximized=false;
		}else{							/**							 * maximize							 */
			getDialog().applyCurrentSafeSizeAsDefault();
			
//			getDialog().setUserData(strUDKeyPosBeforeMaximize,getDialog().getLocalTranslation().clone());
			UserDataI.i().overwriteSafely(
				getDialog(), 
				new MaximizeUD().setPosBeforeMaximize(
					getDialog().getLocalTranslation().clone()
				)
			);
			
			bKeepMaximized=true;
		}
	}

	private void initInfoSectionTitleContextMenu() {
		cmIST = new ContextMenu(getDialog());
		
		cmIST.addNewEntry("Restore to default/initial size", new ApplyContextChoiceCmd() {@Override public void executeContextCommand(ContextButton source) {
			getDialog().restoreDefaultSafeSize(); }});
		
		cmIST.addNewEntry("Update default size to current", new ApplyContextChoiceCmd() {@Override public void executeContextCommand(ContextButton source) {
			getDialog().applyCurrentSafeSizeAsDefault(); }});
		
		cmIST.addNewEntry("Toggle lock pos/size", new ApplyContextChoiceCmd() {@Override public void executeContextCommand(ContextButton source) {
			bLockPosSize=!bLockPosSize;
			if(bLockPosSize){
				DragParentestPanelListenerI.i().setEnabledAt(btnTitleText,false);
				DragParentestPanelListenerI.i().setEnabledAt(getDialog(),false);
				getDialog().setEnableResizing(false);
			}else{
				DragParentestPanelListenerI.i().setEnabledAt(btnTitleText,true);
				DragParentestPanelListenerI.i().setEnabledAt(getDialog(),true);
				getDialog().setEnableResizing(true);
			}
		}});
		
		cmIST.addNewEntry("Toggle Info Visibility", 
			new ApplyContextChoiceCmd() {@Override public void executeContextCommand(ContextButton source) {
				if(btnInfoText.getParent()!=null){
					cntrInfo.removeChild(btnInfoText);
				}else{
					cntrInfo.addChild(btnInfoText, BorderLayout.Position.Center);
				}
			}},
			new HintUpdaterPerCtxtBtn() {
				@Override
				public Boolean call() {
					setPopupHintHelp(btnInfoText.getParent()==null?"show":"hide"); //inverted to show next action on click
					return true;
				}
			}
		);
		
		cmSubBorderSize = cmIST.createSubMenu("global resizable border size");
		cmSubBorderSize.setSingleChoiceMode(true);
		ApplyContextChoiceCmd cmdBorderSize = new ApplyContextChoiceCmd() {
			@Override
			public void executeContextCommand(ContextButton source) {
				int i = Integer.parseInt(source.getText());
//				int i = getStoredValueFromContextButton();
				ResizablePanel.setResizableBorderSizeDefault(i);
				getDialog().setResizableBorderSize(i,i);
			}
		};
		
		for(int i=1;i<=10;i++){
			cmSubBorderSize.addNewEntry(""+i, i, cmdBorderSize, new HintUpdaterPerCtxtBtn() {@Override	public Boolean call() {
				return ResizablePanel.getResizableBorderSizeDefault()==(int)getStoredValueFromContextButton();
			}});
		}
	}
	
	private static class CellParts{
		ButtonCell btnTreeNesting;
		ButtonCell btnItemText;
		Panel pnlCfg;
	}
	
	public static class ButtonCell extends Button{
		public ToolAction	ta;
		public OptionData	od;
		public ButtonCell(String s, ElementId elementId, String style) {
			super(s, elementId, style);
		}
		
//		@Override
//		public void addClickCommands(Command<? super Button>... commands) {
//			AbsorbClickCommandsI.i().addClickCommands(this, commands);
////			super.addClickCommands(commands);
////			AbsorbClickCommandsI.i().absorbClickCommands(this);
//		}
	}
	
	private class ContainerCell extends Container{
		CellParts cp;
		public ContainerCell() {
			super(new BorderLayout(), getDialog().getStyle());
		}
//		public ContainerCell(GuiLayout layout, String style) {
//			super(layout, style);
//		}
	}
	
	private void initBase(){
		focusman = AppI.i().getState(FocusManagerState.class);
		
		curlisExtraClickCmd = new CursorListenerX(){
			@Override
			protected boolean click(CursorButtonEvent event, Spatial target, Spatial capture) {
				return buttonClicked((ButtonCell)capture);
			};
		};
		
  	Command<? super Button> cmdToggleSectionExpanded = new Command<Button>() {@Override	public void execute(Button btn) {
  		((ButtonCell)btn).od.toggleExpanded();
		}};
  	
		crVisibleText = new DefaultCellRenderer<IVisibleText>(getDialog().getStyle()){
			@SuppressWarnings("unchecked")
			private Panel getView(OptionData od, boolean selected, Panel existing) {
				ContainerCell cntr=null;
        if( existing == null ) {
//        	cntr=new ContainerCell(new BorderLayout(), getDialog().getStyle());
					cntr=new ContainerCell();
					cntr.cp = new CellParts();
					
        	cntr.cp.btnTreeNesting = new ButtonCell("", getElement(), getDialog().getStyle());
					cntr.addChild(cntr.cp.btnTreeNesting, Position.West);
					
        	cntr.cp.btnItemText = new ButtonCell("", getElement(), getStyle());
					cntr.addChild(cntr.cp.btnItemText, Position.Center);
        	AbsorbClickCommandsI.i().addClickCommands(cntr.cp.btnItemText,cmdOption);
  				CursorEventControl.addListenersToSpatial(cntr.cp.btnItemText, curlisExtraClickCmd);
	      } else {
	      	cntr = (ContainerCell)existing;
	      }
        
        // ALWAYS update the value!
				cntr.cp.btnItemText.od=od;
				cntr.cp.btnTreeNesting.od=od;
				
				// update the nesting visuals
				String strNesting = "["+(od.isExpanded()?"-":"+")+"]";
				if(od.hmNestedChildrenSubOptions.size()==0){
					strNesting=" ";
				}else{
					AbsorbClickCommandsI.i().addClickCommands(cntr.cp.btnTreeNesting,cmdToggleSectionExpanded);
				}
				cntr.cp.btnTreeNesting.setText(strNesting);
				cntr.cp.btnTreeNesting.setInsets(new Insets3f(0, 
					getNestingStepDistance()*od.getNestingDepth(), 
					0, 0));
				
				// update the text based on the value using a transformation
				cntr.cp.btnItemText.setText(valueToString(od));
				if(od.hmNestedChildrenSubOptions.size()==0){
					PopupHintHelpListenerI.i().setPopupHintHelp(cntr.cp.btnItemText, 
						cntr.cp.btnItemText.getText()+"\n"+od.getStoredValue().getClass().getSimpleName()
//						+":"+od.getStoredValue().toString()
					);
				}
				
				// CONFIGURATOR: each item may have a different kind of configurator from button text to a different visual (like a slider etc)
				if(isEnableItemConfigurator()){ 
					cntr.cp.pnlCfg = createConfigurator(od, cntr.cp.pnlCfg); 
					cntr.addChild(cntr.cp.pnlCfg, Position.East);
				}
				
				return cntr;
			}
			
			@SuppressWarnings({ "unchecked"})
			private Panel getView(ToolAction ta, boolean selected, Panel existing) {
				ButtonCell btnItemText = null;
				
        if( existing == null ) {
        	btnItemText = new ButtonCell(valueToString(ta), getElement(), getStyle());
        	if(ta.getContextMenu()!=null)ContextMenuI.i().applyContextMenuAtSource(btnItemText, ta.getContextMenu());
	      } else {
	      	btnItemText = (ButtonCell)existing;
	      	btnItemText.setText(valueToString(ta));
	      }
        
        ta.updateHintHelp(btnItemText);
        
        btnItemText.ta = ta;
				AbsorbClickCommandsI.i().addClickCommands(btnItemText,btnItemText.ta.cmdAction);
				
				return btnItemText;
			}
			
			@Override
			public Panel getView(IVisibleText value, boolean selected, Panel existing) {
				Panel pnlRet = null;
				
				if(value instanceof OptionData){
					pnlRet = getView((OptionData)value, selected, existing);
				}else
				if(value instanceof ToolAction){
					pnlRet = getView((ToolAction)value, selected, existing);
				}
				
				return pnlRet;
			}
		};
		
		funcVisibleText = new Function<IVisibleText, String>() {
			@Override
			public String apply(IVisibleText vt) {
				return vt.getVisibleText();
			}
		};
		
		crVisibleText.setTransform(funcVisibleText);
	}
	
	protected boolean isEnableItemConfigurator() {
		return false;
	}

	@SuppressWarnings("unchecked")
	protected Panel createConfigurator(OptionData od, Panel pnlCfgExisting) {
		Container cntr = new Container(getDialog().getStyle());
		
		int i=0;
		Panel pnl = automaticConfiguratorCreation(od);
		if(pnl!=null){
			cntr.addChild(pnl,i++);
		}
		
		CmdCfg[] acc = od.getCmdCfgList();
		for(CmdCfg cc:acc){
//		Command<? super Button> cmd = od.getCmdCfgList();
//		if(cmd!=null){
			Button btnCfg = new Button(cc.getTextUniqueKey(), getDialog().getStyle());
			AbsorbClickCommandsI.i().addClickCommands(btnCfg, cc);// btnCfg.addClickCommands(cc);
			if(cc.getHintHelp()!=null)PopupHintHelpListenerI.i().setPopupHintHelp(btnCfg, cc.getHintHelp());
			cntr.addChild(btnCfg, i++);
//			return btnCfg;
//		}
		}
		
//		if(acc.length>0)return cntr;
		
		if(i==0){ //nothing was added to container
			Button btn = new Button("...", getDialog().getStyle());
			PopupHintHelpListenerI.i().setPopupHintHelp(btn, "configuration not available");
			return btn;
		}
		
		return cntr;
	}
	
	private class ContainerEdit extends Container{
		private Button btn;
		private TextField tf;
		
		public ContainerEdit() {
//			super(new BorderLayout(), getDialog().getStyle());
			super(getDialog().getStyle());
		}
		public TextField getTFInput() {
			return tf;
		}
		public void setTf(TextField tf) {
			this.tf = grantMinWidth(tf);
		}
		public Button getBtnShowVal() {
			return btn;
		}
		public void setBtn(Button btn) {
			this.btn = grantMinWidth(btn);
		}
		@SuppressWarnings("unchecked")
		private <T> T grantMinWidth(Panel pnl){
			float fMinWidth=50;
			pnl.updateLogicalState(0); //this will pre-calculate the required good size 
			Vector3f v3fSize = pnl.getSize().clone();
			if(v3fSize.x<fMinWidth)v3fSize.x=fMinWidth;
			SizeAndLocationI.i().setPreferredSize(pnl,v3fSize); //pnl.setPreferredSize(v3fSize);
			return (T)pnl;
		}
	}
	
	@SuppressWarnings("unchecked")
	private Panel createConfiguratorMethodHelp(OptionData od, MethodX mh){
		Method mGetter = mh.getMethod();
		ContainerEdit ce = new ContainerEdit();
		ESimpleType etypeGetterRet=ESimpleType.forClass(mGetter.getReturnType(),false);
		boolean bIsBeanGetter=JavaLangI.i().isBeanGetter(mGetter); 
		boolean bIsSimpleGetter=mGetter.getAnnotation(SimpleVarReadOnly.class)!=null;
		if( (bIsBeanGetter || bIsSimpleGetter) && etypeGetterRet!=null ) {
			String strButtonHintHelp=null;
			try {
				Method mSetter = JavaLangI.i().getBeanSetterFor(mGetter,bIsBeanGetter);
				if(mSetter!=null)od.setHasBean(); //first thing, so if it fails below the problem will be clearly visible
				
				Object objVal = mGetter.invoke(mh.getConcreteObjectInstance()); //collect value from getter method
				String strVal=""+objVal;
				strButtonHintHelp="read-only";
				if(strVal.length()>20){
					strButtonHintHelp+=":"+strVal;
					strVal=strVal.substring(0, 20); //to not mess the dialog
				}
				ce.setBtn(new Button(strVal, getDialog().getStyle())); //show value
				ce.addChild(ce.getBtnShowVal(), 0);
				StringTextJmeI.i().recursivelyApplyTextNoWrap(ce.getBtnShowVal());
				
				if(mSetter!=null){
					strButtonHintHelp=ESimpleType.Boolean.is(etypeGetterRet)?"click to toggle":"click to change value";
					ce.setTf(new TextField(ce.getBtnShowVal().getText(), ce.getBtnShowVal().getStyle()));
					
					Function funcApplyEditedValue = new Function() {
						@Override
						public Object apply(Object input) {
							boolean b=JavaLangI.i().setBeanValueAt(
								mh.getConcreteObjectInstance(), 
								mSetter, 
								mGetter.getReturnType(), 
								ce.getTFInput().getText());
							
							if(!b)MessagesI.i().warnMsg(this, "failed to change value", mSetter, mGetter, ce.getTFInput().getText(), mh);
							
							try {
								// retrieves value possibly validated by the setter/getter
								ce.getBtnShowVal().setText(""+mGetter.invoke(mh.getConcreteObjectInstance()));
							} catch (IllegalAccessException | IllegalArgumentException| InvocationTargetException e) {
								throw new DetailedException(e, "value get should not have failed this second time", mGetter, mSetter, mh, od);
							}
							
							ce.addChild(ce.getBtnShowVal(), 0); //will replace the textfield
								
							return null;
						}
					};
					
					KeyActionListener kalApplyEditedValue = new KeyActionListener() {
						@Override public void keyAction(TextEntryComponent source, KeyAction key) {
							funcApplyEditedValue.apply(null);
						}
					};
					ce.getTFInput().getActionMap().put(new KeyAction(KeyInput.KEY_RETURN), kalApplyEditedValue);
					
	//				ce.getBtnShowVal().addClickCommands(new Command<Button>(){
					AbsorbClickCommandsI.i().addClickCommands(ce.getBtnShowVal(),new Command<Button>(){
						@Override
						public void execute(Button source) {
							if(ESimpleType.Boolean.is(etypeGetterRet)){
								boolean b=ESimpleType.Boolean.parse(source.getText());
								ce.getTFInput().setText(""+!b); //auto set edited as a toggle
								funcApplyEditedValue.apply(null);
							}else{
								ce.addChild(ce.getTFInput(), 0); //will replace the button
								ce.getTFInput().setText(ce.getBtnShowVal().getText());
								focusman.setFocus(ce.getTFInput());
							}
						}});
					
					ce.getBtnShowVal().setColor(ColorI.i().colorChangeCopy(ColorRGBA.Green, 0.35f));
				}else{
					ce.getBtnShowVal().setColor(ColorI.i().colorChangeCopy(ColorRGBA.Red, 0.35f));
				}
				
			} catch (IllegalAccessException | IllegalArgumentException| InvocationTargetException ex) {
				strButtonHintHelp="ERROR: failed to invoke the method '"+mGetter+"' to get the value";
				MessagesI.i().warnMsg(this,strButtonHintHelp,od.getCmdCfgList(), od, mGetter, ex);
				ce.setBtn(new Button("(FAIL)", getDialog().getStyle()));
			}
			
			if(ce.getBtnShowVal()!=null && strButtonHintHelp!=null){
				PopupHintHelpListenerI.i().setPopupHintHelp(ce.getBtnShowVal(), strButtonHintHelp);
			}
		}
		
		return ce;
	}
	
	/**
	 * mainly for primitives, allowing sliders too
	 * TODO but.. how to set it back after changed? a holder? lemur one?
		TODO using limits annotations, allow sliders and TextField input on the very same listbox
		TODO using reflection, look for matching getters and setters (and is...) to create a new child dialog to enable such optoins
	 * @param od
	 * @return
	 */
	protected Panel automaticConfiguratorCreation(OptionData od) {
		// support for full method info, allowing call it using the stored value
		if (od.getStoredValue() instanceof MethodX) {
			return createConfiguratorMethodHelp(od, (MethodX)od.getStoredValue());
		}
		
//		if(!JavaLangI.i().isCanUserTypeIt(od.getStoredValue()))return null;
//		
//		String str=od.getStoredValue().toString();
//		return new Button(str.substring(0, Math.min(10, str.length())), getDialog().getStyle());
		return null;
	}

	@Override
	protected void initContentsContainer() {
		initBase();
		
		initSectionInfo();
		initSectionOptions();
		initSectionInput();
		initSectionTools();
		
		super.initContentsContainer();
	}
	
	protected void initSectionTools() {
		ESection es=ESection.Tools;
		if(getSection(es)==null){
			vlodTools = new VersionedList<ToolAction>();
			
			lstbxTools = new ListBox<ToolAction>(vlodTools, getDialog().getStyle());
			MiscLemurI.i().createListBoxVisibleItemsUpdater(lstbxTools);
			
			lstbxTools.setCellRenderer(crVisibleText);
			
			setSection(es,lstbxTools);
		}
	}
	
	public void putToolActionLater(ToolAction ta){
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(vlodTools==null)return false;
				putToolAction(ta);
				return true;
			}
		});
	}
	public ToolAction putToolAction(ToolAction ta){
		if(!vlodTools.contains(ta)){
			vlodTools.add(ta);
		}
		return ta;
	}
	
	private ArrayList<IGUIUserInteraction> auiLitenersList = new  ArrayList<IGUIUserInteraction> ();
	public void addListOptionsUserInteractionListener(IGUIUserInteraction listener){
		if(!auiLitenersList.contains(listener))auiLitenersList.add(listener);
	}
	
	private void initSectionInput() {
		ESection es=ESection.Input;
		if(getSection(es)==null){
			kalSectionInput = new KeyActionListener() {
				@Override
				public void keyAction(TextEntryComponent source, KeyAction key) {
					switch(key.getKeyCode()){
						case KeyInput.KEY_RETURN:
						case KeyInput.KEY_NUMPADENTER:
							bRequestUserSubmitedInputValueApply=true;
							break;
					}
				}
			};
			
			tfInput = new TextField("", getDialog().getStyle());
			
			tfInput.getActionMap().put(new KeyAction(KeyInput.KEY_NUMPADENTER),kalSectionInput); 
			tfInput.getActionMap().put(new KeyAction(KeyInput.KEY_RETURN),kalSectionInput);
			//tfInput.getActionMap().entrySet()
			
			setSection(es,tfInput);
		}
	}
	
	public static interface IVisibleText{
		String getVisibleText();
	}
	
	private void initSectionOptions() {
		ESection es=ESection.Options; //list items
		if(getSection(es)==null){
			vlodOptions = new VersionedList<OptionData>();
			lstbxOptions = new ListBox<OptionData>(vlodOptions, getDialog().getStyle());
			MiscLemurI.i().createListBoxVisibleItemsUpdater(lstbxOptions);
			
			cmdOption = new Command<Button>() {
				@Override
				public void execute(Button source) {
					buttonClicked((ButtonCell)source);
				}
			};
			
			lstbxOptions.setCellRenderer(crVisibleText);
			
			vrSelection = lstbxOptions.getSelectionModel().createReference();
			
			setSection(es,lstbxOptions);
//			lstbxOptions.setVisibleItems(10); //TODO make automatic
		}
	}

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
//		od.setSectionParent(odParent);
//		od.setTextKey(strNewSectionKey);
		od.setStoredValue(sectionIndicator);
		
		put(odParent,strNewSectionKey,od);
//		hmOptionsRoot.put(strNewSectionKey, od);
		
		return od;
	}
	
	/**
	 * 
	 * @param strSectionParentKey if null, will be root/topmost on the hierarchy
	 * @param strTextOptionKey also is the displayed unique text per section
	 * @param objStoredValue
	 * @return 
	 */
	public OptionData putOption(OptionData odParentSection, String strTextOptionKey, Object objStoredValue){
		OptionData od = new OptionData();
//		od.setSectionParent(odParent);
//		od.setTextKey(strTextOptionKey);
		od.setStoredValue(objStoredValue);
		
		put(odParentSection,strTextOptionKey,od);
		
		return od;
	}
	
	public void remove(OptionData odToRemove) {
		LinkedHashMapX<String, OptionData> hmOpt = hmOptionsRoot;
		if(odToRemove.getSectionParent()!=null){
			hmOpt=odToRemove.getSectionParent().hmNestedChildrenSubOptions;
		}
		
		OptionData odRemoved = hmOpt.removeX(odToRemove.getTextKey()); //TODO fix this!!
		assert odRemoved==odToRemove : odRemoved.toString();
//		OptionData odRemoved = new OptionData().setTextKey("test123");hmOpt.removeX(odToRemove.getTextKey()); //TODO fix this!!
//		assert odRemoved==odToRemove : new Function<Void,String>(){
//			@Override public String apply(Void input) {
//				OptionData odParent = odRemoved.getSectionParent();
//				if(odParent==null)return "parent is null";
//				return odParent.getTextKey();
//			}}.apply(null);
		vlodOptions.remove(odToRemove);
//		return doSomethingRecursively(null, new Function<OptionData, Boolean>() {
//			@Override
//			public Boolean apply(OptionData odToCompare) {
//				if(odToRemove.equals(odToCompare)){
//					LinkedHashMapX<String, OptionData> hmOpt = hmOptionsRoot;
//					if(odToCompare.getSectionParent()!=null){
//						hmOpt=odToCompare.getSectionParent().hmNestedChildrenSubOptions;
//					}
//					
//					hmOpt.removeX(odToRemove.getTextKey());
//					vlodOptions.remove(odToRemove);
//					return true;
//				}
//				return false;
//			}
//		});
	}
//	public int remove(OptionData odToRemove) {
//		return doSomethingRecursively(odToRemove, new Function<OptionData, Boolean>() {
//			@Override
//			public Boolean apply(OptionData odToCompare) {
//				if(odToRemove.equals(odToCompare)){
//					LinkedHashMapX<String, OptionData> hmOpt = hmOptionsRoot;
//					if(odToCompare.getSectionParent()!=null){
//						hmOpt=odToCompare.getSectionParent().hmNestedChildrenSubOptions;
//					}
//					
////					hmOpt.remove(odToRemove.getTextKey()); //TODO enable
//					hmOpt.removeX(odToRemove.getTextKey());
////					hmOptionsRoot.containsKey(odToRemove);
//					vlodOptions.remove(odToRemove);
//					return true;
//				}
//				return false;
//			}
//		});
//	}
	
	private int doSomethingRecursivelyAtRoot(Function<OptionData,Boolean> funcDoSomething) {
		int iCount=0;
		for(OptionData od:hmOptionsRoot.values()){
			iCount+=doSomethingRecursively(od,funcDoSomething);
		}
		return iCount;
	}
	/**
	 * 
	 * @param odParent
	 * @param funcDoSomething
	 * @return how many matched and the apply did anything useful
	 */
	private int doSomethingRecursively(OptionData odParent, Function<OptionData,Boolean> funcDoSomething) {
		int iCount=0;
		if(funcDoSomething.apply(odParent))iCount++;
		if(odParent.isSection()){
			for(OptionData odChild:odParent.hmNestedChildrenSubOptions.values()){
				iCount+=doSomethingRecursively(odChild,funcDoSomething);
			}
		}
		return iCount;
	}
	
	public int setExpandedAll(boolean bExpand){
		return doSomethingRecursivelyAtRoot(new Function<OptionData, Boolean>() {
			@Override
			public Boolean apply(OptionData odToModify) {
				if(!odToModify.isSection())return false;
				
				if(odToModify.isExpanded()!=bExpand){
					odToModify.setExpanded(bExpand);
					return true;
				}
				
				return false;
			}
		});
		
//		int iCount=0;
//		for(OptionData od:hmOptionsRoot.values()){
//			iCount+=setExpandedAllRecursively(od,bExpand);
//		}
//		return iCount;
	}
//	private int setExpandedAllRecursively(OptionData odParent, boolean bExpand) {
//		int iCount=0;
////		if(odParent==null){
////			for(OptionData odChild:hmOptionsRoot.values()){
////				iCount+=setExpandedAllRecursively(odChild, b);
////			}
////		}else{
//			iCount+=doSomethingRecursively(odParent, new Function<OptionData, Boolean>() {
//				@Override
//				public Boolean apply(OptionData odToModify) {
//					if(!SectionIndicator.class.isInstance(odToModify.getStoredValue()))return false;
//					
//					if(odToModify.isExpanded()!=bExpand){
//						odToModify.setExpanded(bExpand);
//						return true;
//					}
//					
//					return false;
//				}
//			});
////		}
//		
//		return iCount;
//	}
//	private void setExpandedAllRecursively(OptionData od, boolean b) {
//		od.setExpanded(b);
//		if(od.isSection()){
//			for(OptionData odChild:od.hmNestedChildrenSubOptions.values()){
//				setExpandedAllRecursively(odChild,b);
//			}
//		}
//	}
	
	private void put(OptionData odParent, String strTextKey, OptionData od){
		od.setSectionParent(odParent);
		od.setTextKey(strTextKey);
		
		HashMap<String,OptionData> hmOpt = hmOptionsRoot;
		if(odParent!=null){
			assert(odParent.isSection());
			hmOpt=odParent.hmNestedChildrenSubOptions;
		}
		
		OptionData odPrevious = hmOpt.put(strTextKey, od);
		if(odPrevious!=null)MessagesI.i().warnMsg(this, "option was already set", odPrevious.toString(), od.toString());
	}
	
	public OptionData findSectionRecursively(OptionData odParent, String strSectionKey){
		assert(odParent.isSection());
		LinkedHashMap<String,OptionData> hmOpt = odParent==null?hmOptionsRoot:odParent.hmNestedChildrenSubOptions;
		if(hmOpt==null)return null;
		
		return findSectionRecursively(hmOpt, strSectionKey);
	}
	private OptionData findSectionRecursively(HashMap<String,OptionData> hmOpt, String strSectionKey){
		OptionData odFound= hmOpt.get(strSectionKey);
		if(odFound!=null)return odFound;
		
		//look for sub-sections
		for(OptionData od:hmOpt.values()){
			if(od.isSection()){
				odFound = findSectionRecursively(od.hmNestedChildrenSubOptions,strSectionKey);
				if(odFound!=null)return odFound;
			}
		}
		
		return null;
	}
	
	private void recreateListItems(){
		vlodOptions.clear();
		recreateListItemsRecursively(hmOptionsRoot,0);
	}
	
	/**
	 * TODO useless right?
	 */
	private Comparator<OptionData>	sortByChildAmount = new Comparator<SimpleGenericDialog.OptionData>() {
		@Override
		public int compare(OptionData o1, OptionData o2) {
			return Integer.compare(o1.hmNestedChildrenSubOptions.size(), o2.hmNestedChildrenSubOptions.size());
		}
	};
	
	private Comparator<OptionData>	sortAtoZ = new Comparator<SimpleGenericDialog.OptionData>() {
		@Override
		public int compare(OptionData o1, OptionData o2) {
			return o1.getTextKey().compareToIgnoreCase(o2.getTextKey());
		}
	};
	
	private boolean	bSortAlphabetically=true;
	private boolean	bCopyClickedOptionTextToInputField=true;
	private Object	objLastSelectedOptionStoredValue;
	
	private void recreateListItemsRecursively(HashMap<String, OptionData> hmOpt, int iDepth){
//		ArrayList<OptionData> aod = new ArrayList<OptionData>(hmOpt.values());
		List<OptionData> aod = Arrays.asList(hmOpt.values().toArray(new OptionData[0]));
		
		if(isSortAlphabetically()){
			ArrayList<OptionData> aodChildLess = new ArrayList<OptionData>();
			ArrayList<OptionData> aodHasChildren = new ArrayList<OptionData>();
			for(OptionData od:aod){
				if(od.isSection()){
					aodHasChildren.add(od);
				}else{
					aodChildLess.add(od);
					od.addCmdCfg(new CmdCfg("Cp") {@Override	public void execute(Button source) {
						JavaLangI.i().copyToClipboard(od.getVisibleText());
					}}.setHintHelp("copy visible text to clipboard"));
				}
			}
			Collections.sort(aodChildLess, sortAtoZ);
			Collections.sort(aodHasChildren, sortAtoZ);
			ArrayList<OptionData> aodAll = new ArrayList<OptionData>();
			aodAll.addAll(aodChildLess); //childless above
			aodAll.addAll(aodHasChildren);
			aod = aodAll;//.toArray(new OptionData[0]);
		}else{
			Collections.sort(aod, sortByChildAmount); //this will just put child-less above and keep insert order
		}
		
		for(OptionData od:aod){
			vlodOptions.add(od);
			if(od.isSection()){
				if(od.isExpanded()){
					recreateListItemsRecursively(od.hmNestedChildrenSubOptions,++iDepth);
				}
			}
		}
	}
	
	public Integer getSelectedOptionIndex(){
		Integer i = lstbxOptions.getSelectionModel().getSelection();
		if(i==null)return null;
		
		if(i>=vlodOptions.size()){
			i = vlodOptions.size()-1;
			lstbxOptions.getSelectionModel().setSelection(i);
			return i;
		}
		
		return lstbxOptions.getSelectionModel().getSelection();
	}
	
	public OptionData getSelectedOptionData(){
		Integer i = getSelectedOptionIndex();
		if(i==null){
			MessagesI.i().warnMsg(this, "nothing selected");
			return null;
		}
		return vlodOptions.get(i);
	}
	
	public String getSelectedOptionVisibleText(){
		return vlodOptions.get(getSelectedOptionIndex()).getVisibleText();
	}
	
	public Object getSelectedOptionValue(){
		int i=getSelectedOptionIndex();
		OptionData od = vlodOptions.get(i);
		if(od.isSection())return null;
//		Object obj = vlodOptions.get(i).getStoredValue();
//		if(obj instanceof SectionIndicator)return null;
//		return obj;
		return od.getStoredValue();
	}
	
	public void requestUpdateListItems(){
		bRequestUpdateListItems=true;
	}
	
	public void update(float tpf) {
		if(bKeepMaximized){
			SizeAndLocationI.i().maximize(getDialog());
		}
		
		if(bRequestSelectedToggleExpandedOnce){
			OptionData od = getSelectedOptionData();
			if(od!=null)od.toggleExpanded();
			bRequestUpdateListItems = true;
			bRequestSelectedToggleExpandedOnce=false;
		}
		
		if(bRequestUpdateListItems){
			recreateListItems();
			bRequestUpdateListItems=false;
		}
		
		/**
		 * the selection seems to only change after a mouse cursor click
		 */
		if(bRequestUpdateOptionSelected){
			updateOptionSelected();
			bRequestUpdateOptionSelected=false;
		}
		
		if(bRequestUserSubmitedInputValueApply){
			vhInputTextSubmitted.setObject(getInputText());
			for(IGUIUserInteraction l:auiLitenersList){
				l.receiveSubmitedUserInputTextEvent(getDialogVisuals(),getInputText());
			}
			bRequestUserSubmitedInputValueApply=false;
		}
		
		if(bReturnJustTheInputTextValue){
//			if(bRequestUserSubmitedInputValueApply){
			if(vrInputTextSubmittedApplyAsTypedTextChoice.update()){
				setChosenValue(getInputText());
//				bRequestUserSubmitedInputValueApply=false;
			}
		}else{ // set as soon an option is selected
			Integer i=getSelectedOptionIndex();
			if(i!=null){
				setChosenValue(getSelectedOptionValue());
			}
		}
		
		clearToolsSelection();
		
		if(isOptionSelected()){
			if(isCloseOnChoiceMade()){
				getDialog().close();
			}
		}
	}
	
	/**
	 * This is required to prevent being unable to click an already selected tool button option.
	 * Also, having them selected is pointless anyway.
	 */
	@Bugfix
	private void clearToolsSelection() {
		if(lstbxTools.getSelectionModel().getSelection()!=null){
			lstbxTools.getSelectionModel().setSelection(-1);
		}
	}

	private void updateOptionSelected() {
		OptionData od = getSelectedOptionData();
		if(od==null)return;
		
		if(od.isSection()){
			bRequestSelectedToggleExpandedOnce=true;
		}else{
			if(bCopyClickedOptionTextToInputField){
				tfInput.setText(od.getTextKey());
				setLastSelectedOptionStoredValue(od.getStoredValue());
			}
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
	public void resizableUpdatedLogicalStateEvent(float tpf,ResizablePanel rzp) {
		update(tpf);
	}

	protected void clearOptions(){
		hmOptionsRoot.clear();
		vlodOptions.clear();
		
		OptionDataDummy odd = new OptionDataDummy();
		put(null, odd.getTextKey(), odd); //the list cant remain empty
		
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(hmOptionsRoot.containsKey(odd.getTextKey()) || vlodOptions.contains(odd)){
					hmOptionsRoot.remove(odd.getTextKey());
					vlodOptions.remove(odd);
					return false; //look for more
				}
//				if(hmOptionsRoot.remove(odd.getTextKey())==null)return false; //retry
				return true;
			}
		}.setName("ClearDummyOption"));
	}

	public boolean isCloseOnChoiceMade() {
		return bCloseOnChoiceMade;
	}

	public void setCloseOnChoiceMade(boolean bCloseOnChoiceMade) {
		this.bCloseOnChoiceMade = bCloseOnChoiceMade;
	}

	public int getNestingStepDistance() {
		return iNestingStepDistance;
	}

	public void setNestingStepDistance(int iNestingStepDistance) {
		if(iNestingStepDistance<1)iNestingStepDistance=1;
		this.iNestingStepDistance = iNestingStepDistance;
	}

	public boolean isSortAlphabetically() {
		return bSortAlphabetically;
	}

	public void setSortAlphabetically(boolean bSortByChildAmount) {
		this.bSortAlphabetically = bSortByChildAmount;
	}
	
	public VersionedReference<String> createInputTextSubmitedVersionedReference(){
		return vhInputTextSubmitted.createReference();
	}
	
	@Override
	public String getTitle() {
		return btnTitleText.getText();
	}
	
	public void setTitle(String str){
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				if(btnTitleText==null)return false;
				
				btnTitleText.setText(str);
				
				MiscJmeI.i().addToName(getDialog(), str, true);
				
				return true;
			}
		});
	}
	
	public LinkedHashMapX<String,OptionData> createOptionDataSnapshot(){
//		ArrayList<OptionData> aodList = new ArrayList<OptionData>();
		LinkedHashMapX<String,OptionData> hmBackup = new LinkedHashMapX<String,OptionData>();
		doSomethingRecursivelyAtRoot(new Function<SimpleGenericDialog.OptionData, Boolean>() {
			@Override
			public Boolean apply(OptionData od) {
//				od.getAllChildrenRecursively(aodList);
				hmBackup.put(od.getTextKey(), od);
				return true;
			}
		});
//		return aodList;
		return hmBackup;
	}

	public boolean isCopyClickedOptionTextToInputField() {
		return bCopyClickedOptionTextToInputField;
	}

	public SimpleGenericDialog setCopyClickedOptionTextToInputField(
			boolean bCopyClickedOptionTextToInputField) {
		this.bCopyClickedOptionTextToInputField = bCopyClickedOptionTextToInputField;
		return this; 
	}

	public Object getLastSelectedOptionStoredValue() {
		return objLastSelectedOptionStoredValue;
	}

	public SimpleGenericDialog setLastSelectedOptionStoredValue(Object objLastSelectedOptionStoredValue) {
		this.objLastSelectedOptionStoredValue = objLastSelectedOptionStoredValue;
		for(IGUIUserInteraction l:auiLitenersList){
			l.receiveLastClickedItemStoredValueEvent(getDialogVisuals(),objLastSelectedOptionStoredValue);
		}
		return this; 
	}

}
