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
import java.util.HashMap;

import com.github.devconslejme.misc.Annotations.Bugfix;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.HWEnvironmentJmeI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.github.devconslejme.misc.jme.TextStringI;
import com.github.devconslejme.misc.lemur.SizeAndLocationI.EResizeApplyMode;
import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingVolume;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.core.GuiComponent;
import com.simsilica.lemur.core.GuiControl;
import com.simsilica.lemur.core.VersionedHolder;
import com.simsilica.lemur.core.VersionedReference;
import com.simsilica.lemur.event.CursorButtonEvent;
import com.simsilica.lemur.event.CursorEventControl;
import com.simsilica.lemur.event.CursorListener;
import com.simsilica.lemur.event.CursorMotionEvent;
import com.simsilica.lemur.style.Attributes;
import com.simsilica.lemur.style.ElementId;
import com.simsilica.lemur.style.StyleAttribute;
import com.simsilica.lemur.style.StyleDefaults;
import com.simsilica.lemur.style.Styles;

// (tab indent=2 spaces)

/**
 * As an impossible layout exception preventer workaround, 
 * this panel will test the parentest layout and if it fails, 
 * it will grow the parentest Panel size little by little,
 * until it works again.  
 * 
 * TODO ? create a component that allow setting each edge size independently, ex.: for inner panels with a single resizable edge only
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ResizablePanel extends PanelBase<ResizablePanel> {
  public static final String ELEMENT_ID = "resizablePanel";
	private BorderLayout layout;
	private Panel contents;
//	private static int iResizableBorderSizeDefault = 2;
	private static VersionedHolder<Integer> vhiResizableBorderSizeDefault = new VersionedHolder<Integer>(2);
	private Vector3f	v3fDragFromPrevious;
	private Vector3f	v3fMinSize = new Vector3f(0,0,0);
	private float fCornerHotSpotRange = 20;
	private ResizerCursorListener dcl = new ResizerCursorListener(this);
	private int	iMouseButtonIndexToDrag=0;
	private EEdge eeInitialHook = null;
//	private static int iGrowParentestFixAttemptLimitGlobal=100;
//	private Integer	iGrowParentestFixAttemptLimit=null;
//	private boolean	bEnabledGrowParentestFixAttemptLimit=false;
	private HashMap<EEdge,Edge> hmEdge = new HashMap<EEdge,Edge>();
	private ResizablePanel rzpParentest = null;
	private boolean bEnableResizing=true;
	private IBorderMargin irb = null;
	private boolean	bSkipGrowFix;
	private boolean	bUseSameBorderAtResizable = false;
	private boolean bUpdateLogicalStateSuccess = false;
//	private long lConsecutiveSuccessCount=0;
	private boolean	bApplyBoundingBoxSize = true;
	private Vector3f	v3fLastSucessSize=null;
	private boolean bMoveToCenterOnce=true;
	
//	public static interface IResizableListener {
//		//public static class VersionedVector3f extends Vector3f implements VersionedObject{}; //???
//		//VersionedReference<Vector3f> vrSize = new VersionedReference<Vector3f>(v3fMinSize); //???
//		public void attendToResizing(ResizablePanel source,Vector3f v3fNewSize);
//	}
//	private ArrayList<IResizableListener> airlList = new ArrayList<IResizableListener>();
	
	public static interface IResizableListener {
		/**
		 * it is meant to provide insta updates whenever changes happen to this panel.
		 */
		public void resizableUpdatedLogicalStateEvent(float tpf, ResizablePanel rzpSource);
		public void resizableRemovedFromParentEvent(ResizablePanel rzpSource);
		public void resizableStillResizingEvent(ResizablePanel rzpSource,Vector3f v3fNewSize);
		public void resizableEndedResizingEvent(ResizablePanel rzpSource);
//		public void closedEvent(ResizablePanel rzpSource);
	}
	private ArrayList<IResizableListener> listeners = new ArrayList<IResizableListener>();
	private CallableXAnon	cxNoWrap = new CallableXAnon() {
			@Override
			public Boolean call() {
				TextStringI.i().recursivelyApplyTextNoWrap(getContents());
				return true;
			}
		}
		.enableLoopMode()
		.setDelaySeconds(10) //will still keep running if contents change to fix them TODO 10s good to spare cpu?
		.setInitialDelay(1f) //good to let the panel be ready
		.setName("ApplyNoWrap") //TODO add parentest dialog title
		;
	
	public static interface IBorderMargin {
		public void setMargin(int i);
	}
	
	public static enum EEdge{
		// !!!!!!!!!!!!!!THIS ORDER IS IMPORTANT!!!!!!!!!!!!!!
		Dummy0(true),
		Top,         //1
		Right,       //2
		TopRight,    //3
		Left,        //4
		TopLeft,     //5
		Bottom,      //6
		Dummy7(true),
		BottomRight, //8
		Dummy9(true),
		BottomLeft,  //10
		// I said THIS ORDER!!! not disorder... :)
		;
		
		private boolean bDummy=false;
		private EEdge(){}
		private EEdge(boolean bDummy){
			this.bDummy=bDummy;
		}
		public boolean isDummy(){return bDummy;}
	} 
	private void initEdges(){
		for(EEdge e:EEdge.values()){
			if(!e.isDummy())hmEdge.put(e, new Edge(e));
		}
	}
	private void setCurrentEdgesPos(Vector3f v3fPos, Vector3f v3fSize){
		getEdge(EEdge.Top)				.set(								null, 					v3fPos.y);
		getEdge(EEdge.Bottom)			.set(								null, v3fPos.y-v3fSize.y);
		getEdge(EEdge.Left)				.set(						v3fPos.x, 							null);
		getEdge(EEdge.Right)			.set(	v3fPos.x+v3fSize.x, 							null);
		getEdge(EEdge.TopRight)		.setFrom(getEdge(EEdge.Top)		,getEdge(EEdge.Right)	);
		getEdge(EEdge.TopLeft)		.setFrom(getEdge(EEdge.Top)		,getEdge(EEdge.Left)	);
		getEdge(EEdge.BottomRight).setFrom(getEdge(EEdge.Bottom),getEdge(EEdge.Right)	);
		getEdge(EEdge.BottomLeft)	.setFrom(getEdge(EEdge.Bottom),getEdge(EEdge.Left)	);
	}
	
	public void setAllEdgesEnabled(boolean bEnabled){
		for(Edge edge:hmEdge.values()){
			edge.setEnabled(bEnabled);
		}
	}
	public static class Edge{
		private EEdge edge;
		private boolean bEnabled=true;
		private Float x,y;
		private float fMaxCurDistFromBorder = 10f;
		
		public Edge(EEdge edge) {
			this.edge=edge;
		}
		
		public void set(Float x,Float y){
			this.x=x;
			this.y=y;
		}
		private void setFrom(Edge ee1, Edge ee2) {
			x = useNotNull(ee1.x,ee2.x);
			y = useNotNull(ee1.y,ee2.y);
		}
		private Float useNotNull(Float f1, Float f2){
			return f1==null ? f2 : f1;
		}
		
		public Float getX(){return x;}
		public Float getY(){return y;}
		public float getMaxCurDistFromResizableBorder() {
			return fMaxCurDistFromBorder;
		}
		public void setMaxCurDistFromResizableBorder(float fMaxCurDistFromBorder) {
			this.fMaxCurDistFromBorder = fMaxCurDistFromBorder;
		}
		public boolean isNearEnough(Vector3f v3fCursor){
			switch(edge){
				case Top:
				case Bottom:
					return Math.abs(getY() - v3fCursor.y) < getMaxCurDistFromResizableBorder();
				case Left:
				case Right:
					return Math.abs(getX() - v3fCursor.x) < getMaxCurDistFromResizableBorder();
				case TopLeft:
				case TopRight:
				case BottomLeft:
				case BottomRight:
					return 	Math.abs(getY() - v3fCursor.y) < getMaxCurDistFromResizableBorder() &&
									Math.abs(getX() - v3fCursor.x) < getMaxCurDistFromResizableBorder();
			}
			throw new DetailedException("bug: "+this);
		}
		public boolean isEnabled() {
			return bEnabled;
		}
		public void setEnabled(boolean bEnabled) {
			this.bEnabled = bEnabled;
		}
	}
	
	public boolean isCursorInsidePanel(float fCursorX, float fCursorY){
		return isCursorInsidePanel(
			new Vector3f(fCursorX,fCursorY,0),
			new Vector3f(getWorldTranslation()), 
			new Vector3f(getPreferredSize())
		);
	}
	private boolean isCursorInsidePanel(Vector3f v3fCursor,Vector3f v3fPos,Vector3f v3fSize){
		boolean bCursorInsidePanel = true;
		if(			v3fCursor.x < v3fPos.x							)bCursorInsidePanel=false;
		else if(v3fCursor.x > v3fPos.x+v3fSize.x	)bCursorInsidePanel=false;
		else if(v3fCursor.y > v3fPos.y							)bCursorInsidePanel=false;
		else if(v3fCursor.y < v3fPos.y-v3fSize.y	)bCursorInsidePanel=false;
		return bCursorInsidePanel;
	}
	
	private void resizeThruDragging(float fCursorX, float fCursorY){
		Vector3f v3fCursor = new Vector3f(fCursorX,fCursorY,0);
		
		boolean bIsChildOfPanel = getParent() instanceof Panel;
		
		Vector3f v3fOldSize = new Vector3f(bIsChildOfPanel ? getSize() : getPreferredSize());
		
		Vector3f v3fOldPos = getWorldTranslation().clone();
				
		setCurrentEdgesPos(v3fOldPos, v3fOldSize);
		
		Vector3f v3fPanelCenter=v3fOldSize.divide(2f);
		
		Vector3f v3fPanelCenterOnAppScreen = getWorldTranslation().add(v3fPanelCenter.x,-v3fPanelCenter.y,0);
		
		EEdge ee=null;
		if(eeInitialHook==null){
			float fDistPanelCenterToCursorX = Math.abs(v3fCursor.x - v3fPanelCenterOnAppScreen.x);
			float fDistPanelCenterToCursorY = Math.abs(v3fCursor.y - v3fPanelCenterOnAppScreen.y); 
			float fMaxDistX = v3fPanelCenter.x;
			float fMaxDistY = v3fPanelCenter.y;
			float fDistToBorderX = fMaxDistX - fDistPanelCenterToCursorX;
			float fDistToBorderY = fMaxDistY - fDistPanelCenterToCursorY;
				
			EEdge eeX=null;
			if(fDistToBorderX < fCornerHotSpotRange){
				eeX = v3fCursor.x>v3fPanelCenterOnAppScreen.x ? EEdge.Right : EEdge.Left;
			}
			
			EEdge eeY=null;
			if(fDistToBorderY < fCornerHotSpotRange){
				eeY = v3fCursor.y>v3fPanelCenterOnAppScreen.y ? EEdge.Top : EEdge.Bottom;
			}
			
			if(eeX==null && eeY==null){
				MessagesI.i().warnMsg(this,"impossible condition: cursor at no edge?");
				return;
			}
			
			if(eeX==null){
				ee=eeY;
			}else
			if(eeY==null){
				ee=eeX;
			}else{
				ee = EEdge.values()[eeX.ordinal()+eeY.ordinal()];
			}
			
			if(hmEdge.get(ee).isEnabled()){
				eeInitialHook=ee;
			}else{
				return;
			}
		}else{
			ee=eeInitialHook;
		}
		
		////////////// resize and move
		Vector3f v3fNewPos = new Vector3f(getLocalTranslation());
		Vector3f v3fNewSize= new Vector3f(v3fOldSize);
		
		//Cursor Position: NEW          Previous
		float fDeltaX = v3fCursor.x - v3fDragFromPrevious.x; // positive to the right
		float fDeltaY = v3fCursor.y - v3fDragFromPrevious.y; // positive downwards
		
		switch(ee){
			case Top:
				fDeltaX=0;
				v3fNewSize.y+=fDeltaY;
				v3fNewPos.y+=fDeltaY;
				break;
			case TopRight:
				v3fNewSize.x+=fDeltaX;
				v3fNewSize.y+=fDeltaY;
				v3fNewPos.y+=fDeltaY;
				break;
			case TopLeft:
				v3fNewSize.x-=fDeltaX;
				v3fNewSize.y+=fDeltaY;
				v3fNewPos.x+=fDeltaX;
				v3fNewPos.y+=fDeltaY;
				break;
			case Right:
				fDeltaY=0;
				v3fNewSize.x+=fDeltaX;
				break;
			case Left:
				fDeltaY=0;
				v3fNewSize.x-=fDeltaX;
				v3fNewPos.x+=fDeltaX;
				break;
			case Bottom:
				fDeltaX=0;
				v3fNewSize.y-=fDeltaY;
				break;
			case BottomRight:
				v3fNewSize.x+=fDeltaX;
				v3fNewSize.y-=fDeltaY;
				break;
			case BottomLeft:
				v3fNewSize.x-=fDeltaX;
				v3fNewSize.y-=fDeltaY;
				v3fNewPos.x+=fDeltaX;
				break;
		}
		
		// constraint
		boolean bUpdateDragFromX=false;
		boolean bUpdateDragFromY=false;
		if(v3fNewSize.x<v3fMinSize.x){
			v3fNewSize.x=v3fOldSize.x;
			v3fNewPos.x=v3fOldPos.x;
		}else{
			bUpdateDragFromX=true;
		}
		
		if(v3fNewSize.y<v3fMinSize.y){
			v3fNewSize.y=v3fOldSize.y;
			v3fNewPos.y=v3fOldPos.y;
		}else{
			bUpdateDragFromY=true;
		}
		
		setPreferredSizeWH(v3fNewSize);
		if(validateParentest()){
			setLocalTranslationXY(v3fNewPos);
			
			if(bUpdateDragFromX)v3fDragFromPrevious.x+=fDeltaX;
			if(bUpdateDragFromY)v3fDragFromPrevious.y+=fDeltaY;
			
			if(!v3fNewSize.equals(v3fOldSize)){ 
//				resizedTo(v3fNewSize);
				for(IResizableListener iuls:listeners){
					iuls.resizableStillResizingEvent(this,v3fNewSize);
				}
				cxNoWrap.setRunImediatelyOnce();
			}
		}else{
			setPreferredSizeWH(v3fOldSize);
		}
		
	}
	
	/**
	 * override for further changes at subclass based on new size 
	 * @param v3fNewSize
	 */
//	protected void resizedTo(Vector3f v3fNewSize) {}

//	@CouldBeAGenericUtilMisc
//	private Panel getParentest(){
//		if(pnlParentest!=null)return pnlParentest;
//		
//		// find it
//		pnlParentest = this;
//		Node nodeParent = getParent();
//		while(nodeParent!=null){
//			if(nodeParent instanceof Panel){
//				pnlParentest=(Panel)nodeParent;
//			}
//			nodeParent=nodeParent.getParent();
//		}
//		
//		return pnlParentest;
//	}
	private ResizablePanel getParentest(){
		if(rzpParentest!=null)return rzpParentest;
		return SpatialHierarchyI.i().getParentest(this,ResizablePanel.class,true);
	}
	
	/**
	 * after resizing, if things change anywhere (even on childs) on the panel, it may break.
	 */
	@Workaround
	private void growParentestFixAttempt(int i){
		ResizablePanel pnlParentest = getParentest();
		
		Vector3f v3f = pnlParentest.getPreferredSize().add(new Vector3f(1, 1, 0));
		if(v3f.equals(new Vector3f(HWEnvironmentJmeI.i().getDisplay().getWidth(),HWEnvironmentJmeI.i().getDisplay().getHeight(),0))){
			SizeAndLocationI.i().safeSizeRecursively(EResizeApplyMode.Restore,pnlParentest);
			return;
		}
		
		if(v3f.x > HWEnvironmentJmeI.i().getDisplay().getWidth())v3f.x=HWEnvironmentJmeI.i().getDisplay().getWidth();
		if(v3f.y > HWEnvironmentJmeI.i().getDisplay().getHeight())v3f.y=HWEnvironmentJmeI.i().getDisplay().getHeight();
		
		MessagesI.i().warnMsg(this,"("+i+") increasing size of "+pnlParentest.getName()+" to "+v3f);
		
		pnlParentest.setPreferredSizeWH(v3f);
	}
	
//	private String strUDKeySafeSizeLast=ResizablePanel.class.getName()+"/SafeSize";
//	private String strUDKeySafeSizeDefault=ResizablePanel.class.getName()+"/SafeSizeDefault";
	
//	public static enum EReSizeApplyMode{
//		Save,
//		Restore,
//		RestoreDefault,
//		UpdateDefaultToCurrent,
//		;
//		public String s(){return toString();}
//	}
	
	public void applyCurrentSafeSizeAsDefault(){
		if(isUpdateLogicalStateSuccess()){
			SizeAndLocationI.i().safeSizeRecursively(EResizeApplyMode.UpdateDefaultToCurrent, this);
		}
	}
	
//	@CouldBeAGenericUtilMisc
//	private void safeSizeRecursively(EReSizeApplyMode eapply, Panel pnl) {
////		if(
////				eapply.equals(EReSizeApplyMode.UpdateDefaultToCurrent) || 
////				pnl.getUserData(strUDKeySafeSizeDefault)==null //initial will be first default
////		){ 
////			pnl.setUserData(strUDKeySafeSizeDefault, pnl.getPreferredSize());
////		}
//		
//		switch(eapply){
//			case Restore:{
//				Vector3f v3fSafeSize = (Vector3f)pnl.getUserData(strUDKeySafeSizeLast);
//				if(v3fSafeSize!=null)pnl.setPreferredSize(v3fSafeSize);
//			}break;
//			case RestoreDefault:{
//				Vector3f v3fSafeSize = (Vector3f)pnl.getUserData(strUDKeySafeSizeDefault);
//				if(v3fSafeSize!=null)pnl.setPreferredSize(v3fSafeSize);
//			}break;
//			case Save:{
//				pnl.setUserData(strUDKeySafeSizeLast,pnl.getPreferredSize());
//			}break;
//			case UpdateDefaultToCurrent:{
//				pnl.setUserData(strUDKeySafeSizeDefault, pnl.getPreferredSize());
//			}break;
//		}
//		
//		for(Spatial sptChild:pnl.getChildren()){
//			if (sptChild instanceof Panel) {
//				safeSizeRecursively(eapply,(Panel)sptChild);
//			}
//		}
//	}
	
	@Override
	public void updateLogicalState(float tpf) {
		if(bMoveToCenterOnce){
			moveToScreenCenterLater();
			bMoveToCenterOnce=false;
		}
		
//		if(true){updateLogicalState(tpf);return;}
		
//		if(getSize().length()>0 && getLocalTranslation().length()==0){
//			/**
//			 * if it's contents are ready (size),
//			 * and it's location was not set (ZERO), also means is outside the screen..
//			 */
//			MiscLemurI.i().moveToScreenCenterXY(this);
//		}
		
		if(bApplyBoundingBoxSize)applyBoundingBoxSizeAfterResizing();
		
		int iGrowParentFixCount=0;
		boolean bFirstSuccessOnChange=false;
		ResizablePanel rzpParentest = getParentest();
		while(true){
			try{
				super.updateLogicalState(tpf);
				
				if(v3fLastSucessSize==null){
					v3fLastSucessSize=getSize().clone();
				}else{
					if(!v3fLastSucessSize.equals(getSize())){
						v3fLastSucessSize.set(getSize());
						bFirstSuccessOnChange=true;
					}
				}
//				if(rzpParentest.v3fLastSucessSize==null){
//					rzpParentest.v3fLastSucessSize=getSize().clone();
//				}else{
//					if(!rzpParentest.v3fLastSucessSize.equals(getSize())){
//						rzpParentest.v3fLastSucessSize.set(getSize());
//						bFirstSuccessOnChange=true;
//					}
//				}
				
				bUpdateLogicalStateSuccess=true;
				break;
			}catch(IllegalArgumentException ex){
				bUpdateLogicalStateSuccess=false;
				
				if(bSkipGrowFix)throw ex;
				if(!ex.getMessage().startsWith("Size cannot be negative:")) throw ex;
				Vector3f v3fSize = rzpParentest.getSize();
				if(v3fSize.x==HWEnvironmentJmeI.i().getDisplay().getWidth() && v3fSize.y==HWEnvironmentJmeI.i().getDisplay().getHeight()){
					if(v3fLastSucessSize!=null){
						SizeAndLocationI.i().safeSizeRecursively(EResizeApplyMode.Restore,getParentest());
//						setPreferredSize(v3fLastSucessSize); //restore last
						v3fLastSucessSize=null; //one try only as something inside may have changed making it completely impossible to work
					}else{
						//prevents endless growth
						debugRecursiveChildPanelSizes("",getParentest());
						MessagesI.i().warnMsg(this,"maximum size limit reached during grow fix");
						throw ex;
					}
				}else{
//					v3fLastSucessSize=null;
					growParentestFixAttempt(iGrowParentFixCount); //TODO if it begins at max size, try to shrink?
					iGrowParentFixCount++;
				}
			}
		}
		
		if(bFirstSuccessOnChange){
			SizeAndLocationI.i().safeSizeInitialize(this);
			SizeAndLocationI.i().safeSizeRecursively(EResizeApplyMode.Save,getParentest());
		}
		
		for(IResizableListener iuls:listeners){
			iuls.resizableUpdatedLogicalStateEvent(tpf,this);
		}
	}
	
	private void debugRecursiveChildPanelSizes(String strIndent,Panel pnl){
		strIndent+="|";
		for(Spatial sptChild:pnl.getChildren()){
			if (sptChild instanceof Panel) {
				Panel pnlChild = (Panel) sptChild;
				MessagesI.i().warnMsg(this,strIndent+pnlChild.getName()+",p"+pnlChild.getPreferredSize()+",s"+pnlChild.getSize());
				debugRecursiveChildPanelSizes(strIndent,pnlChild);
			}
		}
	}
	
//	public void setEnabledGrowParentestFixAttemptLimit(boolean b){
//		this.bEnabledGrowParentestFixAttemptLimit=b;
//	}
	
	private boolean validateParentest() {
		boolean b=false;
		try{
			bSkipGrowFix=true;
			getParentest().updateLogicalState(0.001f); //TODO use current tpf? 
			b=true;
		}catch(Exception ex){
			MessagesI.i().warnMsg(this,"skipping impossible new size: "+ex.getMessage());
		}
		bSkipGrowFix=false;
		return b;
	}

	public static final String LAYER_RESIZABLE_BORDERS = "resizableBorders";
	
	public ResizablePanel(String strStyle) {
    super(false, new ElementId(ELEMENT_ID), strStyle);
    
   	setName(getName()+"/"+ResizablePanel.class.getSimpleName());
    
    initEdges();
    
    this.layout = new BorderLayout();
    getControl(GuiControl.class).setLayout(layout);
    
//    // initial simple size just to not be 0
//    //TODO initialize based on contents world boundings size when setting the contents or on a logical state update if the size is still length 0
//    float fWidth=100;
//    float fHeight=100;
//    getControl(GuiControl.class).setPreferredSize(new Vector3f(fWidth, fHeight, 0));
    
    // Set our layers
    getControl(GuiControl.class).setLayerOrder(LAYER_INSETS, 
                                               LAYER_BORDER, 
                                               LAYER_BACKGROUND,
                                               LAYER_RESIZABLE_BORDERS);
    
    Styles styles = GuiGlobals.getInstance().getStyles();
//    if(strStyle==null)strStyle=styles.getDefaultStyle();
    styles.applyStyles(this, getElementId(), strStyle);
    
    CursorEventControl.addListenersToSpatial(this, dcl);
    
//    moveToScreenCenterLater();
  }
	
	public void moveToScreenCenterLater(){
    QueueI.i().enqueue(new CallableXAnon() {
				@Override
				public Boolean call() {
					if(ResizablePanel.this.getParent()==null)return false; //wait 1st attach
					if(getSize().length()<=0)return false; //contents are not ready, are not updated by lemur yet
					if(getLocalTranslationXY().length()>0)return true; //location was already set, so ignore this initializer. 0,0,0 or not initialized, also means it is outside the screen..
					
					/**
					 * initial default location: centralized on the screen.
					 */
					SizeAndLocationI.i().moveToScreenCenterXY(ResizablePanel.this);
					
					return true;
				}
	    }.setDelaySeconds(1f) //wait a bit for its contents to accomodate TODO could check every frame if the size stopped changing and aply the location.
    );
	}
	
//  public ResizablePanel(Panel pnlContents) {
//  	this(pnlContents.getPreferredSize().x, pnlContents.getPreferredSize().y, pnlContents.getStyle());
//  	setContents(pnlContents);
//	}

	public static class ResizerCursorListener implements CursorListener{
		private ResizablePanel	rzp;

		public ResizerCursorListener(ResizablePanel rzp){
			this.rzp=rzp;
		}
		
		@Override
		public void cursorButtonEvent(CursorButtonEvent event, Spatial target,				Spatial capture) {
			if(capture!=rzp)return;
			
			if(event.getButtonIndex()!=rzp.iMouseButtonIndexToDrag)return;
			
			if(event.isPressed()){
				if(!rzp.isEnableResizing())return; //prevent only from starting resizing, so will have a clean ending
				
				rzp.v3fDragFromPrevious=(new Vector3f(event.getX(),event.getY(),0));
				event.setConsumed(); //acknoledges event absorption
			}else{
				// button UP ends all
				rzp.v3fDragFromPrevious=null;
				rzp.eeInitialHook=(null);
				for(IResizableListener iuls:rzp.listeners){
					iuls.resizableEndedResizingEvent(rzp);
				}
				rzp.cxNoWrap.setRunImediatelyOnce();
				event.setConsumed(); //this also prevents sending the event to other than this panel
			}
		}
		
  	@Override
  	public void cursorMoved(CursorMotionEvent event, Spatial target, Spatial capture) {
  		if(rzp.v3fDragFromPrevious!=null){
  			rzp.resizeThruDragging(event.getX(),event.getY());
  			event.setConsumed(); //acknoledges event absorption 
  		}
  	}

		@Override
		public void cursorEntered(CursorMotionEvent event, Spatial target,				Spatial capture) {
  		if(rzp.v3fDragFromPrevious==null){ //to help on pressing button on border
  		}
		}

		@Override
		public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {
		}
		
  }
  
  @StyleDefaults(ELEMENT_ID)
  public static void initializeDefaultStyles( Attributes attrs ) {
  	ColorRGBA color = ColorRGBA.Cyan.clone();
  	color.a=0.25f;
  	QuadBackgroundComponent qbc = new QuadBackgroundComponent(color);
  	qbc.setMargin(getResizableBorderSizeDefault(), getResizableBorderSizeDefault());
  	attrs.set( LAYER_RESIZABLE_BORDERS, qbc, false );
  }
	public static int getResizableBorderSizeDefault() {
		return vhiResizableBorderSizeDefault.getObject();
	}
	public static void setResizableBorderSizeDefault(int iBorderSizeDefault) {
		vhiResizableBorderSizeDefault.setObject(iBorderSizeDefault);
//		ResizablePanel.iResizableBorderSizeDefault = iBorderSizeDefault;
	}
	public static VersionedReference<Integer> getResizableBorderSizeDefaultVersionedReference(){
		return vhiResizableBorderSizeDefault.createReference();
	}
  
  /**
   * this is good to let the container surround the contents (that could be going beyond the borders limits)
   */
	private void applyBoundingBoxSizeAfterResizing() {
		if(v3fDragFromPrevious!=null)return; // ignore while being resized
		if(!bUpdateLogicalStateSuccess)return; //wait it be ready once at least
		
		BoundingVolume bv = getWorldBound();
		Vector3f v3fBB = ((BoundingBox)bv).getExtent(null).mult(2f);

		Vector3f v3fSize = getSize().clone();
//		Vector3f v3f = getPreferredSize().clone();
//		Vector3f v3fP = getPreferredSize().clone();
		boolean b=false;
		if(v3fSize.x<v3fBB.x){v3fSize.x=v3fBB.x;b=true;}
		if(v3fSize.y<v3fBB.y){v3fSize.y=v3fBB.y;b=true;}
		
		if(b)setPreferredSizeWH(v3fSize);
	}
	
	public void setResizableBorderSize(float x, float y){
		GuiComponent gc = getResizableBorder();
		if(gc instanceof QuadBackgroundComponent){
			((QuadBackgroundComponent)gc).setMargin(x, y);
		}else{
			MessagesI.i().warnMsg(this, "unable to set margin for", gc.getClass(), this);
		}
	}
	
	@StyleAttribute(value=LAYER_RESIZABLE_BORDERS, lookupDefault=false)
	public ResizablePanel setResizableBorder( GuiComponent bg ) {        
		if(!bUseSameBorderAtResizable){
			if(
				QuadBackgroundComponent.class.isInstance(bg) ||
				IBorderMargin.class.isInstance(bg)
			){
				setResizableBordersWork(bg);   
			}else{
				throw new UnsupportedOperationException("invalid border type");
			}
		}
		
		return this;
	}
	private ResizablePanel setResizableBordersWork( GuiComponent bg ) {
		getControl(GuiControl.class).setComponent(LAYER_RESIZABLE_BORDERS, bg);   
		return this;
	}
	
	@Override
	public void setBorder(GuiComponent bg) {
		super.setBorder(bg);
		if(bUseSameBorderAtResizable){
			setResizableBordersWork(bg.clone());
		}
	}
	
	public void setUseSameBorderAtResizable(boolean b){
		this.bUseSameBorderAtResizable=b;
	}
	
  public GuiComponent getResizableBorder() {
    return getControl(GuiControl.class).getComponent(LAYER_RESIZABLE_BORDERS);
  }
  
	/**
	 *  Resets the child contents that will be expanded/collapsed
	 *  with the rollup.
	 * @return 
	 */
	public ResizablePanel setContents( Panel p ) {
	    if( this.contents == p ) {
	        return this;
	    }
	    
	    if( this.contents != null) {
	        layout.removeChild(contents);
	    }
	    
	    this.contents = p;
	    if( this.contents != null ) {
	      if( contents.getParent() == null ) {
	        layout.addChild(contents,  BorderLayout.Position.Center);
	      }
	    }
	    
	    noWrapAllChildrenRecursiveLoopLater();
	    
			return this;
	}
	
	/**
	 * resizing, actually shrinking, may make the BitmapText children to wrap beyond its Panel
	 * bounding limits :(
	 */
	@Bugfix
	private void noWrapAllChildrenRecursiveLoopLater() {
		QueueI.i().enqueue(cxNoWrap);
	}
	
	public Panel getContents(){
		return contents;
	}
	
	public float getCornerHotSpotRange() {
		return fCornerHotSpotRange;
	}

	public ResizablePanel setCornerHotSpotRange(float fCornerHotSpotRange) {
		this.fCornerHotSpotRange = fCornerHotSpotRange;
		return this;
	}

	/**
	 * Is optional as new size will be validated and even post fixed.
	 * @param v3f
	 * @return
	 */
	public ResizablePanel setMinSize(Vector3f v3f){
		this.v3fMinSize=(v3f);
		return this;
	}
	
	public Vector3f getMinSize() {
		return v3fMinSize;
	}

	public int getMouseButtonIndex() {
		return iMouseButtonIndexToDrag;
	}
	
	@Override
	public void setSizeWH(Vector3f size) {
		super.setSizeWH(size);
		bUpdateLogicalStateSuccess=false; //requesting revalidation
	}
	
	@Override
	public void setPreferredSizeWH(Vector3f size) {
		super.setPreferredSizeWH(size);
		bUpdateLogicalStateSuccess=false; //requesting revalidation
	}
	
	/**
	 * the drag activator, defaults to left mouse button.
	 * @param iMouseButtonIndex
	 * @return 
	 */
	public ResizablePanel setMouseButtonIndex(int iMouseButtonIndex) {
		this.iMouseButtonIndexToDrag = iMouseButtonIndex;
		return this;
	}
	public Vector3f getCurrentDragFromLocation() {
		return v3fDragFromPrevious!=null?v3fDragFromPrevious.clone():null;
	}
	public Edge getDraggedEdge() {
		return hmEdge.get(eeInitialHook);
	}
	public ResizablePanel addResizableListener(IResizableListener listener) {
		if(listener==null)throw new DetailedException("invalid null listener");
		
		if(!listeners.contains(listener)){
			listeners.add(listener);
		}else{
			MessagesI.i().warnMsg(this,"listener already added "+listener);
		}
		
		return this;
	}
//	public ResizablePanel addResizableListener(IResizableListener listener) {
//		if(listener==null)throw new DetailedException("invalid null listener");
//		
//		if(!airlList.contains(listener)){
//			airlList.add(listener);
//		}else{
//			warnMsg("listener already added "+listener);
//		}
//		
//		return this;
//	}
	public Edge getEdge(EEdge edge) {
		return hmEdge.get(edge);
	}
	
//	@CouldBeAGenericUtilMisc
//	private void warnMsg(String str){
//		System.err.println("WARN["+this.getClass().getSimpleName()+"]: "+str+"; STE "+Thread.currentThread().getStackTrace()[2]); //TODO log?
//	}
	
//	public static int getGrowParentestFixAttemptLimitGlobal() {
//		return iGrowParentestFixAttemptLimitGlobal;
//	}
//	public static void setGrowParentestFixAttemptLimitGlobal(int iGrowParentestFixAttemptLimitGlobal) {
//		ResizablePanel.iGrowParentestFixAttemptLimitGlobal = iGrowParentestFixAttemptLimitGlobal;
//	}
	public boolean isUpdateLogicalStateSuccess() {
		return bUpdateLogicalStateSuccess;
	}
	
	public boolean isOpened(){
		return getParent()!=null;
	}
	
	public void close(){
		removeFromParent();
	}
	
	@Override
	public boolean removeFromParent() {
		boolean b = super.removeFromParent();
		if(b){
			for(IResizableListener iuls:listeners){
				iuls.resizableRemovedFromParentEvent(this);
			}
		}
		return b;
	}
	public boolean isApplyBoundingBoxSize() {
		return bApplyBoundingBoxSize;
	}
	public void setApplyContentsBoundingBoxSize(boolean bApplyBoundingBoxSize) {
		this.bApplyBoundingBoxSize = bApplyBoundingBoxSize;
	}
	
	public void restoreDefaultSafeSize() {
		SizeAndLocationI.i().safeSizeRecursively(EResizeApplyMode.RestoreDefault, this);
	}
	public boolean isEnableResizing() {
		return bEnableResizing;
	}
	public void setEnableResizing(boolean bEnableResizing) {
		this.bEnableResizing = bEnableResizing;
	}
	
}
