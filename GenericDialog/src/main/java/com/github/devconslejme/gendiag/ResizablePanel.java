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

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.core.GuiComponent;
import com.simsilica.lemur.core.GuiControl;
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
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ResizablePanel extends Panel {
  public static final String ELEMENT_ID = "resizablePanel";
	private BorderLayout layout;
	private Panel contents;
	private static int iBorderSizeDefault = 2;
	private int iBorderSize = iBorderSizeDefault;
	private Vector3f	v3fDragFromPrevious;
	private Vector3f	v3fMinSize = new Vector3f(0,0,0);
	private float fCornerHotSpotRange = 20;
	private ResizerCursorListener dcl = new ResizerCursorListener();
	private int	iMouseButtonIndexToDrag=0;
	private EEdge eeInitialHook = null;
//	private int	iBumpedBorderSize=7;
//	private boolean	bUseBumpBorderMode=false;
//	private boolean	bUsingBumpBorderMode=false;
//	private Integer	iBorderSizeBkp=null;
	private int	iGrowParentestFixAttemptLimit=100;
	private HashMap<EEdge,Edge> hmEdge = new HashMap<EEdge,Edge>();
//	private Vector3f	v3fNewPos = new Vector3f();
//	private Vector3f	v3fNewSize = new Vector3f();
	
//public static class VersionedVector3f extends Vector3f implements VersionedObject{}; //???
//VersionedReference<Vector3f> vrSize = new VersionedReference<Vector3f>(v3fMinSize); //???
	public static interface IResizableListener {
		public void resizedTo(Vector3f v3fNewSize);
	}
	private ArrayList<IResizableListener> airlList = new ArrayList<IResizableListener>();
	
	public static interface IBorderMargin {
		public void setMargin(int i);
	}
	private IBorderMargin irb = null;
	private boolean	bSkipGrowFix;
	private boolean	bUseSameBorderAtResizable = false;
	
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
		
		boolean bDummy=false;
		EEdge(){}
		EEdge(boolean bDummy){
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
			throw new NullPointerException("bug: "+this);
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
				warnMsg("impossible condition: cursor at no edge?");
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
		
		setPreferredSize(v3fNewSize);
		if(validateParentest()){
			setLocalTranslation(v3fNewPos);
			
			if(bUpdateDragFromX)v3fDragFromPrevious.x+=fDeltaX;
			if(bUpdateDragFromY)v3fDragFromPrevious.y+=fDeltaY;
			
			if(!v3fNewSize.equals(v3fOldSize)){
				resizedTo(v3fNewSize);
				for(IResizableListener irl:airlList){
					irl.resizedTo(v3fNewSize);
				}
			}
		}else{
			setPreferredSize(v3fOldSize);
		}
		
	}
	
	/**
	 * override for further changes at subclass based on new size 
	 * @param v3fNewSize
	 */
	protected void resizedTo(Vector3f v3fNewSize) {}

	Panel pnlParentest = null;
	private boolean	bEnabledGrowParentestFixAttemptLimit=false;
	private Panel getParentest(){ //TODO could be outside this class
		if(pnlParentest!=null)return pnlParentest;
		
		// find it
		pnlParentest = this;
		Node nodeParent = getParent();
		while(nodeParent!=null){
			if(nodeParent instanceof Panel){
				pnlParentest=(Panel)nodeParent;
			}
			nodeParent=nodeParent.getParent();
		}
		
		return pnlParentest;
	}
	
	/**
	 * after resizing, if things change anywhere (even on childs) on the panel, it may break.
	 */
	private void growParentestFixAttempt(int i){
		Panel pnlParentest = getParentest();
		Vector3f v3f = pnlParentest.getPreferredSize().add(new Vector3f(1, 1, 0));
		warnMsg("increasing ("+i+") size of "+pnlParentest.getName()+" to "+v3f);
		pnlParentest.setPreferredSize(v3f);
	}
	
	@Override
	public void updateLogicalState(float tpf) {
		int i=0;
		while(true){
			try{
				super.updateLogicalState(tpf);
				break;
			}catch(IllegalArgumentException ex){
				if(bSkipGrowFix)throw ex;
				if(!ex.getMessage().startsWith("Size cannot be negative:")) throw ex;
				if(bEnabledGrowParentestFixAttemptLimit && i>=iGrowParentestFixAttemptLimit){throw ex;} //prevents endless growth
				growParentestFixAttempt(i);
				i++;
			}
		}
	}
	
	public void setEnabledGrowParentestFixAttemptLimit(boolean b){
		this.bEnabledGrowParentestFixAttemptLimit=b;
	}
	
	private boolean validateParentest() {
		boolean b=false;
		try{
			bSkipGrowFix=true;
			getParentest().updateLogicalState(0.001f); //TODO use current tpf? 
			b=true;
		}catch(Exception ex){
			warnMsg("skipping impossible new size: "+ex.getMessage());
		}
		bSkipGrowFix=false;
		return b;
	}

	public static final String LAYER_RESIZABLE_BORDERS = "resizableBorders";
	
	public ResizablePanel(String strStyle) {
		this(100,100,strStyle);
	}
  public ResizablePanel( Vector3f v3fSize, String strStyle ) {
		this(v3fSize.x, v3fSize.y, strStyle);
	}
	public ResizablePanel( float fWidth, float fHeight, String strStyle ) {
    super(false, new ElementId(ELEMENT_ID), strStyle);
    
    initEdges();
    
    this.layout = new BorderLayout();
    getControl(GuiControl.class).setLayout(layout);
    
    getControl(GuiControl.class).setPreferredSize(new Vector3f(fWidth, fHeight, 0));
    
    // Set our layers
    getControl(GuiControl.class).setLayerOrder(LAYER_INSETS, 
                                               LAYER_BORDER, 
                                               LAYER_BACKGROUND,
                                               LAYER_RESIZABLE_BORDERS);
    
//    setResizableBorder(new QuadBackgroundComponent());
//    setResizableBorderMargin(iBorderSize); //to apply default
    
    Styles styles = GuiGlobals.getInstance().getStyles();
    styles.applyStyles(this, getElementId(), strStyle);
    
    CursorEventControl.addListenersToSpatial(this, dcl);
  }
	
  public ResizablePanel(Panel pnlContents) {
  	this(pnlContents.getPreferredSize().x, pnlContents.getPreferredSize().y, pnlContents.getStyle());
  	setContents(pnlContents);
//  	layout.addChild(pnl,  BorderLayout.Position.Center);
	}

	private class ResizerCursorListener implements CursorListener{
		@Override
		public void cursorButtonEvent(CursorButtonEvent event, Spatial target,				Spatial capture) {
			if(capture!=ResizablePanel.this)return;
			
			if(event.getButtonIndex()!=iMouseButtonIndexToDrag)return;
			
			if(event.isPressed()){
				v3fDragFromPrevious=(new Vector3f(event.getX(),event.getY(),0));
				event.setConsumed(); //acknoledges event absorption
			}else{
				// button UP ends all
				v3fDragFromPrevious=null;
				eeInitialHook=(null);
				event.setConsumed(); //this also prevents sending the event to other than this panel
//				resetBumpedResizableBorder();
			}
		}
		
  	@Override
  	public void cursorMoved(CursorMotionEvent event, Spatial target, Spatial capture) {
  		if(v3fDragFromPrevious!=null){
  			resizeThruDragging(event.getX(),event.getY());
//  			resetBumpedResizableBorder(); // must be here or the size of panel contents will be slightly different of the final one causing confusion
  			event.setConsumed(); //acknoledges event absorption 
  		}
  	}

		@Override
		public void cursorEntered(CursorMotionEvent event, Spatial target,				Spatial capture) {
  		if(v3fDragFromPrevious==null){ //to help on pressing button on border
//				if(isUseBumpResizableBorderMode()){
//					iBorderSizeBkp=iBorderSize;
//					setResizableBorderMargin(getBumpedResizableBorderSize());
//					bUsingBumpBorderMode=true;
//				}
  		}
		}

		@Override
		public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {
//			resetBumpedResizableBorder();
		}
		
  }
  
//	private void resetBumpedResizableBorder(){
//		if(bUsingBumpBorderMode){
//			setResizableBorderMargin(iBorderSizeBkp);
//			iBorderSizeBkp=null;
//			bUsingBumpBorderMode=false;
//		}
//	}
	
  @StyleDefaults(ELEMENT_ID)
  public static void initializeDefaultStyles( Attributes attrs ) {
  	ColorRGBA color = ColorRGBA.Cyan.clone();
  	color.a=0.25f;
  	QuadBackgroundComponent qbc = new QuadBackgroundComponent(color);
  	qbc.setMargin(iBorderSizeDefault, iBorderSizeDefault);
  	attrs.set( LAYER_RESIZABLE_BORDERS, qbc, false );
  }
  
	@StyleAttribute(value=LAYER_RESIZABLE_BORDERS, lookupDefault=false)
	public ResizablePanel setResizableBorder( GuiComponent bg ) {        
//		getControl(GuiControl.class).setComponent(LAYER_RESIZABLE_BORDERS, bg);   
//		return this;
//	}
//	@Override
//	public void setBorder(GuiComponent bg) {
		if(!bUseSameBorderAtResizable){
			if(
				QuadBackgroundComponent.class.isInstance(bg) ||
				IBorderMargin.class.isInstance(bg)
			){
				setResizableBordersWork(bg);   
	//			super.setBorder(bg);
			}else{
				throw new UnsupportedOperationException("invalid border type");
			}
		}
		
		return this;
	}
	private ResizablePanel setResizableBordersWork( GuiComponent bg ) {
		getControl(GuiControl.class).setComponent(LAYER_RESIZABLE_BORDERS, bg);   
//		setResizableBorderMargin(iBorderSize);
		return this;
	}
	
	@Override
	public void setBorder(GuiComponent bg) {
		super.setBorder(bg);
//		if(getControl(GuiControl.class).getComponent(LAYER_RESIZABLE_BORDERS)==null){
		if(bUseSameBorderAtResizable){
			setResizableBordersWork(bg.clone());
		}
	}
	
	public void setUseSameBorderAtResizable(boolean b){
		this.bUseSameBorderAtResizable=b;
	}
	
//	/**
//	 * TODO create a component that allow setting each edge size independently
//	 * @param i
//	 * @return
//	 */
//	public ResizablePanel setResizableBorderMargin(int i){
//		this.iBorderSize=(i);
//		
//		GuiComponent border = getBorder();
//		if(border instanceof IBorderMargin){
//			irb.setMargin(i);
//		}else
//		if(border instanceof QuadBackgroundComponent){
//			((QuadBackgroundComponent)border).setMargin(this.iBorderSize, this.iBorderSize);
//		}
//		
//		return this;
//	}
	
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
			return this;
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

	public int getResizableBorderSize() {
		return iBorderSize;
	}
	
//	@Override
//	public QuadBackgroundComponent getBorder() {
//		return (QuadBackgroundComponent) super.getBorder();
//	}
	
//	public static class IndependentBordersQuadBackgroundComponent extends QuadBackgroundComponent{
//		@Override
//		public void calculatePreferredSize(Vector3f size) {
//			super.calculatePreferredSize(size);
//		}
//	}
	
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
//	public boolean isUseBumpResizableBorderMode() {
//		return bUseBumpBorderMode;
//	}
//	public ResizablePanel setUseBumpResizableBorderMode(boolean bUseBumpBorderMode) {
//		this.bUseBumpBorderMode = bUseBumpBorderMode;
//		return this;
//	}
//	public int getBumpedResizableBorderSize() {
//		return iBumpedBorderSize;
//	}
//	public ResizablePanel setBumpedResizableBorderSize(int iBumpedBorderSize) {
//		this.iBumpedBorderSize = iBumpedBorderSize;
//		return this;
//	}
	public ResizablePanel addResizableListener(IResizableListener irl) {
		if(irl==null)throw new NullPointerException("invalid null listener");
		
		if(!airlList.contains(irl)){
			airlList.add(irl);
		}else{
			warnMsg("listener already added "+irl);
		}
		
		return this;
	}
	public Edge getEdge(EEdge edge) {
		return hmEdge.get(edge);
	}
	
	private void warnMsg(String str){ //TODO could be outside this class
		System.err.println("WARN["+this.getClass().getSimpleName()+"]: "+str); //TODO log?
	}
}
