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

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
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
import com.simsilica.lemur.style.StyleAttribute;
import com.simsilica.lemur.style.StyleDefaults;

// (tab indent=2 spaces)

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PanelResizableEnhancer {
	public interface IResizableListener {
		public void resizedTo(Vector3f v3fNewSize);
	}
	
	private Panel	panel;

	private BorderLayout layout;
	private Panel contents;
	private int iBorderSize = 4;
	private QuadBackgroundComponent	qbcBorder = new QuadBackgroundComponent();
	private Vector3f	v3fDragFromPrevious;
	private Vector3f	v3fMinSize = new Vector3f(40,40,0);
	private float fCornerHotSpotRange = 20;
	private ResizerCursorListener dcl = new ResizerCursorListener();
	private int	iMouseButtonIndex=0;

	private IResizableListener	irl;
	
	PanelResizableEnhancer(Panel panel, IResizableListener irl){
		this.panel=panel;
		this.irl=irl;
		initPanel();
	}
	
  public void initPanel() {
    this.layout = new BorderLayout();
    panel.getControl(GuiControl.class).setLayout(layout);
    
    // Set our layers
    panel.getControl(GuiControl.class).setLayerOrder(Panel.LAYER_INSETS, 
    	Panel.LAYER_BORDER, 
    	Panel.LAYER_BACKGROUND,
  		LAYER_RESIZABLE_BORDERS);
    
    panel.setBorder(qbcBorder);
    setBorderSize(iBorderSize); //to apply default
    
    CursorEventControl.addListenersToSpatial(panel, dcl);
  }
  
	public static enum EEdge{
		// !!!!!!!!!!!!!!THIS ORDER IS IMPORTANT!!!!!!!!!!!!!!
		Dummy0,
		Top,         //1
		Right,       //2
		TopRight,    //3
		Left,        //4
		TopLeft,     //5
		Bottom,      //6
		Dummy7,
		BottomRight, //8
		Dummy9,
		BottomLeft,  //10
		// I said THIS ORDER!!! not disorder... :)
		;
		
		Float x,y;
		private float fMaxCurDistFromBorder = 10f;
		
		public void set(Float x,Float y){
			this.x=x;
			this.y=y;
		}
		private void setFrom(EEdge ee1, EEdge ee2) {
			x = useNotNull(ee1.x,ee2.x);
			y = useNotNull(ee1.y,ee2.y);
		}
		private Float useNotNull(Float f1, Float f2){
			return f1==null ? f2 : f1;
		}
		private static void setCurrentEdgesPos(Vector3f v3fPos, Vector3f v3fSize){
			Top			.set(								null, 					v3fPos.y);
			Bottom	.set(								null, v3fPos.y-v3fSize.y);
			Left		.set(						v3fPos.x, 							null);
			Right		.set(	v3fPos.x+v3fSize.x, 							null);
			TopRight		.setFrom(Top,Right);
			TopLeft			.setFrom(Top,Left);
			BottomRight	.setFrom(Bottom,Right);
			BottomLeft	.setFrom(Bottom,Left);
		}
		
		public Float getX(){return x;}
		public Float getY(){return y;}
		public float getMaxCurDistFromBorder() {
			return fMaxCurDistFromBorder;
		}
		public void setMaxCurDistFromBorder(float fMaxCurDistFromBorder) {
			this.fMaxCurDistFromBorder = fMaxCurDistFromBorder;
		}
		public boolean isNearEnough(Vector3f v3fCursor){
			switch(this){
				case Top:
				case Bottom:
					return Math.abs(getY() - v3fCursor.y) < getMaxCurDistFromBorder();
				case Left:
				case Right:
					return Math.abs(getX() - v3fCursor.x) < getMaxCurDistFromBorder();
				case TopLeft:
				case TopRight:
				case BottomLeft:
				case BottomRight:
					return 	Math.abs(getY() - v3fCursor.y) < getMaxCurDistFromBorder() &&
									Math.abs(getX() - v3fCursor.x) < getMaxCurDistFromBorder();
			}
			throw new NullPointerException("bug: "+this);
		}
		
	} 
	
	EEdge eeInitialHook = null;
	
	public boolean isCursorInsidePanel(float fCursorX, float fCursorY){
		return isCursorInsidePanel(
			new Vector3f(fCursorX,fCursorY,0),
			new Vector3f(panel.getWorldTranslation()), 
			new Vector3f(panel.getPreferredSize())
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
		
		Vector3f v3fOldPos=panel.getWorldTranslation().clone();
		Vector3f v3fOldSize = new Vector3f(panel.getPreferredSize());
		EEdge.setCurrentEdgesPos(v3fOldPos, v3fOldSize);
		
		Vector3f v3fPanelCenter=v3fOldSize.divide(2f);
		
		Vector3f v3fPanelCenterOnAppScreen=panel.getWorldTranslation().add(
			v3fPanelCenter.x,-v3fPanelCenter.y,0);
		
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
			
			if(eeX==null && eeY==null)throw new NullPointerException("impossible condition: cursor at no edge?");
			
			if(eeX==null){
				ee=eeY;
			}else
			if(eeY==null){
				ee=eeX;
			}else{
				ee = EEdge.values()[eeX.ordinal()+eeY.ordinal()];
			}
			
			eeInitialHook=ee;
		}else{
			ee=eeInitialHook;
		}
		
		////////////// resize and move
		Vector3f v3fNewPos = panel.getLocalTranslation().clone();
		Vector3f v3fNewSize = v3fOldSize.clone();
		
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
		if(v3fNewSize.x<getMinSize().x){
			v3fNewSize.x=v3fOldSize.x;
			v3fNewPos.x=v3fOldPos.x;
		}else{
			v3fDragFromPrevious.x+=fDeltaX;
		}
		
		if(v3fNewSize.y<getMinSize().y){
			v3fNewSize.y=v3fOldSize.y;
			v3fNewPos.y=v3fOldPos.y;
		}else{
			v3fDragFromPrevious.y+=fDeltaY;
		}
		
		panel.setPreferredSize(v3fNewSize);
		panel.setLocalTranslation(v3fNewPos);
		
		if(irl!=null)irl.resizedTo(v3fNewSize);
	}
	
	public static final String LAYER_RESIZABLE_BORDERS = "resizableBorders";
	
  private class ResizerCursorListener implements CursorListener{
		@Override
		public void cursorButtonEvent(CursorButtonEvent event, Spatial target,				Spatial capture) {
			if(capture!=panel)return;
			
			if(event.getButtonIndex()!=iMouseButtonIndex)return;
			
			if(event.isPressed()){
				v3fDragFromPrevious=new Vector3f(event.getX(),event.getY(),0);
				event.setConsumed(); //acknoledges event absorption
			}else{
				// button UP ends all
				v3fDragFromPrevious=null;
				eeInitialHook=null;
				event.setConsumed(); //this also prevents sending the event to other than this panel
			}
		}
		
  	@Override
  	public void cursorMoved(CursorMotionEvent event, Spatial target, Spatial capture) {
  		if(v3fDragFromPrevious!=null){
  			resizeThruDragging(event.getX(),event.getY());
  			event.setConsumed(); //acknoledges event absorption 
  		}
  	}

		@Override
		public void cursorEntered(CursorMotionEvent event, Spatial target,				Spatial capture) {
		}

		@Override
		public void cursorExited(CursorMotionEvent event, Spatial target,				Spatial capture) {
		}
		
  }
  
  @StyleDefaults("resizablePanel")
  public static void initializeDefaultStyles( Attributes attrs ) {
      attrs.set( "resizableBorders", new QuadBackgroundComponent(ColorRGBA.Gray), false );
  }
  
	@StyleAttribute(value="resizableBorders", lookupDefault=false)
	public void setResizableBorders( GuiComponent bg ) {        
		panel.getControl(GuiControl.class).setComponent(LAYER_RESIZABLE_BORDERS, bg);   
	}
	
  public GuiComponent getResizableBorders() {
    return panel.getControl(GuiControl.class).getComponent(LAYER_RESIZABLE_BORDERS);
  }
  
	/**
	 *  Resets the child contents that will be expanded/collapsed
	 *  with the rollup.
	 */
	public void setContents( Panel p ) {
	    if( this.contents == p ) {
	        return;
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
	}
	
	public float getCornerHotSpotRange() {
		return fCornerHotSpotRange;
	}

	public void setCornerHotSpotRange(float fCornerHotSpotRange) {
		this.fCornerHotSpotRange = fCornerHotSpotRange;
	}

	public int getBorderSize() {
		return iBorderSize;
	}

	public void setBorderSize(int i){
		this.iBorderSize=(i);
    qbcBorder.setMargin(this.iBorderSize, this.iBorderSize);
	}

	public void setMinSize(Vector3f v3f){
		this.v3fMinSize=(v3f);
	}
	
	public Vector3f getMinSize() {
		return v3fMinSize;
	}

	public int getMouseButtonIndex() {
		return iMouseButtonIndex;
	}
	
	/**
	 * the drag activator, defaults to left mouse button.
	 * @param iMouseButtonIndex
	 */
	public void setMouseButtonIndex(int iMouseButtonIndex) {
		this.iMouseButtonIndex = iMouseButtonIndex;
	}
	
}
