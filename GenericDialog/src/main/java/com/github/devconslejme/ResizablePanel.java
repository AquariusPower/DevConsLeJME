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
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.component.BorderLayout;
import com.simsilica.lemur.component.QuadBackgroundComponent;
import com.simsilica.lemur.core.GuiComponent;
import com.simsilica.lemur.core.GuiControl;
import com.simsilica.lemur.dnd.DragAndDropControl;
import com.simsilica.lemur.dnd.DragAndDropListener;
import com.simsilica.lemur.dnd.DragEvent;
import com.simsilica.lemur.dnd.DragStatus;
import com.simsilica.lemur.dnd.Draggable;
import com.simsilica.lemur.style.Attributes;
import com.simsilica.lemur.style.ElementId;
import com.simsilica.lemur.style.StyleAttribute;
import com.simsilica.lemur.style.StyleDefaults;
import com.simsilica.lemur.style.Styles;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ResizablePanel extends Panel implements Draggable {
	private BorderLayout layout;
	private Panel contents;
	private int iBorderSize = 3;
	private QuadBackgroundComponent	qbcBorder = new QuadBackgroundComponent();
//	private ResizerCursorListener	clResizer = new ResizerCursorListener();
//	class ResizerCursorListener implements CursorListener{}
	
	private ResizerDragAndDropListener	dndlCursorListener = new ResizerDragAndDropListener();
	public Vector3f	v3fDragFromPrevious;
	private Vector3f	v3fMinSize = new Vector3f(20,20,0);
	enum EEdge{
		Left,
		Right,
		
		Top, //THIS ORDER MATTERS!
		TopRight, //THIS ORDER MATTERS!
		TopLeft, //THIS ORDER MATTERS!
		
		Bottom, //THIS ORDER MATTERS!
		BottomRight, //THIS ORDER MATTERS!
		BottomLeft, //THIS ORDER MATTERS!
		;
	}
	class ResizerDragAndDropListener implements DragAndDropListener{
		@Override
		public Draggable onDragDetected(DragEvent event) {
			if(event.getTarget() instanceof Draggable){
				return (Draggable)event.getTarget();
			}
			return null;
		}

		@Override
		public void onDragEnter(DragEvent event) {
			if(event.getTarget()==ResizablePanel.this){
				v3fDragFromPrevious = event.getCollision().getContactPoint();
			}
		}

		@Override
		public void onDragExit(DragEvent event) {
			if(event.getTarget()==ResizablePanel.this){
				event.getClass(); //TODO rm tmp debug breakpoint
			}
		}
		
		@Override
		public void onDragOver(DragEvent event) {
			if(event.getTarget()==ResizablePanel.this){
				Vector3f v3fOldSize = new Vector3f(ResizablePanel.this.getPreferredSize());
				Vector3f v3fPanelCenter=v3fOldSize.divide(2f);
				Vector3f v3fPanelCenterOnApp=ResizablePanel.this.getLocalTranslation().add(
					v3fPanelCenter.x,-v3fPanelCenter.y,0);
				
				Vector3f v3fNewSize = v3fOldSize.clone();
				
				//                  NEW                OLD
				float fDeltaX = event.getX() - v3fDragFromPrevious.x; // positive to the right
				float fDeltaY = event.getY() - v3fDragFromPrevious.y; // positive downwards
				
				if(fDeltaX!=0 || fDeltaY!=0){
					event.getClass(); //TODO rm tmp debug breakpoint
				}
				
				EEdge ee=null;
				if(event.getY()>v3fPanelCenterOnApp.y){ee=EEdge.Top;}else{ee=EEdge.Bottom;}
				if(event.getX()>v3fPanelCenterOnApp.x){
					ee=EEdge.values()[ee.ordinal()+1]; //Right
				}else{
					ee=EEdge.values()[ee.ordinal()+2]; //Left
				}
				
				Vector3f v3fNewPos = ResizablePanel.this.getLocalTranslation().clone();
				switch(ee){
					case Left:
						fDeltaY=0;
						break;
					case Right:
						fDeltaY=0;
						break;
						
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
				
				v3fDragFromPrevious.x+=fDeltaX;
				v3fDragFromPrevious.y+=fDeltaY;
				
				// constraint
				if(v3fNewSize.x<v3fMinSize.x)v3fNewSize.x=v3fMinSize.x;
				if(v3fNewSize.y<v3fMinSize.y)v3fNewSize.y=v3fMinSize.y;
				
				ResizablePanel.this.setPreferredSize(v3fNewSize);
				ResizablePanel.this.setLocalTranslation(v3fNewPos);
				
				event.getClass();
			}
		}

		@Override
		public void onDrop(DragEvent event) {
			if(event.getTarget()==ResizablePanel.this){
				v3fDragFromPrevious=null;
			}
		}

		@Override
		public void onDragDone(DragEvent event) {
			if(event.getTarget()==ResizablePanel.this){
				event.getClass();//TODO rem
			}
		}
	}
	
	public static final String LAYER_RESIZABLE_BORDERS = "resizableBorders";
	
  public ResizablePanel( float width, float height, String style ) {
    super(false, new ElementId("resizablePanel"), style);
    
    this.layout = new BorderLayout();
    getControl(GuiControl.class).setLayout(layout);
    
    getControl(GuiControl.class).setPreferredSize(new Vector3f(width, height, 0));
    
    // Set our layers
    getControl(GuiControl.class).setLayerOrder(LAYER_INSETS, 
                                               LAYER_BORDER, 
                                               LAYER_BACKGROUND,
                                               LAYER_RESIZABLE_BORDERS);
    
    setBorder(qbcBorder);
    setBorderSize(iBorderSize);
    
    Styles styles = GuiGlobals.getInstance().getStyles();
    styles.applyStyles(this, getElementId().getId(), style);
    
    addControl(new DragAndDropControl(dndlCursorListener));
  }
	
  @StyleDefaults("resizablePanel")
  public static void initializeDefaultStyles( Attributes attrs ) {
      attrs.set( "resizableBorders", new QuadBackgroundComponent(ColorRGBA.Gray), false );
  }
  
	@StyleAttribute(value="resizableBorders", lookupDefault=false)
	public void setResizableBorders( GuiComponent bg ) {        
	    getControl(GuiControl.class).setComponent(LAYER_RESIZABLE_BORDERS, bg);   
	}
	
  public GuiComponent getResizableBorders() {
    return getControl(GuiControl.class).getComponent(LAYER_RESIZABLE_BORDERS);
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
	
	public void setMinSize(Vector3f v3f){
		this.v3fMinSize=v3f;
	}
	
	public void setBorderSize(int i){
		this.iBorderSize=i;
    qbcBorder.setMargin(iBorderSize, iBorderSize);
	}

	@Override
	public void setLocation(float x, float y) {
	}

	@Override
	public Vector2f getLocation() {
		return null;
	}

	@Override
	public void updateDragStatus(DragStatus status) {
	}

	@Override
	public void release() {
	}
	
}
