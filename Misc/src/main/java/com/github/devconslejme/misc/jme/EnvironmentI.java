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
package com.github.devconslejme.misc.jme;

import java.util.ArrayList;

import org.lwjgl.input.Mouse;
import org.lwjgl.opengl.Display;

import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.app.Application;
import com.jme3.app.state.AbstractAppState;
import com.jme3.math.Vector3f;

/**
 * This way lwjgl3 may replace lwjgl more easily... or any other ways to collect the required values
 * can be used.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class EnvironmentI extends AbstractAppState{
	public static EnvironmentI i(){return GlobalManagerI.i().get(EnvironmentI.class);}
	
	public void configure(){
		GlobalManagerI.i().get(Application.class).getStateManager().attach(this);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		
		if(getDisplay().wasResized()){
			for(IEnvironmentListener l:alisteners){
				l.displayResizedEvent(getDisplay().getWidth(), getDisplay().getHeight());
			}
		}
	}
	
	public static interface IEnvironmentListener{
		void displayResizedEvent(int iW, int iH);
	}
	private ArrayList<IEnvironmentListener> alisteners = new ArrayList<IEnvironmentListener>();
	public void addListener(IEnvironmentListener l){
		if(!alisteners.contains(l))alisteners.add(l);
	}
	
	public static class DisplayI{
		public int getWidth(){
			return Display.getWidth();
		}
		
		public int getHeight(){
			return Display.getHeight();
		}
		
		public void setResizable(boolean b){
			Display.setResizable(b);
		}
		public boolean isResizable(){
			return Display.isResizable();
		}
		
		public boolean wasResized(){
			return Display.wasResized();
		}		
	}
	private DisplayI display = new DisplayI();
	public DisplayI getDisplay(){
		return display;
	}

	public static class MouseI{
		public boolean isButtonDown(int i){
			return Mouse.isButtonDown(i);
		}
		
		public Vector3f get3DPos() {
//			return MiscJmeI.i().toV3f(app.getInputManager().getCursorPosition(), MiscJmeI.i().getZAboveAllAtGuiNode());
			return new Vector3f(Mouse.getX(), Mouse.getY(), MiscJmeI.i().getZAboveAllAtGuiNode());
		}
		
		public int isMouseCursorPressedButtons(){
			int i2=0;
			for(int i=0;i<9;i++){
				if(isButtonDown(i))i2++;
			}
	    return i2;
		}
	}
	private MouseI mouse = new MouseI();
	public MouseI getMouse() {
		return mouse;
	}
	
}
