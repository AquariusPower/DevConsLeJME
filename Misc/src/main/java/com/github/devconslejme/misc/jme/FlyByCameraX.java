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

import com.jme3.input.FlyByCamera;
import com.jme3.renderer.Camera;

/**
 * TODO complete to be attachable to player's head or flying ship (limiting/requesting(listener?) the movement etc..)
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class FlyByCameraX extends FlyByCamera{
	private boolean	bAllowZooming=false;
	private boolean	bAllowMove=false;

	public FlyByCameraX(Camera cam) {
		super(cam);
	}
	
	@Override
	protected void zoomCamera(float value) {
		if(!isAllowZooming())return;
		super.zoomCamera(value);
	}
	
	@Override
	protected void moveCamera(float value, boolean sideways) {
		if(!isAllowMove())return;
		super.moveCamera(value, sideways);
	}
	
	@Override
	protected void riseCamera(float value) {
		if(!isAllowMove())return;
		super.riseCamera(value);
	}
	
	public boolean isAllowZooming() {
		return bAllowZooming;
	}
	
	/**
	 * false if is not using zooming equipment (or is not an eagle)
	 * @param bAllowZooming
	 * @return
	 */
	public FlyByCameraX setAllowZooming(boolean bAllowZooming) {
		this.bAllowZooming = bAllowZooming;
		return this; //for beans setter
	}
	
	public boolean isAllowMove() {
		return bAllowMove;
	}
	
	/**
	 * false when attached at the head of the player's entity 
	 * @param bAllowMove 
	 * @return
	 */
	public FlyByCameraX setAllowMove(boolean bAllowMove) {
		this.bAllowMove = bAllowMove;
		return this; //for beans setter
	}
}
