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

import com.github.devconslejme.misc.GlobalInstanceManagerI;
import com.jme3.bounding.BoundingBox;
import com.jme3.font.BitmapText;
import com.jme3.font.LineWrapMode;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * DevSelfNote: Misc lib class should not exist. As soon coehsion is possible, do it!
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MiscJmeI {
	public static MiscJmeI i(){return GlobalInstanceManagerI.i().get(MiscJmeI.class);}
	
	@SuppressWarnings({ "unchecked" })
	public <T extends Node> T getParentest(Spatial spt, Class<T> clTypeParentest, boolean bIncludeFirst){
		T parentest = null;
		if(bIncludeFirst && clTypeParentest.isInstance(spt))parentest=(T)spt;
		
		Node nodeParent = spt.getParent();
		while(nodeParent!=null){
			if(clTypeParentest.isInstance(nodeParent)){
				parentest=(T)nodeParent;
			}
			nodeParent=nodeParent.getParent();
		}
		
		return parentest;
	}
	
	public Vector3f getBoundingBoxSize(Spatial spt){
		return ((BoundingBox)spt.getWorldBound()).getExtent(null).mult(2f);
	}
	
	public void recursivelyApplyTextNoWrap(Node nodeParent) {
		for(Spatial spt:nodeParent.getChildren()){
			if(spt instanceof BitmapText){
				((BitmapText)spt).setLineWrapMode(LineWrapMode.NoWrap);
			}
			if(spt instanceof Node){
				recursivelyApplyTextNoWrap((Node)spt);
			}
		}
	}
}
