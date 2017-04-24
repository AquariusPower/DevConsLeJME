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

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public interface IEffect<THIS extends IEffect>{
	public boolean isPlaying();
	
	public boolean isWaitingParent();
	THIS setWaitParent(boolean bWaitParentBeSet);
	
	public Vector3f getLocationFrom();

	public Vector3f getLocationTo();

	void assertNotDiscarded();
	
	/**
	 * where the effect geometry will be attached, can be initially null
	 * @param sptOwner
	 * @return
	 */
	THIS setOwner(Spatial sptOwner);
	
	THIS setColor(ColorRGBA colorRef);

	THIS setFromTo(Vector3f v3fFrom, Vector3f v3fTo);

	THIS setNodeParent(Node node);

	Node getNodeParent();
	
	THIS setFollowToMouse(boolean b);

	THIS setFollowToTarget(Spatial spt, Vector3f v3fDisplacement);

	THIS setFollowFromTarget(Spatial spt, Vector3f v3fDisplacement);
	
	THIS useFollowToPosZ(); //TODO make a general setter to use X, Y or Z, and chose to use "From" or "To" as reference
	boolean isUseFollowToPosZ();
	
	THIS getThis();
	
	THIS clone();
	
//		public String getUId();
	
	public void assertConfigIsValidAndFixIt();
	
	public void play(float tpf);
	
	public Object getOwner();
	
	public THIS setSkipDiscardingByOwner();

	public THIS setPlay(boolean b);
	
	public THIS setAsDiscarded();

	public boolean isDiscardingByOwner();
	
	public boolean isDiscarded();
}
