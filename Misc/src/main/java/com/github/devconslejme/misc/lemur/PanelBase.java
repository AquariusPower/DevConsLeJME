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

import com.github.devconslejme.misc.AssertionsI;
import com.jme3.math.Vector3f;
import com.simsilica.lemur.Panel;
import com.simsilica.lemur.style.ElementId;

/**
 * Important to protect panel's Z, and easify settings.
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class PanelBase<SELF extends PanelBase<SELF>> extends Panel {
	public PanelBase(boolean b, ElementId elementId, String strStyle) {
    super(b, elementId, strStyle);
	}
	
	/**
	 * must be overriden by sub-class
	 */
	@SuppressWarnings("unchecked")
	private SELF getThis() {
		return (SELF)this;
	}
	
	@Deprecated
	@Override
	public void setLocalTranslation(float x, float y, float z) {
		assert(AssertionsI.i().useAlternativeMethods(2)); 
		super.setLocalTranslation(x, y, z);
	}
	@Deprecated
	@Override
	public void setLocalTranslation(Vector3f v3f) {
		assert(AssertionsI.i().useAlternativeMethods(2)); 
		super.setLocalTranslation(v3f);
	}
	/**
	 * to help on not messing with Z!!!
	 * ignores Z from param, reuses self current Z
	 * @param localTranslation
	 * @return 
	 */
	public SELF setLocalTranslationXY(Vector3f v3f) {
		super.setLocalTranslation(v3f.x, v3f.y, getLocalTranslation().z);
		return getThis();
	}
	public SELF setLocalTranslationZ(float fZ) {
		Vector3f v3f = this.getLocalTranslation();
		super.setLocalTranslation(v3f.x, v3f.y, fZ);
		return getThis();
	}
	
	/**
	 * will ignore Z
	 * @return
	 */
	public Vector3f getLocalTranslationXY() {
		Vector3f v3f = getLocalTranslation();
		return new Vector3f(v3f.x, v3f.y, 0);
	}
	
	@Deprecated
	@Override
	public void setSize(Vector3f size) {
		assert(AssertionsI.i().useAlternativeMethods(2)); 
		super.setSize(size);
	}
	/**
	 * ignores z
	 * @param v3f
	 */
	public void setSizeWH(Vector3f v3f) { //TODO is this usable in any way? 
		Vector3f v3fCurrentSize = getSize(); //to do not mess with z!!!
		super.setSize(new Vector3f(v3f.x,v3f.y,v3fCurrentSize.z));
	}
	
	@Deprecated
	@Override
	public void setPreferredSize(Vector3f size) {
		assert(AssertionsI.i().useAlternativeMethods(2)); 
		super.setPreferredSize(size);
	}
	/**
	 * ignores z
	 * @param v3f use z as Float.NaN if you want to indicate it will be ignored
	 */
	public void setPreferredSizeWH(Vector3f v3f) {
		Vector3f v3fCurrentSize = getSize(); //to do not mess with z!!!
		super.setPreferredSize(new Vector3f(v3f.x,v3f.y,v3fCurrentSize.z));
	}
}
