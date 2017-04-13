/* 
Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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

import com.jme3.math.Vector3f;
import com.jme3.scene.shape.Line;


/**
 * TODO broken
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class EffectLine extends EffectBaseAbs<EffectLine>{

	@Override
	public EffectLine getThis() {
		return this;
	}
	
	public EffectLine() {
		super();
		getGeom().setMesh(new Line(new Vector3f(0,0,0),new Vector3f(1,1,1)));
	}
	
	@Override
	protected void playWork() {
//		Vector3f v3fTo = getLocationTo();
//		((Line)getGeom().getMesh()).updatePoints(getLocationFrom(), v3fTo);
		((Line)getGeom().getMesh()).updatePoints(
			new Vector3f(),
			new Vector3f(0,0,getLocationFrom().distance(getLocationTo())));
		
		getGeom().lookAt(getLocationTo(), Vector3f.UNIT_Y);
	}
}
