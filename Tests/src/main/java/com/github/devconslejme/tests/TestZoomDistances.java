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
package com.github.devconslejme.tests;

import com.github.devconslejme.misc.jme.GeometryI;
import com.github.devconslejme.misc.jme.MeshI;
import com.github.devconslejme.projman.SimpleApplicationAndStateAbs;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class TestZoomDistances extends SimpleApplicationAndStateAbs {
	public static void main(String[] args) {
		TestZoomDistances test = new TestZoomDistances();
		test.start();
	}

	private float	fDisplacement=10f;
	
	@Override
	public void simpleInitApp() {
		com.github.devconslejme.misc.jme.PkgCfgI.i().configure(this,getGuiNode(),getRootNode());
		initTest();
	}
	
	@Override
	public void update(float tpf) {
	}
	
	/**
	 * public so can be called from devcons user cmds
	 */
	@Override
	public void initTest() {
		super.initTest();
		
		prepareLandMarks(ColorRGBA.Red,1f);
		prepareLandMarks(ColorRGBA.Yellow,-1f);
		
//		getCamera().setLocation(new Vector3f(0,0,86.5f));
//		getCamera().lookAt(Vector3f.ZERO, Vector3f.UNIT_Y);
//		if(isFlyByCameraX()){
////			getFlyByCameraX().setZoomLimits(false, 10,110,11);
//			getFlyByCameraX().setZoomLimits(true, 10, 110, 11);
//			getFlyByCameraX().zoomIn(); //to init aligned
//		}
	}
	
	public void prepareLandMarks(ColorRGBA color,float fDir){
		for(int i=1;i<=10;i++){
			Geometry geom = GeometryI.i().create(MeshI.i().box(0.5f), color);
			if(i%2==0)geom.setLocalScale(1,i*2,1);
			getRootNode().attachChild(geom);
			geom.setLocalTranslation(fDir*i*getDisplacement(), 0, 0);
		}
	}

	public float getDisplacement() {
		return fDisplacement;
	}

	public TestZoomDistances setDisplacement(float fDisplacement) {
		this.fDisplacement = fDisplacement;
		return this; 
	}

}