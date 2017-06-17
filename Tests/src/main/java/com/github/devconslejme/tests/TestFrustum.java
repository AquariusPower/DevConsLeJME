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

import java.awt.Color;

import com.github.devconslejme.misc.jme.StringTextJmeI;
import com.github.devconslejme.projman.SimpleApplicationAndStateAbs;
import com.jme3.font.BitmapText;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestFrustum extends SimpleApplicationAndStateAbs {
	public static void main(String[] args) {
		TestFrustum test = new TestFrustum();
		test.start();
	}

	@Override
	public void simpleInitApp() {
		//TODO com.github.devconslejme.misc.TODO.PkgCfgI.i().configure();
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
		
		for(int i=0;i<10000;i+=100) { //earth surface curves at 5Km distance...
			BitmapText bt = StringTextJmeI.i().createBitmapTextMono("z="+i, ColorRGBA.Blue);
			bt.rotate(0, 180*FastMath.RAD_TO_DEG, 0);
			bt.scale(0.1f);
			bt.move(0, i/10, i);
			getRootNode().attachChild(bt);
		}
	}

}

