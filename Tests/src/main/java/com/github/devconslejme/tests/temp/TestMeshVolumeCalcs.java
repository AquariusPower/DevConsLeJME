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
package com.github.devconslejme.tests.temp;

import com.github.devconslejme.misc.jme.MeshI;
import com.github.devconslejme.misc.jme.MeshI.Cone;
import com.jme3.app.SimpleApplication;
import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.scene.shape.Box;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Sphere;
import com.jme3.scene.shape.Torus;
import com.simsilica.lemur.GuiGlobals;
import com.simsilica.lemur.style.BaseStyles;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class TestMeshVolumeCalcs extends SimpleApplication{
	public static void main(String[] args) {
		TestMeshVolumeCalcs test = new TestMeshVolumeCalcs();
//		test.start();
		test.initTest(new Sphere(5,6,1,false,false),new Vector3f(1,1,1));
		test.initTest(new Sphere(5,6,10,false,false),new Vector3f(1,1,1));
		test.initTest(new Sphere(50,60,10,false,false),new Vector3f(1,1,1)); //more triangles makes it more precise
		test.initTest(new Sphere(5,6,1,false,false),new Vector3f(1,1.25f,1.35f));
		test.initTest(new Sphere(50,60,10,false,false),new Vector3f(1,1.25f,1.35f)); //more triangles makes it more precise
		test.initTest(new Box(1, 1, 1),new Vector3f(1,1,1));
		test.initTest(new Box(1, 2, 3.43f),new Vector3f(1,1.25f,1.35f));
		test.initTest(new Cylinder(5,6,1,1,3,true,false),new Vector3f(1,1.25f,1.35f));
		test.initTest(new Cylinder(5,6,1,0.5f,3,true,false),new Vector3f(1,1.25f,1.35f));
		test.initTest(new Cone(0.45f),new Vector3f(1,1.25f,1.35f));
		test.initTest(new Torus(50,15,0.5f,2.5f),new Vector3f(1,1.25f,1.35f));
		test.initTest(new Torus(50,15,0.5f,1f),new Vector3f(1,1.25f,1.35f)); // this is "almost" a sphere
		test.initTest(new Torus(50,15,0.25f,1f),new Vector3f(1,1.25f,1.35f)); 
	}
	
	@Override
	public void simpleInitApp() {
		GuiGlobals.initialize(this);
		BaseStyles.loadGlassStyle();
		GuiGlobals.getInstance().getStyles().setDefaultStyle(BaseStyles.GLASS);
		//TODO com.github.devconslejme.TODO.PkgCfgI.i().configure();
		
//		initTest(new Box(1, 1, 1),new Vector3f(1,1,1));
	}
	
	/**
	 * public so can be called from devcons user cmds
	 * @param mesh 
	 * @param v3f 
	 */
	public <T extends Mesh> void initTest(T mesh, Vector3f v3f) {
		System.out.println(MeshI.i().volumeOf(mesh,v3f)+"//guessed//"+mesh.getClass()+v3f);
		System.out.println(MeshI.i().volOfMesh(mesh,v3f)+"//MESH//"+mesh.getClass()+v3f);
	}
}