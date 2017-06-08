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

import com.github.devconslejme.game.CharacterI;
import com.github.devconslejme.misc.jme.AppI;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.PhysicsProjectileI;
import com.github.devconslejme.misc.jme.ColorI.EColor;
import com.github.devconslejme.misc.jme.PhysicsI;
import com.github.devconslejme.misc.jme.PhysicsI.PhysicsData;
import com.github.devconslejme.misc.jme.PhysicsProjectileI.PhysicsGun;
import com.github.devconslejme.projman.SimpleApplicationAndStateAbs;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class TestProjectiles extends SimpleApplicationAndStateAbs {
	public static void main(String[] args) {
		TestProjectiles test = new TestProjectiles();
		test.start();
	}

	@Override
	public void simpleInitApp() {
		com.github.devconslejme.game.PkgCfgI.i().configure();
		initTest();
	}
	
	@Override
	public void update(float tpf) {
		throw new UnsupportedOperationException("method not implemented");
	}
	
	/**
	 * public so can be called from devcons user cmds
	 */
	@Override
	public void initTest() {
		CharacterI.i().create(Vector3f.ZERO);
		
		PhysicsI.i().setBulletDebugVisualsEnabled(true);
//		SubdivisionSurfaceModifier s = new SubdivisionSurfaceModifier(modifierStructure, blenderContext);
		
		PhysicsProjectileI.i().setProjectileFromCamToDevTestDbg();
		PhysicsGun pg = PhysicsProjectileI.i().createGun(PhysicsProjectileI.i().getProjectileThrowerDevTestDbgCopy());
		
		float fSize=100;
		float fYFloor=-7;
		PhysicsData pdFloor = PhysicsI.i().spawnOrthoWall(0, fSize, fSize, null, new Vector3f(0,fYFloor,0));
		pdFloor.getInitialOriginalGeometry().getMaterial().setColor(EColor.Color.s(), 
			ColorI.i().colorChangeCopy(ColorRGBA.Brown,0.20f,1f));
//		Geometry geomFloor=GeometryI.i().create(new Box(iSize,0.1f,iSize), ColorI.i().colorChangeCopy(ColorRGBA.Brown,0.20f,1f));
//		geomFloor.move(0,-7f,0);
//		geomFloor.setName("floor");
//		PhysicsI.i().imbueFromWBounds(geomFloor).setTerrain(true)
//			.getRBC().setMass(0f);
//		AppI.i().getRootNode().attachChild(geomFloor);
		
		float fYWalls=fYFloor+1-0.1f; //walls
		PhysicsI.i().spawnOrthoWall(1, fSize, 2.1f, null, new Vector3f(0,fYWalls, fSize/2f));
		PhysicsI.i().spawnOrthoWall(1, fSize, 2.1f, null, new Vector3f(0,fYWalls,-fSize/2f));
		PhysicsI.i().spawnOrthoWall(1, fSize, 2.1f, null, new Vector3f( fSize/2f,fYWalls,0))
			.rotate(0, 90*FastMath.DEG_TO_RAD, 0);
//			.getRBC().getPhysicsRotation().mult(new Quaternion().fromAxes(0, 90*FastMath.DEG_TO_RAD, 0));
//			.getRBC().setPhysicsRotation(new Quaternion().rot);
//			.rotate(0, 90*FastMath.DEG_TO_RAD, 0);
		PhysicsI.i().spawnOrthoWall(1, fSize, 2f, null, new Vector3f(-fSize/2f,fYWalls,0))
			.rotate(0, 90*FastMath.DEG_TO_RAD, 0);
		
		//ramp
		PhysicsI.i().spawnOrthoWall(2, fSize/4f, 1f, null, new Vector3f(0,fYFloor,(fSize/2f)*0.75f))
			.rotate(70*FastMath.DEG_TO_RAD,0,0);
//		initTestWall(iSize,"XP",true,geomFloor.getLocalTranslation().y,null);
//		initTestWall(iSize,"XN",false,geomFloor.getLocalTranslation().y,null);
//		initTestWall(iSize,"ZP",null,geomFloor.getLocalTranslation().y,true);
//		initTestWall(iSize,"ZN",null,geomFloor.getLocalTranslation().y,false);
		
		// some boxes representing the axes (not needed tho)
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.Red,0,0.5f),
			3f,"X-Red"	,new Vector3f(1,0,0).mult(5));
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.Green,0,0.5f),
			2f,"Y-Green",new Vector3f(0,1,0).mult(5));
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.Blue,0,0.5f),
			1f,"Z-Blue"	,new Vector3f(0,0,1).mult(5));
		
	}
	
	public void spawnBox(){
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.randomColor(),0,0.5f),
			3f,"Random",new Vector3f(0,0,0));
	}
	
//	protected void initTestBox(ColorRGBA color, float fVolume, String str){
//		Geometry geom = GeometryI.i().create(MeshI.i().box((float) (Math.cbrt(fVolume)/2f)), color);
//		Node node = new Node("TestBox"+str);node.attachChild(geom);
//		PhysicsI.i().imbueFromWBounds(node,EMatter.Custom1KgPerM3.get());AppI.i().getRootNode().attachChild(node);geom.setName("Box"+str);
//	}
	
//	public void initTestWall(int iSize,String str, Boolean bXP, float fY, Boolean bZP){
//		Geometry geomWall=GeometryI.i().create(new Box(iSize,2f,2f), ColorRGBA.Gray);
////		int i = iSize;///2;
//		Vector3f v3f = new Vector3f();
//		if(bXP!=null){
//			v3f.x=bXP?iSize:-iSize;
//			geomWall.rotate(0, 90*FastMath.DEG_TO_RAD, 0);
//		}
//		if(bZP!=null)v3f.z=bZP?iSize:-iSize;
//		v3f.y=fY;
//		geomWall.move(v3f);
//		geomWall.setName("BoxWall"+str);
//		PhysicsI.i().imbueFromWBounds(geomWall).setTerrain(true)
//			.getRBC().setMass(0f);
//		AppI.i().getRootNode().attachChild(geomWall);
//	}
}
