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
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.PhysicsData;
import com.github.devconslejme.misc.jme.PhysicsI;
import com.github.devconslejme.misc.jme.PhysicsProjectileI;
import com.github.devconslejme.misc.jme.PhysicsProjectileI.EGun;
import com.github.devconslejme.projman.SimpleApplicationAndStateAbs;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;

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
//		CharacterI.i().createBCCX(Vector3f.ZERO);
		CharacterI.i().create(Vector3f.ZERO);
		
		PhysicsI.i().setBulletDebugVisualsEnabled(true);
//		SubdivisionSurfaceModifier s = new SubdivisionSurfaceModifier(modifierStructure, blenderContext);
		
		// to have fun on flycam mode
//		PhysicsProjectileI.i().setProjectileFromCamCurrent(PhysicsProjectileI.i().getProjectileThrowerDevTestDbgCopy());
		
//		boolean bOld=true;
//		if(bOld) {
//			// projectile mass is 0.0065
//			PhysicsThrowProjectiles pp = PhysicsProjectileI.i().getProjectileThrowerDevTestDbgCopy();
//			PhysicsGun pg = PhysicsProjectileI.i().createGun(pp,
//				EMatter.Generic20KgPerM3.get()
//			);
//		}else {
//			PhysicsGun pg = PhysicsProjectileI.i().createGun(
//				new PhysicsThrowProjectiles(EMatterStatus.Bullet762x39mm.get(),710f),
//				EMatterStatus.GunAK47.get()
//			);
//		}
		PhysicsProjectileI.i().createGun(EGun.AK47.get());
		PhysicsProjectileI.i().createGun(EGun.Glock17.get());
		
		float fFullLength=100;
		float fYFloor=-7;
		float fFloorThickness=0.5f;
		PhysicsData pdFloor = PhysicsI.i().spawnWall(
			new Vector3f( fFullLength/2f,fYFloor,0),
			new Vector3f(-fFullLength/2f,fYFloor,0),
			true,fFullLength,0.5f,ColorI.i().colorChangeCopy(ColorRGBA.Brown,0.20f,1f));
//		PhysicsData pdFloor = PhysicsI.i().spawnWall(0, fFullLength, fFullLength, 0f, null, new Vector3f(0,fYFloor,0));
//		pdFloor.getInitialOriginalGeometry().getMaterial().setColor(EColor.Color.s(), 
//			ColorI.i().colorChangeCopy(ColorRGBA.Brown,0.20f,1f));
		
		float fWallStuckOnFloorDepth=fFloorThickness;
		float fWallHeight=2f+fWallStuckOnFloorDepth;
//		float fYWalls=fYFloor+fWallHeight/2f-fWallStuckOnFloorDepth; //walls
		float fYWalls=fYFloor+fFloorThickness/2f-fWallStuckOnFloorDepth; //walls
		PhysicsI.i().spawnWall( //north
			new Vector3f( fFullLength/2f,fYWalls, fFullLength/2f),
			new Vector3f(-fFullLength/2f,fYWalls, fFullLength/2f));
		PhysicsI.i().spawnWall( //south
			new Vector3f( fFullLength/2f,fYWalls,-fFullLength/2f),
			new Vector3f(-fFullLength/2f,fYWalls,-fFullLength/2f));
		PhysicsI.i().spawnWall( //west
			new Vector3f( fFullLength/2f,fYWalls, fFullLength/2f),
			new Vector3f( fFullLength/2f,fYWalls,-fFullLength/2f));
		PhysicsI.i().spawnWall( //east
			new Vector3f(-fFullLength/2f,fYWalls, fFullLength/2f),
			new Vector3f(-fFullLength/2f,fYWalls,-fFullLength/2f));
		
//		PhysicsI.i().spawnWall(1, fFullLength, fWallHeight, 0f, null, new Vector3f(0,fYWalls, fFullLength/2f));
//		PhysicsI.i().spawnWall(1, fFullLength, fWallHeight, 0f, null, new Vector3f(0,fYWalls,-fFullLength/2f));
//		PhysicsI.i().spawnWall(1, fFullLength, fWallHeight, 90*FastMath.DEG_TO_RAD, null, new Vector3f( fFullLength/2f,fYWalls,0));
//		PhysicsI.i().spawnWall(1, fFullLength, fWallHeight, 90*FastMath.DEG_TO_RAD, null, new Vector3f(-fFullLength/2f,fYWalls,0));
		
		//ramp
//		PhysicsI.i().spawnWall(2, fFullLength/4f, 1f, -15*FastMath.DEG_TO_RAD, null, new Vector3f(0,fYFloor,(fFullLength/2f)*0.75f));
		PhysicsI.i().spawnWall(
			new Vector3f(0, fYFloor+fFloorThickness/2f,  (fFullLength/2f)*0.75f),
			new Vector3f(0, fYFloor+fWallHeight-fWallStuckOnFloorDepth, (fFullLength/2f)),
			true,null,null,null);
		
		// some boxes representing the axes (just some non-sense lol)
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.Red,0,0.5f),
			5f,"X-Red"	,new Vector3f(1,0,0).mult(5), null);
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.Green,0,0.5f),
			3f,"Y-Green",new Vector3f(0,1,0).mult(5), null);
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.Blue,0,0.5f),
			1f,"Z-Blue"	,new Vector3f(0,0,1).mult(5), null);
		
		// some small boxes to play with
		spawnBox(0.750f);
		spawnBox(0.500f);
		spawnBox(0.250f);
		spawnBox(0.125f);
//		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.randomColor(),0,0.5f),	0.500f,"SmallBoxV0500",new Vector3f(0,0,0), null);
//		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.randomColor(),0,0.5f),	0.250f,"SmallBoxV0250",new Vector3f(0,0,0), null);
//		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.randomColor(),0,0.5f),	0.125f,"SmallBoxV0125",new Vector3f(0,0,0), null);
	}
	
	public void spawnBox(float fVolume){
		PhysicsI.i().spawnVolumeBox(ColorI.i().colorChangeCopy(ColorRGBA.randomColor(),0,0.5f),
			fVolume,"SmallBoxV"+fVolume,new Vector3f(0,0,0), null);
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
