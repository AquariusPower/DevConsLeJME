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

import java.util.ArrayList;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.ICompositeRestrictedAccessControl;
import com.github.devconslejme.misc.Key;
import com.github.devconslejme.misc.KeyBindCommandManagerI;
import com.github.devconslejme.misc.KeyBindCommandManagerI.CallBoundKeyCmd;
import com.github.devconslejme.misc.KeyCodeManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.PhysicsI.PhysicsData;
import com.jme3.collision.CollisionResult;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;

/**
 * TODO after picking, using the mouse, move and rotate using wheel and moving the mouse, aligned with whatever is just below it
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ManipulatorI {
	public static ManipulatorI i(){return GlobalManagerI.i().get(ManipulatorI.class);}

	public static class CompositeControl implements ICompositeRestrictedAccessControl{private CompositeControl(){};};
	private ICompositeRestrictedAccessControl	cc=new CompositeControl();
		
	private CollisionResult	crManipulating;
	private Key	keyContext;
	
	public void configure(){
		keyContext = KeyCodeManagerI.i().createSpecialExternalContextKey(cc, "ContextManipulatorGrab");
		
		KeyBindCommandManagerI.i().putBindCommandsLater("Ctrl+G", new CallBoundKeyCmd(){
			@Override
			public Boolean callOnKeyReleased(int iClickCountIndex) {
				if(crManipulating!=null){
					PhysicsData pd = PhysicsI.i().getPhysicsDataFrom(crManipulating.getGeometry());
					if(pd!=null)PhysicsI.i().wakeUp(pd);
					crManipulating=null;
				}else{
					ArrayList<CollisionResult> acr = WorldPickingI.i().raycastPiercingAtCenter(null);
					if(acr.size()>0)crManipulating=acr.get(0);
				}
				return true;
			};
		}.setName("ManipulatorGrab"));
		
		KeyBindCommandManagerI.i().putBindCommandsLater(
			KeyCodeManagerI.i().getMouseAxisKey(2,true).composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){@Override	public Boolean callOnKeyReleased(int iClickCountIndex) {
				rotateManipulated(true); return true;};}.setName("ManipGrabRotCW")
		);
		KeyBindCommandManagerI.i().putBindCommandsLater(
			KeyCodeManagerI.i().getMouseAxisKey(2,false).composeCfgPrependModifiers(keyContext),
			new CallBoundKeyCmd(){@Override	public Boolean callOnKeyReleased(int iClickCountIndex) {
				rotateManipulated(false); return true;};}.setName("ManipGrabRotCCW")
		);
		
		initUpdateManipulated();
	}
	
	protected void rotateManipulated(boolean bCW){
		crManipulating.getGeometry().rotate(0, (bCW?1:-1)*10*FastMath.DEG_TO_RAD, 0);
		
		PhysicsData pd = PhysicsI.i().getPhysicsDataFrom(crManipulating.getGeometry());
		if(pd!=null)PhysicsI.i().syncPhysTransfFromSpt(crManipulating.getGeometry());
	}

	protected void initUpdateManipulated() {
		QueueI.i().enqueue(new CallableXAnon() {
			@Override
			public Boolean call() {
				keyContext.setPressedSpecialExternalContextKeyMode(cc, crManipulating!=null);
				if(!keyContext.isPressed())return true;//skip
				updateManipulated(getTPF());
				return true;
			}
		}).enableLoopMode();
	}
	
	protected void updateManipulated(float tpf) {
		AppI.i().placeAtCamWPos(crManipulating.getGeometry(), crManipulating.getDistance()+1f, false);
		crManipulating.getGeometry().rotateUpTo(Vector3f.UNIT_Y);
		
		PhysicsData pd = PhysicsI.i().getPhysicsDataFrom(crManipulating.getGeometry());
		if(pd!=null){
			PhysicsI.i().syncPhysTransfFromSpt(crManipulating.getGeometry());
			PhysicsI.i().resetForces(pd);
		}
		
		HWEnvironmentJmeI.i().putCustomInfo("Grabbed", crManipulating.toString());
	}
	
	
}
