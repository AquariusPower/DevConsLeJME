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

import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class RotateI {
	public static RotateI i(){return GlobalManagerI.i().get(RotateI.class);}
	
	private RotateHelper	rhRotAround;
	private RotateHelper	rhRotVec;
	
	public static class RotateHelper{
		private Node nodeCenter = new Node();
		private Node nodePos = new Node();
		private Quaternion quaAdd=new Quaternion();
		public RotateHelper(){
			nodeCenter.attachChild(nodePos);
		}
	}
	public Vector3f rotateVector(Vector3f v3fTipToRotate, Vector3f v3fUp, float fAddAngleRadians) {
		if(rhRotVec==null)rhRotVec=new RotateHelper();
		
		rhRotVec.nodeCenter.setLocalTranslation(0,0,0);
		rhRotVec.nodePos.setLocalTranslation(v3fTipToRotate);
		
		rhRotVec.quaAdd.fromAngleAxis(fAddAngleRadians, v3fUp);
		rhRotVec.nodeCenter.setLocalRotation(Quaternion.IDENTITY);
		rhRotVec.nodeCenter.rotate(rhRotVec.quaAdd);
		
		return rhRotVec.nodePos.getWorldTranslation();
	}

	public void rotateSpinning(Spatial spt, Vector3f v3fSpotToRotate,	Vector3f v3fUp, float fAngleRadians) {
		Vector3f v3fNewUp = rotateVector(v3fSpotToRotate,v3fUp,fAngleRadians);
		spt.rotateUpTo(v3fNewUp);
	}

	public void rotateAroundPivot(Spatial spt, Spatial sptCenter, float fAddAngleRadians){
		rotateAroundPivot(spt,sptCenter,fAddAngleRadians, sptCenter.getLocalRotation().getRotationColumn(1), false);
	}
	
	/**
	 * 
	 * @param sptToRotate
	 * @param sptPivot
	 * @param fAddAngleRadians (remember u can use f*FastMath.DEG_TO_RAD too)
	 * @param v3fUp if will will be Y
	 * @param bKeepOriginalLocalRotation
	 */
	public void rotateAroundPivot(Spatial sptToRotate, Spatial sptPivot, float fAddAngleRadians, Vector3f v3fUp, boolean bKeepOriginalLocalRotation){
		if(v3fUp==null)v3fUp=sptToRotate.getLocalRotation().getRotationColumn(1);
		
		/**
		 * we need to know where they are in the world
		 */
		Vector3f v3fPos = sptToRotate.getWorldTranslation();
		Vector3f v3fCenter = sptPivot.getWorldTranslation();
		
		Vector3f v3fSub = v3fPos.subtract(v3fCenter);
		Vector3f v3fDir = v3fSub.normalize();
		float fDist = v3fSub.length();
		
		if(rhRotAround==null)rhRotAround=new RotateHelper();
		
		rhRotAround.nodeCenter.setLocalTranslation(v3fCenter);
		rhRotAround.nodePos.setLocalTranslation(v3fPos);
		rhRotAround.nodePos.setLocalRotation(sptToRotate.getLocalRotation());
		
		rhRotAround.quaAdd.fromAngleAxis(fAddAngleRadians, v3fUp);
		rhRotAround.nodeCenter.setLocalRotation(Quaternion.IDENTITY);
		rhRotAround.nodeCenter.rotate(rhRotAround.quaAdd);
//		if(!bKeepOriginalLocalRotation){
//			spt.rotate(quaAdd);  //use lookat?
//		}
		
		sptToRotate.setLocalTranslation(
			sptToRotate.worldToLocal(rhRotAround.nodePos.getWorldTranslation(),null) );
		
		if(!bKeepOriginalLocalRotation){
			sptToRotate.setLocalRotation(rhRotAround.nodePos.getWorldRotation());
		}
		
	}

	public Vector3f getRandomSpotAround(Vector3f v3fAround, float fScale){
		return getRandomSpotAround(v3fAround, true, fScale);
	}
	/**
	 * 
	 * @param v3fAround can be null, last thing applied
	 * @param bNormalize
	 * @param fScale can be null, applied AFTER normalize
	 * @return
	 */
	public Vector3f getRandomSpotAround(Vector3f v3fAround, boolean bNormalize, Float fScale){
		Vector3f v3f = new Vector3f(
				2*FastMath.nextRandomFloat()-1,
				2*FastMath.nextRandomFloat()-1,
				2*FastMath.nextRandomFloat()-1
			);
		
		if(bNormalize)v3f.normalizeLocal();
		if(fScale!=null)v3f.multLocal(fScale);
		if(v3fAround!=null)v3f.addLocal(v3fAround);
		
		return v3f;
	}

	public Vector3f randomDirection(){
		return new Vector3f(
			FastMath.nextRandomFloat()*2f-1f,
			FastMath.nextRandomFloat()*2f-1f,
			FastMath.nextRandomFloat()*2f-1f).normalize();
	}
}
