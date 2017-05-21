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
import com.github.devconslejme.misc.MessagesI;
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
		private Node nodePivotIsParent = new Node();
		private Node nodeChild = new Node();
		private Quaternion quaAdd=new Quaternion();
		public RotateHelper(){
			nodePivotIsParent.attachChild(nodeChild);
		}
	}
	public Vector3f rotateVector(Vector3f v3fTipToRotate, Vector3f v3fUp, float fAddAngleRadians) {
		if(rhRotVec==null)rhRotVec=new RotateHelper();
		//prepare
		rhRotVec.nodePivotIsParent.setLocalTranslation(0,0,0);
		rhRotVec.nodePivotIsParent.setLocalRotation(Quaternion.IDENTITY);
		rhRotVec.nodeChild.setLocalTranslation(v3fTipToRotate);
		//rotate
		rhRotVec.quaAdd.fromAngleAxis(fAddAngleRadians, v3fUp);
		rhRotVec.nodePivotIsParent.rotate(rhRotVec.quaAdd);
		
		return rhRotVec.nodeChild.getWorldTranslation();
	}

	public void rotateSpinning(Spatial spt, Vector3f v3fSpotToRotate,	Vector3f v3fUp, float fAngleRadians) {
		Vector3f v3fNewUp = rotateVector(v3fSpotToRotate,v3fUp,fAngleRadians);
		spt.rotateUpTo(v3fNewUp);
	}

	/**
	 * @param fAddAngleRadians (remember u can use f*FastMath.DEG_TO_RAD too)
	 */
	public void rotateAroundPivot(
		Spatial sptTargetToRotate,
		Spatial sptPivot,
		float fAddAngleRadians, 
		boolean bLookAtMoveDir
	){
		assert(sptTargetToRotate.getParent()==sptPivot.getParent());
		
		Vector3f v3fTargetIniPos = sptTargetToRotate.getLocalTranslation().clone();
		Vector3f v3fPivotLocat=sptPivot.getLocalTranslation().clone();
		Vector3f v3fWDistFromPivotToTarget = v3fTargetIniPos.subtract(v3fPivotLocat);
		
		Vector3f v3fPivotAxisUp=sptPivot.getLocalRotation().getRotationColumn(1).clone();
		
		if(rhRotAround==null)rhRotAround=new RotateHelper();
		// prepare
		rhRotAround.nodePivotIsParent.setLocalTranslation(v3fPivotLocat);
		rhRotAround.nodePivotIsParent.setLocalRotation(Quaternion.IDENTITY); //reset from previous call to this method
		rhRotAround.nodeChild.setLocalTranslation(v3fWDistFromPivotToTarget);
		rhRotAround.nodeChild.setLocalRotation(sptTargetToRotate.getLocalRotation().clone());
		// rotated pos
		rhRotAround.quaAdd.fromAngleAxis(fAddAngleRadians, v3fPivotAxisUp); //rotate around this axis
		rhRotAround.nodePivotIsParent.rotate(rhRotAround.quaAdd);
		
		Vector3f v3fTargetRotPos=rhRotAround.nodeChild.getWorldTranslation().clone();
		sptTargetToRotate.setLocalTranslation(v3fTargetRotPos);
//		sptTargetToRotate.setLocalTranslation(
//				/**
//				 * to convert to the parent's local scale, the parent's world scale is required
//				 */
//			v3fTargetRotPos.divide(sptTargetToRotate.getParent().getWorldScale())
//		);
		
		if(bLookAtMoveDir){
			/**
			 * TODO ??? it would have to calculate the next rotation step to be exact...
			 */
			Vector3f v3fWDistIniToRot = v3fTargetRotPos.subtract(v3fTargetIniPos); //to normalize would be more inexact..
			Vector3f v3fWLookAt = v3fTargetRotPos.add(v3fWDistIniToRot); //inexact predicted new (after this) rotation pos
			sptTargetToRotate.lookAt(v3fWLookAt, sptTargetToRotate.getLocalRotation().getRotationColumn(1).clone());
		}
		
	}
	
	private boolean validateLengthOfDelta(Vector3f v3fDelta, Object... aobjDebug) {
		if(v3fDelta.length()>10000){
//			MessagesI.i().warnMsg(this, "that far away? isnt something wrong?", v3fSub, v3fSub.length(), sptToRotate, sptPivot, fAddAngleRadians, v3fUp, bKeepOriginalLocalRotation);
			MessagesI.i().warnMsg(this, "that far away? isnt something wrong?", aobjDebug);
			return false;
		}
		return true;
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
