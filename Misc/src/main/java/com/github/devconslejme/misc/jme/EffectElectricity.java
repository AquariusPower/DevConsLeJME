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

import java.util.ArrayList;

import com.github.devconslejme.misc.SimulationTimeI;
import com.jme3.math.Vector3f;

/**
 * TODO improve with aura particles shader, mesh, texture? 
 * TODO create like 10 patterns and randomize thru them to be less CPU intensive? also rotate them in teh "direction axis" to look more randomized.
 * TODO the path creation could be a simple/raw/generic at @MiscPackage!!!
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class EffectElectricity extends EffectBaseAbs<EffectElectricity>{
//	private DebugData dbg;
//	private String	strUId = ManageEffectsJmeStateI.i().strLastUId = MiscI.i().getNextUniqueId(ManageEffectsJmeStateI.i().strLastUId);
//	private Vector3f	v3fFrom;
//	private Vector3f	v3fTo;
//	private Vector3f	v3fDirectionNormalized;
//	private float	fDist;
//	private float	fPartMinPerc=0.20f;
//	private float	fPartMaxPerc=1.00f;
//	private float fDeltaPerc = fPartMaxPerc-fPartMinPerc;
	private float fAmplitudePerc = 0.15f;
	private int	iParts;
//	private int	iPartMaxDots = 100;
	private ArrayList<Vector3f>	av3fList  = new ArrayList<Vector3f>() {};
	private Integer	iOverrideThickness=null;
	private ElectricalPath	elp;
//	private int	iMinPathParts;
//	private int	iMaxPathParts;
	
	public EffectElectricity() {
		elp = new ElectricalPath();
	}
	
	@Override
	public EffectElectricity getThis(){return this;}
	
//	@Override
//	public String getUId(){
//		assertNotDiscarded();
//		return strUId;
//	}
	
	
	@Override
	protected void playWork(float tpf){
		MiscJmeI.i().updateMultiLineMesh(getGeom().getMesh(), recreatePath(tpf).toArray(new Vector3f[0]));
		getGeom().getMaterial().getAdditionalRenderState().setLineWidth(getThickNess());
	}
	
	public EffectElectricity setOverrideThickness(int i){
		this.iOverrideThickness=i;
		return getThis();
	}
	
	public int getThickNess(){
		if(iOverrideThickness!=null)return iOverrideThickness;
		
		/**
		 * all in longs to avoid loads of castings...
		 */
//		long lRemainMilis = elp.getHoldUntilMilis() - SimulationTimeI.i().getMillis();
		long lRemainMilis = elp.getRemainingMilis();
		long lMaxThickness = 8;
		long lThicknessStepMilis = elp.getMaxHoldMilis()/lMaxThickness;
		long lCurrentThickness = lRemainMilis/lThicknessStepMilis;
//		return FastMath.nextRandomFloat()*4+1;
		return (int)(lCurrentThickness>=1 ? lCurrentThickness : 1);
	}
	
	public ArrayList<Vector3f> recreatePath(float tpf) {
		assertNotDiscarded();
		return elp.updateElectricalPath(
				tpf,
				getLocationFrom(),
				getLocationTo(),
				fAmplitudePerc
		);
	}
	
	public EffectElectricity setAmplitudePerc(float f) {
		this.fAmplitudePerc=f;
		return getThis();
	}
	
	@Override
	public EffectElectricity clone() {
		EffectElectricity eff = (EffectElectricity)super.clone();//new EffectElectricity();
		eff.fAmplitudePerc=this.fAmplitudePerc;
		return eff;
	}
	
	public ElectricalPath getElectricalPath(){
		return elp;
	}
	
//	public void setMinMaxPerc(float fMin, float fMax) {
//		elp.setMinMaxPerc(fMin, fMax);
//	}

//	public void setPathParts(int iMin, int iMax) {
//		elp.setPathParts(iMin,iMax);
////		this.iMinPathParts=iMin;
////		this.iMaxPathParts=iMax;
//	}

}
