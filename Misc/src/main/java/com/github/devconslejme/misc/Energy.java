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
package com.github.devconslejme.misc;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class Energy {
	private long lEnergyDensity;
	private Long	lLowEnergy;
	private long	lEnergyCapacity;
	private long lStoredEnergy;
	private float	fEnerUnstablePerc;
	
	public Energy(long lEnergyDensity, long lEnergyCapacity, long lLowEnergy,	long lStoredEnergy) {
		super();
		this.lEnergyDensity = lEnergyDensity;
		this.lLowEnergy = lLowEnergy;
		this.lEnergyCapacity = lEnergyCapacity;
		this.lStoredEnergy = lStoredEnergy;
	}
	protected void copy(Energy enFrom, Energy enTo) {
		enTo.lEnergyDensity = enFrom.lEnergyDensity;
		enTo.lLowEnergy = enFrom.lLowEnergy;
		enTo.lEnergyCapacity = enFrom.lEnergyCapacity;
		enTo.lStoredEnergy = enFrom.lStoredEnergy;
	}
	
	public Energy(Energy enCopyFrom){
		copy(enCopyFrom,this);
	}
	
	/**
	 * special instances
	 */
	protected Energy(){}
	
	public long volumeToEnergy(double dVolume) {
		return (long)(dVolume*lEnergyDensity);
	}
	
	public double energyToVolume(){
		return energyToVolume(lStoredEnergy);
	}
	public double energyToVolume(long lEnergyWpMs){
//		return ((double)lEnergyWpMs)/((double)lVolumeToWattPerMilis);
		return lEnergyWpMs/((double)lEnergyDensity);
	}
	
	public long getEnergyStored() {
		return lStoredEnergy;
	}

	public Energy setEnergyStored(long lEnergyWattsPerMilis) {
		this.lStoredEnergy = lEnergyWattsPerMilis;
		return this; //for beans setter
	}

	public long getLowEnergy() {
		return lLowEnergy;
	}

	public Energy setLowEnergy(long lLowEnergy) {
		this.lLowEnergy = lLowEnergy;
		return this; //for beans setter
	}

	public long getEnergyCapacity() {
		return lEnergyCapacity;
	}

	public Energy setEnergyCapacity(long lEnergyCapacity) {
		this.lEnergyCapacity = lEnergyCapacity;
		return this; //for beans setter
	}

	public long consumeEnergy(long lConsume) {
		if(lStoredEnergy>=lConsume){
			lStoredEnergy-=lConsume;
			return lConsume;
		}
		return 0; //TODO consume partial and resulting bevarior is erratic/sluggish 
	}

	public boolean isHasEnergy() {
		return lStoredEnergy>0;
	}

	public boolean isOvercharged(){
	//	if(fEnergyCoreRadius < (fRadius/2f))return false;
	//	return true;
		return lStoredEnergy>lEnergyCapacity;
	}
	public String energyInfo(){
		StringBuilder sb = new StringBuilder();
		sb.append("("
			+"C="+StringI.i().fmtLong(lEnergyCapacity)+">"
			+"S="+StringI.i().fmtLong(lStoredEnergy)+">"
			+"L="+StringI.i().fmtLong(lLowEnergy)+")w/ms, ");
		sb.append(StringI.i().fmtFloat(getPerc()*100f)+"%, ");
		return sb.toString();
	}

	public boolean isLowEnergy(){
		return lStoredEnergy<lLowEnergy; //this was based on the tractor energy 
	}

	public void addEnergy(long l) {
		lStoredEnergy+=l;
	}
	
	/**
	 * 
	 * @param enOther
	 * @param lAbso
	 * @return actually absorbed
	 */
	public long absorb(Energy enOther, long lAbso) {
		lAbso = Math.min(lAbso, enOther.lStoredEnergy);
		lStoredEnergy+=lAbso;
		enOther.lStoredEnergy-=lAbso;
		return lAbso;
	}

	public float getPerc() {
		return (float) (lStoredEnergy / (double)lEnergyCapacity); 
	}
	public float getUnstablePerc() {
		float f = getPerc()-1f;
		return fEnerUnstablePerc = f>=0 ? f : 0;
	}
	public Energy setEnerUnstablePerc(float fEnerUnstablePerc) {
		this.fEnerUnstablePerc = fEnerUnstablePerc;
		return this; //for beans setter
	}
}
