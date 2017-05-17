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

import com.github.devconslejme.misc.jme.ElectricJme;
import com.jme3.math.FastMath;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class Electric {
	/**
	 * each 0.01^3 wold volume = 1 watt/miliseconds
	 * so 1^3 = 100*100*100 w/ms
	 */
	private long lVolumeToWattPerMilis = 1000000;
	private double dWattPerMilisToVolume = 1/((double)lVolumeToWattPerMilis);
	
	private long	lLowEnergy=10000;
	private long	lEnergyCapacity=100000;
	private long lEnergyWattsPerMilis=0;

	public long volumeToEnergy(double dVolume) {
		return (long)(dVolume*lVolumeToWattPerMilis);
	}
	
	public double energyToVolume(){
		return energyToVolume(lEnergyWattsPerMilis);
	}
	public double energyToVolume(long lEnergyWpMs){
//		return ((double)lEnergyWpMs)/((double)lVolumeToWattPerMilis);
		return lEnergyWpMs*dWattPerMilisToVolume;
	}
	
	public long getEnergyWattsPerMilis() {
		return lEnergyWattsPerMilis;
	}

	public Electric setEnergyWattsPerMilis(long lEnergyWattsPerMilis) {
		this.lEnergyWattsPerMilis = lEnergyWattsPerMilis;
		return this; //for beans setter
	}

	public long getLowEnergy() {
		return lLowEnergy;
	}

	public Electric setLowEnergy(long lLowEnergy) {
		this.lLowEnergy = lLowEnergy;
		return this; //for beans setter
	}

	public long getEnergyCapacity() {
		return lEnergyCapacity;
	}

	public Electric setEnergyCapacity(long lEnergyCapacity) {
		this.lEnergyCapacity = lEnergyCapacity;
		return this; //for beans setter
	}

	public long consumeEnergy(long lConsume) {
		if(lEnergyWattsPerMilis>=lConsume){
			lEnergyWattsPerMilis-=lConsume;
			return lConsume;
		}
		return 0; //TODO consume partial and resulting bevarior is erratic/sluggish 
	}

	public boolean isHasEnergyWattsPerMilis() {
		return lEnergyWattsPerMilis>0;
	}

	public boolean isOvercharged(){
	//	if(fEnergyCoreRadius < (fRadius/2f))return false;
	//	return true;
		return lEnergyWattsPerMilis>lEnergyCapacity;
	}
	public String energyInfo(){
		StringBuilder sb = new StringBuilder();
		sb.append("("+lEnergyWattsPerMilis+">"+lLowEnergy+")w/ms, ");
		return sb.toString();
	}

	public boolean isLowEnergy(){
		return lEnergyWattsPerMilis<lLowEnergy; //this was based on the tractor energy 
	}

	public void addEnergy(long l) {
		lEnergyWattsPerMilis+=l;
	}
	
	/**
	 * 
	 * @param elecOther
	 * @param lAbso
	 * @return actually absorbed
	 */
	public long absorb(Electric elecOther, long lAbso) {
		lAbso = Math.min(lAbso, elecOther.lEnergyWattsPerMilis);
		lEnergyWattsPerMilis+=lAbso;
		elecOther.lEnergyWattsPerMilis-=lAbso;
		return lAbso;
	}
}
