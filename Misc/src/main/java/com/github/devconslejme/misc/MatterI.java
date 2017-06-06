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

import java.util.HashMap;

import com.jme3.math.Vector3f;


/**
 * TODO add melting, boiling, freezing temperatures etc
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MatterI {
	public static MatterI i(){return GlobalManagerI.i().get(MatterI.class);}
	
	private static HashMap<String,Matter> hm = new HashMap<String,Matter>();
	private static double fM3toCm3=1000000;
	
	public static enum EMatter{
					/** a generic placeholder, so 1 cubic meter will have a mass of 1.0kg (a cardboard empty box) */
		      Generic1KgPerM3(1000f/fM3toCm3),
		     Generic10KgPerM3(10000f/fM3toCm3),
		    Generic100KgPerM3(100000f/fM3toCm3),
		   /** like water density */
		   Generic1000KgPerM3(1000000f/fM3toCm3),
		  /** around the densest metals in the world */
		  Generic10000KgPerM3(10000000f/fM3toCm3),
		 /** earch core? */
		 Generic100000KgPerM3(100000000f/fM3toCm3),
		/** black holes? */
		Generic1000000KgPerM3(1000000000f/fM3toCm3),
		
		Aluminum(2.7),
		Copper(9.0),
		Gold(19.3),
		Lead(11.34f),
		Osmium(22.6f),
		Tungsten(19.25),
		Water(1.0),
		;
		EMatter(double fDensityGramsPerCm3){
			hm.put(this.toString(),new Matter(this.toString(),fDensityGramsPerCm3));
		}
		public Matter get(){
			return MatterI.i().get(this.toString());
		}
	}
	
	public static class Matter{
		private String	strId;
		private double fDensityGramsPerCm3;
		
		public Matter(String strId, double fDensityGramsPerCm3){
			this.strId = strId;
			this.fDensityGramsPerCm3=fDensityGramsPerCm3;
		}
		public double getDensityGramsPerCm3() {
			return fDensityGramsPerCm3;
		}
		public String getId() {
			return strId;
		}
	}
	
	public static class MatterStatus{
		private Matter mt;
		private double fVolumeCm3;
		private double	dMassGrams;
		private double	fVolumeM3;
		
		public MatterStatus(Matter mt){
			this.mt=mt;
		}

		public double getVolumeCm3() {
			return fVolumeCm3;
		}

		public MatterStatus setVolumeCm3(double fVolumeCm3) {
			this.fVolumeCm3 = fVolumeCm3;
			this.fVolumeM3 = fVolumeCm3/fM3toCm3;
			this.dMassGrams = mt.getDensityGramsPerCm3() * fVolumeCm3;
			return this; 
		}
		
		public double getMassGrams(){
			return dMassGrams;
		}
		
		public double getMassKg(){
			return dMassGrams/1000.0;
		}
		
		public MatterStatus setVolumeM3(double fVolumeM3) {
			setVolumeCm3(fVolumeM3*fM3toCm3);
			return this; 
		}
		
		public Matter getMatter() {
			return mt;
		}

		public double getVolumeM3() {
			return fVolumeM3;
		}

		
	}
	
	public Matter get(String strId){
		return hm.get(strId);
	}
}
