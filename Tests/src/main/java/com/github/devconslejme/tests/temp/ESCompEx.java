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

import java.util.ArrayList;

import com.simsilica.es.EntityComponent;
import com.simsilica.es.PersistentComponent;

/**
 * Component example to have only a single constructor.
 * 
 * usage example:
 * new ESCompEx(oldESCompEx, new CompBean()
 * 	.setBoundingHeightZ(1)
 * 	.setLastFocusTime(2)
 * )
 * 
 * Component: only getters, unmuttable, do not extend;
 * TODO store only things that cant be externally changed neither references?
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ESCompEx implements EntityComponent, PersistentComponent{
	private CompBean bean;
	
	/**
	 * why make it too simple if it can be harmlessly a bit complex? :)
	 */
	public static class CompBean{
		/** to apply Setter's Calls At Parent's bean */
		private CompBean beanTarget;
		private ArrayList<Runnable> arSetterCalls=new ArrayList<Runnable>();
		/** add set call */
		private void add(Runnable r){arSetterCalls.add(r);}
		
		////////// fields
		private float fBoundingHeightZ;
		private long lLastFocusTime;
		
		public CompBean(){
			fBoundingHeightZ=0f;
			lLastFocusTime=-1;
		}
		
		private void copyFrom(CompBean copyFrom){
			this.fBoundingHeightZ=copyFrom.fBoundingHeightZ;
			this.lLastFocusTime=copyFrom.lLastFocusTime;
		}
		
		///////////////// Getters
		public float getBoundingHeightZ() {
			return fBoundingHeightZ;
		}
		public long getLastFocusTime() {
			return lLastFocusTime;
		}
		
		/*************************************************
		 * auto generated monsterified setters using EntitySystemComponentBeanMonsterSetter.xml, 
		 * furtherly quickly macro better-formatted thru Linux Wine + Notepad++ !
		 *************************************************/
		
		/** */
		public CompBean setBoundingHeightZ(float fBoundingHeightZ){add(new Runnable(){@Override public void run(){
			beanTarget.fBoundingHeightZ = fBoundingHeightZ;}});return this;
		}
		
		public CompBean setLastFocusTime(long lLastFocusTime){add(new Runnable(){@Override public void run(){
			beanTarget.lLastFocusTime = lLastFocusTime;}});return this;
		}
		
	}
	
	//////////////// delegate getters
	public float getBoundingHeightZ() {
		return bean.getBoundingHeightZ();
	}
	
	public long getLastFocusTime() {
		return bean.getLastFocusTime();
	}
	
	/**
	 * The "one constructor to rule them all".
	 * @param copyFrom can be null
	 * @param beanNewValues can be null
	 */
	public ESCompEx(ESCompEx copyFrom, CompBean beanNewValues){
		bean = new CompBean();
		
		if(copyFrom!=null)bean.copyFrom(copyFrom.bean); //initialize with original component
		
		if(beanNewValues!=null){
			beanNewValues.beanTarget=this.bean;
			for(Runnable r:beanNewValues.arSetterCalls)r.run();
		}
	}
	
	@Override
	public ESCompEx clone(){
		return new ESCompEx(this,null);
	}
}