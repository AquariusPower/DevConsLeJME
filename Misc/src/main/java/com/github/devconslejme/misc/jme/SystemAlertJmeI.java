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
import com.github.devconslejme.misc.SystemAlertI;
import com.jme3.scene.Spatial;

/**
 * TODO implement a JME only alert, it currently will only work with lemur
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SystemAlertJmeI extends SystemAlertI {
	public static SystemAlertJmeI i(){return GlobalManagerI.i().retrieveOverridingSupers(SystemAlertJmeI.class, null, SystemAlertI.class);}
	
	private Spatial	sptAlert;
	
	public void configure(){
//		if(!GlobalManagerI.i().isSet(SystemAlertI.class)){
//			GlobalManagerI.i().putGlobal(SystemAlertI.class, this); //overrides global
//		}
	}

	public Spatial getAlertSpatial() {
		return sptAlert;
	}
	
	protected void setAlertSpatial(Spatial spt){
		this.sptAlert=spt;
	}
	
	@Override
	public Spatial getActionSourceElement() {
		return (Spatial)super.getActionSourceElement();
	}
	
	@Override
	public StackTraceElement[] showSystemAlert(String strMsg, Object objActionSourceElement) {
		assert(objActionSourceElement==null || Spatial.class.isInstance(objActionSourceElement));
		return super.showSystemAlert(strMsg, objActionSourceElement);
	}
	
//	@Override
//	protected void captureUserInput() {
//		QueueI.i().enqueue(new CallableXAnon() {
//			@Override
//			public Boolean call() {
//				update(getTPF());
//				return true;
//			}
//		});
//	}
	
//	@Override
//	protected boolean updateCapture(float fTPF, CallableX cx) {
//		// TODO Auto-generated method stub
//		throw new UnsupportedOperationException("method not implemented");
//		return super.updateCapture(fTPF, cx);
//	}
	
//	private void update(float tpf) {
//		//TODO do something here
//	}
}
