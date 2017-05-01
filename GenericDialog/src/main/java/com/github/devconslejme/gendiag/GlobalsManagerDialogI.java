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
package com.github.devconslejme.gendiag;

import java.lang.reflect.Method;

import com.github.devconslejme.gendiag.SimpleGenericDialog.OptionData;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.JavaLangI;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class GlobalsManagerDialogI {
	public static GlobalsManagerDialogI i(){return GlobalManagerI.i().get(GlobalsManagerDialogI.class);}

	private SimpleMaintenanceGenericDialog	smd;
	private boolean	bShowInherited;
	
	public GlobalsManagerDialogI(){
		
	}
	
	public void configure(){
		
	}
	
	public void init(){
		smd = new SimpleMaintenanceGenericDialog() {
			@Override
			public void updateMaintenanceList() {
				prepareGlobalsForMaintenance();
			}
		};
		
		DialogHierarchyStateI.i().showDialog(smd.getDialog());
	}
	
	private static class MethodInfo{
		Object o;
		Method m;
		public MethodInfo(Object o, Method m) {
			super();
			this.o = o;
			this.m = m;
		}
		
	}
	
	protected void prepareGlobalsForMaintenance() {
		for(Object o:GlobalManagerI.i().getListCopy()){
			OptionData odGlobal = smd.putSection(null, o.getClass().getSimpleName()+"("+o.getClass().getPackage().getName()+")");
			Method[] am = isShowInherited() ? o.getClass().getMethods() : o.getClass().getDeclaredMethods();
			for(Method m:am){
				smd.putOption(odGlobal, JavaLangI.i().m.get, new MethodInfo(o,m));
			}
		}
	}

	public boolean isShowInherited() {
		return bShowInherited;
	}

	public void setShowInherited(boolean bShowInherited) {
		this.bShowInherited = bShowInherited;
	}
	
}
