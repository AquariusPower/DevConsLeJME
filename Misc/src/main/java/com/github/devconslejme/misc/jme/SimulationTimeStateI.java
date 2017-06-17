/* 
Copyright (c) 2016-2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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

import java.io.IOException;
import java.lang.reflect.Field;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.jme.SavableHelperI.ISavableFieldAccess;
import com.github.devconslejme.misc.jme.SavableHelperI.SaveSkipper;
import com.jme3.app.Application;
import com.jme3.export.JmeExporter;
import com.jme3.export.JmeImporter;
import com.jme3.export.Savable;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SimulationTimeStateI extends SimpleAppState implements Savable,ISavableFieldAccess {
	public static SimulationTimeStateI i(){return GlobalManagerI.i().get(SimulationTimeStateI.class);}
	
	public void configure(){
		AppI.i().attatchAppState(this);
//		GlobalManagerI.i().get(Application.class).getStateManager().attach(this);
	}
	
	@Override
	public void update(float tpf) {
		super.update(tpf);
		SimulationTimeI.i().updateAddFrameTime(tpf);
	}

	/**
	 * This happens on the object being directly used by the application.
	 */
	@Override
	public void write(JmeExporter ex) throws IOException {
		SavableHelperI.i().write(this,ex);
	}
	
	/**
	 * This happens on a new instance, requires {@link #applyValuesFrom(CompositeSavableAbs)} to be made useful.
	 */
	@Override
	public void read(JmeImporter im) throws IOException {
		SavableHelperI.i().read(this,im);
	}

	@Override
	public Object getFieldValue(Field fld) throws IllegalArgumentException,			IllegalAccessException {
		throw new UnsupportedOperationException("method not implemented yet");
//		return null;
	}

	@Override
	public void setFieldValue(Field fld, Object value)			throws IllegalArgumentException, IllegalAccessException {
		throw new UnsupportedOperationException("method not implemented yet");
	}

	@Override
	public SaveSkipper<?> getSkipper() {
		throw new UnsupportedOperationException("method not implemented yet");
//		return null;
	}
}