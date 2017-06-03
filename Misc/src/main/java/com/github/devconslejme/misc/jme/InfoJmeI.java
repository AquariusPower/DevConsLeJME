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

import java.util.HashMap;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.InfoI;
import com.github.devconslejme.misc.InfoI.Info;
import com.github.devconslejme.misc.StringI;
import com.jme3.math.Vector3f;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class InfoJmeI extends InfoI{
	public static InfoJmeI i(){return GlobalManagerI.i().retrieveOverridingSupers(InfoJmeI.class, true, InfoI.class);}
	
	/** do not create the hashmaps with this one, use only the super, this is just for the constructor */
	public static class InfoJme extends Info{
		public InfoJme(String strKey, Vector3f v3f, int iFloatScale) {
			super(strKey,v3f);
			super.iFloatScale=iFloatScale;
		}
	}
	
	/**
	 * 
	 * @param hm
	 * @param strKey
	 * @param v3f if null, will remove the key too
	 * @param iFloatScale
	 */
	public void putAt(HashMap<String,Info> hm, String strKey,Vector3f v3f, int iFloatScale) {
		if(chkRemove(hm, strKey, v3f))return;
//		if(v3f==null)hm.remove(strKey);
		hm.put(strKey, new InfoJme(strKey,v3f,iFloatScale));
	}
	
	@Override
	public String fmtInfoValue(Info inf) {
		if(Vector3f.class.isInstance(inf.getValue())){
			return StringTextJmeI.i().fmtVector3f(
				inf.getValue(), 
				inf.getFloatScale()==null ? getInfoValueFloatScale() : inf.getFloatScale()
			);
		}
		
		return super.fmtInfoValue(inf);
	}

//	@Override
//	public <T extends InfoJme> HashMap<String,T> createHashMap() {
//		return new HashMap<String, InfoJme>();
//	}

}

