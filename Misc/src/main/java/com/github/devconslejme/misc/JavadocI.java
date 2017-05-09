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

import java.awt.Desktop;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.ArrayList;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class JavadocI {
	public static JavadocI i(){return GlobalManagerI.i().get(JavadocI.class);}
	
	private String	strJavadocFolder = "javadoc/";
	
	public void browseJavadoc(MethodX mh) {
		URI uri = mh.getAsJavadocURI();
		try {
			// external web browser 
			Desktop.getDesktop().browse(uri);
		} catch (IOException e) {
			MessagesI.i().warnMsg(this, e.getMessage(), e, mh, uri);
		}
	}

	public ArrayList<MethodX> prepareAllMethodsHelp(Object obj){
		ArrayList<MethodX> amh = new ArrayList<MethodX>();
		
		for(Method m:obj.getClass().getMethods()){
			MethodX mh = new MethodX();
			mh.setObject(obj);
			mh.setMethod(m);
//			mh.setConcrete(obj.getClass());
//			mh.setDeclaring(m.getDeclaringClass());
			mh.setStatic(Modifier.isStatic(m.getModifiers()));
			
			int i=0;
			for(Class<?> p:m.getParameterTypes()){
				if(!JavaLangI.i().isCanUserTypeIt(p))i++;
			}
			mh.setNonUserTypeableParamsCount(i);
			
			amh.add(mh);
		}
		
		return amh;
	}
	
	public String getJavadocFolder() {
		return strJavadocFolder;
	}

	public void setJavadocFolder(String strJavadocFolder) {
		this.strJavadocFolder = strJavadocFolder;
	}
}
