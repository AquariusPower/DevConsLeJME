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

package com.github.devconslejme.deprecatedreminder;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;

import com.github.devconslejme.misc.JavadocI;

/**
 * Deprecated: see {@link JavadocI}
 * 
 * Use alternatively to JavaDocs, to provide easy help for javascripts. 
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
@Deprecated
public class DynamicDocsI {
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {
		ElementType.ANNOTATION_TYPE,
		ElementType.CONSTRUCTOR,
		ElementType.FIELD,
		ElementType.LOCAL_VARIABLE,
		ElementType.METHOD,
		ElementType.PACKAGE,
		ElementType.PARAMETER,
		ElementType.TYPE,
		ElementType.TYPE_PARAMETER,
		ElementType.TYPE_USE
	})
	
	public static @interface DynamicDoc {
	  public String info() default "";
	  public String[] parameters();
	}
	
	@DynamicDoc(info="test info",parameters = {
		"p1 - parm info",
		"p2 - parm info"})     
	public void example(String p1, Integer p2){}
	
	public String getAnnotationAsHelp(Class cl, ElementType eType, String strFilter){
		StringBuilder sb = new StringBuilder();
		for(DynamicDoc dd:getAnnotation(cl, eType, strFilter)){
			if(sb.length()==0)sb.append(System.lineSeparator());
			sb.append(dd.info()+System.lineSeparator());
			for(String str:dd.parameters()){
				sb.append("\t"+str+System.lineSeparator());
			}
		}
		
		// One line header. Inverted as the last will be the 1st
		sb.insert(0,System.lineSeparator());
		sb.insert(0,", Filter: "+strFilter);
		sb.insert(0,", Type: "+eType);
		sb.insert(0,"Class: "+cl.getSimpleName());
		
		return sb.toString();
	}
	
	public ArrayList<DynamicDoc> getAnnotation(Class cl, ElementType eType, String strFilter){
		ArrayList<DynamicDoc> addList = new ArrayList<DynamicDoc>();
		
		switch(eType){
			case ANNOTATION_TYPE:
				break;
			case CONSTRUCTOR:
				for(Constructor val:cl.getConstructors()){
					if(Modifier.isPublic(val.getModifiers())){
//						addList.add(val.getAnnotation(DynamicDoc.class));
					}
				}
				break;
			case FIELD:
				for(Field val:cl.getFields()){
					if(Modifier.isPublic(val.getModifiers())){
						addList.add(val.getAnnotation(DynamicDoc.class));
					}
				}
				break;
			case METHOD:
				for(Method val:cl.getMethods()){
					if(Modifier.isPublic(val.getModifiers())){
						addList.add(val.getAnnotation(DynamicDoc.class));
					}
				}
				break;
			case PACKAGE:
				break;
			case PARAMETER:
				break;
			case TYPE:
				break;
			case TYPE_PARAMETER:
				break;
			case TYPE_USE:
				break;
			default:
				throw new UnsupportedOperationException(""+eType);
		}
		
		return addList;
	}
	
}
