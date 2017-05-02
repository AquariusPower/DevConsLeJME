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

package com.github.devconslejme.misc;

/**
 * if the user can type it, it is simple
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public enum ESimpleType{ //String is not primitive, but still simple
	Boolean(Boolean.class),
	Int(Integer.class),
	Long(Long.class),
	Float(Float.class),
	Double(Double.class),
	String(String.class),
	;
	
	private Class	cl;

	ESimpleType(Class cl){
		this.cl=cl;
	}
	
	public Class getType(){
		return cl;
	}
	
	public boolean is(ESimpleType e){
		return (this==e);
	}
	
	@SuppressWarnings("unchecked")
	public <T> T parse(String strValue){
		Object ret = null;
		switch(this){
			case Boolean: ret=java.lang.Boolean.parseBoolean(strValue); break;
			case Double: ret=java.lang.Double.parseDouble(strValue); break;
			case Float: ret=java.lang.Float.parseFloat(strValue); break;
			case Int: ret=java.lang.Integer.parseInt(strValue); break;
			case Long: ret=java.lang.Long.parseLong(strValue); break;
			case String: ret=(strValue); break;
		}
		return (T)ret;
	}
	
	public static ESimpleType forClass(Class clValue,boolean bMustMatch) throws UnsupportedOperationException{ //@STATIC_OK
		ESimpleType e = null;
		if(clValue==Float.class		|| clValue==float.class		){e=ESimpleType.Float;}else
		if(clValue==Double.class	|| clValue==double.class	){e=ESimpleType.Double;}else
		if(clValue==Integer.class	|| clValue==int.class			){e=ESimpleType.Int;}else
		if(clValue==Long.class		|| clValue==long.class		){e=ESimpleType.Long;}else
		if(clValue==Boolean.class	|| clValue==boolean.class	){e=ESimpleType.Boolean;}else
		if(clValue==String.class														){e=ESimpleType.String;}else
		{
			if(bMustMatch){
				throw new DetailedException("unsupported value class type ", clValue, bMustMatch);
			}else{
				MessagesI.i().warnMsg(ESimpleType.class, "unsupported value class type", clValue, bMustMatch);
			}
		}
		
		return e;
	}
	
}
