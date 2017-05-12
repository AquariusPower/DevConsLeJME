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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
* @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
*/
public class TimeFormatI {
	public static TimeFormatI i(){return GlobalManagerI.i().get(TimeFormatI.class);}
	
	SimpleDateFormat dateFormat = new SimpleDateFormat();
	Date dateRealTime = new Date();
	
	private SimpleDateFormat	sdf  = new SimpleDateFormat("HH:mm:ss", Locale.getDefault()); //TODO use this too? sdf.setTimeZone(TimeZone.getTimeZone("GMT+0"));
	
	public String formatElapsed(long lResolution, long lTime) {
		long lElapsedMilis=TimeConvertI.i().getMilisFrom(lResolution, lTime);
		return sdf.format(new Date(lElapsedMilis - TimeZone.getDefault().getRawOffset()));
	}
	
	public String getRealTimeFormatted() {
		return getRealTimeFormatted(null,null);
	}
	/**
	 * 
	 * @param lMilis current time if null
	 * @param strDateFormatPattern if null will be default full time pattern
	 * @return
	 */
	public String getRealTimeFormatted(Long lMilis,String strDateFormatPattern) {
		if(lMilis==null)lMilis=System.currentTimeMillis();
		if(strDateFormatPattern==null)strDateFormatPattern="yyyy/MM/dd+HH:mm:ss.SSS";
		
		dateFormat.applyPattern(strDateFormatPattern);
		dateRealTime.setTime(lMilis);
	//	System.out.println(XMLDecoder.createHandler(null,null,null));
		return dateFormat.format(dateRealTime);
	}
	
}
