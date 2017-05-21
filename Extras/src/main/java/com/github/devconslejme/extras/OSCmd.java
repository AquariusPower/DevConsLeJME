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

package com.github.devconslejme.extras;

import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.Function;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class OSCmd {
	
	private Function<String, ArrayList<String>>	funcToCmdParams;

	public void configure(Function<String,ArrayList<String>> funcToCmdParams){
		this.funcToCmdParams = funcToCmdParams;
	}
	
	public Boolean runLinuxCmd(String strCmd){
		return runOSCommand("linux '"+strCmd+"'");
	}
	
	/**
	 * Non matching OS cmd will just be ignored/skipped, 
	 * so several OSs can have their related commands one after another without a problem!
	 * 
	 * @param strLine ex.: "linux 'ls 123'"
	 * @return
	 */
	public Boolean runOSCommand(String strLine){
		boolean bOk=true;
		
		ArrayList<String> astrCmdParams = funcToCmdParams.apply(strLine);
		
		String strOSName=System.getProperty("os.name");
		if(!strOSName.equalsIgnoreCase(astrCmdParams.get(0))){
			/**
			 * skip message would be just annoying...
			 */
			return null; //just skip
		}
		
		ArrayList<String> astrOSCmd = new ArrayList<String>();
		if(strOSName.equalsIgnoreCase("linux")){
			// works better/easier using bash
			astrOSCmd.add("bash");
			astrOSCmd.add("-c");
//			astrOSCmd.addAll(getAllParamsStrCopy(astrCmdParams));
			astrOSCmd.addAll(Arrays.asList(
				Arrays.copyOfRange(astrCmdParams.toArray(new String[0]), 1, astrCmdParams.size())
			));
		}
		
		try {
//			LoggingI.i().logMarker("Running OS command:Begin");
			System.out.println("OSCommand:"+astrOSCmd);
			
//			Process p = Runtime.getRuntime().exec(astrOSCmd.toArray(new String[0]));
//			InputStream isErr = p.getErrorStream();
			
			// the process builder accepts each param separated
			ProcessBuilder pb = new ProcessBuilder(astrOSCmd);
			pb.redirectOutput(Redirect.INHERIT);
			pb.redirectError(Redirect.INHERIT);
			Process p = pb.start();
			int iExit = p.waitFor();
			
			System.out.println("Running OS command:End:Return="+iExit);
			bOk = iExit==0;
		} catch (IOException|InterruptedException e) {
			e.printStackTrace();
//			LoggingI.i().logExceptionEntry(e, astrOSCmd.toString());
		}
		
		return bOk;
	}

//	private Collection<? extends String> getAllParamsStrCopy(			ArrayList<String> astrCmdParams) {
//		String.join(" ", astrCmdParams);
//		;
//		
//		ArrayList<String> astr= new ArrayList<String>();
//		
//		for(Object obj:aobjParamsList){
//			if(Float.class.isInstance(obj) || Double.class.isInstance(obj)){
//				astr.add(String.format("%."+iFloatPrecision+"f", obj));
//			}else{
//				astr.add(obj.toString());
//			}
//		}
//		
//		return astr; 
//
//		
//		return null;
//	}
	
}
