#!/bin/bash

#	Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
#	
#	All rights reserved.
#
#	Redistribution and use in source and binary forms, with or without modification, are permitted 
#	provided that the following conditions are met:
#
#	1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
#		and the following disclaimer.
#
#	2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
#		and the following disclaimer in the documentation and/or other materials provided with the distribution.
#	
#	3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
#		or promote products derived from this software without specific prior written permission.
#
#	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
#	WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
#	PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
#	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
#	LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
#	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
#	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN 
#	IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

eval `secinit`

astrRunClasses=(`egrep "public static void main[(]" * -rIc --include="*.java" \
	|grep -v :0 \
	|sed -r 's".*/src/main/java/(.*)[.]java:1$"\1"' \
	|tr "/" "."\
`)&&:

declare -p astrRunClasses
#echoc --info "astrRunClasses='${astrRunClasses[@]}'"

for strRunClass in "${astrRunClasses[@]}";do
	if [[ -z "$strRunClass" ]];then echoc -p "strRunClass='$strRunClass'";exit 1;fi
	
	declare -p strRunClass
	
	anPid=(`pgrep -f "java .* $strRunClass"`)&&:
	if [[ -n "${anPid[@]-}" ]];then
		for nPid in "${anPid[@]}";do
			SECFUNCexecA -ce ps --no-headers -o pid,cmd -p $nPid
			
			if((${#anPid[@]}>1));then
				if ! echoc -q "kill $nPid?";then continue;fi
			fi
			
			SECFUNCexecA -ce kill -SIGKILL $nPid
		done
	else
		echoc --info "no pid found"
	fi
done

