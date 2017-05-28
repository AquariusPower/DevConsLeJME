#!/bin/bash

#	Copyright (c) 2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
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

SECFUNCuniqueLock --waitbecomedaemon

function FUNCchk(){ # <package to validate> <<what package beyond self can it access> ...>
	local lbMore=false;if [[ "$1" == "--basic" ]];then lbMore=true;shift;fi
	
	local lstrPkg="$1";shift
	
	local lastrOtherPkgs=()
	if [[ -n "${1-}" ]];then
		lastrOtherPkgs+=("${@-}")
	fi
	
	lastrOtherPkgs+=($lstrPkg)
	
	echoc --info "Checking @{g}$lstrPkg @{r}${lastrOtherPkgs[@]}"
	
	local lstrOtherPkgs="`echo "${lastrOtherPkgs[@]}" |tr " " "|"`"
	
	####
	# grep: before ':' is the full path for the class file being analised
	# grep -v: after ':' will match only the specified packages, if there is a '/' after the package (therefore a non specified package), it will show the problem
	####
	if echo "$strDevConsDeps" |grep "/$lstrPkg/[^/]*:" |egrep -v ":.*/($lstrOtherPkgs)/[^/]*$";then
		bProblemFound=true
	fi
	
	if $lbMore;then 
		#####
		# deny any other big libraries, 
		# but these should not be anything complex anyway, 
		# that requires other things than the JDK...
		#####
		local lastrDeps=(
			java # the JDK
			com/github/devconslejme/$lstrPkg # self package
			#com/google #TODO guava?
			#com/simsilica/es #TODO Zay-ES
		)
		local lstrOtherDeps="`echo "${lastrDeps[@]}" |tr ' ' '|'`"
#		declare -p lstrOtherDeps
		if 
			echo "$strAllDeps" \
				|grep "^[.]/.*/$lstrPkg/[^/]*class:" \
				|egrep -v ":($lstrOtherDeps)/";then
			bProblemFound=true
		fi
	fi
	
	return 0 #prevents last cmd no-problem error
}

while true;do 
	bProblemFound=false
	
	pwd
	
	strAllDeps="`secJavaDependencyList.sh |sort -u`"
	strDevConsDeps="`echo "$strAllDeps" |grep ":com/github/devconslejme"`"
	echo "totDepsEntries='`echo "$strDevConsDeps" |wc -l`'"

	echoc --info "Any pkg dep shown below will be a @rhigh coupling problem."
	
	# misc can only access misc
	FUNCchk --basic misc
	#~ echo "$strAllDeps" |grep "^[.]/.*/misc/[^/]*class:" |egrep -v ":(java|com/github/devconslejme/misc)"

	FUNCchk misc/jme 				misc
	FUNCchk misc/jme/game 	misc misc/jme 
	FUNCchk misc/lemur 			misc misc/jme  
	FUNCchk es 							misc misc/jme misc/lemur  
	FUNCchk gendiag 				misc misc/jme misc/lemur es
	FUNCchk devcons 				misc misc/jme misc/lemur es gendiag

	FUNCchk --basic extras
	#~ echo "$strAllDeps" |grep "^[.]/.*/extras/[^/]*class:" |egrep -v ":(java|com/github/devconslejme/extras)"

	if $bProblemFound;then
		echoc -p --info --say "problem found!"
	fi
	
	echoc -w -t 600
done
