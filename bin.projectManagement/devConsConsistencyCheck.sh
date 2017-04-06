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

#cd ..

#~ IFS=$'\n' read -d '' -r -a astrFileList < <(find -iname "*.java")&&:
#~ 
#~ function FUNCvalidate(){ # <strFile> <strPkg> <strRestrictedTo>
	#~ local lstrFile="$1";shift
	#~ local lstrPkg="$1";shift
	#~ local lstrRestrictedTo="$1";shift
	#~ 
	#~ if egrep -q "package com[.]github[.]devconslejme[.]${lstrPkg};" "$lstrFile";then
		#~ bMatched=true;
		#~ if \
			#~ egrep "import com[.]github[.]devconslejme[.]" "$lstrFile" |\
				#~ egrep -v "import com[.]github[.]devconslejme[.](${lstrRestrictedTo})" \
		#~ ;then
			#~ bHasProblems=true
			#~ echoc -p "Lacking cohesion: only from $lstrRestrictedTo"
		#~ fi
	#~ fi
#~ }
#~ 
#~ for strFile in "${astrFileList[@]}";do
	#~ bMatched=false;
	#~ bHasProblems=false;
	#~ 
#~ #	SECFUNCdrawLine --stay --left " $strFile " #file info
	#~ SECFUNCdrawLine --left " $strFile " #file info
	#~ 
	#~ # tests import all
	#~ if [[ "$strFile" =~ [.]/Tests/.* ]];then
		#~ continue
	#~ fi
	#~ 
	#~ FUNCvalidate "$strFile" "misc" "misc"
	#~ FUNCvalidate "$strFile" "extras" "extras"
	#~ FUNCvalidate "$strFile" "devcons" "misc|gendiag|devcons"
	#~ FUNCvalidate "$strFile" "gendiag" "misc|gendiag"
	#~ 
	#~ if ! $bMatched;then
		#~ echoc -p "out of scope"
	#~ fi
	#~ 
	#~ if $bHasProblems;then
		#~ echoc --info --say "problems found"
	#~ fi
#~ done

strAllDeps="`secJavaDependencyList.sh |sort -u |grep ":com/github/devconslejme"`"

#~ echoc --info "anything that shows below will be a problem..."

function FUNCchk(){ # <package to validate> <<what package beyond self can it access> ...>
	local lstrPkg="$1";shift
	
	local lastrOtherPkgs=()
	if [[ -n "${1-}" ]];then
		lastrOtherPkgs+=("${@-}")
	fi
	
	lastrOtherPkgs+=($lstrPkg)
	
	echoc --info "Any pkg dep shown below will be a high coupling problem for: @{g}$lstrPkg @{r}${lastrOtherPkgs[@]}"
	
	local lstrOtherPkgs="`echo "${lastrOtherPkgs[@]}" |tr " " "|"`"
	
	# grep: before ':' is the full path for the class file being analised
	if echo "$strAllDeps" |grep "/$lstrPkg/[^/]*:" |egrep -v ":(.*/($lstrOtherPkgs)/)";then
		bProblemFound=true
	fi
	
	return 0 #prevents last cmd no-problem error
}

while true;do 
	bProblemFound=false

	# misc can only access misc
	#echo "$strAllDeps" |grep "/misc/[^/]*:" |egrep -v ":(.*/misc/)"&&:
	FUNCchk misc

	# misc/jme can only access misc or misc/jme
	#echo "$strAllDeps" |grep "/misc/jme/[^/]*:" |egrep -v ":(.*/misc/|.*/misc/jme/)"&&:
	FUNCchk misc/jme misc

	# misc/lemur can only access misc or misc/jme or misc/lemur
	#echo "$strAllDeps" |grep "/misc/lemur/[^/]*:" |egrep -v ":(.*/misc/|.*/misc/jme/|.*/misc/lemur/)"&&:
	FUNCchk misc/lemur misc/jme misc

	# gendiag can only access misc or misc/jme or misc/lemur or gendiag
	#echo "$strAllDeps" |grep "/gendiag/[^/]*:" |egrep -v ":(.*/misc/|.*/misc/jme/|.*/misc/lemur/|.*/gendiag/)"&&:
	FUNCchk gendiag misc/lemur misc/jme misc

	# devcons can only access misc or misc/jme or misc/lemur or gendiag or devcons
	#echo "$strAllDeps" |grep "/devcons/[^/]*:" |egrep -v ":(.*/misc/|.*/misc/jme/|.*/misc/lemur/|.*/gendiag/|.*/devcons/)"&&:
	FUNCchk devcons gendiag misc/lemur misc/jme misc

	# extras can only access extras
	#echo "$strAllDeps" |grep "/extras/[^/]*:" |egrep -v ":(.*/extras/)"&&:
	FUNCchk extras

	if $bProblemFound;then
		echoc -p --info --say "problem found!"
	fi
	
	echoc -w -t 600
done
