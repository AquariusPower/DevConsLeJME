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

cd ..

IFS=$'\n' read -d '' -r -a astrFileList < <(find -iname "*.java")&&:

function FUNCvalidate(){ # <strFile> <strPkg> <strRestrictedTo>
	local lstrFile="$1";shift
	local lstrPkg="$1";shift
	local lstrRestrictedTo="$1";shift
	
	if egrep -q "package com[.]github[.]devconslejme[.]${lstrPkg};" "$lstrFile";then
		bMatched=true;
		if \
			egrep "import com[.]github[.]devconslejme[.]" "$lstrFile" |\
				egrep -v "import com[.]github[.]devconslejme[.](${lstrRestrictedTo})" \
		;then
			echoc -p "Lacking cohesion: only from $lstrRestrictedTo"
		fi
	fi
}

for strFile in "${astrFileList[@]}";do
	bMatched=false;
	
#	SECFUNCdrawLine --stay --left " $strFile " #file info
	SECFUNCdrawLine --left " $strFile " #file info
	
	# tests import all
	if [[ "$strFile" =~ [.]/Tests/.* ]];then
		continue
	fi
	
	FUNCvalidate "$strFile" "misc" "misc"
	FUNCvalidate "$strFile" "extras" "extras"
	FUNCvalidate "$strFile" "devcons" "misc|gendiag|devcons"
	FUNCvalidate "$strFile" "gendiag" "misc|gendiag"
	
	#~ strRestrictedTo="misc" # misc  (only from self)
	#~ if egrep -q "package com[.]github[.]devconslejme[.]misc;" "$strFile";then
		#~ bMatched=true;
		#~ if \
			#~ egrep "import com[.]github[.]devconslejme[.]" "$strFile" |\
				#~ egrep -v "import com[.]github[.]devconslejme[.](${strRestrictedTo})" \
		#~ ;then
			#~ echoc -p "Lacking cohesion: only from $strRestrictedTo"
		#~ fi
	#~ fi
	
	#~ strRestrictedTo="extras" # extras (only from self)
	#~ if egrep -q "package com[.]github[.]devconslejme[.]extras;" "$strFile";then
		#~ bMatched=true;
		#~ if \
			#~ egrep "import com[.]github[.]devconslejme[.]" "$strFile" |\
				#~ egrep -v "import com[.]github[.]devconslejme[.](${strRestrictedTo})" \
		#~ ;then
			#~ echoc -p "Lacking cohesion: only from $strRestrictedTo"
		#~ fi
	#~ fi
	
	#~ strRestrictedTo="misc|gendiag" # (core) 
	#~ if egrep -q "package com[.]github[.]devconslejme.devcons;" "$strFile";then
		#~ bMatched=true;
		#~ if \
			#~ egrep "import com[.]github[.]devconslejme[.]" "$strFile" |\
				#~ egrep -v "import com[.]github[.]devconslejme[.](${strRestrictedTo})" \
		#~ ;then
			#~ echoc -p "Lacking cohesion: only from $strRestrictedTo"
		#~ fi
	#~ fi

	#~ strRestrictedTo="misc" # gendiag
	#~ if egrep -q "package com[.]github[.]devconslejme.gendiag;" "$strFile";then
		#~ bMatched=true;
		#~ if \
			#~ egrep "import com[.]github[.]devconslejme[.]" "$strFile" |\
				#~ egrep -v "import com[.]github[.]devconslejme[.](${strRestrictedTo})" \
		#~ ;then
			#~ echoc -p "Lacking cohesion: only from $strRestrictedTo"
		#~ fi
	#~ fi
	
	if ! $bMatched;then
		echoc -p "out of scope"
	fi
done
