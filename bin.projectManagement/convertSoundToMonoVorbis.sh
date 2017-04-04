#!/bin/bash

#	Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower>
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

##########
### jme requires vorbis mono format to work properly
##########

#astrFileList=(
#	220189__gameaudio__blip-squeak.wav
#	220197__gameaudio__click-basic.wav
#	220183__gameaudio__click-casual.wav
#	220194__gameaudio__click-heavy.wav
#	220195__gameaudio__click-wooden-1.wav
#	220172__gameaudio__flourish-spacey-2.wav
#	220177__gameaudio__quick-ui-or-event-deep.wav
#	220205__gameaudio__teleport-darker.wav
#)

if [[ "${1-}" == "--help" ]];then
	echo "params: sound files to convert"
	exit 0
fi

astrFileList=("$@")

SEDremoveExt="s'(.*)[.][^.]*$'\1'"

for strFile in "${astrFileList[@]}";do
	if [[ ! -f "$strFile" ]];then
		echoc -p "missing strFile='$strFile'"
		exit 1
	fi
	
#	echo "FILE: $strFile"
#	strFileOut="${strFile%.wav}.mono.ogg"
	strFileOut="`echo "$strFile" |sed -r "$SEDremoveExt"`.mono.ogg"
	if [[ ! -f "$strFileOut" ]];then
		# TODO confirm if this mixes all channels into one.
		if ! avconv -i "$strFile" -ac 1 -acodec libvorbis "$strFileOut";then
			echoc -p "conversion failed..."
			exit 1
		fi
	else
		echo "found: $strFileOut"
	fi
done

