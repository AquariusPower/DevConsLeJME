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
package com.github.devconslejme.debug;

import java.util.ArrayList;

import com.github.devconslejme.misc.CheckProblemsI.ICheckProblems;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;

/**
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class DebugTrackProblemsJME implements ICheckProblems{
	public static DebugTrackProblemsJME i(){return GlobalManagerI.i().get(DebugTrackProblemsJME.class);}

	private ArrayList<Node>	anode = new ArrayList<Node>();
	
	@Override
	public int checkProblems(String strMsg, Throwable thr){
		return checkForSpatialsImproperlyModified(strMsg, thr, true).size();
	}
	
	public void addNode(Node node){
		anode.add(node);
	}
	
	private ArrayList<Spatial> checkForSpatialsImproperlyModified(String strMsg, Throwable thr, boolean bDumpMessage){
		if(strMsg!=null){
			if(!strMsg.contains("Make sure you do not modify the scene from another thread!"))return null;
		}
		
		ArrayList<Spatial> asptList = new ArrayList<Spatial>();
		for(Node node:anode){
			asptList.addAll(checkForSpatialsImproperlyModified(node));
		}
//		ArrayList<Spatial> asptList = checkForSpatialsImproperlyModified(GlobalRootNodeI.i());
//		asptList.addAll(checkForSpatialsImproperlyModified(GlobalGUINodeI.i()));
		
		if(bDumpMessage){
			if(asptList.size()>0){
				String strSpatialNames="";
				for(Spatial spt : asptList){
					strSpatialNames+=","+spt.getName();
				}
				
				String strMsgOverride = strMsg;
				strMsgOverride += "Problem spatial name(s):" + strSpatialNames; 
				
				System.err.println(strMsgOverride);
			}
		}
		
		return asptList;
	}
	
	private ArrayList<Spatial> checkForSpatialsImproperlyModified(Node nodeParentest){
		if(nodeParentest.getParent()!=null)throw new DetailedException("parentest node must have no parent...",nodeParentest);
		
		return recursiveFindModifications(nodeParentest,null);
	}
	
	public ArrayList<Spatial> recursiveFindModifications(Node node, ArrayList<Spatial> asptIn){
		if(asptIn==null)asptIn = new ArrayList<Spatial>();
		for(Spatial sptChild : node.getChildren()){
			if(UnsafeDebugHacksI.i().getOrSetFieldValueHK(Spatial.class, sptChild, "refreshFlags", int.class, null, false, null)!=0){
				asptIn.add(sptChild);
			}
			if(sptChild instanceof Node){
				recursiveFindModifications((Node)sptChild,asptIn);
			}
		}
		return asptIn;
	}

}
