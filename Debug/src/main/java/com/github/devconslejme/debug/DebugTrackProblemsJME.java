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
import java.util.Arrays;

import com.github.devconslejme.misc.CheckProblemsI.ICheckProblems;
import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.SimulationTimeI;
import com.github.devconslejme.misc.ThreadX;
import com.github.devconslejme.misc.ThreadX.RunnableX;
import com.github.devconslejme.misc.jme.ColorI;
import com.github.devconslejme.misc.jme.SpatialHierarchyI;
import com.jme3.app.Application;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Sphere;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class DebugTrackProblemsJME implements ICheckProblems{
	public static DebugTrackProblemsJME i(){return GlobalManagerI.i().get(DebugTrackProblemsJME.class);}

	private ArrayList<Node>	anodeList = new ArrayList<Node>();
//	private Application	app;
	
	@Override
	public int checkProblems(Throwable thr){
		return checkForSpatialsImproperlyModified(thr, true).size();
	}
	
	public void configure(Node... anode){
		this.anodeList.addAll(Arrays.asList(anode));
//		app = GlobalManagerI.i().get(Application.class);
	}
	
	private ArrayList<Spatial> checkForSpatialsImproperlyModified(Throwable thr, boolean bDumpMessage){
		ArrayList<Spatial> asptList = new ArrayList<Spatial>();
		
		String strMsg="Make sure you do not modify the scene from another thread!";
		if(thr.getMessage()!=null && thr.getMessage().contains(strMsg)){
			for(Node node:anodeList){
				asptList.addAll(checkForSpatialsImproperlyModified(node));
			}
	//		ArrayList<Spatial> asptList = checkForSpatialsImproperlyModified(GlobalRootNodeI.i());
	//		asptList.addAll(checkForSpatialsImproperlyModified(GlobalGUINodeI.i()));
			
			if(bDumpMessage && asptList.size()>0){
				String str="";
				for(Spatial spt : asptList){
					str+=spt.getName()+"@";
					for(Node node:SpatialHierarchyI.i().getAllParents(spt,true)){
						str+=node.getName()+"/";
					}
				}
				
				MessagesI.i().putReviewableMsg(
					MessagesI.i().output(System.err, "CriticalError", this, DebugTrackProblemsJME.class.getSimpleName()+":SpatialProblem:"+str, asptList));
			}
		}
		
		return asptList;
	}
	
	private ArrayList<Spatial> checkForSpatialsImproperlyModified(Node nodeParentest){
		if(nodeParentest.getParent()!=null)throw new DetailedException("parentest node must have no parent...",nodeParentest);
		
		return recursiveFindModifications(nodeParentest,null);
	}
	
	private ArrayList<Spatial> recursiveFindModifications(Node node, ArrayList<Spatial> asptIn){
		if(asptIn==null)asptIn = new ArrayList<Spatial>();
		for(Spatial sptChild : node.getChildren()){
			if((int)UnsafeDebugHacksI.i().getOrSetFieldValueHK(Spatial.class, sptChild, "refreshFlags", null, false, null)!=0){
				asptIn.add(sptChild);
			}
			if(sptChild instanceof Node){
				recursiveFindModifications((Node)sptChild,asptIn);
			}
		}
		return asptIn;
	}
	
	/**
	 * test modify spatial from another thread problem
	 */
	public void tstModSpt(){
		Geometry geom = new Geometry();
		geom.setName(DebugTrackProblemsJME.class.getSimpleName()+":TestModifySpatialProblem");
//		geom.setMesh(new Line(Vector3f.UNIT_XYZ.mult(100),Vector3f.UNIT_XYZ.mult(100)));
		geom.setMesh(new Sphere(10,10,25));
		geom.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(ColorRGBA.Red));
		anodeList.get(0).attachChild(geom);
		
		ThreadX thread = new ThreadX(new RunnableX() {
			@Override
			public void run() {
//				long lTimePrev=app.getTimer().getTime();
				long lTimePrev=SimulationTimeI.i().getNanoTime();
				while(true){
					/**
					 * only do it if the main thread is being updated
					 * without this limitation, it will cause internal prloblems at Eclipse IDE luna...
					 * and create loads of useless log output entries... 
					 */
					if(lTimePrev != SimulationTimeI.i().getNanoTime()){ 
						geom.setLocalTranslation(FastMath.nextRandomFloat(),0,0);
						
						MessagesI.i().output(System.err, "CreatingTestProblem:", this , geom.getMesh().getClass().getSimpleName()+"/"+geom.getLocalTranslation());
						
						lTimePrev = SimulationTimeI.i().getNanoTime();
					}else{
						int i=0;i++;int i2=i; //to put breakpoint
					}
					
					sleep(10);
				}
			}
		});
		thread.start();
	}

}
