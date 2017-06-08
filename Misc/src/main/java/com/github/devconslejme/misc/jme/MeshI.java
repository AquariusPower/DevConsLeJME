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
package com.github.devconslejme.misc.jme;

import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;

import com.github.devconslejme.misc.CalcI;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.jme3.math.FastMath;
import com.jme3.math.Triangle;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Mesh.Mode;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.VertexBuffer.Type;
import com.jme3.scene.shape.Box;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Sphere;
import com.jme3.util.BufferUtils;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class MeshI {
	public static MeshI i(){return GlobalManagerI.i().get(MeshI.class);}
	
	private boolean	bDebug;
	
	/**
	 * origin 0,0,0 at mesh's center
	 * cone tip at Z=0.5f
	 */
	public static class Cone extends Cylinder{
		static float fRadius1Default=0.01f;//TODO can be 0f?
		
		public Cone() {
			this(1f);
		}
		public Cone(float fScale) {
			this(3, 9, 0.5f*fScale, fScale, true, false);
		}

		public Cone(int axisSamples, int radialSamples, float radius2, float height) {
			this(axisSamples, radialSamples, radius2, height, false, false);
		}
		
		public Cone(int axisSamples, int radialSamples, float radius2, float height,	boolean closed) {
			this(axisSamples, radialSamples, radius2, height, closed, false);
		}
		
		public Cone(int axisSamples, int radialSamples, float radius2, float height, boolean closed, boolean inverted) {
			super(axisSamples, radialSamples, fRadius1Default, radius2, height, closed, inverted);
		}
	}
	
	/**
	 * TODO ignore intersections? but if they de-intersect (move away each other)... too much trouble?
	 * @return in cubic world units
	 */
	public Double volumeOf(Spatial spt){
		ArrayList<Geometry> ageom = new ArrayList<Geometry>();
		if (spt instanceof Node) {
			Node node = (Node) spt;
			ageom.addAll(
				SpatialHierarchyI.i().getAllChildrenOfTypeRecursiveFrom(node, Geometry.class, null));
		}else
		if(spt instanceof Geometry){
			ageom.add((Geometry)spt);
		}
		
		double dVolume=0;
		for(Geometry geom:ageom){
			if(isDebug()){
				MessagesI.i().putReviewableMsg(
					MessagesI.i().output(System.out, "MeshVolume", this, "\n"+ 
						volumeOf(geom)+"//"+geom+"\n"+
						volOfMesh(geom.getMesh(),geom.getWorldScale())+"//MESH//"+geom)); 
			}
			dVolume += volumeOf(geom);
		}
		
		return dVolume;
	}
	
	/**
	 * @return in cubic world units
	 */
	public double volumeOf(Geometry geom){
		Mesh mesh=geom.getMesh();
		Vector3f v3fWScale = geom.getWorldScale();
		return volumeOf(mesh,v3fWScale);
	}
	
	public double volumeOf(Mesh mesh, Vector3f v3fWScale) {
		if (mesh instanceof Box)return volOfBox((Box)mesh,v3fWScale);
		if (mesh instanceof Sphere)return volOfSphere((Sphere)mesh,v3fWScale);
		return volOfMesh(mesh,v3fWScale);
//		throw new UnsupportedOperationException("mesh type not supported yet "+mesh.getClass());
	}
	public double volOfMesh(Mesh mesh, Vector3f v3fWScale) {
		// regular closed meshes
		double d=0;
		for(int i=0;i<mesh.getTriangleCount();i++){
			Triangle tri = new Triangle();
			mesh.getTriangle(i, tri);
			d+=triangleSVol(tri,v3fWScale);
		}
    return Math.abs(d);
	}

	/**
	 * this one does not depend on the mesh, and will be highly precise
	 * @param s
	 * @param v3fWScale
	 * @return
	 */
	public Double volOfSphere(Sphere s, Vector3f v3fWScale) {
		return CalcI.i().sphereVolume(
				s.radius * v3fWScale.x,
				s.radius * v3fWScale.y,
				s.radius * v3fWScale.z
			);
	}

	public double volOfBox(Box box, Vector3f v3fWScale){
		return (double) (
				box.getXExtent()*2f * v3fWScale.x *
				box.getYExtent()*2f * v3fWScale.y *
				box.getZExtent()*2f * v3fWScale.z
			);
	}
	
	public float triangleSVol(Triangle tri, Vector3f v3fWScale) {
		return triangleSVolJme(
				tri.get1().mult(v3fWScale),
				tri.get2().mult(v3fWScale),
				tri.get3().mult(v3fWScale));
	}
	public float triangleSVolJme(Vector3f v3fA, Vector3f v3fB, Vector3f v3fC){
		return v3fA.dot(v3fB.cross(v3fC))/6.0f;
  }
	
	public Mesh box(float fExtent) {
		return new Box(fExtent,fExtent,fExtent);
	}
	
	public Mesh sphereFromVolumeOf(Spatial spt) {
		return sphereFromVolume(volumeOf(spt));
	}
//	/**
//	 * v = (4/3)*pi * r^3
//	 * r^3 = v / ((4/3)*pi)
//	 * r = (v / ((4/3)*pi)) ^ (1/3)
//	 */
//	public float radiusFromVolume(double dVolume) {
//		return (float) Math.cbrt( dVolume / ((4f/3f)*Math.PI) );
//	}
	public Mesh sphereFromVolume(double dVolume) {
		return sphere((float) CalcI.i().radiusFromVolume(dVolume));
	}
	public Mesh sphere(float fRadius) {
		return new Sphere(5, 6, fRadius);
	}
	public Mesh cone(float fScale) {
		return new Cone(fScale);
	}
	
	/**
	 * 
	 * @param av3f each dot from the multi-line
	 * @return
	 */
	public Mesh updateMultiLineMesh(Mesh mesh, Vector3f[] av3f){
		if(mesh==null)mesh=new Mesh();
//		mesh.setStreamed();
		mesh.setMode(Mode.LineStrip);
		
		if(av3f.length>10000){
			//TODO determine how much is a good max amount of dots...
			MessagesI.i().warnMsg(this, "this is a lot of dots! is it ok?", av3f.length);
		}
		
		FloatBuffer fbuf = BufferUtils.createFloatBuffer(av3f);
		mesh.setBuffer(Type.Position,3,fbuf);
		
		ShortBuffer sbuf = BufferUtils.createShortBuffer(av3f.length);
		for(Short si=0;si<sbuf.capacity();si++){sbuf.put(si);}
		
		mesh.setBuffer(Type.Index,1,sbuf);
		
		mesh.updateBound();
		mesh.updateCounts();
		
		return mesh;
	}

	public boolean isDebug() {
		return bDebug;
	}

	public MeshI setDebug(boolean bDebug) {
		this.bDebug = bDebug;
		return this; //for beans setter
	}

	public Cylinder cylinder(float fHeight, float fRadius) {
		return new Cylinder(3,9,fRadius,fHeight);
	}

}
