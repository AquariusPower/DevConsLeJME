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

import com.github.devconslejme.misc.GlobalManagerI;
import com.jme3.math.FastMath;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Mesh.Mode;
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
	
	/**
	 * tip at Z=0
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
	 * 
	 * @param geom
	 * @return in cubic world units 
	 */
	public Double calcVolume(Geometry geom){
		Vector3f v3f = volumeOf(geom);
		if(v3f==null)return null;
		return 
			((double)v3f.x)
			*
			((double)v3f.y)
			*
			((double)v3f.z);
	}
	public Vector3f volumeOf(Geometry geom){
		Mesh mesh=geom.getMesh();
		Vector3f v3fWScale = geom.getWorldScale();
		
		if (mesh instanceof Box) {
			Box box = (Box) mesh;
			return new Vector3f(
				box.getXExtent()*2f * v3fWScale.x,
				box.getYExtent()*2f * v3fWScale.y,
				box.getZExtent()*2f * v3fWScale.z
			);
		}
		
		if (mesh instanceof Sphere) {
			Sphere s = (Sphere) mesh;
			double d = 4 * FastMath.ONE_THIRD * FastMath.PI * //see BoundingSphere.getVolume()
				((double)(s.radius * v3fWScale.x))
				*
				((double)(s.radius * v3fWScale.y))
				*
				((double)(s.radius * v3fWScale.z));
			float f=(float) Math.cbrt(d);
			return new Vector3f(f,f,f); //so.. it returns like a cube, funny...
		}
		
		throw new UnsupportedOperationException("mesh type not supported yet "+mesh.getClass());
	}
	
	public Mesh box(float fExtent) {
		return new Box(fExtent,fExtent,fExtent);
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
		
		FloatBuffer fbuf = BufferUtils.createFloatBuffer(av3f);
		mesh.setBuffer(Type.Position,3,fbuf);
		
		ShortBuffer sbuf = BufferUtils.createShortBuffer(av3f.length);
		for(Short si=0;si<sbuf.capacity();si++){sbuf.put(si);}
		
		mesh.setBuffer(Type.Index,1,sbuf);
		
		mesh.updateBound();
		mesh.updateCounts();
		
		return mesh;
	}

}
