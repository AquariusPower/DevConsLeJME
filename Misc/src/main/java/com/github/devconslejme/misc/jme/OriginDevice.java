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

import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.jme.MeshI.Cone;
import com.github.devconslejme.misc.jme.OriginDevice.NodeAxis;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Matrix3f;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Sphere;
import com.jme3.scene.shape.Torus;

/**
 * Improve with fancyness (shaders, lighting, shadows, sfx and voices hehe).
 * 
 * OrDe starts enabled in auto feed mode.
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class OriginDevice<NODEXS extends NodeAxis> extends Node{
//	private ArrayList<NODEXS> anodeMainShapes=new ArrayList<NODEXS>();
//	private ArrayList<NODEXS> anodeHotShapesList=new ArrayList<NODEXS>();
	private float fRadius;
	private int	iCS;
	private int	iRS;
	private Vector3f	v3fBaseSpeed=new Vector3f(1,1,1).mult(0.0025f);
	private float	fIR=0.1f;
	private float	fRotTorOpac=0.15f;
	private Vector3f v3fSpeed=new Vector3f();
	private float	fTPF;
	private boolean bEnabled=false;
	private Node	nodeLastParent;
	
	public static class NodeAxis<SELF extends NodeAxis> extends Node{
		public NodeAxis(String str) {
			super(str);
		}

		EAxis ea;
		Geometry	geom;
		Geometry	geomWireFrame;
		Node nodeGeometries;
		Vector3f	v3fAdd = Vector3f.UNIT_Y.clone();
		
		@SuppressWarnings("unchecked")
		public SELF getThis(){
			return (SELF)this;
		}
		
		public EAxis getEAxis() {
			return ea;
		}
		public SELF setEAxis(EAxis ea) {
			this.ea = ea;
			return getThis();
		}
		public Geometry getGeom() {
			return geom;
		}
		public SELF setGeom(Geometry geom) {
			this.geom = geom;
			return getThis();
		}
		public Geometry getGeomWireFrame() {
			return geomWireFrame;
		}
		public SELF setGeomWireFrame(Geometry geomWireFrame) {
			this.geomWireFrame = geomWireFrame;
			return getThis();
		}
		public Node getNodeGeometries() {
			return nodeGeometries;
		}
		public SELF setNodeGeometries(Node nodeGeometries) {
			this.nodeGeometries = nodeGeometries;
			return getThis();
		}
		public Vector3f getV3fAdd() {
			return v3fAdd;
		}
		public SELF setV3fAdd(Vector3f v3fAdd) {
			this.v3fAdd = v3fAdd;
			return getThis();
		}

	}
	
	public OriginDevice(){
		setName(OriginDevice.class.getSimpleName());
		
		fRadius=5;
		iCS=50;
		iRS=15;
//		fBaseSpeed=0.0025f;
//		v3fSpeed.set(fBaseSpeed,fBaseSpeed,fBaseSpeed);
		v3fSpeed.set(v3fBaseSpeed);
		
		init();
	}
	
	public void update(float fTPF){
		this.fTPF=fTPF;
		
		if(bEnabled){
			if(getParent()!=null){
				nodeLastParent=getParent();
			}else{
				if(nodeLastParent!=null){
					nodeLastParent.attachChild(this);
				}else{
					MessagesI.i().warnMsg(this, "has no last parent", this);
				}
			}
		}else{
			if(getParent()!=null)removeFromParent();
		}
		
		updateTorusRotations();
		updateAxisMainShapes();
	}
	
	private float getRadius() {
		return fRadius*getLocalScale().x; //TODO the main node should not scale differently from y z
	}

	String strMsgError="The origin rotation must not be modified as it is a world reference.";
	@Deprecated	@Override	public void lookAt(Vector3f position, Vector3f upVector) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated	@Override	public void setLocalRotation(Matrix3f rotation) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated @Override	public void setLocalRotation(Quaternion quaternion) {		throw new UnsupportedOperationException("method not implemented");	}
	@Deprecated	@Override	public Spatial rotate(float xAngle, float yAngle, float zAngle) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated @Override	public Spatial rotate(Quaternion rot) {		throw new UnsupportedOperationException(strMsgError);	}
	@Deprecated @Override	public void rotateUpTo(Vector3f newUp) {		throw new UnsupportedOperationException(strMsgError);	}
	
	protected void init(){
		// origin
		attachChild(NodeI.i().createRotationAxes(null));
		
		// toruses
		createAxis(Vector3f.UNIT_X, MeshI.i().box(0.5f));
//		rotate(EAxis.X.get().getRepresentationShape(), 90, false);//TODO wrong
		
		createAxis(Vector3f.UNIT_Y, new Sphere(10,10,0.5f));
		rotate(EAxis.Y.get().getRotatingTorus(), 90, false);
		
		createAxis(Vector3f.UNIT_Z, new Cone());
	}
	
	protected void updateAxisMainShapes() {
//		for(NODEXS node:anodeMainShapes){
		for(EAxis ea:EAxis.values()){
			NODEXS node = ea.get().getRepresentationShape();
			rotateMainShape(node,getRotSpeedCopy());
		}
	}
	
	protected void rotateMainShape(NODEXS node,Vector3f v3f){
		node.rotate(v3f.x,v3f.y,v3f.z);
	}
	
	public static class AxisInfo{
		private EAxis ea;
		private ColorRGBA color;
		
		private NodeAxis	torus;
		private NodeAxis	tip;
		private NodeAxis	feather;
		private NodeAxis	nodeRepresentationShape;
		
		public ColorRGBA getColor() {
			return color;
		}

		private AxisInfo setColor(ColorRGBA color) {
			this.color = color;
			return this; 
		}
		
		public AxisInfo(EAxis ea) {
			this.ea=ea;
		}

		public EAxis getEAxis() {
			return ea;
		}

		@SuppressWarnings("unchecked")
		public <T extends NodeAxis> T getRepresentationShape() {
			return (T)nodeRepresentationShape;
		}

		@SuppressWarnings("unchecked")
		public <T extends NodeAxis> T getRotatingTorus() {
			return (T)torus;
		}

		public NodeAxis getTorusTip() {
			return tip;
		}

		private AxisInfo setTorusTip(NodeAxis tip) {
			this.tip = tip;
			return this; 
		}

		public NodeAxis getTorusFeather() {
			return feather;
		}

		private AxisInfo setTorusFeather(NodeAxis feather) {
			this.feather = feather;
			return this; 
		}
		
		
	}
	
	public static enum EAxis{
		X,
		Y,
		Z,
		;

		private AxisInfo	axisi;

		public void set(AxisInfo axisi) {
			this.axisi = axisi;
		}
		
		public AxisInfo get(){ return axisi; }
	}
	
	protected void updateTorusRotations() {
		Vector3f v3fSpeed = getRotSpeedCopy();
		rotateTor(EAxis.X.get().getRotatingTorus(),v3fSpeed);//,EAxis.X);
		rotateTor(EAxis.Y.get().getRotatingTorus(),v3fSpeed);//,EAxis.Y);
		rotateTor(EAxis.Z.get().getRotatingTorus(),v3fSpeed);//,EAxis.Z);
	}
	
	protected float rotTorSpeed(NODEXS nodeTor,Vector3f v3fSpeed){
		float fSpeed=0;
		switch(nodeTor.ea){
			case X:
				fSpeed=v3fSpeed.x;
				break;
			case Y:
				fSpeed=v3fSpeed.y;
				break;
			case Z:
				fSpeed=v3fSpeed.z;
				break;
		}
		
		return fSpeed;
	}
	
	protected void rotateTor(NODEXS nodeTor,Vector3f v3fSpeed){//, EAxis ea) {
		nodeTor.rotate(0,rotTorSpeed(nodeTor,v3fSpeed),0);
	}
	
	public int getTpfMilis(){
		return (int) (fTPF*1000);
	}
	
	protected void rotate(NODEXS node, float fAngleDegrees, boolean bZOnly){
		float fRotRad=FastMath.DEG_TO_RAD*fAngleDegrees;
		if(!bZOnly && node.ea==EAxis.X)node.rotate(       0, fRotRad, 0);
		if(!bZOnly && node.ea==EAxis.Y)node.rotate(       0, fRotRad, 0);
		if(           node.ea==EAxis.Z)node.rotate(-fRotRad, fRotRad, 0);
	}
	
	protected EAxis createAxis(Vector3f v3fUp, Mesh mesh) {
		ColorRGBA color = new ColorRGBA(v3fUp.x,v3fUp.y,v3fUp.z,1f);
		float fDisplacementTorus = fRadius+1;
		EAxis ea=null;
		if(v3fUp.x==1){ea=EAxis.X;}
		if(v3fUp.y==1){ea=EAxis.Y;}
		if(v3fUp.z==1){ea=EAxis.Z;}
		AxisInfo axisi = new AxisInfo(ea);
		axisi.setColor(color);
		ea.set(axisi);

		// rotating torus
		axisi.torus=createAxisShape(ea,new Torus(iCS,iRS,fIR,fDisplacementTorus), 
			new Vector3f(0,0,0), fRotTorOpac, v3fUp);
		
		// axis representation shape
		axisi.nodeRepresentationShape=createAxisShape(ea,mesh, v3fUp.mult(fRadius), 0.5f, v3fUp, true, null);
//		axisi.torus.attachChild(axisi.getRepresentationShape());
		
		// static rotation track
		NODEXS nodeTrack=createAxisShape(ea,new Torus(iCS,iRS,0.01f,fDisplacementTorus), 
			new Vector3f(0,0,0), 0.15f, v3fUp);
		MiscJmeI.i().addToName(nodeTrack, "Track", false);
		nodeTrack.lookAt(v3fUp, v3fUp);
//		axisi.torus.attachChild(nodeTrack);
		
		// torus core
		NODEXS nodeCore=createAxisShape(ea,new Torus(iCS,iRS,fIR*0.35f,fDisplacementTorus), 
			new Vector3f(0,0,0), fRotTorOpac+0.5f, v3fUp);
		rotate(nodeCore,90,true);
		axisi.torus.attachChild(nodeCore);
		
		createTorusIntersections(axisi.getRotatingTorus(),fDisplacementTorus,v3fUp);
		
		return ea;
	}
	
	protected void createTorusIntersections(NODEXS nodeTor, float fDisplacementTorus, Vector3f v3fUp) {
		float fIRa=fIR*1.5f;
		float fAlpha=fRotTorOpac+0.25f;
		
		// arrow tip
		NODEXS nodePosit = createAxisShape(nodeTor.ea,new Cone(fIRa*2f),
			new Vector3f(fDisplacementTorus,0,0), fAlpha, v3fUp, false, new Vector3f(1,1,2));
		MiscJmeI.i().addToName(nodePosit, "Intersection", false, true);
		nodePosit.lookAt(v3fUp, v3fUp);
		rotate(nodePosit,-90,false);
		nodeTor.attachChild(nodePosit);
		nodeTor.ea.get().setTorusTip(nodePosit);
//		anodeHotShapesList.add(nodePosit);
		
		// other end
		NODEXS nodeNegat=createAxisShape(nodeTor.ea,new Sphere(10,10,fIRa),
			new Vector3f(-fDisplacementTorus,0,0), fAlpha, v3fUp);
		MiscJmeI.i().addToName(nodeNegat, "Intersection", false, true);
		nodeTor.attachChild(nodeNegat);
		nodeTor.ea.get().setTorusFeather(nodeNegat);
//		anodeHotShapesList.add(nodeNegat);
	}
	
	@SuppressWarnings("unchecked")
	public NODEXS createNodeAxis(String strName){
		return (NODEXS)new NodeAxis(strName);
	}
	
	protected NODEXS createAxisShape(EAxis ea, Mesh mesh, Vector3f v3fPos, float fAlpha, Vector3f v3fUp) {
		return createAxisShape(ea, mesh, v3fPos,  fAlpha,  v3fUp, false, null);
	}
	protected NODEXS createAxisShape(EAxis ea, Mesh mesh, Vector3f v3fPos, float fAlpha, Vector3f v3fUp, boolean bAddWireFrame, Vector3f v3fScale) {
		if(v3fScale==null)v3fScale=new Vector3f(1,1,1);
		NODEXS node = createNodeAxis("Node");
		node.ea=ea;
		Geometry geom = GeometryI.i().create(mesh, ColorI.i().colorChangeCopy(ea.get().getColor(),0,fAlpha), true,null);
		node.setGeom(geom);
		
		node.setNodeGeometries(new Node());
		
		Geometry geomWireFrame=null;
		if(bAddWireFrame){
			geomWireFrame = new Geometry("WireFrame",mesh);
			ColorRGBA colorW = ea.get().getColor().clone();
			colorW.a=1;
			geomWireFrame.setMaterial(ColorI.i().retrieveMaterialUnshadedColor(colorW));
			geomWireFrame.getMaterial().getAdditionalRenderState().setWireframe(true);
			
			node.setGeomWireFrame(geomWireFrame);
		}
		
		// name
		MiscJmeI.i().addToName(geom, OriginDevice.class.getSimpleName(), true);
//		if(v3fUp.x==1){node.ea=EAxis.X;}
//		if(v3fUp.y==1){node.ea=EAxis.Y;}
//		if(v3fUp.z==1){node.ea=EAxis.Z;}
		MiscJmeI.i().addToName(geom, ea.toString(), false);
		if(geomWireFrame!=null){
			MiscJmeI.i().addToName(geomWireFrame, ea.toString(), false);
		}
		
		MiscJmeI.i().addToName(node, geom.getName(), false);
		
		// hierarchy/pos/scale/rotation
		node.nodeGeometries.setLocalScale(v3fScale);
		
		node.nodeGeometries.attachChild(geom);
		if(geomWireFrame!=null)node.nodeGeometries.attachChild(geomWireFrame);
		node.attachChild(node.nodeGeometries);
		
		node.setLocalTranslation(v3fPos);
		node.rotateUpTo(v3fUp);
		
		attachChild(node);
		
		return node;
	}
	
	public Vector3f getRotSpeedCopy() {
		return v3fSpeed.clone();
	}

	public OriginDevice setRotSpeed(Vector3f v3f) {
		this.v3fSpeed.set(v3fBaseSpeed.mult(v3f));
		return this; //for beans setter
	}

	public boolean isEnabled() {
		return bEnabled;
	}

	public OriginDevice<NODEXS> setEnabled(boolean bEnabled) {
		this.bEnabled = bEnabled;
		return this; 
	}

	/**
	 * keep even if empty!
	 * @param aobj
	 * @return
	 */
	public Object debugTest(Object... aobj){
		rotate(EAxis.X.get().getRepresentationShape(), 90, false);//TODO wrong
		return null;
	}
	
}
