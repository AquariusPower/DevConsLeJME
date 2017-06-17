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

import java.util.ArrayList;

import com.github.devconslejme.misc.MessagesI;
import com.github.devconslejme.misc.MultiClickI.CallMultiClickUpdate;
import com.github.devconslejme.misc.MultiClickI.MultiClick;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.github.devconslejme.misc.jme.ActivatorI.ActivetableListenerAbs;
import com.github.devconslejme.misc.jme.MeshI.Cone;
import com.github.devconslejme.misc.jme.OriginDevice.NodeAxis;
import com.github.devconslejme.misc.jme.PhysicsI.RayCastResultX;
import com.github.devconslejme.misc.jme.WorldPickingI.IPickListener;
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
public class OriginDevice<SELF extends OriginDevice,NODEXS extends NodeAxis> extends Node implements IPickListener{
	private float fRadius;
	private int	iCS;
	private int	iRS;
	private float	fTPF;
	private Node	nodeLastParent;
	private boolean bEnabled;
	private Vector3f	v3fBaseSpeed;
	private float	fIR;
	private float	fRotTorOpac;
	private Vector3f v3fSpeed;
	
	@SuppressWarnings("unchecked")
	public SELF getThis(){
		return (SELF)this;
	}
	
	public static class NodeAxis<SELF extends NodeAxis> extends Node{
		public NodeAxis(String str) {
			super(str);
		}

		private EAxis ea;
		private Geometry	geom;
		private Geometry	geomWireFrame;
		private Node nodeGeometries;
		private Vector3f	v3fAdd = Vector3f.UNIT_Y.clone();
		private Quaternion	quaInitialRotation;
		private boolean	bInvertRotation;
		
		@Override
		public NodeAxis clone() {
			return (NodeAxis)super.clone();
		}
		
		@SuppressWarnings("unchecked")
		public SELF getThis(){
			return (SELF)this;
		}
		
		public void resetRotation(){
			setLocalRotation(quaInitialRotation);
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

		public void applyInitialRotation() {
			quaInitialRotation=getLocalRotation().clone();
		}

		public void toggleInvertRotation() {
			bInvertRotation=!bInvertRotation;
		}

		public boolean isInvertRotation() {
			return bInvertRotation;
		}

		public NodeAxis<SELF> setInvertRotation(boolean bInvertRotation) {
			this.bInvertRotation = bInvertRotation;
			return this; 
		}

	}
	
	/**
	 * to be overriden to prepare values to use on methods called on the constructor
	 * call this one too of course
	 */
	protected void constructorPreInitFields() {
		fRadius=5;
		iCS=50;
		iRS=15;
		bEnabled=false;
		fIR=0.1f;
		fRotTorOpac=0.15f;
		
		v3fBaseSpeed=Vector3f.UNIT_XYZ.mult(0.0025f);
		v3fSpeed=new Vector3f(v3fBaseSpeed);
	}
	public OriginDevice(){
		constructorPreInitFields();
		setName(this.getClass().getSimpleName());
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
		
		for(Orbiter obt:aobtList.toArray(new Orbiter[0])){
			if(!obt.isAttached())aobtList.remove(obt);
		}
		
//		updateMultiClickMainShapeMB0();
	}
	
	protected float getRadius() {
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
		rotateAlignment(getAxisInfo(EAxis.Y).getRotatingTorus(), 90, false);
		
		createAxis(Vector3f.UNIT_Z, new Cone());
		
		getAxisInfo(EAxis.X).getRotatingTorus().applyInitialRotation();
		getAxisInfo(EAxis.Y).getRotatingTorus().applyInitialRotation();
		getAxisInfo(EAxis.Z).getRotatingTorus().applyInitialRotation();
		
		// picking 
    WorldPickingI.i().addListener(this);
    
    ActivatorI.i().applyActivetableListener(getAxisInfo(EAxis.X).getRepresentationShape(), ial);
    ActivatorI.i().applyActivetableListener(getAxisInfo(EAxis.Y).getRepresentationShape(), ial);
    ActivatorI.i().applyActivetableListener(getAxisInfo(EAxis.Z).getRepresentationShape(), ial);
	}
	
	ActivetableListenerAbs ial = new ActivetableListenerAbs() {
		@Override
		public boolean activateEvent(Spatial sptSource) {
			NodeAxis na = (NodeAxis)sptSource;
			createPet(na.ea);
			return true;
		}
	};
	
	protected void updateAxisMainShapes() {
//		for(NODEXS node:anodeMainShapes){
		for(EAxis ea:EAxis.values()){
			if(eaExclusiveRotations!=null && ea!=eaExclusiveRotations)continue;
			NODEXS node = getAxisInfo(ea).getRepresentationShape();
			rotateMainShape(node,getRotSpeedCopy());
		}
	}
	
	protected void rotateMainShape(NODEXS node,Vector3f v3f){
		if(bStopRotations)return;
		if(node.isInvertRotation())v3f=v3f.negate();
		node.rotate(v3f.x,v3f.y,v3f.z);
	}
	
	public static class AxisInfo{
		private EAxis ea;
		private ColorRGBA color;
		
		private NodeAxis	torus;
		private NodeAxis	tip;
		private NodeAxis	feather;
		private NodeAxis	nodeRepresentationShape;
		private ColorRGBA	colorRotPlane;
		
		public ColorRGBA getColor() {
			return color;
		}
		
		public ColorRGBA getColorRotPlane() {
			return colorRotPlane;
		}
		
		private AxisInfo setColor(ColorRGBA color) {
			this.color = color;
			/**
			 * will be the other two color components, while the current wil be zeroed
			 */
			this.colorRotPlane=new ColorRGBA().fromIntRGBA(color.asIntRGBA()^0xffFFff00);
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
	
	public static enum EAxis{X,Y,Z,;}
	
	protected void updateTorusRotations() {
		Vector3f v3fSpeed = getRotSpeedCopy();
		rotateTor(getAxisInfo(EAxis.X).getRotatingTorus(),v3fSpeed);//,EAxis.X);
		rotateTor(getAxisInfo(EAxis.Y).getRotatingTorus(),v3fSpeed);//,EAxis.Y);
		rotateTor(getAxisInfo(EAxis.Z).getRotatingTorus(),v3fSpeed);//,EAxis.Z);
	}
	
	AxisInfo[] aai = new AxisInfo[EAxis.values().length];
	private EAxis	eaExclusiveRotations;
	private boolean	bStopRotations;
	private ArrayList<Orbiter>	aobtList=new ArrayList<Orbiter>();
	
	public static class MultiClickAxis extends MultiClick<MultiClickAxis>{
		private EAxis ea;
		private Geometry  geomClicked;
		public AxisInfo	axi;
		public MultiClickAxis(int iButtonIndex, int iMaxClicks,CallMultiClickUpdate cxUpdate) {
			super(iButtonIndex, iMaxClicks,cxUpdate);
		}
		
		@Override	
		public MultiClickAxis reset() {
			geomClicked=null;
			ea=null;
			axi=null;
			
			super.reset();
			
			return getThis();
		}

	}
	
	private boolean	bDebug;
	private MultiClickAxis	mcMainShapeMB0 = new MultiClickAxis(0,3,new CallMultiClickUpdate(){
		@Override	public void applyMultiClick(int totalClicks) {
			switch(totalClicks){
				case 1:
					createPet(mcMainShapeMB0.ea);
					break;
				case 2:
					mcMainShapeMB0.axi.getRotatingTorus().toggleInvertRotation();
					break;
				case 3:
					if(eaExclusiveRotations==mcMainShapeMB0.ea){
						eaExclusiveRotations=null;
					}else{
						eaExclusiveRotations=mcMainShapeMB0.ea;
						for(EAxis ea:EAxis.values()){
							if(ea==eaExclusiveRotations)continue;
							getAxisInfo(ea).getRotatingTorus().resetRotation();
						}
					}
					break;
			}
		}

	}).setHelp("1 click: rotate only the one clicked","2 clicks: invert rotation");
	
	public boolean isDebug() {
		return bDebug;
	}

	public void createPet(EAxis ea) {
		/**
		 * clone node axis intersection arrow tip
		 */
		Node nodePet=new NodeX("Pet");
		NodeAxis nodeCopyFrom = getAxisInfo(ea).getTorusTip();
		nodePet.setLocalTransform(nodeCopyFrom.getLocalTransform());
//		nodePet.setLocalTranslation(
//				getAxisInfo(ea).getRepresentationShape().getLocalTranslation().add(
//						RotateI.i().randomDirection()));
		nodePet.setLocalTranslation(
//			nodePet.worldToLocal(
				getAxisInfo(ea).getTorusTip().getWorldTranslation().add(
					RotateI.i().randomDirection().mult(0.25f))
//				,
//				null)
		);
		Geometry geom = nodeCopyFrom.getGeom().clone();
		nodePet.attachChild(geom);
		geom.setLocalScale(1f, 0.5f, 3f);
		Orbiter obt = new Orbiter(OriginDevice.this, nodePet, geom);
//		getParent().attachChild(obt.get);
		
		/**
		 * randomize orbiting speed making it slower.
		 * the slower orbiting, the longer it will take to shrink and get closer.
		 */
		float fOrbitSpeed = obt.getOrbitSpeed();
		fOrbitSpeed = FastMath.clamp(FastMath.nextRandomFloat()*fOrbitSpeed, fOrbitSpeed/10f, fOrbitSpeed);
		float fDelay=10f / (fOrbitSpeed/obt.getOrbitSpeed());
		if(isDebug())System.out.println("OrbitSpeed="+fOrbitSpeed+", delay="+fDelay);
		obt.setOrbitSpeed(fOrbitSpeed);
		obt.setMaxDelayToFullyShrink(fDelay);
		obt.setMaxDelayToGetCloser(fDelay);
//		obt.setOrbiting(false).setScaling(false).setGetCloser(false); //TODO rm
		
		aobtList.add(obt);
	}
	
	public OriginDevice<SELF, NODEXS> setDebug(boolean bDebug) {
		this.bDebug = bDebug;
		return this; 
	}
	
	public AxisInfo getAxisInfo(EAxis ea) {
		return aai[ea.ordinal()];
	}

	protected float rotTorSpeed(NODEXS nodeTor,Vector3f v3fSpeed){
		float fSpeed=0;
		switch(nodeTor.getEAxis()){
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
		
		return fSpeed * (nodeTor.isInvertRotation() ? -1f : 1f);
	}
	
	protected void rotateTor(NODEXS nodeTor,Vector3f v3fSpeed){//, EAxis ea) {
		if(bStopRotations)return;
		if(eaExclusiveRotations!=null && nodeTor.getEAxis()!=eaExclusiveRotations)return;
		nodeTor.rotate(0,rotTorSpeed(nodeTor,v3fSpeed),0);
	}
	
	public int getTpfMilis(){
		return (int) (fTPF*1000);
	}
	
	protected void rotateAlignment(NODEXS node, float fAngleDegrees, boolean bZOnly){
		float fRotRad=FastMath.DEG_TO_RAD*fAngleDegrees;
		if(!bZOnly && node.getEAxis()==EAxis.X)node.rotate(       0, fRotRad, 0);
		if(!bZOnly && node.getEAxis()==EAxis.Y)node.rotate(       0, fRotRad, 0);
		if(           node.getEAxis()==EAxis.Z)node.rotate(-fRotRad, fRotRad, 0);
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
		aai[ea.ordinal()]=(axisi);

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
		rotateAlignment(nodeCore,90,true);
		axisi.torus.attachChild(nodeCore);
		
		createTorusIntersections(axisi.getRotatingTorus(),fDisplacementTorus,v3fUp);
		
		return ea;
	}
	
	protected void createTorusIntersections(NODEXS nodeTor, float fDisplacementTorus, Vector3f v3fUp) {
		float fIRa=fIR*1.5f;
		float fAlpha=fRotTorOpac+0.25f;
		
		// arrow tip
		NODEXS nodePosit = createAxisShape(nodeTor.getEAxis(),new Cone(fIRa*2f),
			new Vector3f(fDisplacementTorus,0,0), fAlpha, v3fUp, false, new Vector3f(1,1,2));
		MiscJmeI.i().addToName(nodePosit, "IntersectionTip", false, true);
		nodePosit.lookAt(v3fUp, v3fUp);
		rotateAlignment(nodePosit,-90,false);
		nodeTor.attachChild(nodePosit);
		getAxisInfo(nodeTor.getEAxis()).setTorusTip(nodePosit);
		ColorRGBA color = getAxisInfo(nodeTor.getEAxis()).getColorRotPlane();
		nodePosit.getGeom().setMaterial(ColorI.i().retrieveMaterialUnshadedColor(color));
		
		// other end
		NODEXS nodeNegat=createAxisShape(nodeTor.getEAxis(),new Sphere(10,10,fIRa),
			new Vector3f(-fDisplacementTorus,0,0), fAlpha, v3fUp);
		MiscJmeI.i().addToName(nodeNegat, "IntersectionFeather", false, true);
		nodeTor.attachChild(nodeNegat);
		getAxisInfo(nodeTor.getEAxis()).setTorusFeather(nodeNegat);
		nodeNegat.getGeom().setMaterial(ColorI.i().retrieveMaterialUnshadedColor(
			ColorI.i().colorChangeCopy(color, -0.5f)));
	}
	
	@SuppressWarnings("unchecked")
	public NODEXS createNodeAxis(String strName){
		return (NODEXS)new NodeAxis(strName);
	}
	
	protected NODEXS createAxisShape(EAxis ea, Mesh mesh, Vector3f v3fPos, float fAlpha, Vector3f v3fUp) {
		return createAxisShape(ea, mesh, v3fPos,  fAlpha,  v3fUp, false, null);
	}
	protected NODEXS createAxisShape(EAxis ea, Mesh mesh, Vector3f v3fPos, float fAlpha, Vector3f v3fUp, 
			boolean bAddWireFrame, Vector3f v3fScale
	) {
		if(v3fScale==null)v3fScale=new Vector3f(1,1,1);
		NODEXS node = createNodeAxis("Node");
		node.setEAxis(ea);
		Geometry geom = GeometryI.i().create(mesh, ColorI.i().colorChangeCopy(getAxisInfo(ea).getColor(),0,fAlpha), true,null);
		node.setGeom(geom);
		
		node.setNodeGeometries(new Node());
		
		Geometry geomWireFrame=null;
		if(bAddWireFrame){
			geomWireFrame = new Geometry("WireFrame",mesh);
			ColorRGBA colorW = getAxisInfo(ea).getColor().clone();
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
		node.getNodeGeometries().setLocalScale(v3fScale);
		
		node.getNodeGeometries().attachChild(geom);
		if(geomWireFrame!=null)node.getNodeGeometries().attachChild(geomWireFrame);
		node.attachChild(node.getNodeGeometries());
		
		node.setLocalTranslation(v3fPos);
		node.rotateUpTo(v3fUp);
		
		attachChild(node);
		
		return node;
	}
	
	public Vector3f getRotSpeedCopy() {
		return v3fSpeed.clone();
	}

	public SELF setRotSpeed(Vector3f v3f) {
		this.v3fSpeed.set(v3fBaseSpeed.mult(v3f));
		return getThis(); //for beans setter
	}

	public boolean isEnabled() {
		return bEnabled;
	}

	public SELF setEnabled(boolean bEnabled) {
		this.bEnabled = bEnabled;
		return getThis();
	}

	/**
	 * keep even if empty!
	 * @param aobj
	 * @return
	 */
	public Object debugTest(Object... aobj){
		rotateAlignment(getAxisInfo(EAxis.X).getRepresentationShape(), 90, false);//TODO wrong
		return null;
	}
	
	@Override
	public boolean updatePickingEvent(int iButtonIndex, ArrayList<RayCastResultX> acrList, PhysicsData pd, Geometry geom, Spatial sptParentest) {
		for(EAxis ea:EAxis.values()){
			AxisInfo axi = getAxisInfo(ea);
			NODEXS node = axi.getRepresentationShape();
			if(node.hasChild(geom)){
				switch(iButtonIndex){
					case 0: //left
						if(mcMainShapeMB0.geomClicked==null){
							mcMainShapeMB0.geomClicked=geom;
						}else
						if(mcMainShapeMB0.geomClicked!=geom){  
							mcMainShapeMB0.reset(); //changed click target, reset
						}
						
						mcMainShapeMB0.updateIncClicks();
						
						mcMainShapeMB0.ea=ea;
						mcMainShapeMB0.axi=axi;
						return true;
					case 2: //middle
						axi.getRotatingTorus().resetRotation();
						return true;
					case 1: //right
						bStopRotations=!bStopRotations;
						return true;
				}
			}
		}
		return false;
	}
	
	public void test() {
		AppI.i().getRootNode().attachChild(this);
		
		setEnabled(true);

		QueueI.i().enqueue(new CallableXAnon() {@Override	public Boolean call() {
			update(getTPF());
			return true;
		}	}).enableLoopMode();
		
	}
}
