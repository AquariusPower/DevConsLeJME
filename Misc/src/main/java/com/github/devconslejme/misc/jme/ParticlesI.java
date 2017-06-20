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

import java.util.HashMap;

import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.QueueI;
import com.github.devconslejme.misc.Annotations.ToDo;
import com.github.devconslejme.misc.Annotations.Workaround;
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.effect.ParticleEmitter;
import com.jme3.effect.ParticleMesh;
import com.jme3.effect.influencers.RadialParticleInfluencer;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ParticlesI{
	public static ParticlesI i(){return GlobalManagerI.i().get(ParticlesI.class);}

//	private ParticleEmitter debris;
//	private ParticleEmitter waterfall;
//	private ParticleEmitter fire;
	private HashMap<String,ParticleEmitter> hm = new HashMap<>();
	private boolean bAllowParticles;
//	private ParticleEmitter shockwave;
//	private ParticleEmitter smoke;
	
	public void configure() {
		prepareFire();
		prepareSmoke();
//		prepareWaterFall();
		prepareDebris();
		prepareShockWave();
	}
	
	private void prepareShockWave() {
		ParticleEmitter pe = new ParticleEmitter(EParticle.ShockWave.s(), ParticleMesh.Type.Triangle, 1);
		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
		mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/shockwave.png"));
		pe.setMaterial(mat);
		pe.setRotateSpeed(50);
		pe.setStartColor(ColorRGBA.Cyan);
		pe.setEndColor(ColorRGBA.Blue);
		pe.setStartSize(0.1f);
		pe.setEndSize(1f);
		pe.setInWorldSpace(true);
		pe.setGravity(0, 0, 0);
		pe.setLowLife(0.25f);
		pe.setHighLife(0.25f);
//		pe.setQueueBucket(Bucket.);
//		pe.setCullHint(CullHint.Never);
//    debris.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 100, 0));
		hm.put(pe.getName(), pe);
	}
	
	private void prepareDebris() {
		ParticleEmitter pe = new ParticleEmitter(EParticle.Debris.s(), ParticleMesh.Type.Triangle, 3);
    Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
    mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/Debris.png"));
    pe.setMaterial(mat);
    pe.setImagesX(3);
    pe.setImagesY(3); // 3x3 texture animation
    pe.setRotateSpeed(4);
    pe.setSelectRandomImage(true);
    pe.setStartColor(ColorRGBA.Cyan);
    pe.setEndColor(ColorRGBA.Blue);
    pe.setStartSize(0.05f);
    pe.setEndSize(0.01f);
    
    pe.setLowLife(0.5f);
    pe.setHighLife(pe.getLowLife());
    
    pe.setGravity(0, 10f, 0);
    
    RadialParticleInfluencer pari = new RadialParticleInfluencer();
    pari.setRadialVelocity(1f);
    pe.setParticleInfluencer(pari);
    pe.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 10, 0));
    pe.getParticleInfluencer().setVelocityVariation(0.60f);
    
		hm.put(pe.getName(), pe);
	}
	
	@ToDo
	@Deprecated //this is horrible :(
	private void prepareWaterFall() {
		ParticleEmitter pe = new ParticleEmitter(EParticle.WaterFall.s(), ParticleMesh.Type.Triangle, 10);
		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
		
		mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/flame.png")); // 2x2 texture animation
		pe.setMaterial(mat);
		
		pe.setImagesX(2);
		pe.setImagesY(2);
		
		pe.setEndColor(  new ColorRGBA(0f, 1f, 1f, 1f));   // cyan
		pe.setStartColor(new ColorRGBA(0f, 0f, 1f, 0.5f)); // blue
		
//		pe.getParticleInfluencer().setInitialVelocity(new Vector3f(0, -9.8f, 0));
		pe.getParticleInfluencer().setInitialVelocity(new Vector3f(0, -3f, 0));
		
		pe.setStartSize(0.1f);
		pe.setEndSize(1.5f);
		
		pe.setGravity(0, 1, 0);
		pe.setLowLife(1f);
		pe.setHighLife(3f);
		
		pe.getParticleInfluencer().setVelocityVariation(0.3f);
		
    hm.put(pe.getName(), pe);
	}

	private void prepareSmoke() {
		int iParticles=20;
		ParticleEmitter pe = new ParticleEmitter(EParticle.Smoke.s(), ParticleMesh.Type.Triangle, iParticles);
		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
		
		mat.setTexture("Texture", AppI.i().loadTexture("Effects/Smoke/Smoke.png")); // 2x2 texture animation
		pe.setMaterial(mat);
		
		pe.setImagesX(15);
		pe.setSelectRandomImage(true);
		pe.setRandomAngle(true);
		pe.setRotateSpeed(1f);
		
		pe.setStartColor(ColorRGBA.Gray);
		pe.setEndColor(ColorI.i().colorChangeCopy(ColorRGBA.Gray,0f,0.05f));
		
		pe.setLowLife(10f);
		pe.setHighLife(pe.getLowLife());
		
		pe.setParticlesPerSec(iParticles/pe.getLowLife());
		
		pe.setStartSize(0.1f);
		pe.setEndSize(3f);
		
		pe.setGravity(0, -0.5f, 0);
		
//		pe.getParticleInfluencer().setInitialVelocity(new Vector3f(0, -0.1f, 0));
//		pe.getParticleInfluencer().setVelocityVariation(0.3f);
//		pe.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 0.00001f, 0));
//		pe.getParticleInfluencer().setVelocityVariation(0.3f);
		
		hm.put(pe.getName(), pe);
	}
	
	private void prepareFire() {
		int iParticles=10;
		ParticleEmitter pe = new ParticleEmitter(EParticle.Fire.s(), ParticleMesh.Type.Triangle, iParticles);
		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
		
		mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/flame.png")); // 2x2 texture animation
		pe.setMaterial(mat);
		
		pe.setImagesX(2);
		pe.setImagesY(2);
		
		pe.setStartColor(new ColorRGBA(1,1,0.5f,0.5f)); //light yellow transp
		pe.setEndColor(new ColorRGBA(1,0,0,0.1f)); //red transp
		
		pe.setStartSize(1.5f);
		pe.setEndSize(0.1f);
		
		pe.setGravity(0, -10, 0);
		pe.setLowLife(1f);
		pe.setHighLife(pe.getLowLife());
		
		pe.setRandomAngle(true);
		
		pe.setParticlesPerSec(iParticles/pe.getLowLife());

		pe.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 2, 0));
		pe.getParticleInfluencer().setVelocityVariation(0.3f);
		
    hm.put(pe.getName(), pe);
	}
	
//	protected void prepare(String strName, int iParticles, String strTextureFile, int iTotX, int iTotY) {
//		ParticleEmitter pe = new ParticleEmitter(strName, ParticleMesh.Type.Triangle, iParticles);
//		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
//		
//		mat.setTexture("Texture", AppI.i().loadTexture(strTextureFile)); // 2x2 texture animation
//		pe.setMaterial(mat);
//		
//		pe.setImagesX(iTotX);
//		pe.setImagesY(iTotY);
//		
//		pe.setStartColor(new ColorRGBA(1,1,0.5f,0.5f)); //light yellow transp
//		pe.setEndColor(ColorRGBA.Red);
//		
//		pe.setStartSize(1.5f);
//		pe.setEndSize(0.1f);
//		
//		pe.setGravity(0, -10, 0);
//		pe.setLowLife(1f);
//		pe.setHighLife(pe.getLowLife());
//		
//		pe.setParticlesPerSec(iParticles/pe.getLowLife());
//
//		pe.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 2, 0));
//		pe.getParticleInfluencer().setVelocityVariation(0.3f);
//		
//    hm.put(pe.getName(), pe);
//	}

	public enum EParticle{
		Fire,
		WaterFall,
		Debris, 
		ShockWave,
		Smoke,
//		Rain,
		;
		public String s() {return toString();}
	}
	
	public void createAtCamTargetSpot(String strId, float fScale, Float fLifeSpanDelayTimeSeconds) {
		Vector3f v3f = WorldPickingI.i().raycastFromCenterHitSpot();
		if(v3f==null)return;
		createAtMainThread(strId, null, v3f, fScale, fLifeSpanDelayTimeSeconds);
	}
	public void createAtMainThread(String strId, Spatial sptFollow, float fScale, Float fLifeSpanDelayTimeSeconds) {
		createAtMainThread(strId, sptFollow, null, fScale, fLifeSpanDelayTimeSeconds);
	}
	public void createAtMainThread(String strId, Vector3f v3fPos, float fScale, Float fLifeSpanDelayTimeSeconds) {
		createAtMainThread(strId, null, v3fPos, fScale, fLifeSpanDelayTimeSeconds);
	}
	/**
	 * 
	 * @param strId
	 * @param v3fPos
	 * @param fScale
	 * @param fLifeSpanDelayTimeSeconds if null will use the high life time, so will spawn only one round
	 */
	protected void createAtMainThread(String strId, Spatial sptFollow, Vector3f v3fPos, float fScale, Float fLifeSpanDelayTimeSeconds) {
		if(!isAllowParticles())return;
		
		assert sptFollow==null && v3fPos!=null || sptFollow!=null && v3fPos==null;
		
		QueueI.i().enqueue(new CallableXAnon() {
			private ParticleEmitter pe;
			@Override	public Boolean call() {
				boolean bFirstFrame = false;
				
				if(pe==null) {
					pe = hm.get(strId).clone();
					
					pe.setInWorldSpace(true);
					
					pe.setStartSize(pe.getStartSize()*fScale);
					pe.setEndSize(pe.getEndSize()*fScale);
					if(strId.equals(EParticle.Fire.s())){
//						pe.setLowLife(pe.getLowLife()*fScale);
//						pe.setHighLife(pe.getHighLife()*fScale);
						pe.getParticleInfluencer().setInitialVelocity(pe.getParticleInfluencer().getInitialVelocity().mult(fScale));
					}
					
					if(!strId.equals(EParticle.Debris.s())){
						pe.setGravity(pe.getGravity().mult(fScale));
					}
					
					AppI.i().getRootNode().attachChild(pe);
					
					//self queue cfg
					if(sptFollow==null) {
						@Workaround float fABitLessLife = fLifeSpanDelayTimeSeconds==null ? 0.9f : 1f; //this is important to prevent a new particle spawn at the end of the previous one life
						setDelaySeconds(fLifeSpanDelayTimeSeconds==null ? pe.getHighLife()*fABitLessLife : fLifeSpanDelayTimeSeconds);
					}
					
					bFirstFrame=true;
				}else {
					if(sptFollow==null || sptFollow.getParent()==null) {
						pe.setParticlesPerSec(0);
						if(pe.getNumVisibleParticles()==0) {
							pe.removeFromParent();
							QueueI.i().removeLoopFromQueue(this);
						}
					}
				}
				
				/**
				 * the particles seems to be aligned with the camera
				 * so lets bring them a bit nearer to unstuck from objects/walls
				 */
				Vector3f v3fPosNew = sptFollow==null ? v3fPos : sptFollow.getWorldTranslation();
				if(strId.equals(EParticle.ShockWave.s())) { 
					// otherwise, shockwave will may be too much cut out and look bad when for ex, the position it at a wall
					v3fPosNew=AppI.i().getCamWPosCopy(0f).interpolateLocal(v3fPosNew, 0.9f);
				}
				pe.setLocalTranslation(v3fPosNew);
//				pe.getLocalRotation()
				
				if(bFirstFrame) {
					if(strId.equals(EParticle.Debris.s()))pe.emitAllParticles();
				}
				
				return true;
			}
		}).setName("Particles:"+strId+v3fPos+fLifeSpanDelayTimeSeconds)
			.setInitialDelay(0f)
			.setDelaySeconds(1f) //this may be changed 
			.enableLoopMode();
	}
	
//	public ParticleEmitter createFire(Vector3f v3fPos) {
//		ParticleEmitter pe = fire.clone();
//		pe.setLocalTranslation(v3fPos);
//		AppI.i().getRootNode().attachChild(pe);
//		return pe;
//	}
//	
//	public ParticleEmitter createWaterFall(Vector3f v3fPos) {
//		ParticleEmitter pe = waterfall.clone();
//		pe.setLocalTranslation(v3fPos);
//		AppI.i().getRootNode().attachChild(pe);
//		return pe;
//	}
//	
//	public ParticleEmitter createDebris(Vector3f v3fPos) {
//		ParticleEmitter pe = debris.clone();
//		pe.setLocalTranslation(v3fPos);
//    AppI.i().getRootNode().attachChild(pe);
//    pe.emitAllParticles();
//    return pe;
//	}

	public boolean isAllowParticles() {
		return bAllowParticles;
	}

	public ParticlesI setAllowParticles(boolean bAllowParticles) {
		this.bAllowParticles = bAllowParticles;
		return this; 
	}
}
