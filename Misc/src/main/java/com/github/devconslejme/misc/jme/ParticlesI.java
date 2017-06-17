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
import com.github.devconslejme.misc.QueueI.CallableXAnon;
import com.jme3.effect.ParticleEmitter;
import com.jme3.effect.ParticleMesh;
import com.jme3.effect.influencers.RadialParticleInfluencer;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Spatial.CullHint;

/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class ParticlesI{
	public static ParticlesI i(){return GlobalManagerI.i().get(ParticlesI.class);}

	private ParticleEmitter debris;
	private ParticleEmitter waterfall;
	private ParticleEmitter fire;
	private HashMap<String,ParticleEmitter> hm = new HashMap<>();
	private boolean bAllowParticles;
	private ParticleEmitter shockwave;
	
	public void configure() {
		prepareFire();
		prepareWaterFall();
		prepareDebris();
		prepareShockWave();
	}
	
	private void prepareShockWave() {
		shockwave = new ParticleEmitter("ShockWave", ParticleMesh.Type.Triangle, 10);
		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
		mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/shockwave.png"));
		shockwave.setMaterial(mat);
		shockwave.setRotateSpeed(50);
		shockwave.setStartColor(ColorRGBA.Cyan);
		shockwave.setEndColor(ColorRGBA.Blue);
		shockwave.setStartSize(0.1f);
		shockwave.setEndSize(1f);
		shockwave.setInWorldSpace(true);
		shockwave.setNumParticles(1);
		shockwave.setGravity(0, 0, 0);
		shockwave.setLowLife(0.25f);
		shockwave.setHighLife(0.25f);
//		shockwave.setQueueBucket(Bucket.);
//		shockwave.setCullHint(CullHint.Never);
//    debris.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 100, 0));

		hm.put(EParticle.ShockWave.s(), shockwave);
	}
	
	private void prepareDebris() {
    debris = new ParticleEmitter("Debris", ParticleMesh.Type.Triangle, 10);
    Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
    mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/Debris.png"));
    debris.setMaterial(mat);
    debris.setImagesX(3);
    debris.setImagesY(3); // 3x3 texture animation
    debris.setRotateSpeed(4);
    debris.setSelectRandomImage(true);
    RadialParticleInfluencer pari = new RadialParticleInfluencer();
    debris.setParticleInfluencer(pari);
    debris.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 100, 0));
    pari.setRadialVelocity(1f);
//    pari.setHorizontal(true);
    debris.setStartColor(ColorRGBA.Cyan);
    debris.setEndColor(ColorRGBA.Blue);
    debris.setStartSize(1f);
    debris.setEndSize(0.1f);
//    debris.setFacingVelocity(true);
    debris.setInWorldSpace(true);
    debris.setNumParticles(3);
    
//    debris.setGravity(0, 6, 0);
    debris.setGravity(0, 1f, 0);
    debris.getParticleInfluencer().setVelocityVariation(.60f);
    
    hm.put(EParticle.Debris.s(), debris);
	}

	private void prepareWaterFall() {
		waterfall = new ParticleEmitter("Fire", ParticleMesh.Type.Triangle, 30);
		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
		
		mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/flame.png")); // 2x2 texture animation
		waterfall.setMaterial(mat);
		
		waterfall.setImagesX(2);
		waterfall.setImagesY(2);
		
		waterfall.setEndColor(  new ColorRGBA(0f, 1f, 1f, 1f));   // cyan
		waterfall.setStartColor(new ColorRGBA(0f, 0f, 1f, 0.5f)); // blue
		
//		waterfall.getParticleInfluencer().setInitialVelocity(new Vector3f(0, -9.8f, 0));
		waterfall.getParticleInfluencer().setInitialVelocity(new Vector3f(0, -3f, 0));
		
		waterfall.setStartSize(0.1f);
		waterfall.setEndSize(1.5f);
		
		waterfall.setGravity(0, 1, 0);
		waterfall.setLowLife(1f);
		waterfall.setHighLife(3f);
		
		waterfall.getParticleInfluencer().setVelocityVariation(0.3f);
		
    hm.put(EParticle.WaterFall.s(), waterfall);
	}

	private void prepareFire() {
		fire = new ParticleEmitter("Fire", ParticleMesh.Type.Triangle, 30);
		Material mat = AppI.i().newMaterial("Common/MatDefs/Misc/Particle.j3md");
		
		mat.setTexture("Texture", AppI.i().loadTexture("Effects/Explosion/flame.png")); // 2x2 texture animation
		fire.setMaterial(mat);
		
		fire.setImagesX(2);
		fire.setImagesY(2);
		
		fire.setStartColor(new ColorRGBA(1,1,0.5f,0.5f));
		fire.setEndColor(new ColorRGBA(1,0,0,1));
		
		fire.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 2, 0));
		
		fire.setStartSize(1.5f);
		fire.setEndSize(0.1f);
		
		fire.setGravity(0, 0, 0);
		fire.setLowLife(1f);
		fire.setHighLife(3f);
		
		fire.getParticleInfluencer().setVelocityVariation(0.3f);
		
    hm.put(EParticle.Fire.s(), fire);
	}

	public enum EParticle{
		Fire,
		WaterFall,
		Debris, 
		ShockWave,
		;
		public String s() {return toString();}
	}
	
	public void createAtMainThread(String strId, Vector3f v3fPos, float fScale, float fLifeSpanDelayTimeSeconds) {
		if(!isAllowParticles())return;
		
		/**
		 * the particles seems to be aligned with the camera
		 * so lets bring them a bit nearer to unstuck from objects/walls
		 */
		if(strId.equals(EParticle.ShockWave.s()))v3fPos=AppI.i().getCamWPosCopy(0f).interpolateLocal(v3fPos, 0.9f);
		Vector3f v3fPosNew=v3fPos;
		
		QueueI.i().enqueue(new CallableXAnon() {
			private ParticleEmitter pe;
			@Override	public Boolean call() {
				if(pe==null) {
					pe = hm.get(strId).clone();
					
					pe.setStartSize(pe.getStartSize()*fScale);
					pe.setEndSize(pe.getEndSize()*fScale);
					pe.getParticleInfluencer().setInitialVelocity(pe.getParticleInfluencer().getInitialVelocity().mult(fScale));
					pe.setLowLife(pe.getLowLife()*fScale);
					pe.setHighLife(pe.getHighLife()*fScale);
					
					pe.setLocalTranslation(v3fPosNew);
//					pe.getLocalRotation()
					
					AppI.i().getRootNode().attachChild(pe);
					
					if(strId.equals(EParticle.Debris.s()))pe.emitAllParticles();
				}else {
					pe.removeFromParent();
					QueueI.i().removeLoopFromQueue(this);
				}
				return true;
			}
		}).setName("Particles:"+strId+v3fPos+fLifeSpanDelayTimeSeconds)
			.setDelaySeconds(fLifeSpanDelayTimeSeconds)
			.setInitialDelay(0f)
			.enableLoopMode();
	}
	
	public ParticleEmitter createFire(Vector3f v3fPos) {
		ParticleEmitter pe = waterfall.clone();
		pe.setLocalTranslation(v3fPos);
		AppI.i().getRootNode().attachChild(pe);
		return pe;
	}
	
	public ParticleEmitter createWaterFall(Vector3f v3fPos) {
		ParticleEmitter pe = waterfall.clone();
		pe.setLocalTranslation(v3fPos);
		AppI.i().getRootNode().attachChild(pe);
		return pe;
	}
	
	public ParticleEmitter createDebris(Vector3f v3fPos) {
		ParticleEmitter pe = debris.clone();
		pe.setLocalTranslation(v3fPos);
    AppI.i().getRootNode().attachChild(pe);
    pe.emitAllParticles();
    return pe;
	}

	public boolean isAllowParticles() {
		return bAllowParticles;
	}

	public ParticlesI setAllowParticles(boolean bAllowParticles) {
		this.bAllowParticles = bAllowParticles;
		return this; 
	}
}
