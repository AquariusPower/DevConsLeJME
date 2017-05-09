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

package com.github.devconslejme.misc;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.Callable;

/**
 * TODO work with java.util.concurrent.DelayQueue?
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class QueueI {
	public static QueueI i(){return GlobalManagerI.i().get(QueueI.class);}
	
	ArrayList<CallableX> acxList = new ArrayList<CallableX>();
//	private Application	app;
	private long	lTimeResolution;
	private long	lCurrentTime;
	
	public static interface CallableWeak<V> extends Callable<V>{
		/**
		 * without exception thrown, the debug will stop in the exact place where the exception
		 * happens inside a call (finally!!)
		 * 
		 * @return true if succeeded and the queue can be discarded or repeat delay applied. false if failed and it must be retried (even if loop mode, will be imediately without delay)
		 */
		@Override V call();
	}
	
	/**
	 * this can also be used with anonimous classes like: `new CallableX<CXA>{...`
	 * but prefer using {@link CallableXAnon}
	 */
	public static abstract class CXA extends CallableX<CXA>{}
	
	/**
	 * use this one for anonymous classes
	 */
	public static abstract class CallableXAnon extends CallableX<CallableXAnon>{}
	
	/**
	 * TODO could this be a guava's Function class? 
	 * 
	 * use {@link CallableXAnon} for anonymous classes<br>
	 * 
	 * CallableX: "X" like in extra, plus! 
	 */
	public static abstract class CallableX<SELF extends CallableX<SELF>> implements CallableWeak<Boolean>{
		/****************
		 *  THE ORDER of these vars will be automatically (thru IDE) at the toString()
		 ************/
		private String strUId;
		
		private boolean	bPaused;
		
		private String	strName;
		private float	fDelaySeconds=0f;
		private boolean	bLoop=false;
		
		private long lRunAtTime;
		private boolean	bUserCanKill = false;
		private boolean	bUserCanPause = false;
		
		private HashMap<String,Object> hmKeyValue = new HashMap<String,Object>();
		private boolean	bAnonymousClass;
		
		private boolean bRunImediatelyOnce = false;

		private float	fTPF;

		private Method	mEnclosing;

		private StackTraceElement	steInstancedWhen;
		
		private static String strLastUId="0";
//		private boolean bDone;
		
//		/**
//		 * TODO why this was necessary after implementing SELF generics? otherwise it would return Object...
//		 */
//		@Override
//		public abstract Boolean call();
		
		/**
		 * the name is alternatively automatically the enclosing method
		 * @param strName
		 * @return
		 */
		public SELF setName(String strName) {
			this.strName = strName;
			return getThis();
		}
		public SELF setLoopEnabled(boolean b) {
			this.bLoop=b;
			return getThis();
		}
		
		public float getTPF() {
			return fTPF;
		}
		private SELF setTPF(float fTPF) {
			this.fTPF = fTPF;
			return getThis();
		}
		
		/**
		 * IMPORTANT
		 * this must be overriden by sub-classes!
		 * @return
		 */
		@SuppressWarnings("unchecked")
		protected SELF getThis(){
			return (SELF)this;
		}
		
		public SELF enableLoop() {
			this.bLoop=true;
			return getThis();
		}
		
		/**
		 * like killing a pid in linux
		 * @return
		 */
		public SELF killSelf() {
			disableLoop();
			return getThis();
		}
		
		public SELF disableLoop() {
			this.bLoop=false;
			return getThis();
		}
		public SELF setDelaySeconds(float fDelaySeconds) {
			this.fDelaySeconds=(fDelaySeconds);
			updateRunAt();
			return getThis();
		}
		
		/**
		 * see {@link CallableX}
		 */
		public CallableX(){
//			this(1,null,0,false);
//		}
//		public CallableX(float fDelaySeconds, boolean bLoop){
//			this(1,null,fDelaySeconds,bLoop);
//		}
//		public CallableX(String strName, float fDelaySeconds, boolean bLoop){
//			this(1,strName,fDelaySeconds,bLoop);
//		}
//		public CallableX(int iStackAdd, String strName, float fDelaySeconds, boolean bLoop){
			strUId=strLastUId=StringI.i().getNextUniqueId(strLastUId);
			
			setEnclosing(this.getClass().getEnclosingMethod());
			
			setInstancedWhen(Thread.currentThread().getStackTrace()[2]);
			
			// auto name
			if(strName==null){ // for annonimous class
				if(getEnclosing()!=null){
					strName=getEnclosing().getName();
				}
				bAnonymousClass=true;
			}
			
			if(strName==null){
				strName = steInstancedWhen.getMethodName();
				if("<init>".equals(strName)){
					String[] astr = steInstancedWhen.getClassName().split("[.]");
					strName=astr[astr.length-1]+"."+strName;
				}
			}
			
			if(strName==null){
				strName=this.getClass().getSimpleName(); //TODO never used right?
			}
			
//			this.strName = strName;
//			this.bLoop=bLoop;
//			this.fDelaySeconds=(fDelaySeconds);
			
			updateRunAt();
		}
		
		public String getInfoText(){
			String strSeparator = ", ";
			StringBuilder sb = new StringBuilder("");
			sb.append("uid="+getUId()+strSeparator);
			sb.append("'"+getName()+"'"+strSeparator);
			
			if(getEnclosing()!=null){
				/**
				 * is info about the enclosing method, 
				 * there is no accessible concrete object here...
				 */
				MethodX mh = new MethodX();
				mh.setMethod(getEnclosing()); 
				sb.append(mh.getFullHelp(true,false) + strSeparator);
			}
			
			return sb.toString();
		}
		
		public void updateRunAt() {
			this.lRunAtTime = QueueI.i().calcRunAt(fDelaySeconds);
		}
		
		public float getDelaySeconds() {
			return fDelaySeconds;
		}
		
		public boolean isReady(){
			if(bRunImediatelyOnce){
				bRunImediatelyOnce=false;
				return true;
			}
			return QueueI.i().isReady(lRunAtTime);
		}

		public boolean isLoopEnabled() {
			return bLoop;
		}
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("CallableX [strUId=");
			builder.append(strUId);
			builder.append(", strName=");
			builder.append(strName);
			builder.append(", fDelaySeconds=");
			builder.append(fDelaySeconds);
			builder.append(", bPaused=");
			builder.append(bPaused);
			builder.append(", bLoop=");
			builder.append(bLoop);
			builder.append(", lRunAtTimeMilis=");
			builder.append(lRunAtTime);
			builder.append(", bUserCanKill=");
			builder.append(bUserCanKill);
			builder.append(", bUserCanPause=");
			builder.append(bUserCanPause);
			builder.append("]");
			return builder.toString();
		}

		public String getUId() {
			return strUId;
		}

		public boolean isUserCanKill() {
			return bUserCanKill;
		}
		public SELF setUserCanKill(boolean bUserCanKill) {
			this.bUserCanKill = bUserCanKill;
			return getThis();
		}
		public boolean isUserCanPause() {
			return bUserCanPause;
		}
		public SELF setUserCanPause(boolean bUserCanPause) {
			this.bUserCanPause = bUserCanPause;
			return getThis();
		}
		public void togglePause() {
			bPaused=!bPaused;
		}
		public boolean isPaused() {
			return bPaused;
		}
		synchronized public SELF putKeyClassValue(Object objVal) {
			hmKeyValue.put(objVal.getClass().getName(),objVal);
			return getThis();
		}
		synchronized public SELF putKeyValue(String strKey, Object objVal) {
			hmKeyValue.put(strKey,objVal);
			return getThis();
		}
		@SuppressWarnings("unchecked")
		synchronized public <T> T getValue(Class<T> cl) {
			return (T)hmKeyValue.get(cl.getName());
		}
		@SuppressWarnings("unchecked")
		synchronized public <T> T getValue(String strKey) {
			return (T)hmKeyValue.get(strKey);
		}
		public String getName() {
			return strName;
		}
//		public void setName(String strName) {
//			this.strName = strName;
//		}
		public boolean isRunImediatelyOnce() {
			return bRunImediatelyOnce;
		}
		public void runImediatelyOnce() {
			this.bRunImediatelyOnce = true;
		}
		public Method getEnclosing() {
			return mEnclosing;
		}
		private SELF setEnclosing(Method mEnclosing) {
			this.mEnclosing = mEnclosing;
			return getThis();
		}
		public StackTraceElement getInstancedWhen() {
			return steInstancedWhen;
		}
		public SELF setInstancedWhen(StackTraceElement steInstancedWhen) {
			this.steInstancedWhen = steInstancedWhen;
			return getThis();
		}
		
	}
	
	/**
	 * call things that must happen on the proper update moment
	 * @param cx
	 */
	public void enqueue(CallableX cx){
		synchronized(acxList){
			if(!acxList.contains(cx)){
				acxList.add(cx);
			}
		}
	}
	/**
	 * just to let the IDE help me...
	 * @param cx
	 * @return 
	 */
	public CallableXAnon enqueue(CallableXAnon cx){
		enqueue((CallableX)cx);
		return cx;
	}
	
	private boolean isReady(long lRunAt){
		return lRunAt <= lCurrentTime;
	}
	
	private long calcRunAt(float fDelaySeconds){
		return lCurrentTime + (long)(fDelaySeconds * lTimeResolution);
	}
	
	@SuppressWarnings("unchecked")
	public void update(long lCurrentTime, float fTPF) {
		this.lCurrentTime=lCurrentTime;
		
		for(CallableX<? extends CallableWeak<Boolean>> cx:acxList.toArray(new CallableX[0])){
			
			if(cx.isReady()){
				if(cx.isPaused())continue;
				
				cx.setTPF(fTPF);
				if(cx.call()){ 
//					cx.done();
					if(cx.isLoopEnabled()){
						cx.updateRunAt();
					}else{
						synchronized(acxList){
							acxList.remove(cx);
						}
					}
				}else{
					// if a loop queue fails, it will not wait and will promptly retry!
				}
			}
		}
		
//		if(iDoneCount>10){ //TODO what is a good amount to let it be cleaned considering speed/cpu usage?
//			for(CallableX cx:acxList.toArray(new CallableX[]{})){
//				if(cx.isDone())acxList.remove(cx);
//			}
//		}
	}
	
	public void configure(long lTimeResolution) {
		this.lTimeResolution=lTimeResolution;
		if(lTimeResolution < 1000L)throw new DetailedException("timer resolution is too low");
	}
	
	public ArrayList<CallableX> getQueueCopy(){
		return new ArrayList<CallableX>(acxList);
	}
//	public void showQueue(){
//		for(CallableX cx:acxList){
//			LoggingI.i().logSubEntry(cx.toString());
//		}
//	}

	private void killOrPauseToggle(String strUId,boolean bKill) {
		for(CallableX cx:acxList){
			if(strUId.equalsIgnoreCase( strUId.endsWith(".js") ? cx.getName() : cx.getUId() )){
				if(bKill){
					if(cx.isUserCanKill())cx.disableLoop();
				}else{
					if(cx.isUserCanPause())cx.togglePause();
				}
			}
		}
	}

	public void kill(String strUId) {
		killOrPauseToggle(strUId, true);
	}
	
	public void pauseToggle(String strUId) {
		killOrPauseToggle(strUId, false);
	}
	
}
