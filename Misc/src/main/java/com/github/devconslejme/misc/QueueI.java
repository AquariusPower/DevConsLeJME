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

import com.github.devconslejme.misc.QueueI.CallableXAnon;

/**
 * Queue is mainly usefull for lazy initializations that require pre-conditions.
 * But this one can be used also for delayed loops and other things.
 * 
 * TODO create parallel queues of stuff that can be processed outside of the main thread
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
	 * TODO may using this cause some problem?
	 */
	public static abstract class CXA extends CallableX<CXA>{}
	
	/**
	 * use this one for anonymous classes
	 */
	public static abstract class CallableXAnon extends CallableX<CallableXAnon>{
		public CallableXAnon(){
			super(1);
		}
	}
	
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
		
		private String	strName="";
		private float	fDelaySeconds=0f;
		private boolean	bLoop=false;
		
		private long lRunSuccessAtTime;
		private boolean	bUserCanKill = false;
		private boolean	bUserCanPause = false;
		
//		private HashMap<String,Object> hmKeyValue = new HashMap<String,Object>();
		private boolean	bAnonymousClass;
		
		private boolean bRunImediatelyOnce = false;

		private float	fTPF;

		private Method	mEnclosing;

		private StackTraceElement	steInstancedWhere;
		private StackTraceElement[]	asteLastEnqueued;

		private boolean	bLoopModeJustRemoveFromQueueOnceRequest;

		private Long	lFirstRunAtTime=null;

		private long	lRunTryCount=0;
		private long	lFailCount=0;

		public boolean	bForceRemoveFromQueueRequest;
		
		
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
		private SELF setLoopEnabled(boolean b) {
			if(this.bLoop==true && b==false){
				throw new DetailedException("if loop mode was enabled, it cannot be disabled, use the remove queue once option", this);
			}
			this.bLoop=b;
			return getThis();
		}
		
		/**
		 * 
		 * @return the tpf of the application main update method
		 */
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
		
		public SELF enableLoopMode() {
			setLoopEnabled(true);
			return getThis();
		}
		
//		/**
//		 * like killing a pid in linux
//		 * @return
//		 */
//		public SELF killSelf() {
//			disableLoop();
//			return getThis();
//		}
		
//		public SELF endLoopMode() {
//			bLoopModeJustRemoveFromQueueOnceRequest=tr
//			setLoopEnabled(false);
//			return getThis();
//		}
		public SELF setDelaySeconds(float fDelaySeconds) {
			this.fDelaySeconds=(fDelaySeconds);
			updateRunSuccessAt();
			return getThis();
		}
		
		/**
		 * see {@link CallableX}
		 */
		public CallableX(){
			this(0);
		}
		/**
		 * 
		 * @param iIncStack to be used to determine where it was instanced using the stack trace
		 */
		public CallableX(int iIncStack){
			strUId=strLastUId=StringI.i().getNextUniqueId(strLastUId);
			
			setEnclosing(this.getClass().getEnclosingMethod());
			
			setInstancedWhere(Thread.currentThread().getStackTrace()[2+iIncStack]);
			
//			prepareName();
			
			updateRunSuccessAt();
		}
		
		private String prepareNameFromEnclosing(){
			if(getEnclosing()!=null){
				return getEnclosing().getName().trim();
			}
			return null;
		}
		private String prepareNameFromStack(){
			String str = steInstancedWhere.getMethodName().trim(); //can be "<init>".equals(str) too
//			if("<init>".equals(str)){
				String[] astr = steInstancedWhere.getClassName().split("[.]");
				str=astr[astr.length-1]+"."+str;
				str=str.trim();
//			}
			return str;
		}
		private void prepareName(){
			///////////////////////////////////// enclosing
			// auto name
			if(!isNameSetProperly()){ // for annonimous class
				strName = prepareNameFromEnclosing();
				bAnonymousClass=true;
			}
			
			if(!isNameSetProperly()){
				strName = prepareNameFromStack();
			}
			
			// above is about enclosing origin
			if(isNameSetProperly())strName="Something@"+strName;
			
			/////////////////////////////////////// failed
			if(!isNameSetProperly()){
				strName="WARN: unable to automatically prepare a good name #"+this.hashCode(); //TODO this looks bad :(
				MessagesI.i().warnMsg(this, strName, this, getEnclosing(), steInstancedWhere);
			}
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
			}else{
				sb.append(prepareNameFromStack() + strSeparator);
			}
			
			return sb.toString();
		}
		
		private void updateRunSuccessAt() {
			this.lRunSuccessAtTime = QueueI.i().calcRunAt(fDelaySeconds);
		}
		
		public float getDelaySeconds() {
			return fDelaySeconds;
		}
		
		public boolean isReady(){
			if(bRunImediatelyOnce){
				bRunImediatelyOnce=false;
				return true;
			}
			
			if(lFirstRunAtTime!=null && lRunTryCount==0){
				return QueueI.i().isReady(lFirstRunAtTime);
			}
			
			return QueueI.i().isReady(lRunSuccessAtTime);
		}

		public boolean isLoopMode() {
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
			builder.append(lRunSuccessAtTime);
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
		public boolean togglePause() {
			bPaused=!bPaused;
			return bPaused;
		}
		public boolean isPaused() {
			return bPaused;
		}
//		/**
//		 * TODO it is possible to avoid using this by extending the {@link CallableX}
//		 * @param objVal
//		 * @return
//		 */
//		synchronized public SELF putKeyClassValue(Object objVal) {
//			hmKeyValue.put(objVal.getClass().getName(),objVal);
//			return getThis();
//		}
//		/**
//		 * TODO it is possible to avoid using this by extending the {@link CallableX}
//		 * @param strKey
//		 * @param objVal
//		 * @return
//		 */
//		synchronized public SELF putKeyValue(String strKey, Object objVal) {
//			hmKeyValue.put(strKey,objVal);
//			return getThis();
//		}
//		/**
//		 * TODO it is possible to avoid using this by extending the {@link CallableX}
//		 * @param cl
//		 * @return
//		 */
//		@SuppressWarnings("unchecked")
//		synchronized public <T> T getValue(Class<T> cl) {
//			return (T)hmKeyValue.get(cl.getName());
//		}
//		/**
//		 * TODO it is possible to avoid using this by extending the {@link CallableX}
//		 * @param strKey
//		 * @return
//		 */
//		@SuppressWarnings("unchecked")
//		synchronized public <T> T getValue(String strKey) {
//			return (T)hmKeyValue.get(strKey);
//		}
		public boolean isNameSetProperly(){
			return strName!=null && !strName.isEmpty();
		}
		public String getName() {
			return strName;
		}
		public SELF setRunImediatelyOnce() {
			this.bRunImediatelyOnce = true;
			return getThis();
		}
		public boolean isRunImediatelyOnce() {
			return bRunImediatelyOnce;
		}
		/**
		 * sets the first run delay, may be 0 to be imediately
		 * @param f
		 * @return
		 */
		public SELF setInitialDelay(float f) {
			lFirstRunAtTime=QueueI.i().calcRunAt(f);
//			this.bRunImediatelyOnce = true;
			return getThis();
		}
		public Method getEnclosing() {
			return mEnclosing;
		}
		private SELF setEnclosing(Method mEnclosing) {
			this.mEnclosing = mEnclosing;
			return getThis();
		}
		public StackTraceElement getInstancedWhere() {
			return steInstancedWhere;
		}
		public SELF setInstancedWhere(StackTraceElement steInstancedWhen) {
			assert(this.steInstancedWhere==null);
			this.steInstancedWhere = steInstancedWhen;
			return getThis();
		}
		
		public void callAfterRemovedFromQueue() {}
		
		public long getFailCount() {
			return lFailCount;
		}
		private SELF setFailCount(long lFailCount) {
			this.lFailCount = lFailCount;
			return getThis();
		}
		
	}
	
	/**
	 * call things that must happen on the proper update moment
	 * @param cx
	 */
	@SuppressWarnings("unchecked")
	public <T extends CallableX<T>> T enqueue(CallableX<T> cx){
		synchronized(acxList){
			if(!acxList.contains(cx)){
				acxList.add(cx);
			}
			cx.asteLastEnqueued=Thread.currentThread().getStackTrace();
			
			if(cx.getName()==null || cx.getName().isEmpty()){
				cx.prepareName();
			}
		}
		return (T)cx;
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
	
	public void forceRemoveFromQueue(CallableX cx) {
		cx.bForceRemoveFromQueueRequest=true;
	}
	public void removeLoopFromQueue(CallableX cx){
		assert cx.isLoopMode() : "is not in loop mode";
		
		if(acxList.contains(cx)){
			/**
			 * only requests if on queue, to avoid next enqueue with a problematic state 
			 */
			cx.bLoopModeJustRemoveFromQueueOnceRequest=true;
		}
	}
	
	/**
	 * THIS MUST BE CALLED AT THE MAIN THREAD! to cope with JME
	 * @param lCurrentTime
	 * @param fTPF
	 */
	@SuppressWarnings("unchecked")
	public void update(long lCurrentTime, float fTPF) {
		this.lCurrentTime=lCurrentTime;
		
		for(CallableX<? extends CallableWeak<Boolean>> cx:acxList.toArray(new CallableX[0])){
			cx.setTPF(fTPF);
			
			boolean bRun=false;
			boolean bRemove=false;
			
			if(cx.isLoopMode()){ /////////////LOOP MODE
				if(cx.bLoopModeJustRemoveFromQueueOnceRequest){
					bRemove=true;
				}
				
				if(!cx.isPaused()){
					if(cx.isReady()){ //in between delay
						bRun=true;
					}
				}
			}else{ /////////////// RUN ONCE MODE
				if(cx.isReady()){ //wait initial delay if any
					bRun=true;
					bRemove=true;
				}
			}
			
			if(bRun){
				cx.lRunTryCount++; //working or not
				if(cx.call()){ 
					cx.updateRunSuccessAt();
				}else{
					// if a loop queue fails, it will not wait and will promptly retry!
					cx.lFailCount++;
					
					if(!cx.isLoopMode()){
						bRemove=false; //needs to retry, can be a lazy run once init
					}
				}
			}
			
			if(cx.bForceRemoveFromQueueRequest){
				/**
				 * this allows to remove even a non-loop mode failing queued callable.
				 */
				bRemove=true;
				cx.bForceRemoveFromQueueRequest=false; //cleanly reset request
			}
			
			if(bRemove){
				synchronized(acxList){
					acxList.remove(cx);
					cx.callAfterRemovedFromQueue();
					cx.bLoopModeJustRemoveFromQueueOnceRequest=false; //so it can be cleanly re-added to the queue and work correctly
				}
			}
			
		}
		
	}
	
	public void configure(long lTimeResolution) {
		this.lTimeResolution=lTimeResolution;
		if(lTimeResolution < 1000L)throw new DetailedException("timer resolution is too low");
	}
	
	public ArrayList<CallableX> getQueuedWithFailures(long lMinFailures){
		ArrayList<CallableX> acxList = getQueueCopy();
		for(CallableX cx:acxList.toArray(new CallableX[0])){
			if(cx.lFailCount<lMinFailures)acxList.remove(cx);
		}
		return acxList;
	}
	
	public ArrayList<CallableX> getQueueCopy(){
		return new ArrayList<CallableX>(acxList);
	}
//	public void showQueue(){
//		for(CallableX cx:acxList){
//			LoggingI.i().logSubEntry(cx.toString());
//		}
//	}

	private void killOrPauseToggle(String strUIdOrJS,boolean bKill) {
		for(CallableX cx:acxList){
			if(strUIdOrJS.equalsIgnoreCase( strUIdOrJS.endsWith(".js") ? cx.getName() : cx.getUId() )){
				if(bKill){
					if(cx.isUserCanKill()){
						if(cx.isLoopMode()){
							removeLoopFromQueue(cx);//cx.endLoopMode();
						}else{
							forceRemoveFromQueue(cx);
						}
					}
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
