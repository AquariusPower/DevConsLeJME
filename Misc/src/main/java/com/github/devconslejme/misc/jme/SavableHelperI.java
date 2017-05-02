/* 
Copyright (c) 2016, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>

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

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.ESimpleType;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.MessagesI;
import com.jme3.export.InputCapsule;
import com.jme3.export.JmeExporter;
import com.jme3.export.JmeImporter;
import com.jme3.export.OutputCapsule;
import com.jme3.export.Savable;

/**
 * TODO this class should be an automatic access to getters and setters configured by some kind of @Jme3Savable annotation... 
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class SavableHelperI {
	private static SavableHelperI instance = new SavableHelperI();
	public static SavableHelperI i(){return instance;}
	
	public static @interface AutoSavable{
		//TODO use on getters and setters related to save/load, or use on fields that have such getters and setters
	}
	
	/**
	 * The class implementing it is agreeing to properly allow set and get of field values,
	 * therefore this is an expected and safe access/behavior. 
	 * So, the class implementing get and set, will ALWAYS have access to field's value management!
	 * 
	 * All classes from superest to concrete int the inheritance MUST implement these methods.
	 * If some subclass misses such implementation, the superest will simply fail to access the Field object.
	 */
	public static interface IReflexFieldSafeAccess {
		/**
		 * //Use this at superest implementor:<br>
		 * //if(fld.getDeclaringClass()!=CURRENT_SUB_CLASS.class)return super.getFieldValue(fld); //For subclasses uncomment this line<br>
		 * return fld.get(Modifier.isStatic(fld.getModifiers()) ? null : this);<br>
		 * //or if statics are not allowed/expected, use just:<br>
		 * return fld.get(this);<br>
		 */
		public Object getFieldValue(Field fld) throws IllegalArgumentException, IllegalAccessException;
		
		/**
		 * //Use this at superest implementor:<br>
		 * //if(fld.getDeclaringClass()!=CURRENT_SUB_CLASS.class){super.setFieldValue(fld,value);return;} //For subclasses uncomment this line<br>
		 * fld.set(Modifier.isStatic(fld.getModifiers()) ? null : this, value);<br>
		 * //or if statics are not allowed/expected, use just:<br>
		 * fld.set(this,value);<br>
		 */
		public void setFieldValue(Field fld, Object value) throws IllegalArgumentException, IllegalAccessException;
	}

	
	public static interface ISavableFieldAccess extends IReflexFieldSafeAccess,Savable{
		public SaveSkipper<?> getSkipper();
	}
	
	public SavableHelperI(){
		aclGlobalSkip.add(ISaveSkipper.class);
	}
	
	public void setFieldVal(ISavableFieldAccess isfa, Field fld, Object val) throws IllegalArgumentException, IllegalAccessException {
		isfa.setFieldValue(fld, val);
	}
	public Object getFieldVal(ISavableFieldAccess isfa, Field fld) throws IllegalArgumentException, IllegalAccessException {
		return isfa.getFieldValue(fld);
	}
	
	/**
	 * 
	 * @param svLoaded
	 * @return if succeeded to apply to self
	 */
	public boolean applyValuesFrom(ISavableFieldAccess svTargetToStore, ISavableFieldAccess svLoadedSource) {
		return applyValuesFrom(svTargetToStore,svLoadedSource,false);
	}
	private boolean applyValuesFrom(ISavableFieldAccess svTargetToStore, ISavableFieldAccess svLoadedSource, boolean bRestoringSelfBackup) {
		ArrayList<Object> aobjDbg = new ArrayList<Object>();
		aobjDbg.add(svTargetToStore.getClass().getName());
		aobjDbg.add(svTargetToStore);
		
		if(svLoadedSource==null){
			MessagesI.i().warnMsg(this,"null, nothing loaded",aobjDbg);
			return false;
		}
		aobjDbg.add(svLoadedSource.getClass().getName());
		aobjDbg.add(svLoadedSource);
		
		ISavableFieldAccess svBkpSelf = null;
		if(!bRestoringSelfBackup){
			if(svLoadedSource.getSkipper().isFailedToLoad()){
				MessagesI.i().warnMsg(this,"cannot apply values from a 'failed to load' object",aobjDbg);
				return false;
			}
			
			try {
				svBkpSelf = (ISavableFieldAccess)svTargetToStore.getClass().newInstance();
			} catch (InstantiationException | IllegalAccessException e1) {
				throw new DetailedException("missing empty constructor",aobjDbg).initCauseAndReturnSelf(e1);
			}
		}
		
		if(!svTargetToStore.getClass().isInstance(svLoadedSource))throw new DetailedException("incompatible", aobjDbg);
		for(Entry<Field,FieldExtraInfo> entry:getAllFields(svTargetToStore)){
			Field fld=entry.getKey();
			aobjDbg.add(fld.getDeclaringClass());
			aobjDbg.add(fld.getType());
			aobjDbg.add(fld);
			
	//			allowFieldAccess(fld);
				try {
					if(fld.getType().isPrimitive()){}else
					if(String.class.isAssignableFrom(fld.getType())){}else
					{
						throw new DetailedException("direct field value overwrite is only allowed for primitives and String!"
							+" other classes must use their specific getters and setters",aobjDbg); 
					}
					
					Object valLoaded = SavableHelperI.i().getFieldVal(svLoadedSource,fld);
					if(valLoaded==null){
						throw new DetailedException("how can the loaded value be null?", aobjDbg);
					}
					
					SavableHelperI.i().setFieldVal(svTargetToStore,fld,valLoaded);
				} catch (IllegalArgumentException | IllegalAccessException | SecurityException e) {
					if(bRestoringSelfBackup){
						throw new DetailedException("during restore values", aobjDbg).initCauseAndReturnSelf(e);
					}else{
						MessagesI.i().debugInfo(this,e.getMessage(),e,aobjDbg);
						applyValuesFrom(svTargetToStore,svBkpSelf,true); //restore values to remove any inconsistency
					}
					
					break;
	//			}finally{
	//				restoreFieldAccessModeFor(fld, entry.getValue().bAccessible);
				}
		}
		
		return true;
	}
	
	private boolean bAllowFieldAccessHK=false;
	
	private Set<Entry<Field,FieldExtraInfo>> getAllFields(ISavableFieldAccess isfa){
		if(isfa.getSkipper().getFieldExtraInfo()==null){
			isfa.getSkipper().setFieldExtraInfo(new HashMap<Field,FieldExtraInfo>());
			for(Class clOwner:JavaLangI.i().getSuperClassesOf(isfa,false)){
				for(Field fld:clOwner.getDeclaredFields()){
	//				boolean bAccessible = allowFieldAccess(fld);
					if(checkSkip(isfa,fld))continue;
	//				applyFieldExtraInfo(isfa,fld, bAccessible, null);
					applyFieldExtraInfo(isfa,fld, null, null);
	//				getSkipper(isfa).hmFieldAccessible.put(fld,bAccessible);
				}
			}
		}
		
		return isfa.getSkipper().getFieldExtraInfo().entrySet();
	}
	
	private void applyFieldExtraInfo(ISavableFieldAccess isfa, Field fld,Boolean bAccessible,Object valDef){
		FieldExtraInfo fei = isfa.getSkipper().getFieldExtraInfo().get(fld);
		
		if(fei==null){
			fei=new FieldExtraInfo();
			isfa.getSkipper().getFieldExtraInfo().put(fld,fei);
		}
		
		if(valDef!=null){
			fei.valDefault=valDef;
		}
		
		if(bAccessible!=null){
			fei.bAccessible=bAccessible;
		}
		
	//	return fei;
	}
	
	public static interface ISaveSkipper{}
	public static class SaveSkipper<OWNER> implements ISaveSkipper{
		private boolean bThisInstanceIsALoadedTmp = false;
		private OWNER owner=null;
		private boolean	bFailedToLoad = false;
	//	private HashMap<Field,Boolean>	hmFieldAccessible;
		private HashMap<Field,FieldExtraInfo> hmFieldExtraInfo;// = new HashMap<Field,FieldExtraInfo>();
		private ISavableFieldAccess	isfaOwner;
		
		public SaveSkipper(ISavableFieldAccess isfa) {
			this.isfaOwner=isfa;
		}
		
	//	private void assertIsfaOwner(ISavableFieldAccess isfaOwnerCheck){
	//		if(this.isfaOwner!=isfaOwnerCheck)throw new DetailedException("invalid owner", this, this.isfaOwner, isfaOwnerCheck, ISavableFieldAccess.class);
	//	}
		
		public boolean isThisInstanceIsALoadedTmp() {
			return bThisInstanceIsALoadedTmp;
		}
		
	//	public void setThisInstanceIsALoadedTmp(ISavableFieldAccess isfaOwnerCheck) {
		public void setThisInstanceIsALoadedTmp() {
	//		assertIsfaOwner(isfaOwnerCheck);
			this.bThisInstanceIsALoadedTmp = true;
		}
	//	public O getOwner(ISavableFieldAccess isfaOwnerCheck) {
		public OWNER getOwner() {
			return this.owner;
		}
		public boolean isOwnerSet() {
			return owner!=null;
		}
	//	protected void setOwner(ISavableFieldAccess isfaOwnerCheck, O owner) {
		public void setOwner(OWNER owner) {
	//		assertIsfaOwner(isfaOwnerCheck);
			if(owner==null)throw new DetailedException("cannot erase (nullify) the owner", this.owner, this);
			DetailedException.assertNotAlreadySet(this.owner, owner, "owner", this);
			this.owner = owner;
		}
		public boolean isFailedToLoad() {
			return bFailedToLoad;
		}
	//	protected void setFailedToLoad(ISavableFieldAccess isfaOwnerCheck) {
		protected void setFailedToLoad() {
	//		assertIsfaOwner(isfaOwnerCheck);
			this.bFailedToLoad = true;
		}
		protected HashMap<Field, FieldExtraInfo> getFieldExtraInfo() {
			return hmFieldExtraInfo;
		}
	//	public void setFieldExtraInfo(ISavableFieldAccess isfaOwnerCheck, HashMap<Field, FieldExtraInfo> hmFieldExtraInfo) {
		private void setFieldExtraInfo(HashMap<Field, FieldExtraInfo> hmFieldExtraInfo) {
	//		assertIsfaOwner(isfaOwnerCheck);
			DetailedException.assertNotAlreadySet(this.hmFieldExtraInfo, hmFieldExtraInfo, "extrainfo", this);
			this.hmFieldExtraInfo = hmFieldExtraInfo;
		}
		
		private ArrayList<Class<?>> aclSkip = new ArrayList<Class<?>>();
		public void addSkipClassType(Class<?> cl){
			if(cl==null)throw new DetailedException("class is null", this);
			
			if(!aclSkip.contains(cl)){
				aclSkip.add(cl);
			}else{
				MessagesI.i().warnMsg(this,"already set", cl);
			}
		}
	//	private ArrayList<Class<?>> getSkipClassTypeListCopy() {
	//		return null;
	//	}
	}
	
	private ArrayList<Class<?>> aclGlobalSkip = new ArrayList<Class<?>>();
	public void addGlobalSkipClassType(Class<?> cl){
		if(cl==null)throw new DetailedException("class is null", this);
		
		if(!aclGlobalSkip.contains(cl)){
			aclGlobalSkip.add(cl);
		}else{
			MessagesI.i().warnMsg(this,"already set", cl);
		}
	}
	
	protected boolean checkSkip(ISavableFieldAccess isfa, Field fld){
		if(Modifier.isStatic(fld.getModifiers()))return true;
		
	//	if(ISaveSkipper.class.isAssignableFrom(fld.getType()))return true;
	//	if(CompositeControlAbs.class.isAssignableFrom(fld.getType()))return true;
		for(Class<?> cl:aclGlobalSkip){
			if(cl.isAssignableFrom(fld.getType()))return true;
		}
		
		for(Class<?> cl:isfa.getSkipper().aclSkip){
			if(cl.isAssignableFrom(fld.getType()))return true;
		}
		
		/**
		 * table switch for enum, dynamically generated for each switch(){} you code... 
		 * switch( ($SWITCH_TABLE$com$...())[this.ENUM_ID.ordinal] )
		 */
		if(fld.isSynthetic())return true; //this$0, $SWITCH_TABLE$, ...
		
		if(fld.getName().startsWith("$SWITCH_TABLE$")){
			return true; //should just be a synthetic
		}
		
		try {if(SavableHelperI.i().getFieldVal(isfa,fld)==isfa.getSkipper().owner){ //this$0
			return true; //should just be a synthetic
		}} catch (IllegalArgumentException | IllegalAccessException e) {/*wont happen as skipper is inner*/}
		
		return false;
	}
	
	public static class FieldExtraInfo{
		Boolean bAccessible;
		Object valDefault;
	}
	
	public void read(ISavableFieldAccess isfa, JmeImporter im) throws IOException{
		InputCapsule ic = im.getCapsule(isfa);
		for(Entry<Field,FieldExtraInfo> entry:getAllFields(isfa)){
			Field fld=entry.getKey();
	//	for(Class clOwner:MiscI.i().getSuperClassesOf(this)){
	//		for(Field fld:clOwner.getDeclaredFields()){
				Class clField=null;
				
				ArrayList<Object> aobjDbg = new ArrayList<Object>();
				addDbgInfo(aobjDbg,isfa,im,ic,fld.getDeclaringClass(),fld,clField);
				
	//			allowFieldAccess(fld);
				try {
	//				if(checkSkip(fld))continue;
					
					clField = fld.getType();
	//				Object objValDef = getDefaultValueFor(fld,true);
					Object valDef = isfa.getSkipper().getFieldExtraInfo().get(fld).valDefault;
					String strName=fld.getName();
					
					read(isfa,ic,clField,strName,fld,valDef,aobjDbg);
				} catch (IllegalArgumentException | IllegalAccessException | NullPointerException e) {
					MessagesI.i().debugInfo(this, e.getMessage(), e, aobjDbg);
					isfa.getSkipper().bFailedToLoad=true;
					break;
	//			}finally{
	//				restoreFieldAccessModeFor(fld, entry.getValue().bAccessible);
				}
	//		}
		}
		
	}
	
	private Object read(ISavableFieldAccess isfa, InputCapsule ic, Class clField, String strName, Field fld, Object objValDef, ArrayList<Object> aobjDbg) throws IllegalArgumentException, IllegalAccessException, IOException {
		Object valRead = null;
		switch(ESimpleType.forClass(clField,true)){
			case Boolean:	valRead=ic.readBoolean(strName, (boolean)objValDef);break;
			case Double:	valRead=ic.readDouble	(strName, (double)objValDef);	break;
			case Float:		valRead=ic.readFloat	(strName, (float)objValDef);	break;
			case Int:			valRead=ic.readInt		(strName, (int)objValDef);		break;
			case Long:		valRead=ic.readLong		(strName, (long)objValDef);		break;
			case String:	valRead=ic.readString	(strName, (String)objValDef);	break;
		}
		
		if(fld==null){ //called from var
			return valRead;
		}
		
		SavableHelperI.i().setFieldVal(isfa,fld,valRead);
		
		return null;
	}
	
	private void addDbgInfo(ArrayList<Object> aobjDbg, Object... aobj){
		for(Object obj:aobj){
			aobjDbg.add(obj);
		}
	}
	
	/**
	 * A missing default means to always save.
	 * If default value is not present, try to use an invalid value (like Float.Nan) to grant 
	 * value will always be saved in case there is no default.
	 * To grant it will be saved, the default just needs to differ from the actual valid value.
	 */
	private <T> T changeVal(ISavableFieldAccess isfa, Class<T> clValue, Object objValue, Object valueDefault){
		switch(ESimpleType.forClass(clValue,true)){
			case Boolean:	if(valueDefault==null)valueDefault=!((boolean)objValue);	break;
			case Double:	if(valueDefault==null)valueDefault=Double.NaN;						break;
			case Float:		if(valueDefault==null)valueDefault=Float.NaN;							break;
			case Int:
				if(valueDefault==null){
					if(((int)objValue)==Integer.MAX_VALUE){
						valueDefault=((int)objValue)-1;
					}else{
						valueDefault=((int)objValue)+1;
					}
				}
				break;
			case Long:
				if(valueDefault==null){
					if(((long)objValue)==Long.MAX_VALUE){
						valueDefault=((long)objValue)-1;
					}else{
						valueDefault=((long)objValue)+1;
					}
				}
				break;
			case String:	if(valueDefault==null)valueDefault=((String)objValue)+"_Different";	break;
		}
	//	{throw new DetailedException("unsupported value class type "+clValue);}
		
		return (T)valueDefault;
	}
	
	public void write(ISavableFieldAccess isfa, JmeExporter ex) throws IOException{
		OutputCapsule oc = ex.getCapsule(isfa);
		for(Entry<Field,FieldExtraInfo> entry:getAllFields(isfa)){
			Field fld=entry.getKey();
	//	for(Class clOwner:MiscI.i().getSuperClassesOf(this)){
	//		for(Field fld:clOwner.getDeclaredFields()){
				ArrayList<Object> aobjDbg = new ArrayList<Object>();
				
	//			allowFieldAccess(fld);
				try {
	//				if(checkSkip(fld))continue;
					
					String strName = fld.getName();
					Object val = SavableHelperI.i().getFieldVal(isfa,fld);
					Object valDef = isfa.getSkipper().getFieldExtraInfo().get(fld).valDefault;
	//				Object valDef = getDefaultValueFor(fld,false);
					
					addDbgInfo(aobjDbg, oc, fld.getDeclaringClass(), fld, strName, val.getClass().getName(), val, valDef);
					
					write(isfa,oc,strName,val,valDef,aobjDbg);
				} catch (IllegalArgumentException | IllegalAccessException | UnsupportedOperationException e) {
					throw new DetailedException("failed to retrieve field/default value",aobjDbg)
						.initCauseAndReturnSelf(e);
	//			}finally{
	//				restoreFieldAccessModeFor(fld, entry.getValue().bAccessible);
				}
	//		}
		}
		
	}
	
	private void write(ISavableFieldAccess isfa, OutputCapsule oc, String strName, Object val, Object valDef, ArrayList<Object> aobjDbg) throws IOException {
		Class<?> clValue = val.getClass();
		
		switch(ESimpleType.forClass(clValue,true)){
			case Boolean:	oc.write((boolean)val,	strName, changeVal(isfa,boolean.class,	val, valDef));break;
			case Double:	oc.write((double)val,		strName, changeVal(isfa,double.class,	val, valDef));break;
			case Float:		oc.write((float)val,		strName, changeVal(isfa,float.class,		val, valDef));break;
			case Int:			oc.write((int)val,			strName, changeVal(isfa,int.class,			val, valDef));break;
			case Long:		oc.write((long)val,			strName, changeVal(isfa,long.class,		val, valDef));break;
			case String:	oc.write((String)val,		strName, changeVal(isfa,String.class,	val, valDef));break;
		}
	}
	
	public void prepareFields(ISavableFieldAccess isfa) {
		for(Entry<Field,FieldExtraInfo> entry:getAllFields(isfa)){
			Field fld = entry.getKey();
			try {
	//			allowFieldAccess(fld);
				switch(ESimpleType.forClass(fld.getType(),true)){
					case Boolean:
					case Double:
					case Float:
					case Int:
					case Long:
					case String:
						/**
						 * set DEFAULT value
						 */
						Object val = SavableHelperI.i().getFieldVal(isfa,fld);
						if(val==null){
							throw new DetailedException("default (initial) value, cannot be null!", fld); 
						}
						applyFieldExtraInfo(isfa,fld, null, val);
						break;
				}
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new DetailedException("failed to retrieve default value",isfa,fld.getDeclaringClass(),fld.getName(),fld)
					.initCauseAndReturnSelf(e);
	//		}finally{
	//			restoreFieldAccessModeFor(fld, entry.getValue().bAccessible);
			}
		}
	}
}
