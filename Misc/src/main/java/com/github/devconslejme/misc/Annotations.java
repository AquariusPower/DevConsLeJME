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
package com.github.devconslejme.misc;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 *
 */
public class Annotations{

	/**
	 * use this to indicate things that should change one day
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {
			ElementType.ANNOTATION_TYPE,
			ElementType.CONSTRUCTOR,
			ElementType.FIELD,
			ElementType.LOCAL_VARIABLE,
			ElementType.METHOD,
			ElementType.PACKAGE,
			ElementType.PARAMETER,
			ElementType.TYPE,
			ElementType.TYPE_PARAMETER,
			ElementType.TYPE_USE
	})
	public static @interface NonStandard {}
	
	/**
	 * use this to pin point workarounds for dependencies limitations, 
	 * for things that you want them to work differently,
	 * and also are problematic in some way or another  
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {
			ElementType.ANNOTATION_TYPE,
			ElementType.CONSTRUCTOR,
			ElementType.FIELD,
			ElementType.LOCAL_VARIABLE,
			ElementType.METHOD,
			ElementType.PACKAGE,
			ElementType.PARAMETER,
			ElementType.TYPE,
			ElementType.TYPE_PARAMETER,
			ElementType.TYPE_USE
		})
	public static @interface Workaround {}
	
	/**
	 * use this to pin point bugs that may vanish on dependencies update
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {
			ElementType.ANNOTATION_TYPE,
			ElementType.CONSTRUCTOR,
			ElementType.FIELD,
			ElementType.LOCAL_VARIABLE,
			ElementType.METHOD,
			ElementType.PACKAGE,
			ElementType.PARAMETER,
			ElementType.TYPE,
			ElementType.TYPE_PARAMETER,
			ElementType.TYPE_USE
		})
	public static @interface Bugfix {}
	
	/**
	 * not working yet things
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {
			ElementType.ANNOTATION_TYPE,
			ElementType.CONSTRUCTOR,
			ElementType.FIELD,
			ElementType.LOCAL_VARIABLE,
			ElementType.METHOD,
			ElementType.PACKAGE,
			ElementType.PARAMETER,
			ElementType.TYPE,
			ElementType.TYPE_PARAMETER,
			ElementType.TYPE_USE
		})
	public static @interface ToDo {}
	
	/**
	 * methods that could be used by more classes, could be externalized to a misc/util class
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {
			ElementType.ANNOTATION_TYPE,
			ElementType.CONSTRUCTOR,
			ElementType.FIELD,
			ElementType.LOCAL_VARIABLE,
			ElementType.METHOD,
			ElementType.PACKAGE,
			ElementType.PARAMETER,
			ElementType.TYPE,
			ElementType.TYPE_PARAMETER,
			ElementType.TYPE_USE
		})
	public static @interface MiscGenericMigrateOneDay {}
	
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.FIELD})
	public static @interface FloatLimits {
		/** description */
		String desc() default "";
		float min();
		float max();
	}
	
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.FIELD})
	public static @interface DoubleLimits {
		/** description */
		String desc() default "";
		double min();
		double max();
	}
	
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.FIELD})
	public static @interface IntLimits {
		/** description */
		String desc() default "";
		int min();
		int max();
	}
	
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.FIELD})
	public static @interface LongLimits {
		/** description */
		String desc() default "";
		long min();
		long max();
	}
	
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.FIELD})
	public static @interface StringLimits {
		/** description */
		String desc() default "";
		int min() default 0;
		int max();
	}
	
	/**
	 *	to indicate it is a getter or a setter of a bean
	 *	see {@link SimpleVarReadOnly} for alternative
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.METHOD})
	public static @interface Bean{}
	
	/**
	 *	use this on getters to determine that whatever they do will be just a value retrieval,
	 *	nothing will be modified with such method call (ex.: if the getter increments a get count, then do not
	 *	use this annnotation!) 
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.METHOD})
	public @interface SimpleVarReadOnly {}
	
	/**
	 * use this to indicate the method will not (or must not) be called on the main thread.
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(value = {ElementType.METHOD})
	public @interface NotMainThread{}
}