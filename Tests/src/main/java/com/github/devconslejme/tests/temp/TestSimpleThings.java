package com.github.devconslejme.tests.temp;

import com.github.devconslejme.misc.StringI;

public class TestSimpleThings {
	public static void main(String[] args) {
		tst3();
//		tst2();
//		tst1();
	}
	private static void tst3() {
		System.out.println(""+-10000000000000000.00000000000001);
	}
	private static void tst2() {
//		System.out.println(9^(1f/2f));
	}
	public static void tst1(){
		System.out.println(StringI.i().convertToUniqueId(Long.MAX_VALUE));
		System.out.println((Long.MAX_VALUE));
	}
	
	class A<SELF extends A>{
//  public void set(){}
	  public SELF set(){return getThis();}
		public SELF getThis(){return (SELF)this;}
	}
	class B extends A<B>{
	  @Override public B set(){return getThis();}
	  @Override public B getThis(){return this;}
	}
}
