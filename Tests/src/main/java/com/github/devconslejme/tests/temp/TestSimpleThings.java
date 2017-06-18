package com.github.devconslejme.tests.temp;

import java.util.ArrayList;
import java.util.HashMap;

import com.github.devconslejme.misc.CommandLineParser;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.JavaLangI;
import com.github.devconslejme.misc.StringI;
import com.github.devconslejme.misc.TimedDelay;

public class TestSimpleThings {
	public static void main(String[] args) {
		tst9();
//		tst8();
//		tst7();
//		tst6();
//		tst5();
//		tst4();
//		tst3();
//		tst2();
//		tst1();
	}
	
	private static void tst9() {
		ArrayList<String> a = new ArrayList<>();
		long lSum=0;
		int iMax=1000;
		int iblah=0;
		for(int i3=0;i3<iMax;i3++) {
			int i2=0;
			long l=System.nanoTime();
			for(int i=0;i<100000;i++) {
				if(a.iterator().hasNext())i2++; //this seems to be 25% faster!
				//Object[] array = a.toArray();i2+=array.length;
			}
			iblah+=i2;
			lSum+=(System.nanoTime()-l);
		}
		System.out.println(iblah);
		System.out.println(lSum/iMax);
	}

	static HashMap<String,HashMap<String,Integer>> hhm=new HashMap<String,HashMap<String,Integer>>();
	private static void function(HashMap<String,Integer>... array){System.out.println(array);}
	private static void tst8() {
		function( hhm.values().toArray((HashMap<String,Integer>[]) new HashMap<?,?>[0]) );
	}
	private static void tst7() {
		TimedDelay td = new TimedDelay(1f).setUseRealTime(true).setActive(true)
				.setOscilateMode(true)
				;
		
		while(td.getCurrentDelay()<5){
//			System.out.println(td.getCurrentDelayNano()+","+td.getCurrentDelayCalcDynamic(10));
			System.out.println(td.calcRemainderAsPercentualMultBy(10));
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	public static enum ETest{
		TestA,
		TestB
	}
	private static void tst6() {
		GlobalManagerI.i().putConcrete(new TestSimpleThings()); //will auto add the enum too
		CommandLineParser clp = new CommandLineParser("cmdTest 'testString' "
			+JavaLangI.i().enumUId(ETest.TestB)
			+" 123");
		System.out.println(clp.getAllPartsStrListCopy());
	}

	private static void tst5() {
		int i=10;
//		new Object(){{i++;}};
		
	}

	public static class Tst{
		public void a(){}
		public int b(){return 0;}
	}
	private static void tst4() {
		Tst tst = new Tst(){{
			a();
			System.out.println(b());
		}};
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
