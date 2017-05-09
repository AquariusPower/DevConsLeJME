package com.github.devconslejme.tests.temp;

import com.github.devconslejme.misc.StringI;

public class TestSimpleThings {
	public static void main(String[] args) {
		tst1();
	}
	public static void tst1(){
		System.out.println(StringI.i().convertToUniqueId(Long.MAX_VALUE));
		System.out.println((Long.MAX_VALUE));
	}
}
