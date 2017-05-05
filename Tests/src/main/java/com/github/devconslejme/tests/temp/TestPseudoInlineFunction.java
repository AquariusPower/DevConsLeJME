package com.github.devconslejme.tests.temp;

public class TestPseudoInlineFunction {
	public static void main(String[] args) {
		System.out.println("happy life");
		boolean bScaringWeirdConditionHappened=false;
		assert bScaringWeirdConditionHappened : new com.google.common.base.Function<Void,String>(){
			@Override public String apply(Void input) {
				return "weirdest possible message to completely scare the end user MUAHAHA";
			}}.apply(null);
		System.out.println("illusory life");
	}
}
