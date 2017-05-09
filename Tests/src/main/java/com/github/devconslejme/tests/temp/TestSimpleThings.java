package com.github.devconslejme.tests.temp;

import com.github.devconslejme.misc.StringI;

public class TestSimpleThings {
	public static void main(String[] args) {
		System.out.println(StringI.i().convertToUniqueId(Long.MAX_VALUE));
		System.out.println((Long.MAX_VALUE));
//		System.out.println(">>>"+StringI.i().getNextUniqueId(""+(Long.MAX_VALUE-1)));
//		BigInteger bi = new BigInteger(""+(Long.MAX_VALUE-1), 10);
//		BigInteger bi2 = new BigInteger("0", 36);
//		System.out.println(">>>"+(bi.add(bi2)).toString(36));
	}
}
