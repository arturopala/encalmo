package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class NumberUnitTest extends AssertionsForJUnit {
	
	@Test def verifySum1() {
		val a:Expression = 1.0
		val b:Expression = 1.0
		val c = a+b
		assertEquals(Number(2),c.eval);
	}
	
	@Test def verifySum2() {
		val a:Expression = 1.4
		val b:Expression = 11.7
		val c = a+b
		assertEquals(Number(13.1),c.eval);
	}
	
	@Test def verifyDiff1() {
		val a:Expression = 2.0
		val b:Expression = 1.0
		val c = a-b
		assertEquals(ONE,c.eval);
	}
	
	@Test def verifyDiff2() {
		val a:Expression = 10.3
		val b:Expression = 1.7
		val c = a-b
		assertEquals(Number(8.6),c.eval);
	}
	
	@Test def verifyDiff3() {
		val a:Expression = ZERO
		val b:Expression = ZERO
		val c = a-b
		assertEquals(ZERO,c.eval);
	}
	
	@Test def verifyDiff4() {
		val a:Expression = ONE
		val b:Expression = ONE
		val c = a-b
		assertEquals(ZERO,c.eval);
	}
	
	@Test def verifyNeg() {
		val a:Expression = ONE
		val c = -a
		assertEquals(Number(-1),c.eval);
	}
	
}