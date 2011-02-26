package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class ContextUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testContext1() {
		val context = TestContext("1")
		context(context.p1) = 3
		context(context.p2) = 4
		context(context.p3) = 0
		val r_a = context.resolve(a).eval
		assertEquals(Number(14),r_a);
		val r_b = context.resolve(b).eval
		assertEquals(Number(12),r_b);
		val r_c = context.resolve(c).eval
		assertEquals(Number(5),r_c);
		val r_d = context.resolve(d).eval
		assertEquals(Number(10),r_d);
		val r_1 = context.resolve(a*b+d).eval
		assertEquals(Number(178),r_1);
		val r_e = context.resolve(e).eval
		assertEquals(Number(81),r_e);
	}
	
	@Test def testContext2() {
		val context = TestContext("1")
		context(context.p1) = 3
		context(context.p2) = 4
		context(context.p3) = 0
		val r_b = context.resolve(a);
		val r_a = context.evaluate(a);
		assertEquals(Number(14),r_a);
		assertEquals(Sum(Prod(Number(2.0),Number(3.0)),Prod(Number(2.0),Number(4.0))),r_b)
	}
	
	@Test def testContext3() {
		val context = TestContext("1")
		context(context.p1) = 3
		context(context.p2) = 4
		context(context.p3) = 0
		val r_a = context.evaluate(a);
		val r_b = context.resolve(a);
		assertEquals(Number(14),r_a);
		assertEquals(Sum(Prod(Number(2.0),Number(3.0)),Prod(Number(2.0),Number(4.0))),r_b)
	}
	
}