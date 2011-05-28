package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class ExpressionResolverUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testEvaluate1() {
		val context = Context(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		assertEquals(Number(10),context.evaluate(c))
		assertEquals(Number(3),context.evaluate(d))
	}
	
	@Test def testEvaluate2() {
		val context = Context(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		assertEquals(Number(3),context.evaluate(d))
	}
	
	@Test def testResolve1() {
		val context = Context(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		assertEquals(Number(5)*Number(2),context.resolve(c))
	}
	
	@Test def testResolve2() {
		val context = Context(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		assertEquals(Sum(Quot(Prod(Number(5.0),Number(2.0)),Number(5.0)),Number(1.0)),context.resolve(d))
	}
	
	@Test def testSubstitute1() {
		val context = Context(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		assertEquals(Number(5)*Number(2),context.substitute(c))
	}
	
	@Test def testSubstitute2() {
		val context = Context(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		assertEquals(Sum(Quot(Prod(Number(5.0),Number(2.0)),Number(5.0)),Number(1.0)),context.substitute(d))
	}
	
	@Test def testSubstitute3() {
		val context = Calculation(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		assertEquals(Sum(Quot(Prod(Number(5.0),Number(2.0)),Number(5.0)),Number(1.0)),context.substitute(d))
	}
	
	@Test def testSubstitute4() {
		val context = Calculation(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		context.evaluate(c)
		assertEquals(Sum(Quot(Number(10.0),Number(5.0)),Number(1.0)),context.substitute(d))
	}
	
	@Test def testSubstitute5() {
		val context = Calculation(a -> 5, b -> 2, c -> a*b, d -> (c/a+1))
		context.evaluate(d)
		assertEquals(Sum(Quot(Prod(Number(5.0),Number(2.0)),Number(5.0)),Number(1.0)),context.substitute(d))
	}
	
}