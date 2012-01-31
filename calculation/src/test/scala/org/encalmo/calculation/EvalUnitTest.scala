package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class EvalUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testEval1() {
	    val calc = Calculation()
	    calc(c) = a*b
	    calc(d) = (c/a+1)
	    calc(e) = d+c
		calc(f|1) = EvalAt(e,a -> 5, b -> 2)
		calc(f|2) = EvalAt(e,a -> 10, b -> 1)
		calc(f|3) = EvalAt(e,a -> 6, b -> 6)
		calc(x) = 1
		assertEquals(Number(13),calc(f|1).eval)
		assertEquals(Number(12),calc(f|2).eval)
		assertEquals(Number(43),calc(f|3).eval)
		assertEquals(Number(3),calc(e, a -> x, b -> x).eval)
	}
	
	@Test def testEval2() {
	    val calc = Calculation()
	    calc(c) = a*b
	    calc(d) = (c/a+1)
	    calc(e) = d+c
		val f1 = EvalAt(e, a -> 5, b -> 2)
		assertEquals(Sum(Sum(Number(1.0),Quot(Prod(Number(5.0),Number(2.0)),Number(5.0))),Prod(Number(5.0),Number(2.0))),calc.substitute(f1))
	}
	
}