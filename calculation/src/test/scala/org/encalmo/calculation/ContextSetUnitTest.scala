package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class ContextSetUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testContextSet1() {
		val calc = Calculation()
		val context1 = TestContext2("1",1)
		val context2 = TestContext2("2",2)
		calc add context1
		calc add context2
		calc(x) = 3
		calc(y) = 4
		calc(z) = 0
		
		val r_a1 = calc.resolve(context1.a).eval
		assertEquals(Number(14),r_a1);
		val r_b1 = calc.resolve(context1.b).eval
		assertEquals(Number(12),r_b1);
		val r_c1 = calc.resolve(context1.c).eval
		assertEquals(Number(5),r_c1);
		val r_d1 = calc.resolve(context1.d).eval
		assertEquals(Number(10),r_d1);
		val r_11 = calc.resolve(context1.a*context1.b+context1.d).eval
		assertEquals(Number(178),r_11);
		
		val r_a2 = calc.resolve(context2.a).eval
		assertEquals(Number(10),r_a2);
		val r_b2 = calc.resolve(context2.b).eval
		assertEquals(Number(6),r_b2);
		val r_c2 = calc.resolve(context2.c).eval
		assertEquals(Number(3.605551275),r_c2);
		val r_d2 = calc.resolve(context2.d).eval
		assertEquals(Number(7.211102550),r_d2);
		val r_12 = calc.resolve(context2.a*context2.b+context2.d).eval
		assertEquals(Number(67.21110255),r_12);
		
		assertEquals(Number(14),calc.resolve(context1.a).eval);
		assertEquals(Number(12),calc.resolve(context1.b).eval);
		assertEquals(Number(10),calc.resolve(context2.a).eval);
		assertEquals(Number(6),calc.resolve(context2.b).eval);
	}
	
	@Test def testContextSet2() {
		val calc = Calculation()
		val context1 = Context()
		calc add context1
		calc(x) = 3
		calc(y) = 4
		calc(z) = 0
		context1(a) = a
		
		val r_a1 = calc.resolve(a)
		assertEquals(a,r_a1);
	}
	
	@Test def testContextSet3() {
		val calc = Calculation()
		val context1 = Context()
		calc add context1
		calc(x) = 3
		calc(y) = 4
		calc(z) = 0
		context1(a) = pi*b
		val r_a1 = calc.resolve(a)
		assertEquals(pi*b,r_a1);
		context1(b) = 2*c
		val r_a2 = calc.resolve(a)
		//assertEquals(pi*(2*c),r_a2);
	}
	
	//circular reference test
	@Test def testContextSet4() {
		val calc = Calculation()
		val context1 = MapContext()
		calc add context1
		calc(x) = 3
		calc(y) = 4
		calc(z) = 0
		context1(a) = 2*b
		context1(b) = 2*c
		context1(c) = a/4
		try{
			val r_a3 = calc.resolve(a)
			assertEquals(a,r_a3);
			fail
		}
		catch{
			case _ : Throwable =>
		}
	}
	
	@Test def testContextSet5() {
		val calc = Calculation()
		val context1 = TestContext2("1",1)
		calc add context1
		calc(x) = 3
		calc(y) = 4
		calc(z) = 0
		val r_a1 = calc.evaluate(context1.a)
		assertEquals(Number(14),r_a1);
		val r_a2 = calc.resolve(context1.a)
		assertEquals(Sum(Prod(Number(2),Number(3)),Prod(Number(2),Quot(Number(4),Number(1)))),r_a2);
	}
	
	@Test def testContextSet6() {
		val calc = Calculation()
		val context1 = TestContext2("1",1)
		calc add context1
		calc(x) = 3
		calc(y) = 4
		calc(z) = 0
		val r_a2 = calc.resolve(context1.a)
		assertEquals(Sum(Prod(Number(2),Number(3)),Prod(Number(2),Quot(Number(4),Number(1)))),r_a2);
		val r_a1 = calc.evaluate(context1.a)
		assertEquals(Number(14),r_a1);
	}
	
}