package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class ContextUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testContext1() {
        implicit val cache = new ResultsCache()
		val context = TestContext("1")
		context(context.p1) = 3
		context(context.p2) = 4
		context(context.p3) = 0
		val r_a = context.expand(a).eval
		assertEquals(Number(14),r_a);
		val r_b = context.expand(b).eval
		assertEquals(Number(12),r_b);
		val r_c = context.expand(c).eval
		assertEquals(Number(5),r_c);
		val r_d = context.expand(d).eval
		assertEquals(Number(10),r_d);
		val r_1 = context.expand(a*b+d).eval
		assertEquals(Number(178),r_1);
		val r_e = context.expand(e).eval
		assertEquals(Number(81),r_e);
	}
	
	@Test def testContext2() {
        implicit val cache = new ResultsCache()
		val context = TestContext("1")
		context(context.p1) = 3
		context(context.p2) = 4
		context(context.p3) = 0
		val r_b = context.expand(a);
		val r_a = context.evaluate(a);
		assertEquals(Number(14),r_a);
		assertEquals(Sum(Prod(Number(2.0),Number(3.0)),Prod(Number(2.0),Number(4.0))),r_b)
	}
	
	@Test def testContext3() {
        implicit val cache = new ResultsCache()
		val context = TestContext("1")
		context(context.p1) = 3
		context(context.p2) = 4
		context(context.p3) = 0
		val r_a = context.evaluate(a);
		val r_b = context.expand(a);
		assertEquals(Number(14),r_a);
		assertEquals(Sum(Prod(Number(2.0),Number(3.0)),Prod(Number(2.0),Number(4.0))),r_b)
	}
	
	@Test def testUnitConversion1() {
        implicit val cache = new ResultsCache()
        val c1 = ContextFactory()
        val c2 = ContextFactory()
        val s1 = Symbol("s1") unit SI.kg
        val s2 = Symbol("s2") unit SI.cm
        val n = Number(123.45)
        c1(s1) = n
        c2(s2) = n
        val r1 = c1.expand(s1)
        val r2 = c2.expand(s2)
        assertEquals(Number(123.45),r1)
        assertEquals(SI.kg,r1.unit)
        assertEquals(Number(123.45),r2)
        assertEquals(SI.cm,r2.unit)
    }
    
    @Test def testUnitConversion2() {
        implicit val cache = new ResultsCache()
        val c1 = ContextFactory()
        val c2 = ContextFactory()
        val s1 = Symbol("s1") unit SI.kg
        val s2 = Symbol("s2")
        val n = Number(123.45) unit SI.g
        c1(s1) = n
        c2(s2) = n
        val r1 = c1.expand(s1)
        val r2 = c2.expand(s2)
        assertEquals(Number(0.12345),r1)
        assertEquals(SI.kg,r1.unit)
        assertEquals(Number(123.45),r2)
        assertEquals(SI.g,r2.unit)
    }
    
    @Test def testUnitConversion3() {
        implicit val cache = new ResultsCache()
        val c1 = ContextFactory()
        val c2 = ContextFactory()
        val s1 = Symbol("s1") unit SI.km
        val s2 = Symbol("s2") unit SI.cm
        val n = Number(123.45) unit SI.m
        c1(s1) = n
        c2(s2) = n
        val r1 = c1.expand(s1)
        val r2 = c2.expand(s2)
        assertEquals(Number(0.12345),r1)
        assertEquals(SI.km,r1.unit)
        assertEquals(Number(12345),r2)
        assertEquals(SI.cm,r2.unit)
    }
    
    @Test def testMutableContext1() {
        implicit val cache = new ResultsCache()
        val c1 = ContextFactory()
        c1(a) = b
        assertEquals(b,c1.getExpression(a).get)
        c1(c) = dynamic(a,b,d,e) { implicit cache =>
            c1.evaluate(a) match {
                case x if x==b => d
                case _ => e
            }
        }
        assertTrue(c1.getExpression(c).get.isInstanceOf[DynamicExpression])
        assertEquals(d,c1.evaluate(c))
    }
	
}