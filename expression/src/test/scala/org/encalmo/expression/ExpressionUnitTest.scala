package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class ExpressionUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def verifyProd1() {
		val z = (a|b)*3.0
		assertEquals(Prod(Symbol("a",Symbol("b")),Number(Real(3))),z);
		assertEquals(Prod(Symbol("a",b),Number(Real(3))),z);
	}
	
	@Test def verifyProd2() {
		val z = 3.0*a
		assertEquals(Prod(Number(Real(3)),a),z);
	}
	
	@Test def verifyExpr2() {
		val z = (a+b)/(c*3)
		assertEquals(Quot(Sum(a,b),Prod(c,3)),z);
	}
	
	@Test def verifyExpr3() {
		val a:Expression = 2
		val b:Expression = 3
		val c:Expression = 4
		val z1 = (a+b)*c/2
		val z2 = a*c/2+b*c/2
		val z3 = (a*c/2)+(b*c/2)
		assertEquals(z1.eval,z2.eval);
		assertEquals(z1.eval,z3.eval);
	}
	
	@Test def verifyExpr4() {
		val a:Expression = 2
		val b:Expression = 3
		val c:Expression = 4
		val z1 = min(a,b,c)
		assertEquals(Number(2),z1.eval);
	}
	
	@Test def testIdentityMap() {
		val tra:Transformation = {e => e}
		val e1:Expression = a+b
		assertEquals(e1,e1.map(tra))
		val e2:Expression = a-b
		assertEquals(e2,e2.map(tra))
		val e3:Expression = a*b
		assertEquals(e3,e3.map(tra))
		val e4:Expression = a/b
		assertEquals(e4,e4.map(tra))
		val e5:Expression = a%b
		assertEquals(e5,e5.map(tra))
		val e6:Expression = a^b
		assertEquals(e6,e6.map(tra))
		val e7:Expression = hypot(a,b)
		assertEquals(e7,e7.map(tra))
		val e8:Expression = min(a,b)
		assertEquals(e8,e8.map(tra))
		val e9:Expression = -a
		assertEquals(e9,e9.map(tra))
		val e10:Expression = sin(a)
		assertEquals(e10,e10.map(tra))
		val e11:Expression = cos(a)
		assertEquals(e11,e11.map(tra))
		val e12:Expression = tan(a)
		assertEquals(e12,e12.map(tra))
		val e13:Expression = cot(a)
		assertEquals(e13,e13.map(tra))
		val e14:Expression = sqrt(a)
		assertEquals(e14,e14.map(tra))
		val e15:Expression = cbrt(a)
		assertEquals(e15,e15.map(tra))
	}
	
	@Test def testNegativeMap() {
		val tra:Transformation = {e => -e}
		val e1:Expression = a+b
		assertTrue(-e1 ne e1.map(tra))
		val e2:Expression = a-b
		assertTrue(-e2 ne e2.map(tra))
		val e3:Expression = a*b
		assertTrue(-e3 ne e3.map(tra))
		val e4:Expression = a/b
		assertTrue(-e4 ne e4.map(tra))
		val e5:Expression = a%b
		assertTrue(-e5 ne e5.map(tra))
		val e6:Expression = a^b
		assertTrue(-e6 ne e6.map(tra))
		val e7:Expression = hypot(a,b)
		assertTrue(-e7 ne e7.map(tra))
		val e8:Expression = min(a,b)
		assertTrue(-e8 ne e8.map(tra))
		val e9:Expression = -a
		assertTrue(-e9 ne e9.map(tra))
		val e10:Expression = sin(a)
		assertTrue(-e10 ne e10.map(tra))
		val e11:Expression = cos(a)
		assertTrue(-e11 ne e11.map(tra))
		val e12:Expression = tan(a)
		assertTrue(-e12 ne e12.map(tra))
		val e13:Expression = cot(a)
		assertTrue(-e13 ne e13.map(tra))
		val e14:Expression = sqrt(a)
		assertTrue(-e14 ne e14.map(tra))
		val e15:Expression = cbrt(a)
		assertTrue(-e15 ne e15.map(tra))
	}
	
	@Test def verify1() {
		import Symbol._
		val e1:Expression = 1/(r|1)
		dumpRaw(e1)
		assertEquals("Quot(Number(1.0),Symbol(r,Symbol(1)))", e1.toString)
	}
	
	@Test def verify2() {
		import Symbol._
		val e1:Expression = z-1/r
		assertEquals("Diff(Symbol(z),Quot(Number(1.0),Symbol(r)))", e1.toString)
	}
	
	@Test def verify3() {
		import Symbol._
		val e1:Expression = z-1/2d
		//dumpRaw(e1)
		assertEquals("Diff(Symbol(z),Number(0.5))", e1.toString)
	}
	
}