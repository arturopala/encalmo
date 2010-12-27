package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class SelectionUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	val identity:Transformation = {e => e}
	val negation:Transformation = e => e match {
		case a:Auxiliary => e
		case _ => -e
	}
	val resolver1:Transformation = e => e match {
		case s:Symbol => one
		case _ => e
	}
	
	@Test def testSelection1() {
		val expr1 = a*b
		val expr4 = Selection(expr1,Seq.empty[Case])
		assertEquals("Selection(Prod(Symbol(a),Symbol(b)),List())",expr4.toString);
		assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(Neg(Prod(Neg(Symbol(a)),Neg(Symbol(b)))),List())",expr5.toString);
		val expr6 = expr4.map(resolver1)
		assertEquals("Selection(Prod(Number(1.0),Number(1.0)),List())",expr6.toString);
		val expr7 = expr4.select
		assertEquals(expr1,expr7);
		val expr8 = expr6.eval
		assertEquals(one,expr8);
	}
	
	@Test def testSelection2() {
		val expr1 = a*b
		val expr2 = a+b
		val expr4 = expr1 or (IsZero(c) then expr2)
		assertEquals("Selection(Prod(Symbol(a),Symbol(b)),List(Case(Sum(Symbol(a),Symbol(b)),IsZero(Symbol(c)))))",expr4.toString);
		assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(Neg(Prod(Neg(Symbol(a)),Neg(Symbol(b)))),List(Case(Neg(Sum(Neg(Symbol(a)),Neg(Symbol(b)))),IsZero(Neg(Symbol(c))))))",expr5.toString);
		val expr6 = expr4.map(resolver1)
		assertEquals("Selection(Prod(Number(1.0),Number(1.0)),List(Case(Sum(Number(1.0),Number(1.0)),IsZero(Number(1.0)))))",expr6.toString);
		val expr7 = expr4.select
		assertEquals(expr1,expr7);
		val expr8 = expr6.eval
		assertEquals(one,expr8);
	}
	
	@Test def testSelection3() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 or (IsZero(c) then expr2) or (IsNotZero(d) then expr3)
		assertEquals("Selection(Prod(Symbol(a),Symbol(b)),List(Case(Sum(Symbol(a),Symbol(b)),IsZero(Symbol(c))), Case(Quot(Symbol(a),Symbol(b)),IsNotZero(Symbol(d)))))",expr4.toString);
		assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(Neg(Prod(Neg(Symbol(a)),Neg(Symbol(b)))),List(Case(Neg(Sum(Neg(Symbol(a)),Neg(Symbol(b)))),IsZero(Neg(Symbol(c)))), Case(Neg(Quot(Neg(Symbol(a)),Neg(Symbol(b)))),IsNotZero(Neg(Symbol(d))))))",expr5.toString);
		val expr6 = expr4.map(resolver1)
		assertEquals("Selection(Prod(Number(1.0),Number(1.0)),List(Case(Sum(Number(1.0),Number(1.0)),IsZero(Number(1.0))), Case(Quot(Number(1.0),Number(1.0)),IsNotZero(Number(1.0)))))",expr6.toString);
		val expr7 = expr4.select
		assertEquals(expr3,expr7);
		val expr8 = expr6.eval
		assertEquals(one,expr8);
	}
	
	@Test def testSelection4() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 or (IsZero(c)&IsNotZero(d) then expr2)
		assertEquals("Selection(Prod(Symbol(a),Symbol(b)),List(Case(Sum(Symbol(a),Symbol(b)),AndCaseTest(IsZero(Symbol(c)),IsNotZero(Symbol(d))))))",expr4.toString);
		assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(Neg(Prod(Neg(Symbol(a)),Neg(Symbol(b)))),List(Case(Neg(Sum(Neg(Symbol(a)),Neg(Symbol(b)))),AndCaseTest(IsZero(Neg(Symbol(c))),IsNotZero(Neg(Symbol(d)))))))",expr5.toString);
		val expr6 = expr4.map(resolver1)
		assertEquals("Selection(Prod(Number(1.0),Number(1.0)),List(Case(Sum(Number(1.0),Number(1.0)),AndCaseTest(IsZero(Number(1.0)),IsNotZero(Number(1.0))))))",expr6.toString);
		val expr7 = expr4.select
		assertEquals(expr1,expr7);
		val expr8 = expr6.eval
		assertEquals(one,expr8);
		
	}
	
	@Test def testSelection5() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 or (IsZero(c)|IsNotZero(d) then expr2)
		assertEquals("Selection(Prod(Symbol(a),Symbol(b)),List(Case(Sum(Symbol(a),Symbol(b)),OrCaseTest(IsZero(Symbol(c)),IsNotZero(Symbol(d))))))",expr4.toString);
	}
	
	@Test def testSelection6() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 or (IsZero(c)|!IsZero(d) then expr2)
		assertEquals("Selection(Prod(Symbol(a),Symbol(b)),List(Case(Sum(Symbol(a),Symbol(b)),OrCaseTest(IsZero(Symbol(c)),NegCaseTest(IsZero(Symbol(d)))))))",expr4.toString);
	}
	
	@Test def testSelection7() {
		val expr1 = a*b+5
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 or (!IsZero(c)&!IsZero(d) then expr2)
		assertEquals("Selection(Sum(Prod(Symbol(a),Symbol(b)),Number(5.0)),List(Case(Sum(Symbol(a),Symbol(b)),AndCaseTest(NegCaseTest(IsZero(Symbol(c))),NegCaseTest(IsZero(Symbol(d)))))))",expr4.toString);
	}
	
	@Test def testSelection8() {
		val expr1 = a*b+5
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 or (!IsZero(c)&!IsZero(d)|Equals(c,d) then expr2)
		assertEquals("Selection(Sum(Prod(Symbol(a),Symbol(b)),Number(5.0)),List(Case(Sum(Symbol(a),Symbol(b)),OrCaseTest(AndCaseTest(NegCaseTest(IsZero(Symbol(c))),NegCaseTest(IsZero(Symbol(d)))),Equals(Symbol(c),Symbol(d))))))",expr4.toString);
	}
	
}