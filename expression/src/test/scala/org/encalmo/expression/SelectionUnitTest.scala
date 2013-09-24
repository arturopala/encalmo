package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

class SelectionUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	val identity:Transformation = {case ex => ex}
	val negation:Transformation = {
		case a:Auxiliary => a
		case ex => -ex
	}
	val resolver1:Transformation = {
		case s:Symbol => ONE
		case ex => ex
	}
	
	val resolver2:Transformation = {
		case ex if ex==a => 3
		case ex if ex==b => 5
		case ex if ex==c => 0
		case ex => ex
	}
	
	val resolver3:Transformation = {
		case ex if ex==a => 7
		case ex if ex==b => 3
		case ex if ex==c => 5
		case ex => ex
	}
	
	@Test def testSelection1() {
		val expr1 = a*b
		val expr4 = Selection(Seq.empty[Case],CaseExpression(expr1))
		assertEquals(Selection(Seq.empty[Case],CaseExpression(a*b)),expr4)
        assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(List(),Some(CaseExpression(Inv(Prod(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\")))))))",expr5.toString)
        val expr6 = expr4.map(resolver1)
		assertEquals("Selection(List(),Some(CaseExpression(Prod(Number(1,_),Number(1,_)))))",expr6.toString)
        val expr7 = expr4.select
		assertEquals(expr1,expr7)
        val expr8 = expr6.eval()
		assertEquals(ONE,expr8)
    }
	
	@Test def testSelection2() {
		val expr1 = a*b
		val expr2 = a+b
		val expr4 = expr1 unless (IsZero(c) thenUse expr2)
		assertEquals("Selection(List(Case(CaseExpression(Sum(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),IsZero(Symbol(name=\"c\",unit=\"_\")))),Some(CaseExpression(Prod(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\")))))",expr4.toString)
        assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(List(Case(CaseExpression(Inv(Sum(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\"))))),IsZero(Inv(Symbol(name=\"c\",unit=\"_\"))))),Some(CaseExpression(Inv(Prod(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\")))))))",expr5.toString)
        val expr6 = expr4.map(resolver1).asInstanceOf[Selection]
		assertEquals("Selection(List(Case(CaseExpression(Sum(Number(1,_),Number(1,_))),IsZero(Number(1,_)))),Some(CaseExpression(Prod(Number(1,_),Number(1,_)))))",expr6.toString)
        val expr7 = expr6.select
		assertEquals(Prod(Number(1),Number(1)),expr7)
        val expr8 = expr6.trim
        assertEquals(Selection(List(),Some(CaseExpression(Prod(Number(1),Number(1))))),expr8)
        val expr11 = expr6.eval()
        assertEquals(Number(1),expr11)
		val expr9 = expr4.map(resolver2)
		assertTrue(expr4 ne expr9)
		assertEquals(Number(8),expr9.eval())
		val expr10 = expr4.map(resolver3)
		assertTrue(expr4 ne expr10)
		assertEquals(Number(21),expr10.eval())
	}
	
	@Test def testSelection3() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 unless (IsZero(c) thenUse expr2) unless (IsNotZero(d) thenUse expr3)
		assertEquals("Selection(List(Case(CaseExpression(Quot(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),IsNotZero(Symbol(name=\"d\",unit=\"_\"))), Case(CaseExpression(Sum(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),IsZero(Symbol(name=\"c\",unit=\"_\")))),Some(CaseExpression(Prod(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\")))))",expr4.toString)
        assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(List(Case(CaseExpression(Inv(Quot(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\"))))),IsNotZero(Inv(Symbol(name=\"d\",unit=\"_\")))), Case(CaseExpression(Inv(Sum(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\"))))),IsZero(Inv(Symbol(name=\"c\",unit=\"_\"))))),Some(CaseExpression(Inv(Prod(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\")))))))",expr5.toString)
        val expr6 = expr4.map(resolver1).asInstanceOf[Selection]
		assertEquals("Selection(List(Case(CaseExpression(Quot(Number(1,_),Number(1,_))),IsNotZero(Number(1,_))), Case(CaseExpression(Sum(Number(1,_),Number(1,_))),IsZero(Number(1,_)))),Some(CaseExpression(Prod(Number(1,_),Number(1,_)))))",expr6.toString)
        val expr7 = expr6.select
		assertEquals(Quot(Number(1),Number(1)),expr7)
        val expr8 = expr6.eval()
		assertEquals(ONE,expr8)
    }
	
	@Test def testSelection4() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 unless (IsZero(c) && IsNotZero(d) thenUse expr2)
		assertEquals("Selection(List(Case(CaseExpression(Sum(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),AndCaseTest(IsZero(Symbol(name=\"c\",unit=\"_\")),IsNotZero(Symbol(name=\"d\",unit=\"_\"))))),Some(CaseExpression(Prod(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\")))))",expr4.toString)
        assertEquals(expr4,expr4.map(identity))
		val expr5 = expr4.map(negation)
		assertTrue(expr4 ne expr5)
		assertEquals("Selection(List(Case(CaseExpression(Inv(Sum(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\"))))),AndCaseTest(IsZero(Inv(Symbol(name=\"c\",unit=\"_\"))),IsNotZero(Inv(Symbol(name=\"d\",unit=\"_\")))))),Some(CaseExpression(Inv(Prod(Inv(Symbol(name=\"a\",unit=\"_\")),Inv(Symbol(name=\"b\",unit=\"_\")))))))",expr5.toString)
        val expr6 = expr4.map(resolver1).asInstanceOf[Selection]
		assertEquals("Selection(List(Case(CaseExpression(Sum(Number(1,_),Number(1,_))),AndCaseTest(IsZero(Number(1,_)),IsNotZero(Number(1,_))))),Some(CaseExpression(Prod(Number(1,_),Number(1,_)))))",expr6.toString)
        val expr7 = expr6.select
		assertEquals(Prod(Number(1),Number(1)),expr7)
        val expr8 = expr6.eval()
		assertEquals(ONE,expr8)

    }
	
	@Test def testSelection5() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 unless (IsZero(c) || IsNotZero(d) thenUse expr2)
		assertEquals("Selection(List(Case(CaseExpression(Sum(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),OrCaseTest(IsZero(Symbol(name=\"c\",unit=\"_\")),IsNotZero(Symbol(name=\"d\",unit=\"_\"))))),Some(CaseExpression(Prod(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\")))))",expr4.toString)
    }
	
	@Test def testSelection6() {
		val expr1 = a*b
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 unless (IsZero(c) ||! IsZero(d) thenUse expr2)
		assertEquals("Selection(List(Case(CaseExpression(Sum(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),OrCaseTest(IsZero(Symbol(name=\"c\",unit=\"_\")),NegCaseTest(IsZero(Symbol(name=\"d\",unit=\"_\")))))),Some(CaseExpression(Prod(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\")))))",expr4.toString)
    }
	
	@Test def testSelection7() {
		val expr1 = a*b+5
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 unless (!IsZero(c) &&! IsZero(d) thenUse expr2)
		assertEquals("Selection(List(Case(CaseExpression(Sum(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),AndCaseTest(NegCaseTest(IsZero(Symbol(name=\"c\",unit=\"_\"))),NegCaseTest(IsZero(Symbol(name=\"d\",unit=\"_\")))))),Some(CaseExpression(Sum(Prod(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\")),Number(5,_)))))",expr4.toString)
    }
	
	@Test def testSelection8() {
		val expr1 = a*b+5
		val expr2 = a+b
		val expr3 = a/b
		val expr4 = expr1 unless (!IsZero(c) &&! IsZero(d) || IsEqualTo(c,d) thenUse expr2)
		assertEquals("Selection(List(Case(CaseExpression(Sum(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\"))),OrCaseTest(AndCaseTest(NegCaseTest(IsZero(Symbol(name=\"c\",unit=\"_\"))),NegCaseTest(IsZero(Symbol(name=\"d\",unit=\"_\")))),IsEqualTo(Symbol(name=\"c\",unit=\"_\"),Symbol(name=\"d\",unit=\"_\"))))),Some(CaseExpression(Sum(Prod(Symbol(name=\"a\",unit=\"_\"),Symbol(name=\"b\",unit=\"_\")),Number(5,_)))))",expr4.toString)
    }
	
}