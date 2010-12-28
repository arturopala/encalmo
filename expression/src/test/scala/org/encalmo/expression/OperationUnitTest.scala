package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class OperationUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def verifyMin1() {
		import Symbol._
		val e1:Expression = min(3,4,5,6,7,8,9,10)
		assertEquals(e1.eval,Number(3))
	}
	
	@Test def verifyMin2() {
		import Symbol._
		val e2:Expression = 10/2;
		val e3:Expression = 10/3;
		val e4:Expression = 10/4;
		val e1:Expression = min(e2,e3,e4)
		assertEquals(e1.eval,Number(2.5))
	}
	
	@Test def verifyMin3() {
		import Symbol._
		val e2:Expression = sin(10/2);
		val e3:Expression = sin(10/3);
		val e4:Expression = sin(10/4);
		val e1:Expression = min(e2,e3,e4)
		assertEquals(e1.eval,e2.eval)
	}
	
	@Test def verifyMax1() {
		import Symbol._
		val e1:Expression = min(3,4,5,6,7,8,9,10)
		assertEquals(e1.eval,Number(10))
	}
	
	@Test def verifyMax2() {
		import Symbol._
		val e2:Expression = 10/2;
		val e3:Expression = 10/3;
		val e4:Expression = 10/4;
		val e1:Expression = min(e2,e3,e4)
		assertEquals(e1.eval,Number(5))
	}
	
	@Test def verifyMax4() {
		import Symbol._
		val e2:Expression = sin(10/2);
		val e3:Expression = sin(10/3);
		val e4:Expression = sin(10/4);
		val e1:Expression = min(e2,e3,e4)
		assertEquals(e1.eval,e4.eval)
	}
	
	@Test def verifyPi1() {
		import Symbol._
		val e1:Expression = PI*10
		assertEquals(Number(31.41592653589793),e1.eval)
	}
	
	@Test def verifyE1() {
		import Symbol._
		val e1:Expression = EUL*PI
		assertEquals(Number(8.539734222673566),e1.eval)
	}

}