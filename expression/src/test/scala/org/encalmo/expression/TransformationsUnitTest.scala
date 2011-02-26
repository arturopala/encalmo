package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import java.io.{StringWriter,PrintWriter}

class TransformationsUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testSimplifyPowerAndRoot() {
		val expr1 = (sqrt(x))^2
		val texpr1 = expr1.map(Transformations.simplifyPowerAndRoot)
		assertEquals(x,texpr1)
		val e1 = (x*y)^2
		val expr2 = (sqrt(e1))^2
		val texpr2 = expr2.map(Transformations.simplifyPowerAndRoot)
		assertEquals(e1,texpr2)
		val texpr22 = expr2.mapAll(Transformations.simplifyPowerAndRoot)
		assertEquals(e1,texpr22)
		val texpr3 = sqrt(e1).map(Transformations.simplifyPowerAndRoot)
		assertEquals(x*y,texpr3)
		val texpr32 = sqrt(expr2).mapAll(Transformations.simplifyPowerAndRoot)
		assertEquals(x*y,texpr32)
	}

}