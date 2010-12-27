package org.encalmo.printer

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class PlainTextPrinterTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testContext1() {
		val o:TextOutput = new TextOutput(java.util.Locale.US)
		val e = a*b+((c^2)/sin(4.126))
		PlainTextExpressionPrinter.print(e,o)
		o.printConsole
		assertEquals("a * b + c ^ 2 / sin4.126",o.getResult)//TODO localization
	}
	
	@Test def testContext2() {
		val e = a*b+((c^2)/sin(4.126))
		val o = PlainTextExpressionPrinter.print(e)
		o.printConsole
		assertEquals("a * b + c ^ 2 / sin4,126",o.getResult)//TODO localization
	}
	
}