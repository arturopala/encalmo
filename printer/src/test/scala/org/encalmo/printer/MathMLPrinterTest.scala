package org.encalmo.printer

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class MathMLPrinterTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testContext1() {
		val o:MathMLOutput = new MathMLOutput
		val a1 = Symbol1("Ar")
		val a2 = Symbol2("Ar",BasicSymbols.a)
		val a3 = Symbol3("Ar",BasicSymbols.a,BasicSymbols.beta)
		val a4 = Symbol4("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.gamma)
		val a5 = Symbol5("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.c,BasicSymbols.d)
		val d = alpha!z
		val b = a5|d
		val e = (a1+a2+a3+a4+a5)*(b-1.3)+(sqrt(c^(2-r))/(sin(4.126)+phiv))+root(l*k,f-1)/cbrt(.123^f)
		o.open
		MathMLExpressionPrinter.print(e,o)
		o.close
		o.printConsole
	}
	
}