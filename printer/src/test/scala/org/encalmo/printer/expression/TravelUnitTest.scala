package org.encalmo.printer.expression

import org.encalmo.printer._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class TravelUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def test1() {
		val e:Expression = (a*b/c+4.125/(6.13*(z-1/2d)*8))+sin(1.1*k)/-(2-(x^(max(k,l,m))))
		val writer = new java.io.PrintWriter(Console.out)
		val t1 = new PlainTextExpressionPrinterTraveler(writer, java.util.Locale.ENGLISH)
		val t2 = new PlainTextExpressionPrinterTraveler(writer, new java.util.Locale("PL"))
		e.travel(null,t1)
		writer.write("\r\n")
		e.travel(null,t2)
		writer.close
	}
	
}