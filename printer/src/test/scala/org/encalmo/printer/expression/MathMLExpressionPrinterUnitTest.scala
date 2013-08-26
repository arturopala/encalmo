package org.encalmo.printer.expression

import org.encalmo.printer._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class MathMLExpressionPrinterUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def test1() {
		val o:MathMLOutput = new MathMLOutput(java.util.Locale.ENGLISH)
		val a1 = Symbol("Ar")
		val a2 = Symbol("Ar",BasicSymbols.a)
		val a3 = Symbol("Ar",BasicSymbols.a,BasicSymbols.beta)
		val a4 = Symbol("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.gamma)
		val a5 = Symbol("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.c,BasicSymbols.d)
		val d = alpha!z
		val b = a5|d
		val e = (a1+a2+a3+a4+a5)*(b-1.3)+(sqrt(c^(2-r))/(sin(4.126)+phiv))+root(l*k,f-1)/cbrt(.123^f)+max(a1,a2,a3,a4)
		o.open
		MathMLExpressionPrinter.print(e)(o)()
		o.close
		o.printConsole
		
		val sel1 = 1 unless (InRangeLLE(0.75,a3,1.4) thenUse (1.56-(0.75*a3))) unless (GreaterThan(a,1.4) thenUse (1/(a3^2)))
		val rel = a2*(5+sel1)
		val o2:MathMLOutput = new MathMLOutput(new java.util.Locale("PL"))
		o.open
        MathMLExpressionPrinter.print(e)(o)()
        o.close
        o.printConsole
	}
	
	@Test def test2() {
	    val o:MathMLOutput = new MathMLOutput(new java.util.Locale("PL"))
        val a2 = Symbol("Ar",BasicSymbols.a)
        val a3 = Symbol("Ar",BasicSymbols.a,BasicSymbols.beta)
        val sel1 = 1 unless (InRangeLLE(0.75,a3,1.4) thenUse (1.56-(0.75*a3))) unless (GreaterThan(a,1.4) thenUse (1/(a3^2)))
        val rel = a2*(5+sel1)
        o.open
        MathMLExpressionPrinter.print(rel)(o)()
        o.close
        o.printConsole
	}
	
	@Test def test3() {
	    import BasicSymbols._
        val o:MathMLOutput = new MathMLOutput(new java.util.Locale("PL"))
	    val e = a*b
        o.open
        MathMLExpressionPrinter.print(e)(o)()
        o.close
        assertEquals(o.getResult,"""<ml:math xmlns:ml="http://www.w3.org/1998/Math/MathML" scriptsizemultiplier="0.95" scriptminsize="6pt">
  <ml:mstyle mathcolor="#000000" mathbackground="" mathsize="11" mathvariant="normal">
    <ml:mrow mathvariant="italic" class="symb">
      <ml:mi>a</ml:mi>
    </ml:mrow>
    <ml:mo form="infix" lspace="thinmathspace" rspace="thinmathspace">&CenterDot;</ml:mo>
    <ml:mrow mathvariant="italic" class="symb">
      <ml:mi>b</ml:mi>
    </ml:mrow>
  </ml:mstyle></ml:math>&#8203;""")
    }
    
    @Test def test4() {
        import BasicSymbols._
        val o:MathMLOutput = new MathMLOutput(new java.util.Locale("PL"))
        val e = a*(b-c)
        o.open
        MathMLExpressionPrinter.print(e)(o)()
        o.close
        assertEquals(o.getResult,"""<ml:math xmlns:ml="http://www.w3.org/1998/Math/MathML" scriptsizemultiplier="0.95" scriptminsize="6pt">
  <ml:mstyle mathcolor="#000000" mathbackground="" mathsize="11" mathvariant="normal">
    <ml:mrow mathvariant="italic" class="symb">
      <ml:mi>a</ml:mi>
    </ml:mrow>
    <ml:mo form="infix" lspace="thinmathspace" rspace="thinmathspace">&CenterDot;</ml:mo>
    <ml:mrow>
      <ml:mfenced open="(" close=")" separators=";">
        <ml:mrow>
          <ml:mrow mathvariant="italic" class="symb">
            <ml:mi>b</ml:mi>
          </ml:mrow>
          <ml:mo form="infix" lspace="thinmathspace" rspace="thinmathspace">&minus;</ml:mo>
          <ml:mrow mathvariant="italic" class="symb">
            <ml:mi>c</ml:mi>
          </ml:mrow>
        </ml:mrow>
      </ml:mfenced>
    </ml:mrow>
  </ml:mstyle></ml:math>&#8203;""")
    }
	
}