package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class CalculationUnitTest extends AssertionsForJUnit {
	
	@Test def test2 = {
		import BasicSymbols._
		val c = Calculation()
		val c1 = Calculation("1") 
		val c2 = Calculation("2") 
		val c3 = Calculation("3") 
		c add c1
		c add c2
		c add c3
		c1(a) = 10
		c2(a) = 5
		c3(a) = 1
        assertEquals(Number(10),c.evaluate(a))
        assertEquals(Number(5),c.evaluate(c2(a)))
        assertEquals(Number(1),c.evaluate(c3(a)))
        assertEquals(Number(1),c2.evaluate(c3(a)))
        assertEquals(Number(5),c3.evaluate(c2(a)))
		assertEquals(Number(10),c1.evaluate(a))
		assertEquals(Number(5),c2.evaluate(a))
		assertEquals(Number(1),c3.evaluate(a))
	}
	
	@Test def test1 = {
		import BasicSymbols._
        implicit val cache = new ResultsCache()
		val c1 = Calculation("1") 
		
	    val fi = phiv
	    val bf = b|f
	    val tf = t|f
	    val tw = t|w
	    val Nfcr = N|(fi+"cr")
	    val Nxcr = N|"xcr"
	    val NRc = N|"Rc"
	    val Nmax = N|"max"
	    val fd = f|d
	    val Ix = I|x
	    val Iy = I|y
	    val Io = I|o
	    val mix = mi|x
	    val io = i|o
	    val ix = i|x
	    val iy = i|y
	    val Iomega = I|omega
	    val Iomega2 = I|(omega+"2")
	    val mif = mi|fi
	    val It = I|t
	    val lambdax = lambda|x
	    val lambdaf = lambda|fi
	    val lambdad = lambda over ("_")
		
		c1 put (h -> 0.4)
	    c1 put (bf -> 0.155)
	    c1 put (tf -> 0.00144)
	    c1 put (tw -> 0.00216)
	    c1 put (A -> 233.9E-4)
	    c1 put (Ix -> 30370E-8)
	    c1 put (Iy -> Ix)
	    c1 put (ix -> ( sqrt(Ix/A) ))
	    c1 put (mix -> 1)
	    c1 put (mif -> 1)
	    c1 put (l -> 3)
	    c1 put (lambdax -> ( mix*l/ix ))
	    c1 put (lambdaf -> ( sqrt((Ix+Iy)/((Iomega/((mif*l)^2))+((G*It)/((PI^2)*E)))) ))
	    c1 put (E -> 205E9)
	    c1 put (G -> 80E9)
	    c1 put (fd -> 215E6)
	    c1 put (NRc -> ( A*fd ))
	    c1 put (Nxcr -> ( (PI^2)*E*Ix/((mix*l)^2) ))
	    c1 put (Io -> ( Ix+Iy ))
	    c1 put (io -> ( hypot(ix,ix) ))
	    c1 put (Iomega2 -> ( 2*(Ix*(h^2)/4) ))
	    c1 put (Iomega -> ( 2*415000E-12 ))
	    c1 put (It -> ( 2*183E-8 ))
	    c1 put (Nfcr -> ( 1/(io^2)*(((PI^2)*E*Iomega)/((mif*l)^2)+G*It) ))
	    c1 put (lambdad -> ( 1.15*sqrt(NRc/min(Nxcr,Nfcr)) ))
	    c1 put (n -> 1.2)
	    c1 put (fi -> ( (1+(lambdad^(2*n)))^(-(1/n)) ))
	    c1 put (Nmax -> ( fi*NRc ))
		
		val re10 = c1.evaluate(ix)
		val re11 = c1.expand(ix)
		val re12 = c1.substitute(ix)
		val re20 = c1.evaluate(io)
		val re21 = c1.expand(io)
		val re22 = c1.substitute(io)
		val re30 = c1.evaluate(lambdad)
		val re31 = c1.expand(lambdad)
		val re32 = c1.substitute(lambdad)
		assertEquals(Number(0.11394815169211275),re10)
		assertEquals(sqrt(Quot(Number(3.037E-4),Number(0.02339))),re11)
		assertEquals(sqrt(Quot(Number(3.037E-4),Number(0.02339))),re12)
		assertEquals(Number(0.16114702153033258),re20)
		assertEquals(hypot(sqrt(Quot(Number(3.037E-4),Number(0.02339))),sqrt(Quot(Number(3.037E-4),Number(0.02339)))),re21)
		assertEquals(hypot(Number(0.11394815169211275),Number(0.11394815169211275)),re22)
		assertEquals(Number(0.6002191097678474),re30)
		//assertEquals(???,re31)
		assertEquals(Prod(Number(1.15),sqrt(Quot(Number(5028850),min(Number(68274085.06724685), Number(18460553.81536839))))),re32)
		
	}

}