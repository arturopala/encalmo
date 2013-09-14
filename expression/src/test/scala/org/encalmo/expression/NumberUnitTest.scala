package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class NumberUnitTest extends AssertionsForJUnit {
	
	@Test def verifySum1() {
		val a:Expression = 1.0
		val b:Expression = 1.0
		val c = a+b
		assertEquals(Number(2),c.eval);
	}
	
	@Test def verifySum2() {
		val a:Expression = 1.4
		val b:Expression = 11.7
		val c = a+b
		assertEquals(Number(13.1),c.eval);
	}
	
	@Test def verifyDiff1() {
		val a:Expression = 2.0
		val b:Expression = 1.0
		val c = a-b
		assertEquals(ONE,c.eval);
	}
	
	@Test def verifyDiff2() {
		val a:Expression = 10.3
		val b:Expression = 1.7
		val c = a-b
		assertEquals(Number(8.6),c.eval);
	}
	
	@Test def verifyDiff3() {
		val a:Expression = ZERO
		val b:Expression = ZERO
		val c = a-b
		assertEquals(ZERO,c.eval);
	}
	
	@Test def verifyDiff4() {
		val a:Expression = ONE
		val b:Expression = ONE
		val c = a-b
		assertEquals(ZERO,c.eval);
	}
	
	@Test def verifyNeg() {
		val a:Expression = ONE
		val c = -a
		assertEquals(Number(-1),c.eval);
	}
    
    @Test def verifyPower() {
        assertEquals(Number(4),Power(2,2).eval);
        assertEquals(Number(25),Power(5,2).eval);
        assertEquals(Number(5),Power(25,0.5).eval);
    }
    
    @Test def verifyZERO() {
        import BasicSymbols._
        assertEquals(ONE,ZERO^ZERO)
        assertEquals(ZERO,ZERO^a)
        assertEquals(ONE,a^ZERO)
    }
	
	@Test def testFormatForPrint() {
		val n1 = Number(0.289765)
		val ffp1 = n1.formattedToPrint
		assertFalse(ffp1.isNegative)
		assertFalse(ffp1.hasExponent)
		assertEquals(0,ffp1.integer,0.000001)
		assertEquals(0,ffp1.exponent)
		assertEquals(0.289765,ffp1.fraction,0.000001)
		
		val n2 = Number(0.000000289765)
		val ffp2 = n2.formattedToPrint
		assertFalse(ffp2.isNegative)
		assertTrue(ffp2.hasExponent)
		assertEquals(289,ffp2.integer)
		assertEquals(-9,ffp2.exponent)
		assertEquals(0.765,ffp2.fraction,0.000001)
		
		val n3 = Number(12.0023)
		val ffp3 = n3.formattedToPrint
		assertFalse(ffp3.isNegative)
		assertFalse(ffp3.hasExponent)
		assertEquals(12,ffp3.integer)
		assertEquals(0,ffp3.exponent)
		assertEquals(.0023,ffp3.fraction,0.000001)
		
		val n4 = Number(129876598.0023678)
		val ffp4 = n4.formattedToPrint
		assertFalse(ffp4.isNegative)
		assertTrue(ffp4.hasExponent)
		assertEquals(129,ffp4.integer)
		assertEquals(6,ffp4.exponent)
		assertEquals(.876598,ffp4.fraction,0.000001)
		
		val n5 = Number(1298765980000.000)
		val ffp5 = n5.formattedToPrint
		assertFalse(ffp5.isNegative)
		assertEquals(1,ffp5.integer)
		assertEquals(12,ffp5.exponent)
		assertEquals(.29876598,ffp5.fraction,0.000001)
		
		val n6 = Number(0.0000056789)
		val ffp6 = n6.formattedToPrint
		assertFalse(ffp6.isNegative)
		assertEquals(5,ffp6.integer)
		assertEquals(-6,ffp6.exponent)
		assertEquals(.6789,ffp6.fraction,0.000001)
		
		val n7 = Number(-0.0000056789)
		val ffp7 = n7.formattedToPrint
		assertTrue(ffp7.isNegative)
		assertTrue(ffp7.hasExponent)
		assertEquals(5,ffp7.integer)
		assertEquals(-6,ffp7.exponent)
		assertEquals(.6789,ffp7.fraction,0.000001)
		
		val n8 = Number(1.0)
		val ffp8 = n8.formattedToPrint
		assertFalse(ffp8.isNegative)
		assertFalse(ffp8.hasExponent)
		assertEquals(1,ffp8.integer)
		assertEquals(0,ffp8.exponent)
		assertEquals(0,ffp8.fraction,0.000001)
		
		val n9 = Number(0)
		val ffp9 = n9.formattedToPrint
		assertFalse(ffp9.isNegative)
		assertFalse(ffp9.hasExponent)
		assertEquals(0,ffp9.integer)
		assertEquals(0,ffp9.exponent)
		assertEquals(0,ffp9.fraction,0.000001)
		
		val n10 = Number(-1.0)
		val ffp10 = n10.formattedToPrint
		assertTrue(ffp10.isNegative)
		assertFalse(ffp10.hasExponent)
		assertEquals(1,ffp10.integer)
		assertEquals(0,ffp10.exponent)
		assertEquals(0,ffp10.fraction,0.000001)
	}
	
	@Test def testFormatForPrint2 {
		val n:Number = (Number(122E-6)/Number(201.062E-9)).eval.asInstanceOf[Number]
		val fn = n.formattedToPrint
		assertTrue(!fn.isNegative)
	}

    @Test def testAdjust() {
        val a = Number(12.1311)
        val b = a.adjustTo(0.01)
        assertEquals(Number(12.13),b)
        val c = a.adjustTo(0.1)
        assertEquals(Number(12.1),c)
        val d = a.adjustTo(1)
        assertEquals(Number(12),d)
        val e = a.adjustTo(5)
        assertEquals(Number(15),e)
        val f = a.adjustTo(10)
        assertEquals(Number(10),f)
        val g = a.adjustTo(20)
        assertEquals(Number(20),g)
    }
	
}