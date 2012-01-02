package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class NumberUnitOfValueUnitTest extends AssertionsForJUnit {
    
    @Test def testEquals() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 120 unit SI.cm
        val c:Number = 1.2 unit SI.g
        assertEquals(1.2 unit SI.m, 120 unit SI.cm)
        assertEquals(1200 unit SI.m, 1.2 unit SI.km)
        assertFalse((1.2 unit SI.m) == (1.2 unit SI.g))
        assertEquals(1.2 unit SI.m2, 12000 unit SI.cm2)
        assertEquals(1.2 unit SI.m2, 1200000 unit SI.mm2)
        assertEquals(1.2 unit SI.m3, 1200000 unit SI.cm3)
        assertEquals(1.2 unit SI.m3, 1200000000l unit SI.mm3)
    }
	
	@Test def testSum1() {
		val a:Number = 1.2
		val b:Number = 2.1
		val c = (a+b).eval
		assertEquals(Number(3.3),c);
		assertEquals(EmptyUnitOfValue,c.unit)
	}
	
	@Test def testSum2() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.cm
        val c = (a+b).eval
        assertEquals(Number(1.221),c);
        assertEquals(SI.m,c.unit)
        val d = (b+a).eval
        assertEquals(Number(1.221),d);
        assertEquals(SI.m,d.unit)
    }
	
	@Test def testSum3() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.km
        val c = (a+b).eval
        assertEquals(Number(2.1012),c);
        assertEquals(SI.km,c.unit)
        val d = (b+a).eval
        assertEquals(Number(2.1012),d);
        assertEquals(SI.km,d.unit)
    }
	
	@Test def testSum4() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.g
        val c = (a+b).eval
        assertEquals(Number(3.3),c);
        assertTrue(c.unit.isInstanceOf[IllegalUnitOfValue])
        assertEquals("!m+g!",c.unit.toNameString);
        val d = (b+a).eval
        assertEquals(Number(3.3),d);
        assertTrue(d.unit.isInstanceOf[IllegalUnitOfValue])
        assertEquals("!g+m!",d.unit.toNameString);
    }
	
	@Test def testSum5() {
        SI.units.foreach( u => {
            val a:Number = 1.2 unit u
            val b:Number = 2.1 unit u
            val c = (a+b).eval
            assertEquals(Number(3.3),c.eval);
            assertEquals(u,c.unit)
        })
    }
	
	@Test def testSubtract1() {
        val a:Number = 1.2
        val b:Number = 2.1
        val c = (a-b).eval
        assertEquals(Number(-0.9),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testSubtract2() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.cm
        val c = (a-b).eval
        assertEquals(Number(1.179),c);
        assertEquals(SI.m,c.unit)
        val d = (b-a).eval
        assertEquals(Number(-1.179),d);
        assertEquals(SI.m,d.unit)
    }
    
    @Test def testSubtract3() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.km
        val c = (a-b).eval
        assertEquals(Number(-2.0988),c);
        assertEquals(SI.km,c.unit)
        val d = (b-a).eval
        assertEquals(Number(2.0988),d);
        assertEquals(SI.km,d.unit)
    }
    
    @Test def testSubtract4() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.g
        val c = (a-b).eval
        assertEquals(Number(-0.9),c.eval);
        assertTrue(c.unit.isInstanceOf[IllegalUnitOfValue])
        assertEquals("!m-g!",c.unit.toNameString);
        val d = (b-a).eval
        assertEquals(Number(0.9),d);
        assertTrue(d.unit.isInstanceOf[IllegalUnitOfValue])
        assertEquals("!g-m!",d.unit.toNameString);
    }
    
    @Test def testSubtract5() {
        SI.units.foreach( u => {
            val a:Number = 1.2 unit u
            val b:Number = 2.1 unit u
            val c = (a-b).eval
            assertEquals(Number(-0.9),c.eval);
            assertEquals(u,c.unit)
            val d = (b-a).eval
            assertEquals(Number(0.9),d.eval);
            assertEquals(u,d.unit)
        })
    }
	
	@Test def testMultiply1() {
        val a:Number = 1.2
        val b:Number = 2.1
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
	
	@Test def testMultiply2() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.m
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.m2,c.unit)
    }
    
    @Test def testMultiply1a() {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testMultiply2a() {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.cm
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMultiply3() {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.m
        val c = (a*b).eval
        assertEquals(Number(0.0252),c);
        assertEquals(SI.m2,c.unit)
        val d = (b*a).eval
        assertEquals(Number(0.0252),d);
        assertEquals(SI.m2,d.unit)
    }
    
    @Test def testMultiply4() {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.g
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals("cm*g",c.unit.toNameString)
        val d = (b*a).eval
        assertEquals(Number(2.52),d);
        assertEquals("cm*g",d.unit.toNameString)
    }
    
    @Test def testDivide1() {
        val a:Number = 1.2
        val b:Number = 2.1
        val c = (a/b).eval
        assertEquals(Number(0.571428571),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide2() {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.cm
        val c = (a/b).eval
        assertEquals(Number(0.571428571),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide3() {
        val a:Number = 2 unit SI.m
        val b:Number = 5 unit SI.cm
        val c = (a/b).eval
        assertEquals(Number(40),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide4() {
        val a:Number = 2 unit SI.cm
        val b:Number = 5 unit SI.m
        val c = (a/b).eval
        assertEquals(Number(0.004),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide5() {
        val a:Number = 2 unit SI.cm
        val b:Number = 5 unit SI.kg
        val c = (a/b).eval
        assertEquals(Number(0.4),c);
        assertEquals("cm/kg",c.unit.toNameString)
    }
    
    @Test def testModulo1() {
        val a:Number = 10
        val b:Number = 4
        val c = (a%b).eval
        assertEquals(Number(2),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testModulo2() {
        val a:Number = 10 unit SI.m
        val b:Number = 4
        val c = (a%b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testModulo3() {
        val a:Number = 10 unit SI.m
        val b:Number = 4 unit SI.m
        val c = (a%b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testModulo4() {
        val a:Number = 10 unit SI.m
        val b:Number = 4 unit SI.cm
        val c = (a%b).eval
        assertEquals(Number(0),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testModulo5() {
        val a:Number = 1000 unit SI.cm
        val b:Number = 0.04 unit SI.m
        val c = (a%b).eval
        assertEquals(Number(0),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testPower1() {
        val a:Number = 4 unit SI.cm
        val b:Number = 3
        val c = (a^b).eval
        assertEquals(Number(64),c);
        assertEquals(SI.cm3,c.unit)
    }
    
    @Test def testPower2() {
        val a:Number = 4 unit SI.cm3
        val b:Number = -3
        val c = (a^b).eval
        assertEquals(Number(1/64d),c);
        assertEquals("1/cm9",c.unit.toNameString)
    }
    
    @Test def testPower3() {
        val a:Number = 4 unit SI.m
        val b:Number = 2
        val c = (a^b).eval
        assertEquals(Number(16),c);
        assertEquals("m2",c.unit.toNameString)
    }
    
    @Test def testPower4() {
        val a:Number = 4 unit SI.m2
        val b:Number = 1/2d
        val c = (a^b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testPower5() {
        val a:Number = 8 unit SI.cm3
        val b:Number = 1/3d
        val c = (a^b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testPower6() {
        val a:Number = 8 unit SI.cm3
        val b:Number = 2 unit SI.g
        val c = (a^b).eval
        assertEquals(Number(64),c);
        assertEquals(SI.cm6,c.unit)
    }
    
    @Test def testPower7() {
        val a:Number = 8 unit SI.cm3
        val b:Number = 2.2 unit SI.g
        val c = (a^b).eval
        assertEquals(Number(97.005860257),c);
        assertEquals("cm6.6",c.unit.toNameString)
    }
    
    @Test def testRoot1() {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2
        val c = root(a,b).eval
        assertEquals(Number(4),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testRoot2() {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2.1
        val c = root(a,b).eval
        assertEquals(Number(3.74447097),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testMin1() {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2 unit SI.cm2
        val c = min(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMin2() {
        val a:Number = 2 unit SI.m2
        val b:Number = 201 unit SI.cm2
        val c = min(a,b).eval
        assertEquals(Number(201),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMin3() {
        val a:Number = 2 unit SI.cm2
        val b:Number = 201 unit SI.mm2
        val c = min(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMin4() {
        val a:Number = 2 unit SI.cm2
        val b:Number = 3 unit SI.g
        val c = min(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMax1() {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2 unit SI.cm2
        val c = max(a,b).eval
        assertEquals(Number(16),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMax2() {
        val a:Number = 2 unit SI.m2
        val b:Number = 201 unit SI.cm2
        val c = max(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m2,c.unit)
    }
    
    @Test def testMax3() {
        val a:Number = 2 unit SI.cm2
        val b:Number = 201 unit SI.mm2
        val c = max(a,b).eval
        assertEquals(Number(201),c);
        assertEquals(SI.mm2,c.unit)
    }
    
    @Test def testMax4() {
        val a:Number = 2 unit SI.cm2
        val b:Number = 3 unit SI.g
        val c = max(a,b).eval
        assertEquals(Number(3),c);
        assertEquals(SI.g,c.unit)
    }
    
    @Test def testHypot1() {
        val a:Number = 3 unit SI.m
        val b:Number = 400 unit SI.cm
        val c = hypot(a,b).eval
        assertEquals(Number(5),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testHypot2() {
        val a:Number = 3 unit SI.m
        val b:Number = 400 unit SI.cm
        val c = hypot(b,a).eval
        assertEquals(Number(500),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testNegation1() {
        val a:Number = 3 unit SI.m
        val b = -a
        assertEquals(Number(-3),b)
        assertEquals(SI.m,b.unit)
    }
    
    @Test def testSqrt1() {
        val a:Number = 25 unit SI.m2
        val b = sqrt(a).eval
        assertEquals(Number(5),b)
        assertEquals(SI.m,b.unit)
    }
    
    @Test def testSqrt2() {
        val a:Number = 16 unit SI.m4
        val b = sqrt(a).eval
        assertEquals(Number(4),b)
        assertEquals(SI.m2,b.unit)
    }
    
    @Test def testCbrt1() {
        val a:Number = 27 unit SI.m3
        val b = cbrt(a).eval
        assertEquals(Number(3),b)
        assertEquals(SI.m,b.unit)
    }
    
    @Test def testCbrt2() {
        val a:Number = 8 unit SI.m6
        val b = cbrt(a).eval
        assertEquals(Number(2),b)
        assertEquals(SI.m2,b.unit)
    }
    
    @Test def testAbs1() {
        val a:Number = -8.45 unit SI.m6
        val b = abs(a).eval
        assertEquals(Number(8.45),b)
        assertEquals(SI.m6,b.unit)
    }
    
    @Test def testExp1() {
        val a:Number = 8.45
        val b = exp(a).eval
        assertEquals(Number(4675.072735512),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testLn1() {
        val a:Number = 8.45
        val b = ln(a).eval
        assertEquals(Number(2.134166441),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testLog1() {
        val a:Number = 8.45
        val b = log(a).eval
        assertEquals(Number(0.926856709),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin1() {
        val a:Number = 30
        val b = sin(a).eval
        assertEquals(Number(0.5),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin1a() {
        val a:Number = 30.rad unit SI.rad
        val b = sin(a).eval
        assertEquals(Number(0.5),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin2() {
        val a:Number = 45
        val b = sin(a).eval
        assertEquals(Number(0.707106781),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin2a() {
        val a:Number = 45.rad unit SI.rad
        val b = sin(a).eval
        assertEquals(Number(0.707106781),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin3() {
        val a:Number = 90
        val b = sin(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin3a() {
        val a:Number = 90.rad unit SI.rad
        val b = sin(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testCos1() {
        val a:Number = 60
        val b = cos(a).eval
        assertEquals(Number(0.5),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testTan1() {
        val a:Number = 45
        val b = tan(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testCot1() {
        val a:Number = 45
        val b = tan(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
	
}