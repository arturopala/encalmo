package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class NumberUnitOfValueUnitTest extends AssertionsForJUnit {
    
    @Test def testEquals {
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
        assertFalse((1.2 unit SI.m) == (1.2 unit SI.m2))
    }
    
    @Test def testInequals {
        assertTrue(Number(5.123) > Number(3.211))
        assertTrue(Number(3.222) < Number(3.223))
        assertTrue((1000 unit SI.mm) < (1.2 unit SI.m))
        assertTrue((1000 unit SI.mm) <= (1.2 unit SI.m))
        assertTrue((0.019 unit SI.m) < Number(0.2))
        assertTrue((0.019 unit SI.m) <= Number(0.2))
        val a:Number = 12000 unit SI.N/SI.m
        val b:Number = 1.2 unit SI.m*SI.MPa
        assertTrue(a < b)
        assertTrue(b > a)
    }
    
    @Test def testInequals2 {
        val c = ((1930000 unit SI.Nm)/(1.247 unit SI.m3*SI.MPa)).eval
        assertTrue(c<2.5)
    }
	
	@Test def testSum1 {
		val a:Number = 1.2
		val b:Number = 2.1
		val c = (a+b).eval
		assertEquals(Number(3.3),c);
		assertEquals(EmptyUnitOfValue,c.unit)
	}
	
	@Test def testSum2 {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.cm
        val c = (a+b).eval
        assertEquals(Number(1.221),c);
        assertEquals(SI.m,c.unit)
        val d = (b+a).eval
        assertEquals(Number(1.221),d);
        assertEquals(SI.m,d.unit)
    }
	
	@Test def testSum3 {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.km
        val c = (a+b).eval
        assertEquals(Number(2.1012),c);
        assertEquals(SI.km,c.unit)
        val d = (b+a).eval
        assertEquals(Number(2.1012),d);
        assertEquals(SI.km,d.unit)
    }
	
	@Test def testSum4 {
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
	
	@Test def testSum5 {
        SI.units.foreach( u => {
            val a:Number = 1.2 unit u
            val b:Number = 2.1 unit u
            val c = (a+b).eval
            assertEquals(Number(3.3),c.eval);
            assertEquals(u,c.unit)
        })
    }
    
    @Test def testSum6 {
        val a:Number = 1.2 unit SI.N
        val b:Number = 2.1 unit SI.Pa*SI.m2
        val c = (a+b).eval
        assertEquals(Number(3.3),c.eval);
        assertEquals(SI.N,c.unit)
        val d = (b+a).eval
        assertEquals(Number(3.3),d.eval);
        assertEquals(SI.N,d.unit)
    }
    
    @Test def testSum7 {
        val a:Number = 1.2 unit SI.MN
        val b:Number = 2.1 unit SI.Pa*SI.m2
        val c = (a+b).eval
        assertEquals(Number(1.2000021),c.eval);
        assertEquals(SI.MN,c.unit)
        val d = (b+a).eval
        assertEquals(Number(1.2000021),d.eval);
        assertEquals(SI.MN,d.unit)
    }
	
	@Test def testSubtract1 {
        val a:Number = 1.2
        val b:Number = 2.1
        val c = (a-b).eval
        assertEquals(Number(-0.9),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testSubtract2 {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.cm
        val c = (a-b).eval
        assertEquals(Number(1.179),c);
        assertEquals(SI.m,c.unit)
        val d = (b-a).eval
        assertEquals(Number(-1.179),d);
        assertEquals(SI.m,d.unit)
    }
    
    @Test def testSubtract3 {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.km
        val c = (a-b).eval
        assertEquals(Number(-2.0988),c);
        assertEquals(SI.km,c.unit)
        val d = (b-a).eval
        assertEquals(Number(2.0988),d);
        assertEquals(SI.km,d.unit)
    }
    
    @Test def testSubtract4 {
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
    
    @Test def testSubtract5 {
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
    
    @Test def testSubtract6 {
        val a:Number = 1.2 unit SI.N
        val b:Number = 2.1 unit SI.Pa*SI.m2
        val c = (a-b).eval
        assertEquals(Number(-0.9),c.eval);
        assertEquals(SI.N,c.unit)
        val d = (b-a).eval
        assertEquals(Number(0.9),d.eval);
        assertEquals(SI.N,d.unit)
    }
	
	@Test def testMultiply1 {
        val a:Number = 1.2
        val b:Number = 2.1
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
	
	@Test def testMultiply2 {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1 unit SI.m
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.m2,c.unit)
    }
    
    @Test def testMultiply1a {
        val a:Number = 1.2 unit SI.m
        val b:Number = 2.1
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testMultiply2a {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.cm
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMultiply3 {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.m
        val c = (a*b).eval
        assertEquals(Number(0.0252),c);
        assertEquals(SI.m2,c.unit)
        val d = (b*a).eval
        assertEquals(Number(0.0252),d);
        assertEquals(SI.m2,d.unit)
    }
    
    @Test def testMultiply4 {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.g
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals("cm*g",c.unit.toNameString)
        val d = (b*a).eval
        assertEquals(Number(2.52),d);
        assertEquals("cm*g",d.unit.toNameString)
    }
    
    @Test def testMultiply5 {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.cm2
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals("cm3",c.unit.toNameString)
        val d = (b*a).eval
        assertEquals(Number(2.52),d);
        assertEquals("cm3",d.unit.toNameString)
    }
    
    @Test def testMultiply6 {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.m2
        val c = (a*b).eval
        assertEquals(Number(0.0252),c);
        assertEquals("m3",c.unit.toNameString)
        val d = (b*a).eval
        assertEquals(Number(0.0252),d);
        assertEquals("m3",d.unit.toNameString)
    }
    
    @Test def testMultiply7 {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.km2
        val c = (a*b).eval
        assertEquals(Number(0.0000252),c);
        assertEquals("km3",c.unit.toNameString)
        val d = (b*a).eval
        assertEquals(Number(0.0000252),d);
        assertEquals("km3",d.unit.toNameString)
    }
    
    @Test def testMultiply8 {
        val a:Number = 1.2 unit SI.N/SI.m2
        val b:Number = 2.1 unit SI.Pa
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.Pa*SI.Pa,c.unit)
        val d = (b*a).eval
        assertEquals(Number(2.52),d);
        assertEquals(SI.Pa*SI.Pa,d.unit)
    }
    
    @Test def testMultiply9 {
        val a:Number = 1.2 unit SI.N/SI.m2
        val b:Number = 2.1 unit SI.kPa
        val c = (a*b).eval
        assertEquals(Number(0.00252),c);
        assertEquals(SI.kPa*SI.kPa,c.unit)
        val d = (b*a).eval
        assertEquals(Number(0.00252),d);
        assertEquals(SI.kPa*SI.kPa,d.unit)
    }
    
    @Test def testMultiply10 {
        val a:Number = 1.2 unit SI.N/SI.mm2
        val b:Number = 2.1 unit SI.MPa
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.MPa*SI.MPa,c.unit)
        val d = (b*a).eval
        assertEquals(Number(2.52),d);
        assertEquals(SI.MPa*SI.MPa,d.unit)
    }
    
    @Test def testMultiply11 {
        val a:Number = 1.2 unit SI.N/SI.m2
        val b:Number = 2.1 unit SI.m
        val c = (a*b).eval
        assertEquals(Number(2.52),c);
        assertEquals(SI.N/SI.m,c.unit)
        val d = (b*a).eval
        assertEquals(Number(2.52),d);
        assertEquals(SI.N/SI.m,d.unit)
    }
    
    @Test def testDivide1 {
        val a:Number = 1.2
        val b:Number = 2.1
        val c = (a/b).eval
        assertEquals(Number(0.571428571),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide2 {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.cm
        val c = (a/b).eval
        assertEquals(Number(0.571428571),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide2a {
        val a:Number = 1.2 unit SI.cm
        val b:Number = 2.1 unit SI.cm2
        val c = (a/b).eval
        assertEquals(Number(0.571428571),c);
        assertEquals("1/cm",c.unit.toNameString)
    }
    
    @Test def testDivide3 {
        val a:Number = 2 unit SI.m
        val b:Number = 5 unit SI.cm
        val c = (a/b).eval
        assertEquals(Number(40),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide4 {
        val a:Number = 2 unit SI.cm
        val b:Number = 5 unit SI.m
        val c = (a/b).eval
        assertEquals(Number(0.004),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testDivide5 {
        val a:Number = 2 unit SI.cm
        val b:Number = 5 unit SI.kg
        val c = (a/b).eval
        assertEquals(Number(0.4),c);
        assertEquals("cm/kg",c.unit.toNameString)
    }
    
    @Test def testDivide6 {
        val a:Number = 2 unit SI.cm
        val b:Number = 5 unit SI.m2
        val c = (a/b).eval
        assertEquals(Number(0.004),c);
        assertEquals("1/m",c.unit.toNameString)
        val d = (b/a).eval
        assertEquals(Number(250),d);
        assertEquals("m",d.unit.toNameString)
    }
    
    @Test def testDivide7 {
        val a:Number = 2 unit SI.cm2
        val b:Number = 5 unit SI.m
        val c = (a/b).eval
        assertEquals(Number(0.00004),c);
        assertEquals("m",c.unit.toNameString)
        val d = (b/a).eval
        assertEquals(Number(25000),d);
        assertEquals("1/m",d.unit.toNameString)
    }
    
    @Test def testDivide8 {
        val a:Number = 2 unit SI.Pa*SI.m2
        val b:Number = 5 unit SI.N
        val c = (a/b).eval
        assertEquals(Number(0.4),c);
        assertEquals(EmptyUnitOfValue,c.unit)
        val d = (b/a).eval
        assertEquals(Number(2.5),d);
        assertEquals(EmptyUnitOfValue,d.unit)
    }
    
    @Test def testModulo1 {
        val a:Number = 10
        val b:Number = 4
        val c = (a%b).eval
        assertEquals(Number(2),c);
        assertEquals(EmptyUnitOfValue,c.unit)
    }
    
    @Test def testModulo2 {
        val a:Number = 10 unit SI.m
        val b:Number = 4
        val c = (a%b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testModulo3 {
        val a:Number = 10 unit SI.m
        val b:Number = 4 unit SI.m
        val c = (a%b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testModulo4 {
        val a:Number = 10 unit SI.m
        val b:Number = 4 unit SI.cm
        val c = (a%b).eval
        assertEquals(Number(0),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testModulo5 {
        val a:Number = 1000 unit SI.cm
        val b:Number = 0.04 unit SI.m
        val c = (a%b).eval
        assertEquals(Number(0),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testPower1 {
        val a:Number = 4 unit SI.cm
        val b:Number = 3
        val c = (a^b).eval
        assertEquals(Number(64),c);
        assertEquals(SI.cm3,c.unit)
    }
    
    @Test def testPower2 {
        val a:Number = 4 unit SI.cm3
        val b:Number = -3
        val c = (a^b).eval
        assertEquals(Number(1/64d),c);
        assertEquals("1/cm9",c.unit.toNameString)
    }
    
    @Test def testPower3 {
        val a:Number = 4 unit SI.m
        val b:Number = 2
        val c = (a^b).eval
        assertEquals(Number(16),c);
        assertEquals("m2",c.unit.toNameString)
    }
    
    @Test def testPower4 {
        val a:Number = 4 unit SI.m2
        val b:Number = 1/2d
        val c = (a^b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testPower5 {
        val a:Number = 8 unit SI.cm3
        val b:Number = 1/3d
        val c = (a^b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testPower6 {
        val a:Number = 8 unit SI.cm3
        val b:Number = 2 unit SI.g
        val c = (a^b).eval
        assertEquals(Number(64),c);
        assertEquals(SI.cm6,c.unit)
    }
    
    @Test def testPower7 {
        val a:Number = 8 unit SI.cm3
        val b:Number = 2.2 unit SI.g
        val c = (a^b).eval
        assertEquals(Number(97.005860257),c);
        assertEquals("cm6.6",c.unit.toNameString)
    }
    
    @Test def testRoot1 {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2
        val c = root(a,b).eval
        assertEquals(Number(4),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testRoot2 {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2.1
        val c = root(a,b).eval
        assertEquals(Number(3.74447097),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testMin1 {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2 unit SI.cm2
        val c = min(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMin2 {
        val a:Number = 2 unit SI.m2
        val b:Number = 201 unit SI.cm2
        val c = min(a,b).eval
        assertEquals(Number(201),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMin3 {
        val a:Number = 2 unit SI.cm2
        val b:Number = 201 unit SI.mm2
        val c = min(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMin4 {
        val a:Number = 2 unit SI.cm2
        val b:Number = 3 unit SI.g
        val c = min(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMin5 {
        val a:Number = 2 unit SI.Pa
        val b:Number = 3 unit SI.N/SI.m2
        val c = min(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.Pa,c.unit)
    }
    
    @Test def testMax1 {
        val a:Number = 16 unit SI.cm2
        val b:Number = 2 unit SI.cm2
        val c = max(a,b).eval
        assertEquals(Number(16),c);
        assertEquals(SI.cm2,c.unit)
    }
    
    @Test def testMax2 {
        val a:Number = 2 unit SI.m2
        val b:Number = 201 unit SI.cm2
        val c = max(a,b).eval
        assertEquals(Number(2),c);
        assertEquals(SI.m2,c.unit)
    }
    
    @Test def testMax3 {
        val a:Number = 2 unit SI.cm2
        val b:Number = 201 unit SI.mm2
        val c = max(a,b).eval
        assertEquals(Number(201),c);
        assertEquals(SI.mm2,c.unit)
    }
    
    @Test def testMax4 {
        val a:Number = 2 unit SI.cm2
        val b:Number = 3 unit SI.g
        val c = max(a,b).eval
        assertEquals(Number(3),c);
        assertEquals(SI.g,c.unit)
    }
    
    @Test def testMax5 {
        val a:Number = 2 unit SI.Pa
        val b:Number = 3 unit SI.N/SI.m2
        val c = max(a,b).eval
        assertEquals(Number(3),c);
        assertEquals(SI.N/SI.m2,c.unit)
    }
    
    @Test def testHypot1 {
        val a:Number = 3 unit SI.m
        val b:Number = 400 unit SI.cm
        val c = hypot(a,b).eval
        assertEquals(Number(5),c);
        assertEquals(SI.m,c.unit)
    }
    
    @Test def testHypot2 {
        val a:Number = 3 unit SI.m
        val b:Number = 400 unit SI.cm
        val c = hypot(b,a).eval
        assertEquals(Number(500),c);
        assertEquals(SI.cm,c.unit)
    }
    
    @Test def testNegation1 {
        val a:Number = 3 unit SI.m
        val b = -a
        assertEquals(Number(-3),b)
        assertEquals(SI.m,b.unit)
    }
    
    @Test def testSqrt1 {
        val a:Number = 25 unit SI.m2
        val b = sqrt(a).eval
        assertEquals(Number(5),b)
        assertEquals(SI.m,b.unit)
    }
    
    @Test def testSqrt2 {
        val a:Number = 16 unit SI.m4
        val b = sqrt(a).eval
        assertEquals(Number(4),b)
        assertEquals(SI.m2,b.unit)
    }
    
    @Test def testCbrt1 {
        val a:Number = 27 unit SI.m3
        val b = cbrt(a).eval
        assertEquals(Number(3),b)
        assertEquals(SI.m,b.unit)
    }
    
    @Test def testCbrt2 {
        val a:Number = 8 unit SI.m6
        val b = cbrt(a).eval
        assertEquals(Number(2),b)
        assertEquals(SI.m2,b.unit)
    }
    
    @Test def testAbs1 {
        val a:Number = -8.45 unit SI.m6
        val b = abs(a).eval
        assertEquals(Number(8.45),b)
        assertEquals(SI.m6,b.unit)
    }
    
    @Test def testExp1 {
        val a:Number = 8.45
        val b = exp(a).eval
        assertEquals(Number(4675.072735512),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testLn1 {
        val a:Number = 8.45
        val b = ln(a).eval
        assertEquals(Number(2.134166441),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testLog1 {
        val a:Number = 8.45
        val b = log(a).eval
        assertEquals(Number(0.926856709),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin1 {
        val a:Number = 30
        val b = sin(a).eval
        assertEquals(Number(0.5),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin1a {
        val a:Number = 30.rad unit SI.rad
        val b = sin(a).eval
        assertEquals(Number(0.5),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin2 {
        val a:Number = 45
        val b = sin(a).eval
        assertEquals(Number(0.707106781),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin2a {
        val a:Number = 45.rad unit SI.rad
        val b = sin(a).eval
        assertEquals(Number(0.707106781),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin3 {
        val a:Number = 90
        val b = sin(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testSin3a {
        val a:Number = 90.rad unit SI.rad
        val b = sin(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testCos1 {
        val a:Number = 60
        val b = cos(a).eval
        assertEquals(Number(0.5),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testTan1 {
        val a:Number = 45
        val b = tan(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testCot1 {
        val a:Number = 45
        val b = tan(a).eval
        assertEquals(Number(1),b)
        assertEquals(EmptyUnitOfValue,b.unit)
    }
    
    @Test def testConvertTo {
        val a:Number = 45 unit SI.cm
        val b = a.convertTo(SI.km)
        assertEquals(Number(0.00045),b)
        assertEquals(SI.km,b.unit)
        val c = a.convertTo(SI.mm)
        assertEquals(Number(450),c)
        assertEquals(SI.mm,c.unit)
        val d = a.convertTo(SI.m,Some(0.5))
        assertEquals(Number(0.5),d)
        assertEquals(SI.m,d.unit)
        val e = a.convertTo(SI.km,Some(0.2))
        assertEquals(ZERO,e)
        assertEquals(SI.km,e.unit)
    }
	
}