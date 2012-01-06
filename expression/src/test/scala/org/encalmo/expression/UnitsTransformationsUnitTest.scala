package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import java.io.{StringWriter,PrintWriter}

class UnitsTransformationsUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testSimplify0() {
        val a:UnitOfValue = SI.m/SI.m
        val as:Expression = UnitOfValue.simplifyUnit(a)
        assertEquals("",as.unit.toNameString)
    }
	
	@Test def testSimplify1() {
		val a:UnitOfValue = SI("m4/m").get
		val as:Expression = UnitOfValue.simplifyUnit(a)
		assertEquals(SI.m3,as)
	}
	
	@Test def testSimplify2() {
        val b:UnitOfValue = SI("m4/m").get
        val a = b*b
        val as:Expression = UnitOfValue.simplifyUnit(a)
        assertEquals(SI.m6,as)
    }
    
    @Test def testSimplify3() {
        val a:UnitOfValue = SI.m4/SI.cm
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("100*m3",as.toNameString)
    }
    
    @Test def testSimplify4() {
        val a:UnitOfValue = SI.cm4/SI.mm
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m3/100000",as.toNameString)
    }
    
    @Test def testSimplify5() {
        val a:UnitOfValue = SI.m/SI.cm4
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("100000000/m3",as.toNameString)
    }
    
    @Test def testSimplify6() {
        val a:UnitOfValue = SI.cm/SI.cm4
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("1/cm3",as.toNameString)
    }
    
    @Test def testSimplify7() {
        val a:UnitOfValue = (SI.N*SI.m)/SI.m2
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify8() {
        val a:UnitOfValue = (SI.m*SI.N)/SI.m2
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify9() {
        val a:UnitOfValue = SI.m2/(SI.m*SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m/N",as.toNameString)
    }
    
    @Test def testSimplify10() {
        val a:UnitOfValue = SI.m2/(SI.N*SI.m)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m/N",as.toNameString)
    }
    
    @Test def testSimplify11() {
        val a:UnitOfValue = (SI.m*SI.N)/(SI.m2*SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("1/m",as.toNameString)
    }
    
    @Test def testSimplify12() {
        val a:UnitOfValue = (SI.N*SI.m)/(SI.m2*SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("1/m",as.toNameString)
    }
    
    @Test def testSimplify13() {
        val a:UnitOfValue = (SI.N*SI.m)/(SI.N*SI.m2)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("1/m",as.toNameString)
    }
    
    @Test def testSimplify14() {
        val a:UnitOfValue = (SI.m2/SI.N)*(SI.N/SI.m)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m",as.toNameString)
    }
    
    @Test def testSimplify15() {
        val a:UnitOfValue = (SI.N/SI.m)*(SI.m2/SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m",as.toNameString)
    }
    
    @Test def testSimplify16() {
        val a:UnitOfValue = (SI.N/SI.m)*(SI.N/SI.m2)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("N2/m3",as.toNameString)
    }
    
    @Test def testSimplify17() {
        val a:UnitOfValue = (SI.m/SI.N)*(SI.m2/SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m3/N2",as.toNameString)
    }
    
    @Test def testSimplify18() {
        val a:UnitOfValue = (SI.N/SI.m)*SI.m2
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m*N",as.toNameString)
    }
    
    @Test def testSimplify19() {
        val a:UnitOfValue = (SI.N/SI.m2)*SI.m
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify20() {
        val a:UnitOfValue = SI.m*(SI.N/SI.m2)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify21() {
        val a:UnitOfValue = (SI.m2/SI.N)*(SI.m/SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m3/N2",as.toNameString)
    }
    
    @Test def testSimplify22() {
        val a:UnitOfValue = (SI.cm2/SI.kN)*(SI.cm/SI.kN)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("cm3/kN2",as.toNameString)
    }
    
    @Test def testSimplify23() {
        val a:UnitOfValue = (SI.m2/SI.N)*(SI.cm/SI.kN)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m3/(100000*N2)",as.toNameString)
    }
    
    @Test def testSimplify24() {
        val a:UnitOfValue = (SI.m2/(SI.N*SI.m))*(SI.m/SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m2/N2",as.toNameString)
    }
    
    @Test def testSimplify25() {
        val a:UnitOfValue = (SI.m/SI.N)*(SI.m/SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m2/N2",as.toNameString)
    }
    
    @Test def testSimplify26() {
        val a:UnitOfValue = (SI.m/SI.kg)*(SI.m/SI.N)
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("m2/(kg*N)",as.toNameString)
    }
    
    @Test def testSimplify27() {
        val a:UnitOfValue = (SI.N/SI.m)/SI.m
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("N/m2",as.toNameString)
    }
    
    @Test def testSimplify28() {
        val a:UnitOfValue = (SI.N/SI.m2)*SI.m
        val as:UnitOfValue = UnitOfValue.simplifyUnit(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testExpand0() {
        val a:UnitOfValue = SI.N/SI.m
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("g/s2",as._1.toNameString)
        assertEquals("1000.0",as._2.toString())
    }
    
    @Test def testExpand1() {
        val a:UnitOfValue = SI.Pa/SI.m
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("g/(m2*s2)",as._1.toNameString)
        assertEquals("1000.0",as._2.toString())
    }
    
    @Test def testExpand3() {
        val a:UnitOfValue = (SI.Pa*SI.s)/SI.m
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("g/(m2*s)",as._1.toNameString)
        assertEquals("1000.0",as._2.toString())
    }
    
    @Test def testExpand3a() {
        val a:UnitOfValue = SI.Pa/SI.N
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("1/m2",as._1.toNameString)
        assertEquals("1.0",as._2.toString())
    }
    
    @Test def testExpand4a() {
        val a:UnitOfValue = (SI.Pa*SI.s)/SI.N
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("s/m2",as._1.toNameString)
        assertEquals("1.0",as._2.toString())
    }
    
    @Test def testExpand4b() {
        val a:UnitOfValue = (SI.Pa*SI.s)/(SI.m*SI.N)
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("s/m3",as._1.toNameString)
        assertEquals("1.0",as._2.toString())
    }
    
    @Test def testExpand4c() {
        val a:UnitOfValue = (SI.Pa*SI.ms)/(SI.cm*SI.kN)
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("s/m3",as._1.toNameString)
        assertEquals("1.0E-4",as._2.toString())
    }
    
    @Test def testExpand4d() {
        val a:UnitOfValue = (SI.Pa*SI.ms)/(SI.km*(SI.kN*SI.GPa))
        val as:(UnitOfValue,Double) = UnitOfValue.expandUnit(a)
        assertEquals("s3/(g*m2)",as._1.toNameString)
        assertEquals("1.0000000000000001E-21",as._2.toString())
    }
    
    @Test def testExpand5() {
        val a:UnitOfValue = SI.Pa
        val b:UnitOfValue = SI.N/SI.m2
        assertTrue(a.isSameExpandedUnitAndMultiplier(b))
        assertTrue(b.isSameExpandedUnitAndMultiplier(a))
    }
    
    @Test def testExpand6() {
        val a:UnitOfValue = SI.m3
        val b:UnitOfValue = SI.m4/SI.m
        assertTrue(a.isSameExpandedUnitAndMultiplier(b))
        assertTrue(b.isSameExpandedUnitAndMultiplier(a))
    }
    
    @Test def testExpand7() {
        val a:UnitOfValue = SI.GPa
        val b:UnitOfValue = SI.N/SI.m2
        assertTrue(a.isSameExpandedUnit(b))
        assertTrue(b.isSameExpandedUnit(a))
        assertEquals(Number(1E9),Number(1,a).convertTo(b))
        assertEquals(Number(1E-9),Number(1,b).convertTo(a))
    }
    
    @Test def testExpand8() {
        val a:UnitOfValue = SI.m2*SI.Pa
        val b:UnitOfValue = SI.N
        assertTrue(a.isSameExpandedUnit(b))
        assertTrue(b.isSameExpandedUnit(a))
        assertEquals(Number(1),Number(1,a).convertTo(b))
        assertEquals(Number(1),Number(1,b).convertTo(a))
    }
    
    @Test def testExpand9() {
        val a:UnitOfValue = SI.N/SI.m
        val b:UnitOfValue = SI.Pa*SI.m
        val ea = a.expandedUnit
        assertTrue(a.isSameExpandedUnit(b))
        assertTrue(b.isSameExpandedUnit(a))
        assertEquals(Number(1),Number(1,a).convertTo(b))
        assertEquals(Number(1),Number(1,b).convertTo(a))
    }

}