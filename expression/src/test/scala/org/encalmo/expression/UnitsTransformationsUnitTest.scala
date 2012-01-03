package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import java.io.{StringWriter,PrintWriter}

class UnitsTransformationsUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testSimplify1() {
		val a:UnitOfValue = SI("m4/m").get
		val as:Expression = UnitOfValue.simplify(a)
		assertEquals(SI.m3,as)
	}
	
	@Test def testSimplify2() {
        val b:UnitOfValue = SI("m4/m").get
        val a = b*b
        val as:Expression = UnitOfValue.simplify(a)
        assertEquals(SI.m6,as)
    }
    
    @Test def testSimplify3() {
        val a:UnitOfValue = SI.m4/SI.cm
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("100*m3",as.toNameString)
    }
    
    @Test def testSimplify4() {
        val a:UnitOfValue = SI.cm4/SI.mm
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m3/100000",as.toNameString)
    }
    
    @Test def testSimplify5() {
        val a:UnitOfValue = SI.m/SI.cm4
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("100000000/m3",as.toNameString)
    }
    
    @Test def testSimplify6() {
        val a:UnitOfValue = SI.cm/SI.cm4
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("1/cm3",as.toNameString)
    }
    
    @Test def testSimplify7() {
        val a:UnitOfValue = (SI.N*SI.m)/SI.m2
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify8() {
        val a:UnitOfValue = (SI.m*SI.N)/SI.m2
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify9() {
        val a:UnitOfValue = SI.m2/(SI.m*SI.N)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m/N",as.toNameString)
    }
    
    @Test def testSimplify10() {
        val a:UnitOfValue = SI.m2/(SI.N*SI.m)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m/N",as.toNameString)
    }
    
    @Test def testSimplify11() {
        val a:UnitOfValue = (SI.m*SI.N)/(SI.m2*SI.N)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("1/m",as.toNameString)
    }
    
    @Test def testSimplify12() {
        val a:UnitOfValue = (SI.N*SI.m)/(SI.m2*SI.N)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("1/m",as.toNameString)
    }
    
    @Test def testSimplify13() {
        val a:UnitOfValue = (SI.N*SI.m)/(SI.N*SI.m2)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("1/m",as.toNameString)
    }
    
    @Test def testSimplify14() {
        val a:UnitOfValue = (SI.m2/SI.N)*(SI.N/SI.m)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m",as.toNameString)
    }
    
    @Test def testSimplify15() {
        val a:UnitOfValue = (SI.N/SI.m)*(SI.m2/SI.N)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m",as.toNameString)
    }
    
    @Test def testSimplify16() {
        val a:UnitOfValue = (SI.N/SI.m)*(SI.N/SI.m2)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("N2/m3",as.toNameString)
    }
    
    @Test def testSimplify17() {
        val a:UnitOfValue = (SI.m/SI.N)*(SI.m2/SI.N)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m3/N2",as.toNameString)
    }
    
    @Test def testSimplify18() {
        val a:UnitOfValue = (SI.N/SI.m)*SI.m2
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m*N",as.toNameString)
    }
    
    @Test def testSimplify19() {
        val a:UnitOfValue = (SI.N/SI.m2)*SI.m
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify20() {
        val a:UnitOfValue = SI.m*(SI.N/SI.m2)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("N/m",as.toNameString)
    }
    
    @Test def testSimplify21() {
        val a:UnitOfValue = (SI.m2/SI.N)*(SI.m/SI.N)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("m3/N2",as.toNameString)
    }
    
    @Test def testSimplify22() {
        val a:UnitOfValue = (SI.cm2/SI.kN)*(SI.cm/SI.kN)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("cm3/kN2",as.toNameString)
    }
    
    @Test def testSimplify23() {
        val a:UnitOfValue = (SI.m2/SI.N)*(SI.cm/SI.kN)
        val as:UnitOfValue = UnitOfValue.simplify(a)
        assertEquals("cm3/kN2",as.toNameString)
    }

}