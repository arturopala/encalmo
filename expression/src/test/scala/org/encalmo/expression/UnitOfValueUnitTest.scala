package org.encalmo.expression

import org.junit.Assert.assertEquals
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class UnitOfValueUnitTest extends AssertionsForJUnit {
    
    @Test def testMultiplier {
        assertEquals(1,SI.m2.multiplier,0)
        assertEquals(0.0001,SI.cm2.multiplier,0)
        assertEquals(0.000000001,SI.mm3.multiplier,0)
        assertEquals(10,10*SI.m2.multiplier,0) 
    }
    
    @Test def testNameBuilder1 {
    	val nb1 = new UnitOfValueNameBuilder()
    	Prod(SI.kg,SI.m).travel(traveler=nb1)
    	assertEquals("kg*m", nb1.toResult)
    } 
    
    @Test def testNameBuilder2 {
        val nb1 = new UnitOfValueNameBuilder()
        Quot(Prod(SI.kg,SI.m),SI.N).travel(traveler=nb1)
        assertEquals("kg*m/N", nb1.toResult)
    }
    
    @Test def testNameBuilder2a {
        val nb1 = new UnitOfValueNameBuilder()
        Quot(Prod(SI.m,SI.kg),SI.N).travel(traveler=nb1)
        assertEquals("kg*m/N", nb1.toResult)
    }
    
    @Test def testNameBuilder3 {
        val nb1 = new UnitOfValueNameBuilder()
        Quot(Prod(SI.kg,SI.m),Prod(SI.N,SI.g)).travel(traveler=nb1)
        assertEquals("kg*m/(g*N)", nb1.toResult)
    } 
    
    @Test def testName1 {
        val u = SI.m*SI.kg
        assertEquals("kg*m", u.toNameString)
    } 
    
    @Test def testName1a {
        val u = (SI.m*SI.kg) dim 2
        assertEquals("[kg*m]2", u.toNameString)
    } 
    
    @Test def testName1b {
        val u = SI.m*SI.kg
        val u2 = u*u
        assertEquals("[kg*m]2", u2.toNameString)
    } 
    
    @Test def testName1c {
        val u = (SI.m*SI.kg)
        val u2 = SI.m*u
        assertEquals("kg*m*m", u2.toNameString)
    }  
    
    @Test def testName1d {
        val u = (EmptyUnitOfValue/SI.kg)
        assertEquals("1/kg", u.toNameString)
    } 
    
    @Test def testName2 {
        val u = SI.m*SI.kg/SI.N
        assertEquals("kg*m/N", u.toNameString)
    }
    
    @Test def testName3 {
        val u = SI.m*SI.kg/(SI.N*SI.Pa)
        assertEquals("kg*m/(N*Pa)", u.toNameString)
    }
    
    @Test def testName4 {
        val u = ((SI.m*SI.kg) dim 3) / ((SI.N*SI.Pa) dim 2)
        assertEquals("[kg*m]3/[N*Pa]2", u.toNameString)
    }
    
    @Test def testName5 {
        val u1 = SI.g*SI.cm
        val u = ((SI.m*u1) dim 3) / ((SI.N*u1) dim 2)
        assertEquals("[cm*g*m]3/[cm*g*N]2", u.toNameString)
    }

    @Test def testConvertUnits {
    	assertEquals(Real(5000.0), Real(5).convert(SI.km,SI.m))
    	assertEquals(Real(12.39), Real(12390000).convert(SI.m2,SI.km2))
    }
    
    @Test def testMultiplyUnits {
    	assertEquals("kg*m",(SI.m*SI.kg).toNameString)
    	assertEquals("m2",(SI.m*SI.m).toNameString)
    	assertEquals("m",(SI.m*EmptyUnitOfValue).toNameString)
    	assertEquals("m",(EmptyUnitOfValue*SI.m).toNameString)
    }
    
    @Test def testDivideUnits {
    	assertEquals("m/kg",(SI.m/SI.kg).toNameString)
    	assertEquals("",(SI.m/SI.m).toNameString)
    	assertEquals("m",(SI.m/EmptyUnitOfValue).toNameString)
    	assertEquals("1/m",(EmptyUnitOfValue/SI.m).toNameString)
    }
    
    @Test def testAddUnits {
    	assertEquals("!m+kg!",(SI.m+SI.kg).toNameString)
    	assertEquals("m",(SI.m+SI.m).toNameString)
    	assertEquals("m",(SI.m+EmptyUnitOfValue).toNameString)
    	assertEquals("m",(EmptyUnitOfValue+SI.m).toNameString)
    }
    
    @Test def testSubtractUnits {
    	assertEquals("!m-kg!",(SI.m-SI.kg).toNameString)
    	assertEquals("m",(SI.m-SI.m).toNameString)
    	assertEquals("m",(SI.m-EmptyUnitOfValue).toNameString)
    	assertEquals("m",(EmptyUnitOfValue-SI.m).toNameString)
    }
    
    @Test def testIllegalUnit {
    	assertEquals("!m+kg!",(SI.m+SI.kg).toNameString)
    	assertEquals("!m+kg!*N",((SI.m+SI.kg)*SI.N).toNameString)
    }
    
    @Test def testPercent{
        assertEquals(0.01,SI.percent.simplifiedUnit._2,0)
        assertEquals(0.001,SI.permille.simplifiedUnit._2,0)
        assertEquals(0.01,(SI.kg*SI.percent).simplifiedUnit._2,0)
    }
    

}