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
    
    @Test def testNameBuilder {
    	val nb1 = new UnitOfValueNameBuilder()
    	Prod(SI.kg,SI.m).travel(traveler=nb1)
    	assertEquals("kg*m", nb1.toResult)
    	val nb2 = new UnitOfValueNameBuilder()
    	Quot(Prod(SI.kg,SI.m),SI.N).travel(traveler=nb2)
    	assertEquals("kg*m/N", nb2.toResult)
    }
    
    @Test def testConvertUnits {
    	assertEquals(Real(5000.0), Real(5).convert(SI.km,SI.m))
    	assertEquals(Real(12.39), Real(12390000).convert(SI.m2,SI.km2))
    }
    
    @Test def testMultiplyUnits {
    	assertEquals("m*kg",(SI.m*SI.kg).toNameString)
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
    

}