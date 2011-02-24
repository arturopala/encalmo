package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class UnitOfValueUnitTest extends AssertionsForJUnit {
    
    @Test def test1 = {
        
        assertEquals(1,SI.m2.multiplier,0)
        assertEquals(0.0001,SI.cm2.multiplier,0)
        assertEquals(0.000000001,SI.mm3.multiplier,0)
        assertEquals(10,10*SI.m2.multiplier,0)
        
    }

}