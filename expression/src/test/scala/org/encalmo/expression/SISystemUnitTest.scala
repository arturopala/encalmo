package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

class SISystemUnitTest extends AssertionsForJUnit {
    
    @Test def testIsLargerThen {
        assertTrue(SI.m isLargerThan SI.cm)
        assertTrue(SI.km isLargerThan SI.m)
        assertTrue(SI.cm isLargerThan SI.mm)
        assertTrue(SI.kg isLargerThan SI.g)
        assertFalse(SI.m isLargerThan SI.km)
        assertFalse(SI.cm isLargerThan SI.m)
        assertFalse(SI.mm isLargerThan SI.cm)
        assertFalse(SI.g isLargerThan SI.kg)
    }
    
    @Test def testIsEqual {
        assertTrue(SI.m == SI.m)
        assertTrue(SI.km == SI.km)
        assertTrue(SI.cm == SI.cm)
        assertTrue(SI.m != SI.km)
        assertTrue(SI.km != SI.cm)
        assertTrue(SI.cm != SI.mm)
    }
    
    @Test def testToStringName {
        assertEquals(SI.m.toNameString,"m") 
        assertEquals(SI.km.toNameString,"km") 
        assertEquals(SI.mm.toNameString,"mm") 
        assertEquals(SI.cm.toNameString,"cm") 
        assertEquals(SI.m2.toNameString,"m2") 
        assertEquals(SI.km2.toNameString,"km2")
        assertEquals(SI.cm2.toNameString,"cm2") 
        assertEquals(SI.m3.toNameString,"m3") 
        assertEquals(SI.cm3.toNameString,"cm3") 
        assertEquals(SI.mm3.toNameString,"mm3") 
        assertEquals(SI.N.toNameString,"N") 
        assertEquals(SI.kN.toNameString,"kN") 
        assertEquals(SI.MN.toNameString,"MN") 
        assertEquals(SI.Pa.toNameString,"Pa") 
        assertEquals(SI.kPa.toNameString,"kPa") 
        assertEquals(SI.MPa.toNameString,"MPa") 
        assertEquals(SI.GPa.toNameString,"GPa") 
        assertEquals(SI.g.toNameString,"g") 
        assertEquals(SI.kg.toNameString,"kg") 
    }
    
    @Test def testApplyString {
        assertTrue(SI("m") == Some(SI.m))
        assertTrue(SI("m2") == Some(SI.m2))
        assertTrue(SI("m3") == Some(SI.m3))
        assertTrue(SI("m4") == Some(SI.m4))
        assertTrue(SI("m6") == Some(SI.m6))
        assertTrue(SI("m8") == Some(SI.m8))
        assertTrue(SI("km") == Some(SI.km))
        assertTrue(SI("cm") == Some(SI.cm))
        assertTrue(SI("mm") == Some(SI.mm))
        assertTrue(SI("km") == Some(SI.km))
        assertTrue(SI("km2") == Some(SI.km2))
        assertTrue(SI("N") == Some(SI.N))
        assertTrue(SI("kN") == Some(SI.kN))
        assertTrue(SI("kPa") == Some(SI.kPa))
        assertTrue(SI("g") == Some(SI.g))
        assertTrue(SI("kg") == Some(SI.kg))
    }

}