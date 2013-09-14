package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

class SISystemUnitTest extends AssertionsForJUnit {
    
    @Test def testBaseUnit {
        assertEquals(SI.N, SI.kN.baseUnit)
        assertEquals(SI.m, SI.mm.baseUnit)
        assertEquals(SI.m2, SI.mm2.baseUnit)
        assertEquals(SI.m3, SI.cm3.baseUnit)
        assertEquals(SI.N*SI.N, (SI.kN*SI.kN).baseUnit)
        assertEquals(SI.N*SI.N*SI.N, (SI.kN*SI.kN*SI.kN).baseUnit)
    }
    
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
        assertEquals(SI.m.face,"m")
        assertEquals(SI.km.face,"km")
        assertEquals(SI.mm.face,"mm")
        assertEquals(SI.cm.face,"cm")
        assertEquals(SI.m2.face,"m2")
        assertEquals(SI.km2.face,"km2")
        assertEquals(SI.cm2.face,"cm2")
        assertEquals(SI.m3.face,"m3")
        assertEquals(SI.cm3.face,"cm3")
        assertEquals(SI.mm3.face,"mm3")
        assertEquals(SI.N.face,"N")
        assertEquals(SI.kN.face,"kN")
        assertEquals(SI.MN.face,"MN")
        assertEquals(SI.Pa.face,"Pa")
        assertEquals(SI.kPa.face,"kPa")
        assertEquals(SI.MPa.face,"MPa")
        assertEquals(SI.GPa.face,"GPa")
        assertEquals(SI.g.face,"g")
        assertEquals(SI.kg.face,"kg")
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
    
    @Test def testFind {
        SI.units.foreach(u => {
            assertEquals(u, SI.find(u.name.baseName,u.scale,u.dimension).get)
        })
    }
    
    @Test def testDimension {
        assertEquals(SI.m2, SI.m dim 2)
        assertEquals(SI.cm2, SI.cm dim 2)
        assertEquals(SI.m3, SI.m dim 3)
        assertEquals(SI.cm3, SI.cm dim 3)
    }
    
    @Test def testScale {
        assertEquals(SI.cm, SI.m exp -2)
        assertEquals(SI.m, SI.cm exp 2)
        assertEquals(SI.mm, SI.m exp -3)
        assertEquals(SI.m, SI.mm exp 3)
        assertEquals(SI.g, SI.kg exp -3)
        assertEquals(SI.kg, SI.g exp 3)
    }
    
    @Test def testExpand1 {
        assertEquals(SI.kg*SI.m/SI.s2, SI.expand(SI.N))
        assertEquals(SI.N*SI.m, SI.expand(SI.Nm))
    }

}