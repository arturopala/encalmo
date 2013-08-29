package org.encalmo.structures

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._
import org.encalmo.structures.eurocode.steel.Steel
import org.encalmo.expression.BasicSymbols

class StructuresComponentsUnitTest extends AssertionsForJUnit {

    @Test def shouldSymbolsBeNotSame() = {
        val fy1 = Steel.S355.fy
        val fy2 = Steel.S355.fy
        assertTrue(fy1.dictionary.isDefined)
        assertTrue(fy2.dictionary.isDefined)
        assertNotSame(fy1,fy2)
        assertTrue(BasicSymbols.a.dictionary.isEmpty)
        assertTrue(BasicSymbols.a.dictionary.isEmpty)
        assertSame(BasicSymbols.a,BasicSymbols.a)
    }


}
