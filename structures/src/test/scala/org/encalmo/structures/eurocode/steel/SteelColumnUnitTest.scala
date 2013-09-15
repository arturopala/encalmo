package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.Worksheet
import org.encalmo.expression.SI
import org.junit.Assert._

class SteelColumnUnitTest extends AssertionsForJUnit {

    @Test
    def testSteelColumn() = {
        val steelColumn = new SteelColumn("", Steel.S355, HESteelSection.HE_220_A, 5 unit SI.m, 5 unit SI.m, 1000 unit SI.kN)
        def worksheet = new Worksheet("Steel Column Test") {
            this add steelColumn
            override val document = defaultDocument(steelColumn.info)
        }
        val path = "target/test-results/" + worksheet.name + ".html"
        val results = worksheet.printHtml(path)
        assertTrue("Ned should be less then NbRd",results.cache(steelColumn.NbRd).toDouble > results.cache(steelColumn.Ned).toDouble)
    }

}
