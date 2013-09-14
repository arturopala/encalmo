package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.Worksheet
import org.encalmo.expression.SI

class SteelColumnUnitTest extends AssertionsForJUnit {

    @Test
    def testSteelColumn() = {
         def worksheet = new Worksheet("Steel Column Test") {
              val steelColumn = new SteelColumn("",Steel.S355,HESection.HE_220_A, 5 unit SI.m, 5 unit SI.m, 1000 unit SI.kN)
              this add steelColumn
             override val document = defaultDocument(steelColumn.info)
         }
         worksheet.printHtml()
    }

}
