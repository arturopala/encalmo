package org.encalmo.structures.eurocode.fasteners

import org.encalmo.expression._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.Worksheet
import org.encalmo.expression.SI
import org.junit.Assert._

class HexagonHeadBoltUnitTest extends AssertionsForJUnit {

    @Test
    def testSteelColumn() = {
        val bolt = HexagonHeadBolt.M30(BoltClass.C_6_8,40 unit SI.mm)
        def worksheet = new Worksheet("Hexagon Head Bolt Test") {
            this add bolt
            override val document = defaultDocument(bolt.info)
        }
        val path = "target/test-results/" + worksheet.name + ".html"
        val results = worksheet.printHtml(path)
    }

}
