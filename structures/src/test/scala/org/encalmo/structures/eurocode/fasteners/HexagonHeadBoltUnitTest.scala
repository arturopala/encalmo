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
        val bolt = HexagonHeadBolt.M27(BoltClass.C_6_8,40 unit SI.mm)
        def worksheet = new Worksheet("Hexagon Head Bolt Test") {
            this add bolt
            bolt(bolt.t) = 10 unit SI.mm
            override val document = defaultDocument(bolt.info)
        }
        val path = "target/test-results/" + worksheet.name + ".html"
        val results = worksheet.renderHtml(path)
    }

}
