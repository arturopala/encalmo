package org.encalmo.structures.eurocode.steel

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.Worksheet

class AngleSteelSectionUnitTest extends AssertionsForJUnit {

    @Test
    def testAnglEqualLegSection() = {
        val angle = AngleEqualLegSection.L_120_x_120_x_10
        def worksheet = new Worksheet("Angle Equal Leg Section Test") {
            this add angle
            override val document = defaultDocument(angle.info)
        }
        val path = "target/test-results/" + worksheet.name + ".html"
        val results = worksheet.printHtml(path)

    }

}
