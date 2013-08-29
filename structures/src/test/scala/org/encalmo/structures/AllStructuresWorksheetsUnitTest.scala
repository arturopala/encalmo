package org.encalmo.structures

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.eurocode.timber.{SlupDrewnianyZlozonyWkrety, DzwigarDrewnianyKlejonyTrapezowy}
import org.encalmo.structures.eurocode.concrete.{DomeWorksheet, RectangularConreteSiloCalculation, BetonSprezonyDzwigarTypuI}
import org.encalmo.structures.eurocode.steel.CircularSteelSiloCalculation
import org.encalmo.structures.eurocode.composite.CompositeSlabWorksheet
import org.encalmo.structures.miscellaneous.TRBWorksheet

class AllStructuresWorksheetsUnitTest extends AssertionsForJUnit {

    @Test
    def testDzwigarDrewnianyKlejonyTrapezowy() = {
          new DzwigarDrewnianyKlejonyTrapezowy().printHtml()
    }

    @Test
    def testSlupDrewnianyZlozonyWkrety() = {
        new SlupDrewnianyZlozonyWkrety().printHtml()
    }

    @Test
    def testBetonSprezonyDzwigarTypuI() = {
        new BetonSprezonyDzwigarTypuI().printHtml()
    }

    @Test
    def testCircularSteelSiloCalculation() = {
        new CircularSteelSiloCalculation().printHtml()
    }

    @Test
    def testRectangularConreteSiloCalculation() = {
        new RectangularConreteSiloCalculation().printHtml()
    }

    @Test
    def testDomeCalculationDocument() = {
        new DomeWorksheet().printHtml
    }

    @Test
    def testCompositeSlabCalculationDocument() = {
        new CompositeSlabWorksheet().printHtml()
    }

    @Test
    def testTRBCalculationDocument() = {
        new TRBWorksheet().printHtml()
    }

}
