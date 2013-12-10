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
    def testDzwigarDrewnianyKlejonyTrapezowy():Unit = {
          new DzwigarDrewnianyKlejonyTrapezowy().testRenderHtml()
    }

    @Test
    def testSlupDrewnianyZlozonyWkrety():Unit = {
        new SlupDrewnianyZlozonyWkrety().testRenderHtml()
    }

    @Test
    def testBetonSprezonyDzwigarTypuI():Unit = {
        new BetonSprezonyDzwigarTypuI().testRenderHtml()
    }

    @Test
    def testCircularSteelSiloCalculation():Unit = {
        new CircularSteelSiloCalculation().testRenderHtml()
    }

    @Test
    def testRectangularConreteSiloCalculation():Unit = {
        new RectangularConreteSiloCalculation().testRenderHtml()
    }

    @Test
    def testDomeCalculationDocument():Unit = {
        new DomeWorksheet().testRenderHtml()
    }

    @Test
    def testCompositeSlabCalculationDocument():Unit = {
        new CompositeSlabWorksheet().testRenderHtml()
    }

    @Test
    def testTRBCalculationDocument():Unit = {
        new TRBWorksheet().testRenderHtml()
    }

}
