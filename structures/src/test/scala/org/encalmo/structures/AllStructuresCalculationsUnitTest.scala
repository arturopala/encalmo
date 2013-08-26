package org.encalmo.structures

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.eurocode.timber.{SlupDrewnianyZlozonyWkrety, DzwigarDrewnianyKlejonyTrapezowy}
import org.encalmo.structures.eurocode.concrete.{DomeCalculationDocument, RectangularConreteSiloCalculation, BetonSprezonyDzwigarTypuI}
import org.encalmo.structures.eurocode.steel.CircularSteelSiloCalculation
import org.encalmo.structures.eurocode.composite.CompositeSlabCalculationDocument
import org.encalmo.structures.miscellaneous.TRBCalculationDocument

class AllStructuresCalculationsUnitTest extends AssertionsForJUnit {

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
        new DomeCalculationDocument().printHtml
    }

    @Test
    def testCompositeSlabCalculationDocument() = {
        new CompositeSlabCalculationDocument().printHtml()
    }

    @Test
    def testTRBCalculationDocument() = {
        new TRBCalculationDocument().printHtml()
    }

}
