package org.encalmo.structures.eurocode.steel

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.document.StylesConfigSymbols._
import org.encalmo.structures.Predefined
import org.encalmo.structures.Predefined._
import org.encalmo.structures.eurocode.actions.silos.ParticulateSolid
import org.encalmo.structures.eurocode.actions.silos.ThinWalledCircularSlenderSilosWithSteepHopper
import scalax.file.Path
import org.encalmo.structures.CalculationDocument

/**
 * Calculation of the circular steel silos with steep hopper
 */
class CircularSteelSilosCalculation extends CalculationDocument {
    
    import BasicSymbols._

    override val name = "km-silos"
   
    val particulateSolid = ParticulateSolid.Cement
    val steel = Steel.S275
    val silos = new ThinWalledCircularSlenderSilosWithSteepHopper(
            diameter = 3.0,
            heightOfChamber = 10.0,
            heightOfHopper = 3.0,
            thicknessOfChamberWall = 0.007,
            thicknessOfHopperWall = 0.01,
            diameterOfOutlet = 0.2,
            particulateSolid = particulateSolid,
            wallType = 1
    )
    
    calc add silos
    
    override val doc = Document("",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z \"Konstrukcji Metalowych\". Semestr zimowy 2011/2012."),
        		Section("Autor: Artur Opala, 61315. Prowadzący: dr inż. Jacek Dudkiewicz, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            NumSection("Zadanie projektowe",
                Section(styleComment,"Silos stalowy jednokomorowy na cement usytuowany we Wrocławiu przy ul. Oś Inkubacji 1.")
            ),
            NumSection("Wykaz materiałów źródłowych",
            	NumSection(styleComment," [1991-4] Norma PN-EN 1991-4 \"Eurokod 1. Oddziaływania na konstrukcje. Część 4: Silosy i zbiorniki.\"")
            ),
            NumSection("Dane wejściowe",
                    silos.inputGeometry,
                    steel.info,
                    particulateSolid.properties
            ),
            NumSection("Oddziaływania na silos przy napełnianiu i opróżnianiu",
                silos.calculatedGeometry,
                particulateSolid.characteristicValues,
                silos.volumes,
                silos.fillingSymmetricalLoad,
                silos.fillingPatchLoad,
                silos.dischargeSymmetricalLoad,
                silos.dischargePatchLoad,
                silos.fillingHopperLoad,
                silos.dischargeHopperLoad
            )
           /*,
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")*/
        )
    )

}
