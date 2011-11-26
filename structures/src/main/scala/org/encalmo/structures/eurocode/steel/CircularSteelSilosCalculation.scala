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

class CircularSteelSilos {
    
    import BasicSymbols._
    
    val calc = Calculation()
   
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
    
    val doc1 = Document("",
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
            silos.inputGeometry,
            particulateSolid.properties,
            silos.calculatedGeometry,
            particulateSolid.characteristicValues,
            silos.volumes,
            silos.fillingSymmetricalLoad,
            silos.fillingPatchLoad,
            silos.dischargeSymmetricalLoad,
            silos.dischargePatchLoad,
            silos.fillingHopperLoad,
            silos.dischargeHopperLoad
           /*,
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")*/
        )
    )
    
    @Test def printHtml:Unit = {
        val layout = Predefined.layout
        val prefs:HtmlOutputPreferences = HtmlOutputPreferences().withCustomStyleSheet(Path("src/main/resources/style.css"))
        val output:HtmlOutput = new HtmlOutput(layout, new java.util.Locale("PL"),prefs)
        output.open
        HtmlTextDocumentPrinter.print(doc1,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/km-silos.html"))
    }
    
    @Test def printPdf:Unit = {
        val layout = Predefined.layout
        val output:XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open
        XslFoTextDocumentPrinter.print(doc1,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/km-silos.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/km-silos.pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc1,o)
        o.printConsole
    }

}
