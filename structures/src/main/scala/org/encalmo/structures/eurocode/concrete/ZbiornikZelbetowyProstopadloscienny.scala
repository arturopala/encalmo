package org.encalmo.structures.eurocode.concrete

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
import org.encalmo.structures.eurocode.actions.ParticulateSolid
import org.encalmo.structures.eurocode.actions.RectangularSlenderSilosFlatBottom

class ZbiornikZelbetowyProstopadloscienny {
    
    import BasicSymbols._
    
    val calc = Calculation()
   
    val solid = ParticulateSolid.Flyash
    val concrete = Concrete.C_20_25
    val reinforcement = ReinforcingSteel.B500SP
    val silos = new RectangularSlenderSilosFlatBottom(5.7,6.0,12,4.2,0.2,0.4,0.25,solid,3,concrete,reinforcement)
    
    calc add silos
    
    val doc1 = Document("",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z \"Konstrukcji Betonowych\". Semestr letni 2010/2011."),
        		Section("Autor: Artur Opala, 61315. Prowadzący: dr inż. Włodzimierz Wydra, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            NumSection("Zadanie projektowe",
                Section(styleComment,"Silos żelbetowy jednokomorowy na popiół lotny usytuowany wewnątrz hali przemysłowej we Wrocławiu przy ul. Oś Inkubacji 1. Beton C20/25, stal zbrojeniowa B500SP, klasa ekspozycji XC1.")
            ),
            NumSection("Wykaz materiałów źródłowych",
            	NumSection(styleComment," Norma PN-EN 1991-4 \"Eurokod 1. Oddziaływania na konstrukcje. Część 4: Silosy i zbiorniki.\""),
            	NumSection(styleComment," Norma PN-EN 1992-1-1 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-1: Reguły ogólne i reguły dla budynków.\""),
            	NumSection(styleComment," Norma PN-EN 1992-1-2 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-2: Reguły ogólne. Projektowanie z uwagi na warunki pożarowe.\""),
            	NumSection(styleComment," Norma PN-EN 1992-3 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 3: Silosy i zbiorniki na ciecze.\"")
            ),
            silos.inputGeometry,
            solid.properties,
            concrete.info,
            reinforcement.info,
            silos.calculatedGeometry,
            solid.characteristicValues,
            silos.volumes,
            silos.fillingSymmetricalLoad,
            silos.fillingPatchLoad,
            silos.dischargeSymmetricalLoad,
            silos.dischargePatchLoad,
            silos.loadsOnSiloBottom
           /*,
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")*/
        )
    )
    
    @Test def printPdf:Unit = {
        val layout = Predefined.layout
        val output:XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open
        XslFoTextDocumentPrinter.print(doc1,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/kb-silos.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/kb-silos.pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc1,o)
        o.printConsole
    }

}
