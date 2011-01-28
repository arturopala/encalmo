package org.encalmo.examples

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.document.StylesConfigSymbols._

class DzwigarDrewnianyKlejonyTrapezowy extends AssertionsForJUnit {
    
    val calc = Calculation("1")
    
    val l = BasicSymbols.l is "rozpiętość obliczeniowa"
    val h1 = BasicSymbols.h|1 is "wysokość przekroju (niższa)"
    val h2 = BasicSymbols.h|2 is "wysokość przekroju (wyższa)"
    val b = BasicSymbols.b is "szerokość przekroju"
    
    calc(l) = 20
    calc(b) = 0.2
    calc(h1) = 1.5
    calc(h2) = 2
        
      
    val doc1 = Document(Predefined.style1,"",
        Predefined.stylesConfig,
        Chapter("",
            Section("Ćwiczenie projektowe nr 2 z \"Konstrukcji Drewnianych\". Autor: Artur Opala"),
            Section(""),

            NumSection("Dane do projektowania",
                NumSection("Geometria dźwigara",
                     Evaluate(calc,l,h1,h2,b)),""
            )
        )
    )
    
    
    
    @Test def printPdf:Unit = {
        val output:XslFoOutput = new XslFoOutput(Layout(), new java.util.Locale("PL"))
        output.open
        XslFoTextDocumentPrinter.print(doc1,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/kd1-dzwigar.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/kd1-dzwigar.pdf")
    }

}