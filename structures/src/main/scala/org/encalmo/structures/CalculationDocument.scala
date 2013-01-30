package org.encalmo.structures

import org.encalmo.calculation.Calculation
import org.encalmo.document.Document
import org.encalmo.fop.FOPHelper
import org.encalmo.printer.document.HtmlTextDocumentPrinter
import org.encalmo.printer.document.PlainTextDocumentPrinter
import org.encalmo.printer.document.XslFoTextDocumentPrinter
import org.encalmo.printer.HtmlOutput
import org.encalmo.printer.HtmlOutputPreferences
import org.encalmo.printer.TextOutput
import org.encalmo.printer.XslFoOutput
import org.junit.Test

import scalax.file.Path

/**
 * Base class for documented calculations
 * @author artur
 */
abstract class CalculationDocument extends Calculation {
    
    /** Documented calculation */
    val calc:Calculation = this
    
    /** Produced document */
    implicit val doc:Document
    
    /** Calculation's output files base name */
    val name:String
    
    @Test def printHtml:Unit = {
        val layout = Predefined.layout
        val prefs:HtmlOutputPreferences = HtmlOutputPreferences().withCustomStyleSheet(Path.fromString("src/main/resources/style.css"))
        val output:HtmlOutput = new HtmlOutput(layout, new java.util.Locale("PL"),prefs)
        output.open
        HtmlTextDocumentPrinter.print(doc,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/"+name+".html"))
    }
    
    @Test def printPdf:Unit = {
        val layout = Predefined.layout
        val output:XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open
        XslFoTextDocumentPrinter.print(doc,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/"+name+".fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/"+name+".pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc,o)
        o.printConsole
    }
    
}