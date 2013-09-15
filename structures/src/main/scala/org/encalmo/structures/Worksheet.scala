package org.encalmo.structures

import org.encalmo.calculation.{Reckoner, Results, Calculation}
import org.encalmo.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.printer.document.HtmlTextDocumentPrinter
import org.encalmo.printer.document.PlainTextDocumentPrinter
import org.encalmo.printer.document.XslFoTextDocumentPrinter
import org.encalmo.printer.HtmlOutput
import org.encalmo.printer.TextOutput
import org.encalmo.printer.XslFoOutput
import org.junit.Test

import scalax.file.Path
import org.encalmo.structures.Predefined._
import org.encalmo.printer.HtmlOutputPreferences

/**
 * Base class for documented calculations
 * @author artur
 */
abstract class Worksheet(name: String) extends Calculation(name) {
    
    /** Document to print */
    def document:Document = defaultDocument()
    /** Calculation results */
    def results: Results = Reckoner.reckon

    def defaultDocument(component: DocumentComponent = Section(Evaluate(this.topologicallySortedConnectedSymbols:_*)(this))): Document = Document("",
        Predefined.stylesConfig,
        Chapter("",
            Section(""),
            Section(""),
            Section(styleTitle,name),
            component
        )
    )

    def printHtml(path: String): Results =  {
        val layout = Predefined.layout
        val prefs: HtmlOutputPreferences = HtmlOutputPreferences().withCustomStyleSheet(Path.fromString("src/main/resources/style.css"))
        val output: HtmlOutput = new HtmlOutput(layout, new java.util.Locale("PL"), prefs)
        output.open()
        val res = results
        HtmlTextDocumentPrinter.print(document)(output)(res)
        output.close()
        output.saveToFile(new java.io.File(path))
        res
    }

    def printXslFo(path: String): Results = {
        val layout = Predefined.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        val res = results
        XslFoTextDocumentPrinter.print(document)(output)(res)
        output.close()
        output.saveToFile(new java.io.File(path))
        res
    }

    def printPdf(path: String): Results = {
        val layout = Predefined.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        val res = results
        XslFoTextDocumentPrinter.print(document)(output)(res)
        output.close()
        FOPHelper.buildPDF(output.getResult, path)
        res
    }

    def printText(path: String): Results = {
        val output: TextOutput = new TextOutput(new java.util.Locale("PL"))
        output.open()
        val res = results
        PlainTextDocumentPrinter.print(document)(output)(res)
        output.close()
        output.saveToFile(new java.io.File(path))
        res
    }

    @Test def printHtml():Unit = {
        val path = "target/test-results/" + name + ".html"
        printHtml(path)
    }

    @Test def printXslFo():Unit = {
        val path = "target/test-results/" + name + ".fo"
        printXslFo(path)
    }

    @Test def printPdf():Unit = {
        val path = "target/test-results/" + name + ".pdf"
        printPdf(path)
    }

    @Test def printText():Unit = {
        val path = "target/test-results/" + name + ".txt"
        printText(path)
    }
}