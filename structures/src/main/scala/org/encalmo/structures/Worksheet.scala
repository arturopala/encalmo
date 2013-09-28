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
import org.encalmo.style.PredefinedStyles
import PredefinedStyles._
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
        PredefinedStyles.stylesConfig,
        Chapter("",
            Section(""),
            Section(""),
            Section(styleTitle,name),
            component
        )
    )

    def printHtml(path: String): Results =  printHtml(results)(path)

    def printHtml(results: Results)(path: String): Results =  {
        val layout = PredefinedStyles.layout
        val prefs: HtmlOutputPreferences = HtmlOutputPreferences().withCustomStyleSheet(Path.fromString("src/main/resources/style.css"))
        val output: HtmlOutput = new HtmlOutput(layout, new java.util.Locale("PL"), prefs)
        output.open()
        HtmlTextDocumentPrinter.print(document)(output)(results)
        output.close()
        output.saveToFile(new java.io.File(path))
        results
    }

    def printXslFo(path: String): Results = printXslFo(results)(path)

    def printXslFo(results: Results)(path: String): Results = {
        val layout = PredefinedStyles.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        XslFoTextDocumentPrinter.print(document)(output)(results)
        output.close()
        output.saveToFile(new java.io.File(path))
        results
    }

    def printPdf(path: String): Results = printPdf(results)(path)

    def printPdf(results: Results)(path: String): Results = {
        val layout = PredefinedStyles.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        XslFoTextDocumentPrinter.print(document)(output)(results)
        output.close()
        FOPHelper.buildPDF(output.getResult, path)
        results
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