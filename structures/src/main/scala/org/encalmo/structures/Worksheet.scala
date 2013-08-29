package org.encalmo.structures

import org.encalmo.calculation.{Reckoner, Context, Results, Calculation}
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
import org.encalmo.expression.{Expression,Symbol}

/**
 * Base class for documented calculations
 * @author artur
 */
abstract class Worksheet(name: String) extends Calculation(name) {
    
    /** Produced document */
    implicit val document:Document

    def results: Results = Reckoner.reckon

    def printHtml(path: String) {
        val layout = Predefined.layout
        val prefs: HtmlOutputPreferences = HtmlOutputPreferences().withCustomStyleSheet(Path.fromString("src/main/resources/style.css"))
        val output: HtmlOutput = new HtmlOutput(layout, new java.util.Locale("PL"), prefs)
        output.open()
        HtmlTextDocumentPrinter.print(document)(output)(results)
        output.close()
        output.saveToFile(new java.io.File(path))
    }

    def printXslFo(path: String) {
        val layout = Predefined.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        XslFoTextDocumentPrinter.print(document)(output)(results)
        output.close()
        output.saveToFile(new java.io.File(path))
    }

    def printPdf(path: String) {
        val layout = Predefined.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        XslFoTextDocumentPrinter.print(document)(output)(results)
        output.close()
        FOPHelper.buildPDF(output.getResult, path)
    }

    def printText(path: String) {
        val output: TextOutput = new TextOutput(new java.util.Locale("PL"))
        output.open()
        PlainTextDocumentPrinter.print(document)(output)(results)
        output.close()
        output.saveToFile(new java.io.File(path))
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