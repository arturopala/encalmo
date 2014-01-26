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
 * @author opala.artur
 */
abstract class Worksheet(name: String) extends Calculation(name) {
    
    /** Document to render */
    def document:Document = defaultDocument()
    /** Calculate results */
    def results: Results = Reckoner.reckon(this)

    def defaultDocument(component: DocumentComponent = Section(Evaluate(this.topologicallySortedConnectedSymbols:_*)(this))): Document = Document("",
        PredefinedStyles.stylesConfig,
        Chapter("",
            Section(""),
            Section(""),
            Section(styleTitle,name),
            component
        )
    )

    def renderHtml(): HtmlOutput =  renderHtml(results)

    def renderHtml(path: String): Results =  renderHtml(results, path)

    def renderHtml(output: HtmlOutput): HtmlOutput =  renderHtml(results, output)

    def renderHtml(results: Results, path: String): Results =  {
        val output: HtmlOutput = renderHtml(results)
        output.saveToFile(new java.io.File(path))
        results
    }

    def renderHtml(results: Results): HtmlOutput =  {
        val layout = PredefinedStyles.layout
        val prefs: HtmlOutputPreferences = HtmlOutputPreferences().withCustomStyleSheet(Path.fromString("src/main/resources/style.css"))
        val output: HtmlOutput = new HtmlOutput(layout, new java.util.Locale("PL"), prefs)
        output.open()
        renderHtml(results, output)
        output.close()
        output
    }

    def renderHtml(results: Results, output: HtmlOutput): HtmlOutput =  {
        HtmlTextDocumentPrinter.print(document)(output)(results)
        output
    }

    def renderXslFo(path: String): Results = renderXslFo(results)(path)

    def renderXslFo(results: Results)(path: String): Results = {
        val layout = PredefinedStyles.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        XslFoTextDocumentPrinter.print(document)(output)(results)
        output.close()
        output.saveToFile(new java.io.File(path))
        results
    }

    def renderPdf(path: String): Results = renderPdf(results)(path)

    def renderPdf(results: Results)(path: String): Results = {
        val layout = PredefinedStyles.layout
        val output: XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open()
        XslFoTextDocumentPrinter.print(document)(output)(results)
        output.close()
        FOPHelper.buildPDF(output.getResult, path)
        results
    }

    def renderText(path: String): Results = {
        val output: TextOutput = new TextOutput(new java.util.Locale("PL"))
        output.open()
        val res = results
        PlainTextDocumentPrinter.print(document)(output)(res)
        output.close()
        output.saveToFile(new java.io.File(path))
        res
    }

    @Test def testRenderHtml():Unit = {
        val path = "target/test-results/" + name + ".html"
        renderHtml(path)
    }

    @Test def testRenderXslFo():Unit = {
        val path = "target/test-results/" + name + ".fo"
        renderXslFo(path)
    }

    @Test def testRenderPdf():Unit = {
        val path = "target/test-results/" + name + ".pdf"
        renderPdf(path)
    }

    @Test def testRenderText():Unit = {
        val path = "target/test-results/" + name + ".txt"
        renderText(path)
    }
}