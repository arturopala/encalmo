package org.encalmo.printer.document
import org.encalmo.printer._
import org.encalmo.document._
import org.encalmo.fop._

/**
 * PDF document printer. 
 * @author artur.opala
 */
object PdfDocumentPrinter extends DocumentPrinter[PdfOutput,Array[Byte]] {
	
	override def print(input:Document,output:PdfOutput):PdfOutput = {
		val xslFoOutput = output.toXslFoOutput
		XslFoTextDocumentPrinter.print(input, xslFoOutput)
		FOPHelper.buildPDF(xslFoOutput.getResult, output.stream)
		output
	}

}