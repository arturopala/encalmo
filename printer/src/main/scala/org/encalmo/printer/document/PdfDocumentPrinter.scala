package org.encalmo.printer.document

import org.encalmo.printer._
import org.encalmo.document._
import org.encalmo.fop._
import org.encalmo.calculation.Results

/**
 * PDF document printer. 
 * @author artur.opala
 */
object PdfDocumentPrinter extends DocumentPrinter[PdfOutput,Array[Byte]] {
	
	override def print(input:Document)(output:PdfOutput)(results: Results):PdfOutput = {
		val xslFoOutput = output.toXslFoOutput
		XslFoTextDocumentPrinter.print(input)(xslFoOutput)(results)
		FOPHelper.buildPDF(xslFoOutput.getResult, output.stream)
		output
	}

}