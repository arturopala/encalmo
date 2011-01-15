package org.encalmo.printer.document

import org.encalmo.printer._
import org.encalmo.document._

/**
 * Prints document as plain text 
 * @author artur.opala
 */
object PlainTextDocumentPrinter extends TextDocumentPrinter {
	
	override def print(input:Document,output:TextOutput = new TextOutput):TextOutput = {
		//val t = new PlainTextExpressionPrinterTraveler(output.asWriter, output.locale)
		//e.travel(t = t)
		output
	}

}

