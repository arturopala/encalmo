package org.encalmo.printer

import org.encalmo.expression._
import org.encalmo.expression.PlainTextExpressionPrinterTraveler

/**
 * Prints expressions as plain text 
 * @author artur.opala
 */
object PlainTextExpressionPrinter extends TextExpressionPrinter {
	
	override def print(e:Expression,output:TextOutput = new TextOutput):TextOutput = {
		val t = new PlainTextExpressionPrinterTraveler(output.asWriter, output.locale)
		e.travel(t = t)
		output
	}

}

