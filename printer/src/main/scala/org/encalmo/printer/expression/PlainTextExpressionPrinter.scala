package org.encalmo.printer.expression

import org.encalmo.printer._
import org.encalmo.expression._
import org.encalmo.common._

/**
 * Prints expressions as plain text 
 * @author artur.opala
 */
object PlainTextExpressionPrinter extends TextExpressionPrinter {
	
	override def print(e:Expression,output:TextOutput = new TextOutput):TextOutput = {
		val t = new PlainTextExpressionPrinterTraveler(output.asWriter, output.locale)
		e.travel(traveler = t)
		output
	}

}

