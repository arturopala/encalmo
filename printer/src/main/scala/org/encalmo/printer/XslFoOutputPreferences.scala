package org.encalmo.printer

import scala.collection.mutable.LinkedHashMap

/**
 * OutputPreferences
 * @author artur.opala
 */
case class XslFoOutputPreferences(
	expressionPrintStrategy:String = "table" //list|section|table
){
	
	def withExpressionPrintStrategy(s:String):XslFoOutputPreferences = copy(expressionPrintStrategy=s)
	
}