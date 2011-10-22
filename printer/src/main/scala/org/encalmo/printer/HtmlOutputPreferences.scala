package org.encalmo.printer

import scala.collection.mutable.LinkedHashMap

/**
 * OutputPreferences
 * @author artur.opala
 */
case class HtmlOutputPreferences(
	expressionPrintStrategy:String = "table" //list|section|table
){
	
	def withExpressionPrintStrategy(s:String):HtmlOutputPreferences = copy(expressionPrintStrategy=s)
	
}