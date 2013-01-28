package org.encalmo.printer

import scala.collection.mutable.LinkedHashMap
import scalax.file.Path

/**
 * OutputPreferences
 * @author artur.opala
 */
case class HtmlOutputPreferences(
	expressionPrintStrategy:String = "table", //list|section|table
	skipDocumentStyles:Boolean = false,
	customStyleSheet:String = null 
){
	
	def withExpressionPrintStrategy(s:String):HtmlOutputPreferences = copy(expressionPrintStrategy=s)
	
	def withSkipDocumentStyles(b:Boolean):HtmlOutputPreferences = copy(skipDocumentStyles=b)
	
	def withCustomStyleSheet(path:Path):HtmlOutputPreferences = copy(customStyleSheet = path.string,skipDocumentStyles=true)
	
	def withCustomStyleSheet(stylesheet:String):HtmlOutputPreferences = copy(customStyleSheet = stylesheet,skipDocumentStyles=true)
	
	def isCustomStyleSheet = customStyleSheet!=null
}