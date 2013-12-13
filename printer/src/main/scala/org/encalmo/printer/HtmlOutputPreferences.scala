package org.encalmo.printer

import scala.collection.mutable.LinkedHashMap
import scalax.file.Path

/**
 * OutputPreferences
 * @author artur.opala
 */
case class HtmlOutputPreferences(
	formulaLayoutStrategy:String = "table", //list|section|table
	ignoreDocumentStyles:Boolean = false,
	customStyleSheet:Option[String] = None,
  isRenderHeaderAndBodyTags:Boolean = true
){
	
	def withFormulaLayoutStrategy(s:String):HtmlOutputPreferences = copy(formulaLayoutStrategy=s)
	
	def ignoreDocumentStyles(b:Boolean):HtmlOutputPreferences = copy(ignoreDocumentStyles=b)
	
	def withCustomStyleSheet(path:Path):HtmlOutputPreferences = copy(customStyleSheet = Option(path.string), ignoreDocumentStyles=true)
	def withCustomStyleSheet(stylesheet:String):HtmlOutputPreferences = copy(customStyleSheet = Option(stylesheet), ignoreDocumentStyles=true)
	
	def hasCustomStyleSheet = customStyleSheet.isDefined
  def hasNotCustomStyleSheet = customStyleSheet.isEmpty

  def asFragmentOfPage():HtmlOutputPreferences = copy(isRenderHeaderAndBodyTags = false)
  def asCompletePage():HtmlOutputPreferences = copy(isRenderHeaderAndBodyTags = true)
}