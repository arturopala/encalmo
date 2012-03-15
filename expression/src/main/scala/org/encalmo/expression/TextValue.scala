package org.encalmo.expression

/**
 * Text value expression
 * @author artur.opala
 */
case class TextValue(text:String) extends Value {

	def typeId = "Text"

}