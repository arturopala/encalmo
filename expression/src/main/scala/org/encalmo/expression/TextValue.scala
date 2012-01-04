package org.encalmo.expression

/**
 * Text value expression
 * @author artur.opala
 */
case class TextValue(text:String) extends Value {

	override def calculate(operator:String, v:Value):Option[Value] = None
	
	override def calculate(operator:String, v1:Value,v2:Value):Option[Value] = None

}