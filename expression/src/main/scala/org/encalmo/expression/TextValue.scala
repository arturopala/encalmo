package org.encalmo.expression

/**
 * Text value expression
 * @author artur.opala
 */
case class TextValue(text:String) extends Value {

	/** 
	 * Calculates result of the operation with single argument. 
	 */
	def calculate(operator:String, v:Value):Option[Value] = None
	
	/** 
	 * Calculates result of the operation with two arguments. 
	 */
	def calculate(operator:String, v1:Value,v2:Value):Option[Value] = None

}