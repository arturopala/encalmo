package org.encalmo.expression

/**
 * Text value expression
 * @author artur.opala
 */
case class TextValue(text:String) extends Value {
	def typeId = 'Text
    def face = "\"" + text + "\""

    override def ===(e: Expression): Expression = e match {
        case TextValue(otherText) => BooleanValue(text == otherText)
        case _ => FALSE
    }
}