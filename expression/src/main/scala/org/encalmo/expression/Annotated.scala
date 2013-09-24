package org.encalmo.expression

/** Expression annotated with description */
case class Annotated(expression: Expression, description: Option[String]) extends Transparent {

    override def eval(): Expression = expression.eval()
    def wrap(e: Expression): Transparent = Annotated(e,None)
    override def ##(d:String): Annotated = Annotated(expression,concatenate(description,Option(d)))
}

object Annotated {
    def apply(expression: Expression, description: String): Annotated = Annotated(expression,Option(description))
}
