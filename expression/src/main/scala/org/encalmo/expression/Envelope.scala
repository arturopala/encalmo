package org.encalmo.expression

/**
 * Envelope of expression. Links expression with unit.
 */
case class Envelope(

		expression:Expression,
		override val unit:Option[UnitOfValue]

) extends Expression {

	override def eval = wrap(expression.eval)
	
	override def map(f:Expression => Expression):Expression = wrap(f(expression))
	
	def wrap(r:Expression):Envelope = {
		r match {
			case Envelope(e,u) => {
				if(u==unit) r.asInstanceOf[Envelope] else Envelope(e,unit)
			}
			case _ => Envelope(r,unit)
		}
	}
	
	override val children:Seq[Expression] = if(unit.isDefined) Seq(expression,unit.get) else Seq(expression)

}