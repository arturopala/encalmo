package org.encalmo.expression

/**
 * Proxy expression
 */
case class Proxy(

		expression:Expression,
		override val unit:Option[UnitOfValue]

) extends Expression {

	override def eval = wrap(expression.eval)
	
	override def map(f:Expression => Expression):Expression = wrap(f(expression))
	
	def wrap(r:Expression):Proxy = {
		r match {
			case Proxy(e,u) => {
				if(u==unit) r.asInstanceOf[Proxy] else Proxy(e,unit)
			}
			case _ => Proxy(r,unit)
		}
	}
	
	override def children = expression.children
	
	override def + (e:Expression):Expression = e match {
		  case Proxy(e,u) => {
		     Proxy(Sum(expression,e),unit)
		  }
		  case _ => Proxy(Sum(expression,e),unit)
	  }

}