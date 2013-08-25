package org.encalmo.expression

import scala.language.postfixOps

/**
 * Lazy expression of function returning Expression
 * @author artur.opala
 */
case class ExprFx(fx:() => Expression) extends Expression {
	
  override def eval():Expression = fx() eval()
  
  override def map(f:Expression=>Expression):Expression = f(fx())
  
}