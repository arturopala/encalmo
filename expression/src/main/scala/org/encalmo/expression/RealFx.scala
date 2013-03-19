package org.encalmo.expression

/**
 * Lazy expression of function returning Real
 * @author artur.opala
 */
case class RealFx(fx:() => Real) extends Expression {
	
  override def eval():Expression = Number(fx())
  
  override def map(f:Expression=>Expression):Expression = f(Number(fx()))
  
}