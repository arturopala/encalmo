package org.encalmo.expression

/**
 * Constant expression
 * @author artur.opala
 */
case class Constant(s:Symbol,e:Expression) extends Expression {
	
  override def eval():Expression = e
  
}

/**
 * PI number constant
 * @author artur.opala
 */
object pi extends Constant(Symbol("$pi"),Number(Real.pi))

/**
 * E number constant
 * @author artur.opala
 */
object ee extends Constant(Symbol("$e"),Number(Real.e))