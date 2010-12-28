package org.encalmo.expression

/**
 * Constant expression
 * @author artur.opala
 */
case class Constant[t<:Expression](s:Symbol,e:t) extends Expression {
	
  override def eval():t = e
  
}

/**
 * PI number constant
 * @author artur.opala
 */
object PI extends Constant[Number](BasicSymbols.Pi,Number(Real.pi))

/**
 * E number constant
 * @author artur.opala
 */
object EUL extends Constant[Number](BasicSymbols.eul,Number(Real.e))