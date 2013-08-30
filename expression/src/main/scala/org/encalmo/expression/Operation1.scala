package org.encalmo.expression


/**
 * One argument operation trait
 * @author artur.opala
 */
trait Operation1 extends Operation {

  /** Operation argument */
  def expression:Expression
  
  /** Children expressions */
  override val children:Seq[Expression] = Seq(expression)
  
  /**
   * Operation copy with exchanged parameters
   * @param e
   */
  def copy(e:Expression):Operation1
  
  final override def eval():Expression = {
	  val ev = expression eval()
	  ev match {
	 	  case v:Value => calculate(v)
	 	  case _ if ev ne expression => copy(ev)
	 	  case _  => this
	  }
  }
  
  /** Default calculate implementation invokes Value#calculate */ 
  def calculate(v:Value):Expression = {
      Value.calculate(operator,v).getOrElse(copy(v))
  }
  
  final override def map(f:Transformation):Expression = {
	  val ve = expression.map(f)
	  if(ve eq expression) f(this) else f(copy(ve))
  }

  
}

object Operation1{
  def unapply(o:Operation1) = Some(o.expression,o.operator,o.precedence)
}