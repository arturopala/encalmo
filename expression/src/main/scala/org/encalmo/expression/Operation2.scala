package org.encalmo.expression


/**
 * Two argument operation trait
 * @author artur.opala
 */
trait Operation2 extends Operation {
	
  def l:Expression
  def r:Expression
  
  /** Children expressions */
  override val children:Seq[Expression] = Seq(l,r)
  
  /** Makes copy of this operation type with provided arguments */
  def copy(l:Expression,r:Expression):Operation2

  override def face = operator.name + "(" + l.face + "," + r.face + ")"
  
  final override def eval():Expression = {
	  val le = l eval()
      val re = r eval()
      (le,re) match {
	 	  case (v1:Value,v2:Value) => calculate(v1,v2) 
	 	  case _ if (le ne l) || (re ne r) => copy(le,re)
	 	  case _ => this
	  }
  }
  
  /** Default calculate implementation invokes Value#calculate(v1,v2) */ 
  def calculate(v1:Value,v2:Value):Expression = {
      Value.calculate(operator,v1,v2).getOrElse(copy(v1,v2))
  }
  
  final override def map(f:Transformation):Expression = {
	  val vl = l.map(f)
	  val vr = r.map(f)
	  if((vl eq l) && (vr eq r)) f(this) else f(copy(vl,vr))
  }

}

object Operation2 {
  def unapply(o:Operation2) = Some(o.l,o.r,o.operator,o.precedence)
}

