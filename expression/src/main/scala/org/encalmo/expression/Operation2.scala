package org.encalmo.expression

import org.encalmo.common._

/**
 * Two argument operation trait
 * @author artur.opala
 */
trait Operation2 extends Operation {
	
  def l:Expression
  def r:Expression
  
  /** Children expressions */
  override def children:Seq[Expression] = Seq(l,r)
  
  def calculate(lv:Value,rv:Value):Expression
  def copy(l:Expression,r:Expression):Operation2
  
  final override def eval():Expression = {
	  val le = l eval;
	  val re = r eval;
	  (le,re) match {
	 	  case _ if (le.isInstanceOf[Value] && re.isInstanceOf[Value]) => calculate(le.asInstanceOf[Value],re.asInstanceOf[Value]);
	 	  case _ if (le!=l || re!=r) => copy(le,re);
	 	  case _ => this;
	  }
  }
  
  final override def map(f:Transformation):Expression = {
	  val vl = l.map(f); 
	  val vr = r.map(f);
	  if(vl==l && vr==r) f(this) else f(copy(vl,vr))
  }

}

object Operation2 {
  def unapply(o:Operation2) = Some(o.l,o.r,o.operator,o.precedence)
}

