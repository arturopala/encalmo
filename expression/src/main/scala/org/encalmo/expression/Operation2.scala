package org.encalmo.expression

import org.encalmo.common._

/**
 * Two argument operations root type
 * @author artur.opala
 */
trait Operation2 extends Operation {
	
  def l:Expression
  def r:Expression
  
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
  
  final override def travel(parent:Node[Expression] = null,traveler:Traveler[Expression], position:Int=0):Unit = {
	  val n = Node(parent,this,position)
	  traveler.onEnter(n)
	  traveler.onBeforeChildEnter(n,0,l)
	  l.travel(n,traveler,0)
	  traveler.onAfterChildExit(n,0,l)
	  traveler.onBetweenChildren(n,l,r);
	  traveler.onBeforeChildEnter(n,1,r)
	  r.travel(n,traveler,1)
	  traveler.onAfterChildExit(n,1,r)
	  traveler.onExit(n)
  }

}

object Operation2 {
  def unapply(o:Operation2) = Some((o.l,o.r,o.operator,o.precedence))
}

