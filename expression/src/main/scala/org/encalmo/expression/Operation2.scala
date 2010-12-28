package org.encalmo.expression

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
  
  final override def travel(parent:Node = null,t:Traveler, position:Int=0):Unit = {
	  val n = Node(parent,this,position)
	  t.onEnter(n)
	  t.onBeforeChildEnter(n,0,l)
	  l.travel(n,t,0)
	  t.onAfterChildExit(n,0,l)
	  t.onBetweenChildren(n,l,r);
	  t.onBeforeChildEnter(n,1,r)
	  r.travel(n,t,1)
	  t.onAfterChildExit(n,1,r)
	  t.onExit(n)
  }

}

object Operation2 {
  def unapply(o:Operation2) = Some((o.l,o.r,o.operator,o.precedence))
}

