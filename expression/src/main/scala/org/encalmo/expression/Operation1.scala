package org.encalmo.expression

/**
 * One argument operations root type
 * @author artur.opala
 */
trait Operation1 extends Operation {

/** Operation argument */
def e:Expression
  
  /**
   * Returns resulting Real value
   * @param r number
   */
  def doReal(r:Real):Real
  
  /**
   * Operation copy with exchanged parameters
   * @param e
   */
  def doCopy(e:Expression):Operation1
  
  final override def eval():Expression = {
	  val ev = e eval; 
	  ev match {
	 	  case Number(r) => Number(doReal(r));
	 	  case _ if(ev!=e) => doCopy(ev);
	 	  case _=> this
	  }
  }
  
  final override def map(f:Transformation):Expression = {
	  val ve = e.map(f);
	  if(ve==e) f(this) else f(doCopy(ve))
  }
  
  final override def travel(parent:Node = null, t:Traveler, position:Int=0):Unit = {
	  val n = Node(parent,this,position)
	  t.onEnter(n)
	  t.onBeforeChildEnter(n,0,e)
	  e.travel(n,t)
	  t.onAfterChildExit(n,0,e)
	  t.onExit(n)
  }
  
}

object Operation1{
  def unapply(o:Operation1) = Some(o.e,o.operator,o.precedence)
}