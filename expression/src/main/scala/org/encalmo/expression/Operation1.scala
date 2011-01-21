package org.encalmo.expression

import org.encalmo.common._

/**
 * One argument operations root type
 * @author artur.opala
 */
trait Operation1 extends Operation {

  /** Operation argument */
  def e:Expression
  
  /** Children expressions */
  override def children:Seq[Expression] = Seq(e)
  
  /**
   * Returns resulting Real value
   * @param r number
   */
  def calculate(v:Value):Expression
  
  /**
   * Operation copy with exchanged parameters
   * @param e
   */
  def copy(e:Expression):Operation1
  
  final override def eval():Expression = {
	  val ev = e eval; 
	  ev match {
	 	  case _ if(ev.isInstanceOf[Value]) => calculate(ev.asInstanceOf[Value]);
	 	  case _ if(ev!=e) => copy(ev);
	 	  case _ if(ev==e)=> this
	  }
  }
  
  final override def map(f:Transformation):Expression = {
	  val ve = e.map(f);
	  if(ve==e) f(this) else f(copy(ve))
  }
  
  /*final override def travel(parent:Node[Expression] = null, traveler:Traveler[Expression], position:Int=0):Unit = {
	  val n = Node(parent,this,position)
	  traveler.onEnter(n)
	  traveler.onBeforeChildEnter(n,0,e)
	  e.travel(n,traveler)
	  traveler.onAfterChildExit(n,0,e)
	  traveler.onExit(n)
  }*/
  
}

object Operation1{
  def unapply(o:Operation1) = Some(o.e,o.operator,o.precedence)
}