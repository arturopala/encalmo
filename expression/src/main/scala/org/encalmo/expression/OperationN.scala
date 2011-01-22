package org.encalmo.expression

import org.encalmo.common._

/**
 * Operation with N-arguments
 * (usually more than two)
 * @author artur.opala
 */
trait OperationN extends Operation {
	
  def args:Seq[Expression]
  
  /** Children expressions */
  override def children:Seq[Expression] = args
	
  /**
   * Real operation execution
   * @param r number
   * @return
   */
  def calculate(v:Value*):Expression
  
  /**
   * Operation copy with exchanged parameters
   * @param e
   * @return
   */
  def copy(e:Expression*):OperationN
	
  final override def eval():Expression = {
	  val ps = args.map(_.eval).partition(_.isInstanceOf[Value])
	  if(ps._1.isEmpty){
	 	  copy(args:_*)
	  }else{
		  val result:Expression = calculate(ps._1.map(_.asInstanceOf[Value]):_*)
		  if(ps._2.isEmpty){
		 	  result
		  } else {
		 	  copy((ps._2.+:(result)):_*)
		  }
	  }
  }
  
  final override def map(f:Transformation):Expression = {
	  val m = args.map(e => e.map(f))
	  if(args.zip(m).forall(t => t._1 eq t._2)){
	 	  f(this)
	  }else{
	 	  f(copy(m:_*))
	  }
  }
  
}

/**
 * Companion object of OperationN trait
 * @author artur.opala
 */
object OperationN {
	
  def unapply(o:OperationN) = Some(Seq(o.args:_*),o.operator)
	
}