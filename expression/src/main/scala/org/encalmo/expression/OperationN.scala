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
  override val children:Seq[Expression] = args
	
  /**
   * Real operation execution
   * @param r number
   * @return
   */
  def calculate(v:Value*):Expression
  
  /**
   * Operation copy with exchanged parameters
   */
  def copy(e:Expression*):OperationN
	
  final override def eval():Expression = {
	  val ps = args.map(_.eval).partition(_.isInstanceOf[Value])
	  if(ps._1.isEmpty){
	 	  if(args.sameElements(ps._2)){
	 	 	  this // returns this if none argument has been transformed by eval
	 	  }else{
	 		  copy(ps._2:_*) // returns copy with transformed arguments
	 	  }
	  }else{
		  val result:Expression = calculate(ps._1.map(_.asInstanceOf[Value]):_*)
		  if(ps._2.isEmpty){
		 	  result // returns only evaluation
		  } else {
		 	  val newargs = ps._2.+:(result)
		 	  if(args.sameElements(newargs)){
		 	 	  this // returns this if none argument has been transformed by eval and calculate
		 	  }else{
		 		  copy(newargs:_*) // returns mix of both
		 	  }
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