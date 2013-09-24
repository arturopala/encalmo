package org.encalmo.expression

/** Operation selecting one of many arguments */
trait SelectOneOperation extends OperationN
/** Operation aggregating all arguments */
trait AggregateOperation extends OperationN

/**
 * Operation with N-arguments
 * (usually more than two)
 * @author artur.opala
 */
trait OperationN extends Operation {
	
  def args:Seq[Expression]
  
  /** Children expressions */
  override val children:Seq[Expression] = args
  
  /** Makes copy of this operation type with provided arguments */
  def copy(e:Expression*):OperationN

  override def face = operator.name + "(" + args.foldLeft("")((s,e) => if(!s.isEmpty) s + "," + e.face else e.face) + ")"
	
  final override def eval():Expression = {
	  val ps = args.map(_.eval()).partition(_.isInstanceOf[Value])
	  if(ps._1.isEmpty){ // if there is not a single Value
	 	  if(args sameElements ps._2){
	 	 	  this // returns this if none argument has been transformed by eval
	 	  }else{
	 		  copy(ps._2:_*) // returns copy with transformed arguments
	 	  }
	  }else{
		  val result:Expression = calculate(ps._1.map(_.asInstanceOf[Value]):_*)
		  if(ps._2.isEmpty){ //if only Value
		 	  result // returns calculated result
		  } else {
		 	  val newargs = ps._2.+:(result)
		 	  if(args sameElements newargs){
		 	 	  this // returns this if none argument has been transformed by eval and calculate
		 	  }else{
		 		  copy(newargs:_*) // returns mix of both
		 	  }
		  }
	  }
  }
  
  /** Default calculate implementation invokes reduceLeft with Value#calculate(left,right) */ 
  def calculate(v:Value*):Expression = {
      v.reduceLeft[Value]((v1,v2) => Value.calculate(operator,v1,v2).getOrElse(throw new IllegalArgumentException("("+v1+","+v2+")")))
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