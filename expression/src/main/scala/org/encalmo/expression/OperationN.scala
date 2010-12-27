package org.encalmo.expression

trait OperationN extends Operation {
	
  def args:Seq[Expression]
	
  /**
   * Real operation execution
   * @param r number
   * @return
   */
  def doReal(r:Real*):Real
  
  /**
   * Operation copy with exchanged parameters
   * @param e
   * @return
   */
  def doCopy(e:Expression*):OperationN
	
  final override def eval():Expression = {
	  val r = args.map(_.eval).partition(_.isInstanceOf[Number])
	  if(r._2.isEmpty)
	 	  Number(doReal(r._1.map(_.asInstanceOf[Number].r):_*))
	  else
	 	  this
  }
  
  final override def map(f:Transformation):Expression = {
	  val m = args.map(e => e.map(f))
	  if(args.zip(m).forall(t => t._1 eq t._2)){
	 	  f(this)
	  }else{
	 	  f(doCopy(m:_*))
	  }
  }
  
  final override def travel(parent:Node = null, t:Traveler, position:Int=0):Unit = {
	  val n = Node(parent,this,position)
	  t.onEnter(n)
	  var i=0
	  var pe:Expression = null
	  args.foreach(e => {
	 	  if(i>0){
	 	 	  t.onBetweenChildren(n,pe,e)
	 	  }
	 	  t.onBeforeChildEnter(n,i,e)
	 	  e.travel(n,t,i)
	 	  t.onAfterChildExit(n,i,e)
	 	  i = i+1
	 	  pe = e
	  })
	  t.onExit(n)
  }
  
}

object OperationN {
  def unapply(o:OperationN) = Some((o.args,o.operator))
}