package org.encalmo.expression

/**
 * Multi-argument (with number of arguments usually greater than 2) infix operation trait
 * @author artur.opala
 */
trait MultipleInfixOperation extends OperationN with PrimitiveOperation

/**
 * Multi-argument sum operation
 */
case class MultiSum(args:Expression*) extends MultipleInfixOperation {
	
	override def calculate(v:Value*):Expression = {
			val ps = v.partition(_.isInstanceOf[Number])
			val result:Number = new Number(ps._1.map(_.asInstanceOf[Number].r.d).reduceLeft[Double]((b,a) => b+a))
			if(ps._2.isEmpty){
				result
			}else{
				MultiSum((ps._2.:+(result)):_*)
			}
	}
	override def copy(x:Expression*):MultiSum = MultiSum(x:_*)
	override val operator = "+"
		
	override def + (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ZERO.eq(e) => this; 
	  	case Sum(l,r) => MultiSum((args.:+(l).:+(r)):_*); 
	  	case MultiSum(seq@_*) => MultiSum((args++seq):_*)
	  	case _ => MultiSum((args.:+(e)):_*)
	}
}

/**
 * Multi-argument multiplication operation
 */
case class MultiProd(args:Expression*) extends MultipleInfixOperation {
	
	override def calculate(v:Value*):Expression = {
		val ps = v.partition(_.isInstanceOf[Number])
		val result:Number = new Number(ps._1.map(_.asInstanceOf[Number].r.d).reduceLeft[Double]((b,a) => b*a))
		if(ps._2.isEmpty){
			result
		}else{
			MultiSum((ps._2.:+(result)):_*)
		}
	}
	override def copy(x:Expression*):MultiProd = MultiProd(x:_*)
	override val operator = "*"
		
	override def * (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ZERO.eq(e) => this; 
	  	case Prod(l,r) => MultiProd((args.:+(l).:+(r)):_*); 
	  	case MultiProd(seq@_*) => MultiProd((args++seq):_*)
	  	case _ => MultiProd((args.:+(e)):_*)
	}
}