package org.encalmo.expression

/**
 * Sum operation
 */
case class Sum(args:Expression*) extends MultipleInfixOperation {
	
	override def calculate(v:Value*):Expression = {
			val ps = v.partition(_.isInstanceOf[Number])
			if(ps._1.isEmpty){
				this
			}else{
				val result:Number = Number(Real(ps._1.map(_.asInstanceOf[Number].r.d).reduceLeft[Double]((b,a) => b+a)))
				if(ps._2.isEmpty){
					result
				}else{
					Sum((ps._2.:+(result)):_*)
				}
			}
	}
	
	override def toString = "Sum("+args.mkString(",")+")"
	
	override def copy(x:Expression*):Sum = Sum(x:_*)
	override val operator = "+"
	override val precedence = 1
		
	override def + (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ZERO.eq(e) => this; 
	  	case Sum(seq@_*) => Sum((args++seq):_*)
	  	case _ => Sum((args.:+(e)):_*)
	}
}

/**
 * Multiplication operation
 * @author artur.opala
 */
case class Prod(args:Expression*) extends MultipleInfixOperation {
	
	override def calculate(v:Value*):Expression = {
			val ps = v.partition(_.isInstanceOf[Number])
			if(ps._1.isEmpty){
				this
			}else{
				val result:Number = Number(Real(ps._1.map(_.asInstanceOf[Number].r.d).reduceLeft[Double]((b,a) => b*a)))
				if(ps._2.isEmpty){
					result
				}else{
					Prod((ps._2.:+(result)):_*)
				}
			}
	}
	
	override def toString = "Prod("+args.mkString(",")+")"
	
	override def copy(x:Expression*):Prod = Prod(x:_*)
	override val operator = "*"
	override val precedence = 11
		
	override def * (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ZERO.eq(e) => this; 
	  	case Prod(seq@_*) => Prod((args++seq):_*)
	  	case _ => Prod((args.:+(e)):_*)
	}
}

/**
 * Difference operation
 */
case class Diff(l: Expression, r: Expression) extends InfixOperation {
	override def calculate(lv: Value, rv: Value): Expression = (lv, rv) match {
	case (Number(lr), Number(rr)) => Number(lr - rr)
	case _ => copy(lv, rv)
	}
	override def copy(l: Expression, r: Expression) = Diff(l, r)
	override val operator = "-"
	override val precedence = 2
}

/**
 * Division operation
 */
case class Quot(l: Expression, r: Expression) extends InfixOperation {
	override def calculate(lv: Value, rv: Value): Expression = (lv, rv) match {
	case (Number(lr), Number(rr)) => Number(lr / rr)
	case _ => copy(lv, rv)
	}
	override def copy(l: Expression, r: Expression) = Quot(l, r)
	override val operator = "/"
	override val precedence = 11
}

/**
 * Modulo operation
 */
case class Modulo(l: Expression, r: Expression) extends InfixOperation {
	override def calculate(lv: Value, rv: Value): Expression = (lv, rv) match {
	case (Number(lr), Number(rr)) => Number(lr % rr)
	case _ => copy(lv, rv)
	}
	override def copy(l: Expression, r: Expression) = Modulo(l, r)
	override val operator = "%"
		override val precedence = 12
}

/**
 * Negation operation
 */
case class Neg(e: Expression) extends PrefixOperation {
	override def calculate(v: Value): Expression = v match {
	case Number(r) => Number(-r)
	case _ => copy(v)
	}
	override def copy(e: Expression) = Neg(e)
	override val operator = "-"
	override val precedence = 50
}

/**
 * Power operation
 */
case class Power(l: Expression, r: Expression) extends InfixOperation {
	override def calculate(lv: Value, rv: Value): Expression = (lv, rv) match {
		case (Number(lr), Number(rr)) => Number(lr ^ rr)
		case _ => copy(lv, rv)
	}
	override def copy(l: Expression, r: Expression) = Power(l, r)
	override def operator = "^"
	override val precedence = 50
}