package org.encalmo.expression

/**
 * Sum operation
 */
case class Sum(val l: Expression, val r: Expression) extends InfixOperation {
	override def calculate(lv: Value, rv: Value): Expression = (lv, rv) match {
		case (Number(lr), Number(rr)) => Number(lr + rr)
		case _ => copy(lv, rv)
	}
	override def copy(l: Expression, r: Expression) = Sum(l, r)
	override val operator = "+"
	override val precedence = 1
	
	/*override def + (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ZERO.eq(e) => this; 
	  	case Sum(l,r) => MultiSum(this.l,this.r,l,r); 
	  	case MultiSum(seq@_*) => MultiSum(seq.+:(r).+:(l):_*)
	  	case _ => MultiSum(l,r,e)
	}*/
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
 * Multiplication operation
 */
case class Prod(l: Expression, r: Expression) extends InfixOperation {
	override def calculate(lv: Value, rv: Value): Expression = (lv, rv) match {
	case (Number(lr), Number(rr)) => Number(lr * rr)
	case _ => copy(lv, rv)
	}
	override def copy(l: Expression, r: Expression) = Prod(l, r)
	override val operator = "*"
	override val precedence = 10
	
	/*override def * (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ZERO.eq(e) => this; 
	  	case Prod(l,r) => MultiProd(this.l,this.r,l,r); 
	  	case MultiProd(seq@_*) => MultiProd(seq.+:(r).+:(l):_*)
	  	case _ => MultiProd(l,r,e)
	}*/
}

object Prod {
	
	def apply(pl: Prod, r: Expression) = MultiProd(pl.l,pl.r,r)
	def apply(pl: Prod, pr: Prod) = MultiProd(pl.l,pl.r,pr.l,pr.r)
	def apply(l: Expression, pr: Prod) = MultiProd(l,pr.l,pr.r)
	
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