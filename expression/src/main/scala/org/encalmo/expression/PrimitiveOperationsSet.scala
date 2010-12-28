package org.encalmo.expression

/*
 * Primitive operations set
 */

case class Sum(val l:Expression,val r:Expression) extends InfixOperation {
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr+rr)
	  case _ => copy(lv,rv)
  }
  override def copy(l:Expression,r:Expression) = Sum(l,r)
  override val operator = "+"
  override val precedence = 1
}

case class Diff(l:Expression,r:Expression) extends InfixOperation {
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr-rr)
	  case _ => copy(lv,rv)
  }
  override def copy(l:Expression,r:Expression) = Diff(l,r)
  override val operator = "-"
  override val precedence = 2
}

case class Prod(l:Expression,r:Expression) extends InfixOperation  {
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr*rr)
	  case _ => copy(lv,rv)
  }
  override def copy(l:Expression,r:Expression) = Prod(l,r)
  override val operator = "*"
  override val precedence = 10
}

case class Quot(l:Expression,r:Expression) extends InfixOperation  {
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr/rr)
	  case _ => copy(lv,rv)
  }
  override def copy(l:Expression,r:Expression) = Quot(l,r)
  override val operator = "/"
  override val precedence = 11
}

case class Modulo(l:Expression,r:Expression) extends InfixOperation  {
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr%rr)
	  case _ => copy(lv,rv)
  }
  override def copy(l:Expression,r:Expression) = Modulo(l,r)
  override val operator = "%"
  override val precedence = 12
}

case class Neg(e:Expression) extends PrefixOperation  {
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(-r)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = Neg(e)
  override val operator = "-"
  override val precedence = 50
}

case class Power(l:Expression,r:Expression) extends InfixOperation  {
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr^rr)
	  case _ => copy(lv,rv)
  }
  override def copy(l:Expression,r:Expression) = Power(l,r)
  override def operator = "^"
  override val precedence = 50
}