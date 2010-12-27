package org.encalmo.expression

/*
 * Primitive operations set
*/

case class Sum(val l:Expression,val r:Expression) extends InfixOperation {
  override def doReal(lr:Real,rr:Real):Real = lr+rr
  override def doCopy(l:Expression,r:Expression) = Sum(l,r)
  override def operator = "+"
  override def precedence = 1
}

case class Diff(l:Expression,r:Expression) extends InfixOperation {
  override def doReal(lr:Real,rr:Real):Real = lr-rr
  override def doCopy(l:Expression,r:Expression) = Diff(l,r)
  override def operator = "-"
  override def precedence = 2
}

case class Prod(l:Expression,r:Expression) extends InfixOperation  {
  override def doReal(lr:Real,rr:Real):Real = lr*rr
  override def doCopy(l:Expression,r:Expression) = Prod(l,r)
  override def operator = "*"
  override def precedence = 10
}

case class Quot(l:Expression,r:Expression) extends InfixOperation  {
  override def doReal(lr:Real,rr:Real):Real = lr/rr
  override def doCopy(l:Expression,r:Expression) = Quot(l,r)
  override def operator = "/"
  override def precedence = 11
}

case class Modulo(l:Expression,r:Expression) extends InfixOperation  {
  override def doReal(lr:Real,rr:Real):Real = lr%rr
  override def doCopy(l:Expression,r:Expression) = Modulo(l,r)
  override def operator = "%"
  override def precedence = 12
}

case class Neg(e:Expression) extends PrefixOperation  {
  override def doReal(r:Real):Real = -r
  override def doCopy(e:Expression) = Neg(e)
  override def operator = "-"
  override def precedence = 50
}

case class Power(l:Expression,r:Expression) extends InfixOperation  {
  override def doReal(lr:Real,rr:Real):Real = lr^rr
  override def doCopy(l:Expression,r:Expression) = Power(l,r)
  override def operator = "^"
  override def precedence = 50
}