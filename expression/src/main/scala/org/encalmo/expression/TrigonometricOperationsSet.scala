package org.encalmo.expression

trait TrigonometricOperation extends Operation1 with NamedOperation

case class sin(e:Expression) extends TrigonometricOperation{
  override def doReal(r:Real):Real = r.sin
  override def doCopy(e:Expression) = sin(e)
  override def operator = "sin"
}

case class cos(e:Expression) extends TrigonometricOperation{
  override def doReal(r:Real):Real = r.cos
  override def doCopy(e:Expression) = cos(e)
  override def operator = "cos"
}

case class tan(e:Expression) extends TrigonometricOperation{
  override def doReal(r:Real):Real = r.tan
  override def doCopy(e:Expression) = tan(e)
  override def operator = "tan"
}

case class cot(e:Expression) extends TrigonometricOperation{
  override def doReal(r:Real):Real = r.cot
  override def doCopy(e:Expression) = cot(e)
  override def operator = "cot"
}

trait InverseTrigonometricOperation extends Operation1 with NamedOperation

case class arcsin(e:Expression) extends InverseTrigonometricOperation{
  override def doReal(r:Real):Real = r.arcsin
  override def doCopy(e:Expression) = arcsin(e)
  override def operator = "arcsin"
}

case class arccos(e:Expression) extends InverseTrigonometricOperation{
  override def doReal(r:Real):Real = r.arccos
  override def doCopy(e:Expression) = arccos(e)
  override def operator = "arccos"
}

case class arctan(e:Expression) extends InverseTrigonometricOperation{
  override def doReal(r:Real):Real = r.arctan
  override def doCopy(e:Expression) = arctan(e)
  override def operator = "arctan"
}

case class arccot(e:Expression) extends InverseTrigonometricOperation{
  override def doReal(r:Real):Real = r.arccot
  override def doCopy(e:Expression) = arccot(e)
  override def operator = "arccot"
}

trait HyperbolicOperation extends Operation1 with NamedOperation

case class sinh(e:Expression) extends HyperbolicOperation{
  override def doReal(r:Real):Real = r.sinh
  override def doCopy(e:Expression) = sinh(e)
  override def operator = "sinh"
}

case class cosh(e:Expression) extends HyperbolicOperation{
  override def doReal(r:Real):Real = r.cosh
  override def doCopy(e:Expression) = cosh(e)
  override def operator = "cosh"
}

case class tanh(e:Expression) extends HyperbolicOperation{
  override def doReal(r:Real):Real = r.tanh
  override def doCopy(e:Expression) = tanh(e)
  override def operator = "tanh"
}

case class coth(e:Expression) extends HyperbolicOperation{
  override def doReal(r:Real):Real = r.coth
  override def doCopy(e:Expression) = coth(e)
  override def operator = "coth"
}