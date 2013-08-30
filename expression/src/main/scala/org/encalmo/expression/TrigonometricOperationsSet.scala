package org.encalmo.expression

trait TrigonometricOperation extends Operation1 with NamedOperation

case class sin(expression:Expression) extends TrigonometricOperation{
  override def copy(e:Expression) = sin(e)
  override val operator = "sin"
}

case class cos(expression:Expression) extends TrigonometricOperation{
  override def copy(e:Expression) = cos(e)
  override val operator = "cos"
}

case class tan(expression:Expression) extends TrigonometricOperation{
  override def copy(e:Expression) = tan(e)
  override val operator = "tan"
}

case class cot(expression:Expression) extends TrigonometricOperation{
  override def copy(e:Expression) = cot(e)
  override val operator = "cot"
}

trait InverseTrigonometricOperation extends Operation1 with NamedOperation

case class arcsin(expression:Expression) extends InverseTrigonometricOperation{
  override def copy(e:Expression) = arcsin(e)
  override val operator = "arcsin"
}

case class arccos(expression:Expression) extends InverseTrigonometricOperation{
  override def copy(e:Expression) = arccos(e)
  override val operator = "arccos"
}

case class arctan(expression:Expression) extends InverseTrigonometricOperation{
  override def copy(e:Expression) = arctan(e)
  override val operator = "arctan"
}

case class arccot(expression:Expression) extends InverseTrigonometricOperation{
  override def copy(e:Expression) = arccot(e)
  override val operator = "arccot"
}

trait HyperbolicOperation extends Operation1 with NamedOperation

case class sinh(expression:Expression) extends HyperbolicOperation{
  override def copy(e:Expression) = sinh(e)
  override val operator = "sinh"
}

case class cosh(expression:Expression) extends HyperbolicOperation{
  override def copy(e:Expression) = cosh(e)
  override val operator = "cosh"
}

case class tanh(expression:Expression) extends HyperbolicOperation{
  override def copy(e:Expression) = tanh(e)
  override val operator = "tanh"
}

case class coth(expression:Expression) extends HyperbolicOperation{
  override def copy(e:Expression) = coth(e)
  override val operator = "coth"
}