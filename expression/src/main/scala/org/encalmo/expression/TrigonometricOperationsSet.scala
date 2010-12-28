package org.encalmo.expression

trait TrigonometricOperation extends Operation1 with NamedOperation

case class sin(e:Expression) extends TrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.sin)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = sin(e)
  override val operator = "sin"
}

case class cos(e:Expression) extends TrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.cos)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = cos(e)
  override val operator = "cos"
}

case class tan(e:Expression) extends TrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.tan)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = tan(e)
  override val operator = "tan"
}

case class cot(e:Expression) extends TrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.cot)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = cot(e)
  override val operator = "cot"
}

trait InverseTrigonometricOperation extends Operation1 with NamedOperation

case class arcsin(e:Expression) extends InverseTrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.arcsin)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = arcsin(e)
  override val operator = "arcsin"
}

case class arccos(e:Expression) extends InverseTrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.arccos)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = arccos(e)
  override val operator = "arccos"
}

case class arctan(e:Expression) extends InverseTrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.arctan)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = arctan(e)
  override val operator = "arctan"
}

case class arccot(e:Expression) extends InverseTrigonometricOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.arccot)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = arccot(e)
  override val operator = "arccot"
}

trait HyperbolicOperation extends Operation1 with NamedOperation

case class sinh(e:Expression) extends HyperbolicOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.sinh)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = sinh(e)
  override val operator = "sinh"
}

case class cosh(e:Expression) extends HyperbolicOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.cosh)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = cosh(e)
  override val operator = "cosh"
}

case class tanh(e:Expression) extends HyperbolicOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.tanh)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = tanh(e)
  override val operator = "tanh"
}

case class coth(e:Expression) extends HyperbolicOperation{
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.coth)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = coth(e)
  override val operator = "coth"
}