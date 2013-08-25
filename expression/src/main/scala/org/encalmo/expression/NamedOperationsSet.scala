package org.encalmo.expression

/**
 * Square root operation
 */
case class sqrt(e:Expression) extends Operation1 with NamedOperation {

  override def copy(x:Expression) = sqrt(x)
  override val operator = "sqrt"
}

/**
 * Cube root operation
 */
case class cbrt(e:Expression) extends Operation1 with NamedOperation {

  override def copy(e:Expression) = cbrt(e)
  override val operator = "cbrt"
}

/**
 * Free root operation
 */
case class root(l:Expression,r:Expression) extends Operation2 with NamedOperation {

  override def copy(l:Expression,r:Expression) = root(l,r)
  override val operator = "root"
}

/**
 * Minimum function
 */
case class min(args:Expression*) extends OperationN with NamedOperation {

  override def copy(x:Expression*):min = min(x:_*)
  override val operator = "min"
}

/**
 * Maximum function
 */
case class max(args:Expression*) extends OperationN with NamedOperation {

  override def copy(x:Expression*) = max(x:_*)
  override val operator = "max"
}

/**
 *
 * @author artur.opala
 */
case class hypot(l:Expression,r:Expression) extends Operation2 with NamedOperation  {

  override def copy(le:Expression,re:Expression) = hypot(le,re)
  override val operator = "hypot"
      
  def expand:Expression = sqrt(square(l)+square(r))
}

/**
 * Exponent function
 */
case class exp(e:Expression) extends Operation1 with NamedOperation {

  override def copy(e:Expression) = exp(e)
  override val operator = "exp"
}

/**
 * Natural logarithm (base is e) function
 */
case class ln(e:Expression) extends Operation1 with NamedOperation {

  override def copy(e:Expression) = ln(e)
  override val operator = "ln"
}

/**
 * Common logarithm (base is 10) function
 */
case class log(e:Expression) extends Operation1 with NamedOperation {

  override def copy(e:Expression) = log(e)
  override val operator = "log"
}

/**
 * Rounding operation
 * default strategy: RoundingMode.HALF
 */
case class round(e:Expression,rm:RoundingMode = RoundingMode.HALF) extends Operation1 with NamedOperation with Transparent {
	
  override def calculate(v:Value):Expression = v match {
	  case number: Number => {
          val rounded: Double = rm.round(number.r.d)
          if(number.r.sameValue(rounded)){
              number
          } else {
              val n = Number(Real(rounded),number.unit)
              n.isRounded = true
              n.original = Some(number.r)
              n
          }

	  }
	  case _ => copy(v)
  }
  override def copy(x:Expression) = round(x,rm)
  override val operator = "~"
}

/**
 * Absolute value operation
 */
case class abs(e:Expression) extends Operation1 with NamedOperation {

  override def copy(e:Expression) = abs(e)
  override val operator = "abs"
}