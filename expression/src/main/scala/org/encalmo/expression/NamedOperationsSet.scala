package org.encalmo.expression

/**
 * Square root operation
 */
case class sqrt(e:Expression) extends Operation1 with NamedOperation {
	
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.sqrt)
	  case _ => copy(v)
  }
  override def copy(x:Expression) = sqrt(x)
  override val operator = "sqrt"
}

/**
 * Cube root operation
 */
case class cbrt(e:Expression) extends Operation1 with NamedOperation {
	
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.cbrt)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = cbrt(e)
  override val operator = "cbrt"
}

/**
 * Free root operation
 */
case class root(l:Expression,r:Expression) extends Operation2 with NamedOperation {
	
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr.root(rr))
	  case _ => copy(lv,rv)
  }
  override def copy(l:Expression,r:Expression) = root(l,r)
  override val operator = "root"
}

/**
 * Minimum function
 */
case class min(args:Expression*) extends OperationN with NamedOperation {
	
  override def calculate(v:Value*):Expression = {
	  if(v.forall(_.isInstanceOf[Number]))
	 	  new Number(v.map(_.asInstanceOf[Number].r.d).reduceLeft[Double]((b,a) => Math.min(a,b)))
	  else
	 	  copy(v:_*)
  }
  override def copy(x:Expression*):min = min(x:_*)
  override val operator = "min"
}

/**
 * Maximum function
 */
case class max(args:Expression*) extends OperationN with NamedOperation {
	
  override def calculate(v:Value*):Expression = {
	  if(v.forall(_.isInstanceOf[Number]))
	 	  new Number(v.map(_.asInstanceOf[Number].r.d).reduceLeft[Double]((b,a) => Math.max(a,b)))
	  else
	 	  copy(v:_*)
  }
  override def copy(x:Expression*) = max(x:_*)
  override val operator = "max"
}

/**
 * {@link java.lang.Math}
 * @author artur.opala
 */
case class hypot(l:Expression,r:Expression) extends Operation2 with NamedOperation  {
  
  override def calculate(lv:Value,rv:Value):Expression = (lv,rv) match {
	  case (Number(lr),Number(rr)) => Number(lr.hypot(rr))
	  case _ => copy(lv,rv)
  }
  override def copy(le:Expression,re:Expression) = hypot(le,re)
  override val operator = "hypot"
}

/**
 * Exponent function
 */
case class exp(e:Expression) extends Operation1 with NamedOperation {
	
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.exp)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = exp(e)
  override val operator = "exp"
}

/**
 * Natural logarithm (base is e) function
 */
case class ln(e:Expression) extends Operation1 with NamedOperation {
	
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.ln)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = ln(e)
  override val operator = "ln"
}

/**
 * Common logarithm (base is 10) function
 */
case class log(e:Expression) extends Operation1 with NamedOperation {
	
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.log)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = log(e)
  override val operator = "log"
}

/**
 * Rounding operation
 * default strategy: RoundingMode.CEILING
 */
case class round(e:Expression,rm:RoundingMode = RoundingMode.HALF) extends Operation1 with NamedOperation {
	
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(rm.round(r.d))
	  case _ => copy(v)
  }
  override def copy(x:Expression) = round(x,rm)
  override val operator = "~"
}

/**
 * Absolute value operation
 */
case class abs(e:Expression) extends Operation1 with NamedOperation {
	
  override def calculate(v:Value):Expression = v match {
	  case Number(r) => Number(r.abs)
	  case _ => copy(v)
  }
  override def copy(e:Expression) = abs(e)
  override val operator = "abs"
}