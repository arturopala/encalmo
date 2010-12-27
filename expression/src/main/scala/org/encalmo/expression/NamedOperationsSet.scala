package org.encalmo.expression

/*
 * Named operations set
 */

/**
 * Square root operation
 */
case class sqrt(e:Expression) extends Operation1 with NamedOperation {
	
  override def doReal(r:Real):Real = r.sqrt
  override def doCopy(x:Expression) = sqrt(x)
  override def operator = "sqrt"
}

/**
 * Cube root operation
 */
case class cbrt(e:Expression) extends Operation1 with NamedOperation {
	
  override def doReal(r:Real):Real = r.cbrt
  override def doCopy(e:Expression) = cbrt(e)
  override def operator = "cbrt"
}

/**
 * Free root operation
 */
case class root(l:Expression,r:Expression) extends Operation2 with NamedOperation {
	
  override def doReal(lr:Real,rr:Real):Real = lr.root(rr)
  override def doCopy(l:Expression,r:Expression) = root(l,r)
  override def operator = "root"
}

/**
 * Minimum function
 */
case class min(args:Expression*) extends OperationN with NamedOperation {
	
  override def doReal(r:Real*):Real = r.reduceLeft[Real]((b,a) => Real(Math.min(b.d,a.d)))
  override def doCopy(x:Expression*) = min(x:_*)
  override def operator = "min"
}

/**
 * Maximum function
 */
case class max(args:Expression*) extends OperationN with NamedOperation {
	
  override def doReal(r:Real*):Real = r.reduceLeft[Real]((b,a) => Real(Math.max(b.d,a.d)))
  override def doCopy(x:Expression*) = max(x:_*)
  override def operator = "max"
}

/**
 * {@link java.lang.Math}
 * @author artur.opala
 */
case class hypot(l:Expression,r:Expression) extends Operation2 with NamedOperation  {
  
  override def doReal(lr:Real,rr:Real):Real = lr hypot rr
  override def doCopy(le:Expression,re:Expression) = hypot(le,re)
  override def operator = "hypot"
}

/**
 * Exponent function
 */
case class exp(e:Expression) extends Operation1 with NamedOperation {
	
  override def doReal(r:Real):Real = r.exp
  override def doCopy(e:Expression) = exp(e)
  override def operator = "exp"
}

/**
 * Natural logarithm (base is e) function
 */
case class ln(e:Expression) extends Operation1 with NamedOperation {
	
  override def doReal(r:Real):Real = r.ln
  override def doCopy(e:Expression) = ln(e)
  override def operator = "ln"
}

/**
 * Common logarithm (base is 10) function
 */
case class log(e:Expression) extends Operation1 with NamedOperation {
	
  override def doReal(r:Real):Real = r.log
  override def doCopy(e:Expression) = log(e)
  override def operator = "log"
}