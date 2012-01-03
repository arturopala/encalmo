package org.encalmo.expression
import java.text.DecimalFormatSymbols

object Real {

  def apply(d: Double): Real = {
    if (d == 0) zero
    else if (d == 1) one
    else if (d == Math.Pi) pi
    else if (d == Math.E) e
    else new Real(d)
  }

  def unapply(r: Real) = Some(r.d)

  val zero = new Real(0)
  val one = new Real(1)
  val pi = new Real(Math.Pi)
  val e = new Real(Math.E)

  private def getFormat(locale: java.util.Locale): java.text.DecimalFormat = {
    val format = java.text.NumberFormat.getNumberInstance(locale).asInstanceOf[java.text.DecimalFormat]
    format.setDecimalSeparatorAlwaysShown(false)
    format
  }

  val defaultFormat: java.text.DecimalFormat = getFormat(java.util.Locale.getDefault)
  
  val stringFormat = new java.text.DecimalFormat("0.###############",new java.text.DecimalFormatSymbols(java.util.Locale.ENGLISH))

  def format(r: Real): String = defaultFormat.format(r.d)

  def format(r: Real, format: java.text.NumberFormat): String = format.format(r.d)

  def format(r: Real, locale: java.util.Locale): String = getFormat(locale).format(r.d)

  def format(r: Real, pattern: String, locale: java.util.Locale): String = {
    val format = getFormat(locale)
    format.applyPattern(pattern);
    format.format(r.d)
  }
}

/**
 * Real type is a real number facade
 * @author artur.opala
 */
class Real(val d: Double) {

  def double: Double = d

  def +(r: Real): Real = Real(d + r.d)
  def -(r: Real): Real = Real(d - r.d)
  def *(r: Real): Real = Real(d * r.d)
  def /(r: Real): Real = Real(d / r.d)
  def %(r: Real): Real = Real(d % r.d)
  def unary_-(): Real = Real(-d)
  def ^(r: Real): Real = Real(Math.pow(d, r.d))
  def abs: Real = Real(Math.abs(d))
  def sin: Real = Real(Math.sin(d))
  def cos: Real = Real(Math.cos(d))
  def tan: Real = Real(Math.tan(d))
  def cot: Real = Real(1 / Math.tan(d))
  def sinh: Real = Real(java.lang.Math.sinh(d))
  def cosh: Real = Real(java.lang.Math.cosh(d))
  def tanh: Real = Real(java.lang.Math.tanh(d))
  def coth: Real = Real(1 / java.lang.Math.tanh(d))
  def arcsin: Real = Real(Math.asin(d))
  def arccos: Real = Real(Math.acos(d))
  def arctan: Real = Real(Math.atan(d))
  def arccot: Real = Real(Math.Pi / 2 - Math.atan(d))
  def min(r: Real): Real = Real(Math.min(d, r.d))
  def max(r: Real): Real = Real(Math.max(d, r.d))
  def avg(r: Real): Real = Real((d + r.d) / 2)
  def sqrt: Real = Real(Math.sqrt(d))
  def cbrt: Real = Real(java.lang.Math.cbrt(d))
  def hypot(r: Real): Real = Real(java.lang.Math.hypot(d, r.d))
  def root(r: Real): Real = Real(java.lang.Math.pow(d, 1 / r.d))
  def exp: Real = Real(java.lang.Math.exp(d))
  def expm1: Real = Real(java.lang.Math.expm1(d))
  def ln: Real = Real(java.lang.Math.log(d))
  def log: Real = Real(java.lang.Math.log10(d))
  def log1p: Real = Real(java.lang.Math.log1p(d))
  def <(r: Real): Boolean = this.d < r.d
  def >(r: Real): Boolean = this.d > r.d
  def >=(r: Real): Boolean = this.d >= r.d
  def <=(r: Real): Boolean = this.d <= r.d
  def <>(r: Real): Boolean = this.d != r.d
  def <(r: Int): Boolean = this.d < r
  def >(r: Int): Boolean = this.d > r
  def >=(r: Int): Boolean = this.d >= r
  def <=(r: Int): Boolean = this.d <= r
  def <>(r: Int): Boolean = this.d != r
  def isNegative: Boolean = d < 0
  def isPositive: Boolean = d > 0
  def isNonNegative: Boolean = d >= 0
  def isNonPositive: Boolean = d <= 0
  def isZero: Boolean = d == 0
  def rad: Real = Real(Math.toRadians(d))
  def deg: Real = Real(Math.toDegrees(d))
  def isInt = (d.toInt == d)
  def inverse:Real = {
      val l = java.lang.Math.log10(d)
      if(Real(l).isInt) Real(Math.pow(10,-l))
      else Real(1/d)
  }
  
  def toInt:Int = d.toInt
  def toLong:Long = d.toLong

  def absInt: Int = Math.floor(Math.abs(d)).toInt

  /**
   * Equals two real numbers if they differs no more then 0.00000001
   */
  override def equals(a: Any): Boolean = a match {
    case r: Real => {
      this.eq(r) || r.d == d || (Math.abs(r.d - d) < 0.00000001) || (d==Double.NaN && r.d==Double.NaN) || (d==Double.NegativeInfinity && r.d==Double.NegativeInfinity) || (d==Double.PositiveInfinity && r.d==Double.PositiveInfinity) 
    }
    case _ => false
  }

  def convert(u1: UnitOfValue, u2: UnitOfValue): Real = if(u1.multiplier==u2.multiplier) this else Real(d * u1.multiplier/u2.multiplier)

  override def toString = Real.stringFormat.format(d)
}