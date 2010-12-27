package org.encalmo.expression

object Real{
	
  def apply(d:Double):Real = {
	  if(d==0) zero
	  else if(d==1) one
	  else if(d==Math.Pi) pi
	  else if(d==Math.E) e
	  else new Real(d)
  }
  
  def unapply(r:Real) = Some(r.d)
	
  val zero = new Real(0)
  val one = new Real(1)
  val pi = new Real(Math.Pi)
  val e = new Real(Math.E)
	
  /*val f0:java.text.DecimalFormat = new java.text.DecimalFormat("0")
  val f1:java.text.DecimalFormat = new java.text.DecimalFormat("0.#")
  val f2:java.text.DecimalFormat = new java.text.DecimalFormat("0.##")
  val f3:java.text.DecimalFormat = new java.text.DecimalFormat("0.###")
  val f4:java.text.DecimalFormat = new java.text.DecimalFormat("0.####")
  
  def format(r:Real):(String,String) = r.d match {
      case d if d<1E-12 => (f3.format(d*1E12),"-12")
      case d if d<1E-9 => (f3.format(d*1E9),"-9")
      case d if d<1E-6 => (f3.format(d*1E6),"-6")
      case d if d<1E-5 => (f3.format(d*1E5),"-5")
      case d if d<1E-4 => (f3.format(d*1E4),"-4")
      case d if d<1E-3 => (f3.format(d*1E3),"-3")
      case d if d<0 => (f3.format(d),null)
      case d if d>1E9 => (f3.format(d/1E9),"9")
      case d if d>1E6 => (f3.format(d/1E6),"6")
      case d if d>1E5 => (f3.format(d/1E5),"5")
      case d if d>1E4 => (f3.format(d/1E4),"4")
      case d if d>1E3 => (f3.format(d/1E3),"3")
      case _ => (f4.format(r.d),null)
  }*/
  
  private def getFormat(locale:java.util.Locale):java.text.DecimalFormat = {
	  val format = java.text.NumberFormat.getNumberInstance(locale).asInstanceOf[java.text.DecimalFormat]
	  format.setDecimalSeparatorAlwaysShown(false)
	  format
  }
  
  val defaultFormat:java.text.DecimalFormat = getFormat(java.util.Locale.getDefault)
  
  def format(r:Real):String = defaultFormat.format(r.d)
  
  def format(r:Real,format:java.text.NumberFormat):String = format.format(r.d)
  
  def format(r:Real,locale:java.util.Locale):String = getFormat(locale).format(r.d)
  
  def format(r:Real,pattern:String,locale:java.util.Locale):String = {
	  val format = getFormat(locale)
	  format.applyPattern(pattern);
	  format.format(r.d)
  }
}

/**
 * Real type is a real number facade
 * @author artur.opala
 */
class Real(val d:Double){
	
  def double:Double = d
  
  def + (r:Real):Real = Real(d + r.d)
  def - (r:Real):Real = Real(d - r.d)
  def * (r:Real):Real = Real(d * r.d)
  def / (r:Real):Real = Real(d / r.d)
  def % (r:Real):Real = Real(d % r.d)
  def unary_-():Real = Real(-d)
  def ^ (r:Real):Real = Real(Math.pow(d,r.d))
  def abs:Real = Real(Math.abs(d))
  def sin:Real = Real(Math.sin(d))
  def cos:Real = Real(Math.cos(d))
  def tan:Real = Real(Math.tan(d))
  def cot:Real = Real(1/Math.tan(d))
  def sinh:Real = Real(java.lang.Math.sinh(d))
  def cosh:Real = Real(java.lang.Math.cosh(d))
  def tanh:Real = Real(java.lang.Math.tanh(d))
  def coth:Real = Real(1/java.lang.Math.tanh(d))
  def arcsin:Real = Real(Math.asin(d))
  def arccos:Real = Real(Math.acos(d))
  def arctan:Real = Real(Math.atan(d))
  def arccot:Real = Real(Math.Pi/2-Math.atan(d))
  def min(r:Real):Real = Real(Math.min(d,r.d))
  def max(r:Real):Real = Real(Math.max(d,r.d))
  def avg(r:Real):Real = Real((d+r.d)/2)
  def sqrt:Real = Real(Math.sqrt(d))
  def cbrt:Real = Real(java.lang.Math.cbrt(d))
  def hypot(r:Real):Real = Real(java.lang.Math.hypot(d,r.d))
  def root(r:Real):Real = Real(java.lang.Math.pow(d,1/r.d))
  def exp:Real = Real(java.lang.Math.exp(d))
  def expm1:Real = Real(java.lang.Math.expm1(d))
  def ln:Real = Real(java.lang.Math.log(d))
  def log:Real = Real(java.lang.Math.log10(d))
  def log1p:Real = Real(java.lang.Math.log1p(d))
  def < (r:Real):Boolean = this.d<r.d
  def > (r:Real):Boolean = this.d>r.d
  def >= (r:Real):Boolean = this.d>=r.d
  def <= (r:Real):Boolean = this.d<=r.d
  def <> (r:Real):Boolean = this.d!=r.d
  def < (r:Int):Boolean = this.d<r
  def > (r:Int):Boolean = this.d>r
  def >= (r:Int):Boolean = this.d>=r
  def <= (r:Int):Boolean = this.d<=r
  def <> (r:Int):Boolean = this.d!=r
  def isNegative:Boolean = d<0
  def isPositive:Boolean = d>0
  def isNonNegative:Boolean = d>=0
  def isNonPositive:Boolean = d<=0
  def isZero:Boolean = d==0
  def rad:Real = Real(Math.toRadians(d))
  def deg:Real = Real(Math.toDegrees(d))
  def isInt = (d.toInt==d)
  
  override def equals(a:Any):Boolean = a match {	
	  case r:Real => r.d-d<0.000001
	  case _ => false
  }
  
  override def toString = String.valueOf(d)
}