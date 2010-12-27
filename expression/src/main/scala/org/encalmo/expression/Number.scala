package org.encalmo.expression

import java.io.PrintWriter

/**
 * Number
 * @author artur.opala
 *
 */
case class Number(r:Real) extends Value {
  
  def format:String = Real.format(r)
  
  def format(format:java.text.NumberFormat):String = Real.format(r, format);
  
  def format(locale:java.util.Locale):String = Real.format(r,locale)
  
  def format(pattern:String,locale:java.util.Locale):String = Real.format(r,pattern,locale)
  
  override def + (e:Expression):Expression = e match {
	  //case Number(r2) => Number(r+r2)
	  case _ => super.+(e)
  }
  
  override def - (e:Expression):Expression = e match {
	  //case Number(r2) => Number(r-r2)
	  case _ => super.-(e)
  }
  
  override def * (e:Expression):Expression = e match {
	  //case Number(r2) => Number(r*r2)
	  case _ => super.*(e)
  }
  
  override def / (e:Expression):Expression = e match {
	  //case Number(r2) => Number(r/r2)
	  case _ => super./(e)
  }
  
  override def % (e:Expression):Expression = e match {
	  //case Number(r2) => Number(r%r2)
	  case _ => super.%(e)
  }
  
  override def ^ (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => one
	  case Number(r2) if r2==Real.one => this
	  //case Number(r2) => Number(r^r2)
	  case _ => super.^(e)
  }
  
  override def unary_-():Expression = Number(-r)
  
  override def +- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => this
	  //case Number(r2) => Number(r-r2)
	  case _ => super.+-(e)
  }
  
  override def *- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => zero
	  case Number(r2) if r2==Real.one => Number(-r)
	  //case Number(r2) => Number(r*(-r2))
	  case _ => super.*-(e)
  }
  
  override def /- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.one => Number(-r)
	  //case Number(r2) => Number(r/(-r2))
	  case _ => super./-(e)
  }
  
  override def %- (e:Expression):Expression = e match {
	  case Number(r2) => Number(r%(-r2))
	  case _ => super.%-(e)
  }
  
  override def ^- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => one
	  case Number(r2) if r2==Real.one => this
	  //case Number(r2) => Number(r^(-r2))
	  case _ => super.^-(e)
  }
  
  override def < (e:Expression):Boolean = e match {case Number(r1) => r<r1; case _ => false}
  override def > (e:Expression):Boolean = e match {case Number(r1) => r>r1; case _ => false}
  override def >= (e:Expression):Boolean = e match {case Number(r1) => r>=r1; case _ => false}
  override def <= (e:Expression):Boolean = e match {case Number(r1) => r<=r1; case _ => false}
  override def <> (e:Expression):Boolean = e match {case Number(r1) => r<>r1; case _ => false}
  
  def isInt = r.isInt
  
  override def equals(a:Any):Boolean = a match {	
	  case o:Number if zero.eq(o) => r==Real.zero
	  case o:Number if one.eq(o) => r==Real.one
	  case o:Number => o.r.equals(r)
	  case _ => false
  }
}

/**
 * Object representation of the number Zero
 * @author artur.opala
 *
 */
object zero extends Number(Real.zero){
	
  override def + (e:Expression):Expression = e
  override def - (e:Expression):Expression = -e
  override def ^ (e:Expression):Expression = e match {
	  case _ if zero.eq(e) => one; 
	  case _=> zero
  }
  
  override def unary_-():Expression = this
  override def isInt = false
  
  override def equals(a:Any):Boolean = a match {	
	  case o:Number if o.r==Real.zero => true
	  case _ => false
  }
  
}

/**
 * Object representation of the number One
 * @author artur.opala
 *
 */
object one extends Number(Real.one){
	
  override def * (e:Expression):Expression = e
  override def / (e:Expression):Expression = Quot(one,e)
  override def % (e:Expression):Expression = e
  override def ^ (e:Expression):Expression = one
  override def isInt = true
  
  override def equals(a:Any):Boolean = a match {	
	  case o:Number if o.r==Real.one => true
	  case _ => false
  }
 
}