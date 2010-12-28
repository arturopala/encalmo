package org.encalmo.expression

import java.io.PrintWriter

/**
 * Real number expression
 * @author artur.opala
 */
case class Number(r:Real) extends Value {
  
  def format:String = Real.format(r)
  
  def format(format:java.text.NumberFormat):String = Real.format(r, format);
  
  def format(locale:java.util.Locale):String = Real.format(r,locale)
  
  def format(pattern:String,locale:java.util.Locale):String = Real.format(r,pattern,locale)
  
  override def + (e:Expression):Expression = e match {
	  case _ => super.+(e)
  }
  
  override def - (e:Expression):Expression = e match {
	  case _ => super.-(e)
  }
  
  override def * (e:Expression):Expression = e match {
	  case _ => super.*(e)
  }
  
  override def / (e:Expression):Expression = e match {
	  case _ => super./(e)
  }
  
  override def % (e:Expression):Expression = e match {
	  case _ => super.%(e)
  }
  
  override def ^ (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => ONE
	  case Number(r2) if r2==Real.one => this
	  case _ => super.^(e)
  }
  
  override def unary_-():Expression = Number(-r)
  
  override def +- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => this
	  case _ => super.+-(e)
  }
  
  override def *- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => ZERO
	  case Number(r2) if r2==Real.one => Number(-r)
	  case _ => super.*-(e)
  }
  
  override def /- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.one => Number(-r)
	  case _ => super./-(e)
  }
  
  override def %- (e:Expression):Expression = e match {
	  case Number(r2) => Number(r%(-r2))
	  case _ => super.%-(e)
  }
  
  override def ^- (e:Expression):Expression = e match {
	  case Number(r2) if r2==Real.zero => ONE
	  case Number(r2) if r2==Real.one => this
	  case _ => super.^-(e)
  }
  
  override def < (e:Expression):Boolean = e match {case Number(r1) => r<r1; case _ => false}
  override def > (e:Expression):Boolean = e match {case Number(r1) => r>r1; case _ => false}
  override def >= (e:Expression):Boolean = e match {case Number(r1) => r>=r1; case _ => false}
  override def <= (e:Expression):Boolean = e match {case Number(r1) => r<=r1; case _ => false}
  override def <> (e:Expression):Boolean = e match {case Number(r1) => r<>r1; case _ => false}
  
  def isInt = r.isInt
  
  override def equals(a:Any):Boolean = a match {	
	  case o:Number if ZERO.eq(o) => r==Real.zero
	  case o:Number if ONE.eq(o) => r==Real.one
	  case o:Number => o.r.equals(r)
	  case _ => false
  }
}

/**
 * Object representation of the number Zero
 * @author artur.opala
 *
 */
object ZERO extends Number(Real.zero){
	
  override def + (e:Expression):Expression = e
  override def - (e:Expression):Expression = -e
  override def ^ (e:Expression):Expression = e match {
	  case _ if ZERO.eq(e) => ONE; 
	  case _=> ZERO
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
object ONE extends Number(Real.one){
	
  override def * (e:Expression):Expression = e
  override def / (e:Expression):Expression = Quot(ONE,e)
  override def % (e:Expression):Expression = e
  override def ^ (e:Expression):Expression = ONE
  override def isInt = true
  
  override def equals(a:Any):Boolean = a match {	
	  case o:Number if o.r==Real.one => true
	  case _ => false
  }
 
}