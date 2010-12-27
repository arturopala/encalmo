package org.encalmo.expression

/**
 * Root type of all expressions.
 * @author artur.opala
 * 
 */
trait Expression {
  
  /**
   * Evaluates expression. 
   * Subtypes should calculate possibly the simplest and numerical form of this expression.
   */
  def eval():Expression = this
  
  /**
   * Maps this expression with tranformation function. 
   * Subtypes should return own copy with custom arguments after transformation
   * @param f transformate
   * @return tranformed expression
   */
  def map(f:Transformation):Expression = f(this)
  
  /**
   * Travels internal structure of the expression 
   * @param t traveler
   */
  def travel(parent:Node = null, t:Traveler, position:Int=0):Unit = {
	  val n = Node(parent,this,position)
	  t.onEnter(n)
	  t.onExit(n)
  }
  
  /**
   * Assigns this expression to the symbol s
   * @param s symbol
   * @return new definition of s
   */
  def as(s:Symbol):Expression = Definition(s,this)
  
  def or(c:Case):Selection = Selection(this,Seq(c))
  
  def + (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if zero.eq(e) => this; 
	  case _ => Sum(this,e)
  }
  
  def - (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if zero.eq(e) => this; 
	  case _ => Diff(this,e)
  }
  
  def * (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if one.eq(e) => this; 
	  case _ if zero.eq(e) => zero; 
	  case _ => Prod(this,e)
  }
  
  def / (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if one.eq(e) => this; 
	  case _ => Quot(this,e)
  }
  
  def % (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if one.eq(e) => this; 
	  case _ => Modulo(this,e)
  }
  
  def ^ (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if zero.eq(e) => one; 
	  case _ if one.eq(e) => this; 
	  case _ => Power(this,e)
  }
  
  def unary_-():Expression = this match {
	  case Void => this
	  case Unknown => Unknown
	  case _ => Neg(this)
  }
  
  def +- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r) => this+(Number(-r)); 
	  case _ => this+(Neg(e))
  }
  
  def *- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r) => *(Number(-r));
	  case _ => *(Neg(e))
  }
  
  def /- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r) => /(Number(-r));
	  case _ => /(Neg(e))
  }
  
  def %- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r) => %(Number(-r)); 
	  case _ => %(Neg(e))
  }
  
  def ^- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r) => ^(Number(-r)); 
	  case _ => ^(Neg(e))
  }
  
  def < (e:Expression):Boolean = false
  def > (e:Expression):Boolean = false
  def >= (e:Expression):Boolean = false
  def <= (e:Expression):Boolean = false
  def <> (e:Expression):Boolean = false
  
}

object Expression {

}