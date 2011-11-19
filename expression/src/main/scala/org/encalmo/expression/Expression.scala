package org.encalmo.expression

import org.encalmo.common._

/**
 * Root type of all expressions.
 * @author artur.opala
 */
trait Expression extends TreeLike[Expression] {
  
  /**
   * Evaluates expression. 
   * Subtypes should calculate possibly the simplest and numerical form of this expression.
   */
  def eval():Expression = this
  
  def unit:UnitOfValue = EmptyUnitOfValue
  
  def or(c:Case):Selection = Selection(CaseExpression(this),Seq(c))
  
  def + (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if ZERO.eq(e) => this; 
	  case Sum(l,r) => Sum(this,l,r); 
	  case _ => Sum(this,e)
  }
  
  def - (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if ZERO.eq(e) => this; 
	  case _ => Diff(this,e)
  }
  
  def * (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if ONE.eq(e) => this; 
	  case _ if ZERO.eq(e) => ZERO; 
	  case Prod(l,r) => Prod(this,l,r);
	  case _ => Prod(this,e)
  }
  
  def / (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if ONE.eq(e) => this; 
	  case _ => Quot(this,e)
  }
  
  def % (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if ONE.eq(e) => this; 
	  case _ => Modulo(this,e)
  }
  
  def ^ (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ if ZERO.eq(e) => ONE; 
	  case _ if ONE.eq(e) => this; 
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
	  case Number(r,u) => this+(Number(-r,u)); 
	  case _ => this+(Neg(e))
  }
  
  def *- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => *(Number(-r,u));
	  case _ => *(Neg(e))
  }
  
  def /- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => /(Number(-r,u));
	  case _ => /(Neg(e))
  }
  
  def %- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => %(Number(-r,u)); 
	  case _ => %(Neg(e))
  }
  
  def ^- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => ^(Number(-r,u)); 
	  case _ => ^(Neg(e))
  }
  
  def < (e:Expression):Boolean = false
  def > (e:Expression):Boolean = false
  def >= (e:Expression):Boolean = false
  def <= (e:Expression):Boolean = false
  def <> (e:Expression):Boolean = false
  
  def printable = true
  
}

object Expression {

}