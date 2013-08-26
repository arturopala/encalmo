package org.encalmo.expression

import org.encalmo.common._

/**
 * Root type of all expressions.
 * @author artur.opala
 */
trait Expression extends TreeNode[Expression] {
  
  /**
   * Evaluates expression. 
   * Subtypes should calculate possibly the simplest and numerical form of this expression.
   */
  def eval():Expression = this
  
  def unit:UnitOfValue = EmptyUnitOfValue
  
  def unless(testCase: Case):Selection = Selection(Seq(testCase), Some(CaseExpression(this)))
  
  def + (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Sum(l,r) => Sum(this,l,r)
      case _ => Sum(this,e)
  }
  
  def - (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ => Diff(this,e)
  }
  
  def * (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Prod(l,r) => Prod(this,l,r)
      case _ => Prod(this,e)
  }
  
  def / (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ => Quot(this,e)
  }
  
  def % (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case _ => Modulo(this,e)
  }
  
  def ^ (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case ZERO => ONE
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
	  case Number(r,u) => this+ Number(-r, u)
      case _ => this+ Neg(e)
  }
  
  def *- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => *(Number(-r,u))
      case _ => *(Neg(e))
  }
  
  def /- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => /(Number(-r,u))
      case _ => /(Neg(e))
  }
  
  def %- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => %(Number(-r,u))
      case _ => %(Neg(e))
  }
  
  def ^- (e:Expression):Expression = e match {
	  case Void => this
	  case Unknown => Unknown
	  case Number(r,u) => ^(Number(-r,u))
      case _ => ^(Neg(e))
  }
  
  def < (e:Expression):Boolean = false
  def > (e:Expression):Boolean = false
  def >= (e:Expression):Boolean = false
  def <= (e:Expression):Boolean = false
  def <> (e:Expression):Boolean = false
  
  def printable = true
  
  def nounit:Expression = NoUnit(this)
  def setunit(u:UnitOfValue):Expression = SetUnit(this,u)
  def setunit(s:String):Expression = SetUnit(this,SI(s).getOrElse(EmptyUnitOfValue))
  
}

object Expression {
    
    def simplify(e:Expression):Expression = {
        e.mapAll(Transformations.simplifyQuot)
    }

}