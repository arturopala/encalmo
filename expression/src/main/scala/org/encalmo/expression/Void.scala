package org.encalmo.expression

/**
 * Special purpose expression object meaning void, empty, tranparent, non-existent expression behaving like 1 or 0
 * @author artur.opala
 */
object Void extends Expression {
	
  def dump(w:java.io.PrintWriter) = w.print("void")
	
  override def + (e:Expression):Expression = e
  
  override def - (e:Expression):Expression = -e
  
  override def * (e:Expression):Expression = e
  
  override def / (e:Expression):Expression = 1/e
  
  override def % (e:Expression):Expression = Void
  
  override def ^ (e:Expression):Expression = Void
  
  override def unary_-():Expression = Void
  
  override def +- (e:Expression):Expression = -e
  
  override def *- (e:Expression):Expression = -e
  
  override def /- (e:Expression):Expression = 1/-e
  
  override def %- (e:Expression):Expression = Void
  
  override def ^- (e:Expression):Expression = Void
	
}