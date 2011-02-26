package org.encalmo.expression

/**
 * Special purpose expression object meaning unknown expression
 * @author artur.opala
 */
object Unknown extends Expression {

  def dump(w: java.io.PrintWriter): Unit = w.print("unknown")
  
  override def + (e:Expression):Expression = Unknown
  
  override def - (e:Expression):Expression = Unknown
  
  override def * (e:Expression):Expression = Unknown
  
  override def / (e:Expression):Expression = Unknown
  
  override def % (e:Expression):Expression = Unknown
  
  override def ^ (e:Expression):Expression = Unknown
  
  override def unary_-():Expression = Unknown
  
  override def +- (e:Expression):Expression = Unknown
  
  override def *- (e:Expression):Expression = Unknown
  
  override def /- (e:Expression):Expression = Unknown
  
  override def %- (e:Expression):Expression = Unknown
  
  override def ^- (e:Expression):Expression = Unknown

}