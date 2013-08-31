package org.encalmo.expression

/**
 * Function definition expression
 * @author artur.opala
 */
case class Function(expr:Expression, vars:Symbol*) extends Expression {
    
   override val children = Seq(expr)
   
   override def eval() = {
        expr.eval()
    }
   
   override def map(f:Transformation):Expression = {
        val ve = f(expr.map(f))
       if(ve==expr) f(this) else f(new Function(ve,vars:_*))
    }
  
}