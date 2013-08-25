package org.encalmo.expression

/**
 * Case expression wrapper
 * @author artur.opala
 */
case class CaseExpression(expr:Expression = Unknown) extends Expression {
    
    override def children = Seq(expr)
    
    final override def eval():Expression = expr.eval()
    
    final override def map(f:Transformation):Expression = f(expr.map(f))
    
    

}