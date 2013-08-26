package org.encalmo.expression

/**
 * Case expression wrapper
 * @author artur.opala
 */
case class CaseExpression(expression:Expression) extends Expression with Auxiliary {
    
    override def children = Seq(expression)
    
    final override def eval():Expression = expression.eval()
    
    final override def map(f:Transformation): CaseExpression = {
        val ve = expression.map(f)
        f(
            if(ve eq expression) this else CaseExpression(ve)
        ) match {
            case ce: CaseExpression => ce
            case other => CaseExpression(other)
        }
    }

    def isVoid: Boolean = expression eq Void

}