package org.encalmo.expression

import scala.language.postfixOps

/**
 * Case is a part of the Selection expression.
 * If case test evaluates to true then expression is used by parent Selection.
 * @author artur.opala
 */
case class Case(ce:CaseExpression,t:CaseTest) extends Expression with Auxiliary {

    override def children = Seq(ce,t)

    def test:Boolean = t.test

    final override def eval():Expression = {
            val ev = ce eval()
            if(ev.ne(ce.expr)) Case(CaseExpression(ev),t) else this
    }
    
    /** Maps only test expressions */
    final override def map(f:Transformation):Expression = {
            val vt = t.map(f)
            if(vt==t) f(this) else {
                vt match {
                    case t1: CaseTest =>
                        f(Case(ce, t1))
                    case _ =>
                        f(Case(ce, Never))
                }
            }
    }

}

/**
 * Special purpose empty case
 * @author artur.opala
 */
object EmptyCase extends Case(CaseExpression(Void),Never)