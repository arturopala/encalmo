package org.encalmo.expression

import scala.language.postfixOps

/**
 * Case is a part of the Selection expression.
 * If case test evaluates to true then expression is used by parent Selection.
 * @author artur.opala
 */
case class Case(caseExpression: CaseExpression, caseTest: CaseTest) extends Expression with Auxiliary {

    override val children = Seq(caseExpression,caseTest)

    def test:Option[Boolean] = caseTest.test

    override def face = "{if "+caseTest.face+" then "+ caseExpression.face + "}"

    final override def eval():Expression = {
            val ev = caseExpression.eval()
            if(ev ne caseExpression.expression) Case(CaseExpression(ev),caseTest) else this
    }

    final override def map(f:Transformation): Case = {
        val vt = caseTest.map(f) match {
            case ct: CaseTest => ct
            case _ => Never
        }
        val vc = caseExpression.map(f)
        f(
            if((vt eq caseTest) && (vc eq caseExpression)) this else Case(vc,vt)
        ) match {
            case c: Case => c
            case _ => EmptyCase
        }
    }

}

/**
 * Special purpose empty case
 * @author artur.opala
 */
object EmptyCase extends Case(CaseExpression(Void),Never) {

    final override def face = "{empty case}"

}