package org.encalmo.expression

/**
 * Selection is a composite expression consisting of one default expression 
 * and number of test cases. Each test case consist of expression and test.
 * Selection evaluates to the expression of case which test first returns true, 
 * or to the default expression if neither case succeeds.
 * @author artur.opala
 *
 */
case class Selection(cases: Seq[Case], default: Option[CaseExpression] = None) extends Expression with Auxiliary {

    override val children = default.map(cases :+ _) getOrElse cases

    /** Return true if only one choice exists */
    def isSingle: Boolean = cases.isEmpty || (default.isEmpty && cases.size==1)

    /**
     * Examines cases. Returns expression of case which test first returns true,
     * or default expression if neither case succeeds.
     */
    def select: Expression = {
        cases.find(_.test match {case Some(b) => b; case None => {return this}}) map (_.caseExpression.expression) getOrElse {
            default.map(_.expression).getOrElse(throw new IllegalStateException("No default expression defined for selection"))
        }
    }

    /** Trims this selection to one case element */
    def trim: Expression = {
        cases.find(_.test match {case Some(b) => b; case None => {throw new IllegalStateException()}}) map (cas => Selection(Seq(cas))) getOrElse {
            default.map(e => Selection(Seq.empty,Some(e))).getOrElse(throw new IllegalStateException("No default expression defined for selection"))
        }
    }

    /**
     * Examines test cases. Evaluates expression of case which test first returns true,
     * or default expression if neither case succeeds.
     * @return
     */
    override def eval(): Expression = {
        cases.find(_.test match {case Some(b) => b; case None => return this}) map (_.caseExpression.expression.eval()) getOrElse {
            default.map(_.expression.eval()).getOrElse(throw new IllegalStateException("No default expression defined for selection"))
        }
    }

    /**
     * Maps test case expressions and default case
     */
    final override def map(f: Transformation): Expression = {
        val nc: Seq[Case] = cases.map(testCase => testCase.map(f))
        val nd = default.map(_.map(f))
        f(
            if((default eq nd) && cases.zip(nc).forall(t => t._1 eq t._2)) this else Selection(nc, nd)
        )
    }

    /**
     * Returns new selection with appended case
     */
    override def unless(newCase: Case): Selection = Selection(newCase +: cases, default)

}

object Selection {

    def apply(cases: Seq[Case], default: CaseExpression): Selection = Selection(cases, Some(default))

}

class CaseTestNotReadyToEvaluateException extends Exception