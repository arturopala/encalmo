package org.encalmo.expression

/**
 * Case is a part of the Selection. 
 * If case test evaluates to true then expression is used by parent Selection.
 * @author artur.opala
 */
case class Case(ce:CaseExpression,t:CaseTest) extends Expression with Auxiliary {

    override def children = Seq(ce,t)

    def test:Boolean = t.test

    final override def eval():Expression = {
            val ev = ce eval; 
            if(ev.ne(ce.expr)) Case(CaseExpression(ev),t) else this
    }

    final override def map(f:Transformation):Expression = {
            val ve = ce.map(f);
            val vt = t.map(f);
            if(ve==ce.expr && vt==t) f(this) else {
                if(vt.isInstanceOf[CaseTest]){
                    f(Case(CaseExpression(ve),vt.asInstanceOf[CaseTest]))
                }else{
                    f(Case(CaseExpression(ve),Never))
                }
            }
    }

}

/**
 * Special purpose empty case
 * @author artur.opala
 */
object EmptyCase extends Case(CaseExpression(Void),Never)