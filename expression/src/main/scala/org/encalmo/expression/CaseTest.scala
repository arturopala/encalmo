package org.encalmo.expression

/**
 * CaseTest has key method 'test' invoked by parent Case
 * @author artur.opala
 *
 */
trait CaseTest extends Expression with Auxiliary {

    def test:Boolean

    def then(e:Expression):Case = e match {
        case s:Symbol => Case(CaseExpression(s),this)
        case _ => Case(CaseExpression(e),this)
    }
    
    def operator:Seq[String]
    
    /**
     * Returns AndCaseTest
     */
    def &(t:CaseTest):CaseTest = AndCaseTest(this,t)
    
    /**
     * Returns AndCaseTest with NegCaseTest of argument t
     */
    def &!(t:CaseTest):CaseTest = AndCaseTest(this,NegCaseTest(t))
    
    /**
     * Returns OrCaseTest
     */
    def |(t:CaseTest):CaseTest = OrCaseTest(this,t)
    
    /**
     * Returns OrCaseTest with NegCaseTest of argument t
     */
    def |!(t:CaseTest):CaseTest = OrCaseTest(this,NegCaseTest(t))
    
    /**
     * Returns NegCaseTest
     */
    def unary_! = NegCaseTest(this)

}

/**
 * CaseTest never returning true
 */
object Never extends CaseTest {

    override def operator:Seq[String] = Seq()
    
    override def test:Boolean = false
}

/**
 * CaseTest always returning true
 */
object Always extends CaseTest {

    override def operator:Seq[String] = Seq()

    override def test:Boolean = true
}

/**
 * IsZero tests if given expression evaluates to zero
 * @author artur.opala
 */
case class IsZero(e:Expression) extends CaseTest {

    override def children = Seq(e)

    override def operator:Seq[String] = Seq("","= 0")

    override def test:Boolean = ZERO.eq(e.eval)

    override def map(f:Transformation):Expression = {
        val ve = e.map(f);
        if(ve==e) f(this) else f(IsZero(ve))
    }

}

/**
 * IsNotZero tests if given expression not evaluates to zero
 * @author artur.opala
 */
case class IsNotZero(e:Expression) extends CaseTest {

    override def children = Seq(e)

    override def operator:Seq[String] = Seq("","!= 0")

    override def test:Boolean = ZERO.ne(e.eval)

    override def map(f:Transformation):Expression = {
        val ve = e.map(f);
        if(ve==e) f(this) else f(IsNotZero(ve))
    }

}

/**
 * IsEqual tests if given expressions are equals after evaluation
 * @author artur.opala
 */
case class Equals(t1:Expression,t2:Expression) extends CaseTest {

    override def children = Seq(t1,t2)

    override def operator:Seq[String] = Seq("","=","")

    override def test:Boolean = (t1==t2)

    override def map(f:Transformation):Expression = {
        val vl = t1.map(f); 
        val vr = t2.map(f);
        if(vl==t1 && vr==t2) f(this) else f(Equals(vl,vr))
    }

}

/**
 * GreaterThan tests if first expression evals to number greater then the second
 * @author artur.opala
 */
case class GreaterThan(t1:Expression,t2:Expression) extends CaseTest {

    override def children = Seq(t1,t2)
    
    override def operator:Seq[String] = Seq("",">","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            (v1,v2) match {
            case (n1:Number,n2:Number) => n1.r.d>n2.r.d
            case _ => throw new IllegalArgumentException
            }
    }

    override def map(f:Transformation):Expression = {
            val vl = t1.map(f); 
            val vr = t2.map(f);
            if(vl==t1 && vr==t2) f(this) else f(GreaterThan(vl,vr))
    }

}

/**
 * InRange tests if second expression after evaluation is between of the first and the third
 * @author artur.opala
 */
case class InRange(t1:Expression,t2:Expression,t3:Expression) extends CaseTest {

    override def children = Seq(t1,t2,t3)
    
    override def operator:Seq[String] = Seq("",">=","<","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            val v3 = t3.eval
            (v1,v2,v3) match {
            case (n1:Number,n2:Number,n3:Number) => n2.r.d>=n1.r.d && n2.r.d<n3.r.d
            case _ => throw new IllegalArgumentException
            }
    }

    override def map(f:Transformation):Expression = {
            val v1 = t1.map(f); 
            val v2 = t2.map(f);
            val v3 = t3.map(f);
            if(v1==t1 && v2==t2 && v3==t3) f(this) else f(InRange(v1,v2,v3))
    }

}

/**
 * AndCaseTest tests if both nested cases returns true
 * @author artur.opala
 */
case class AndCaseTest(t1:CaseTest,t2:CaseTest) extends CaseTest{

    override def children = Seq(t1,t2)
    
    override def operator:Seq[String] = Seq("","&","")

    override def test:Boolean = t1.test && t2.test

    override def map(f:Transformation):Expression = {
            val vl = t1.map(f); 
            val vr = t2.map(f);
            if(vl==t1 && vr==t2) f(this) else f(AndCaseTest(vl.asInstanceOf[CaseTest],vr.asInstanceOf[CaseTest]))
    }

}

/**
 * OrCaseTest tests if one or both nested cases returns true
 * @author artur.opala
 */
case class OrCaseTest(t1:CaseTest,t2:CaseTest) extends CaseTest{

    override def children = Seq(t1,t2)
    
    override def operator:Seq[String] = Seq("","|","")

    override def test:Boolean = t1.test || t2.test

    override def map(f:Transformation):Expression = {
            val vl = t1.map(f); 
            val vr = t2.map(f);
            if(vl==t1 && vr==t2) f(this) else f(OrCaseTest(vl.asInstanceOf[CaseTest],vr.asInstanceOf[CaseTest]))
    }

}

/**
 * NegCaseTest tests if nested case returns false
 * @author artur.opala
 */
case class NegCaseTest(t:CaseTest) extends CaseTest{

    override def children = Seq(t)
    
    override def operator:Seq[String] = Seq("!","")

    override def test:Boolean = !t.test

    override def map(f:Transformation):Expression = {
            val vt = t.map(f); 
            if(vt==t) f(this) else f(NegCaseTest(vt.asInstanceOf[CaseTest]))
    }

}

