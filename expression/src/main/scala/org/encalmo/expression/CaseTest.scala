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
 * GreaterThan test
 * @author artur.opala
 */
case class GreaterThan(t1:Expression,t2:Expression) extends CaseTest {

    override def children = Seq(t1,t2)
    
    override def operator:Seq[String] = Seq("",">","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            (v1,v2) match {
            case (n1:Number,n2:Number) => n1>n2
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
 * GreaterThan test
 * @author artur.opala
 */
case class GreaterOrEqualThan(t1:Expression,t2:Expression) extends CaseTest {

    override def children = Seq(t1,t2)
    
    override def operator:Seq[String] = Seq("",">=","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            (v1,v2) match {
            case (n1:Number,n2:Number) => n1>=n2
            case _ => throw new IllegalArgumentException
            }
    }

    override def map(f:Transformation):Expression = {
            val vl = t1.map(f); 
            val vr = t2.map(f);
            if(vl==t1 && vr==t2) f(this) else f(GreaterOrEqualThan(vl,vr))
    }

}

/**
 * LowerThan test
 * @author artur.opala
 */
case class LowerThan(t1:Expression,t2:Expression) extends CaseTest {

    override def children = Seq(t1,t2)
    
    override def operator:Seq[String] = Seq("","<","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            (v1,v2) match {
            case (n1:Number,n2:Number) => n1<=n2
            case _ => throw new IllegalArgumentException
            }
    }

    override def map(f:Transformation):Expression = {
            val vl = t1.map(f); 
            val vr = t2.map(f);
            if(vl==t1 && vr==t2) f(this) else f(LowerThan(vl,vr))
    }

}

/**
 * LowerOrEqualThan test
 * @author artur.opala
 */
case class LowerOrEqualThan(t1:Expression,t2:Expression) extends CaseTest {

    override def children = Seq(t1,t2)
    
    override def operator:Seq[String] = Seq("","<=","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            (v1,v2) match {
            case (n1:Number,n2:Number) => n1<=n2
            case _ => throw new IllegalArgumentException("LowerOrEqualThan("+v1+","+v2+")")
            }
    }

    override def map(f:Transformation):Expression = {
            val vl = t1.map(f); 
            val vr = t2.map(f);
            if(vl==t1 && vr==t2) f(this) else f(LowerOrEqualThan(vl,vr))
    }

}

/**
 * InRangeLEL test
 * @author artur.opala
 */
case class InRangeLEL(t1:Expression,t2:Expression,t3:Expression) extends CaseTest {

    override def children = Seq(t1,t2,t3)
    
    override def operator:Seq[String] = Seq("","<=","<","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            val v3 = t3.eval
            (v1,v2,v3) match {
            case (n1:Number,n2:Number,n3:Number) => n1<=n2 && n2<n3
            case _ => throw new IllegalArgumentException("InRangeLEL("+v1+","+v2+","+v3+")")
            }
    }

    override def map(f:Transformation):Expression = {
            val v1 = t1.map(f); 
            val v2 = t2.map(f);
            val v3 = t3.map(f);
            if(v1==t1 && v2==t2 && v3==t3) f(this) else f(InRangeLEL(v1,v2,v3))
    }

}

/**
 * InRangeLLE test
 * @author artur.opala
 */
case class InRangeLLE(t1:Expression,t2:Expression,t3:Expression) extends CaseTest {

    override def children = Seq(t1,t2,t3)
    
    override def operator:Seq[String] = Seq("","<","<=","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            val v3 = t3.eval
            (v1,v2,v3) match {
            case (n1:Number,n2:Number,n3:Number) => n1<n2 && n2<=n3
            case _ => throw new IllegalArgumentException("InRangeLLE("+v1+","+v2+","+v3+")")
            }
    }

    override def map(f:Transformation):Expression = {
            val v1 = t1.map(f); 
            val v2 = t2.map(f);
            val v3 = t3.map(f);
            if(v1==t1 && v2==t2 && v3==t3) f(this) else f(InRangeLLE(v1,v2,v3))
    }

}

/**
 * InRangeLL test
 * @author artur.opala
 */
case class InRangeLL(t1:Expression,t2:Expression,t3:Expression) extends CaseTest {

    override def children = Seq(t1,t2,t3)
    
    override def operator:Seq[String] = Seq("","<","<","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            val v3 = t3.eval
            (v1,v2,v3) match {
            case (n1:Number,n2:Number,n3:Number) => n1<n2 && n2<n3
            case _ => throw new IllegalArgumentException("InRangeLL("+v1+","+v2+","+v3+")")
            }
    }

    override def map(f:Transformation):Expression = {
            val v1 = t1.map(f); 
            val v2 = t2.map(f);
            val v3 = t3.map(f);
            if(v1==t1 && v2==t2 && v3==t3) f(this) else f(InRangeLL(v1,v2,v3))
    }

}

/**
 * InRangeLELE test
 * @author artur.opala
 */
case class InRangeLELE(t1:Expression,t2:Expression,t3:Expression) extends CaseTest {

    override def children = Seq(t1,t2,t3)
    
    override def operator:Seq[String] = Seq("","<=","<=","")

    override def test:Boolean = {
            val v1 = t1.eval
            val v2 = t2.eval
            val v3 = t3.eval
            (v1,v2,v3) match {
            case (n1:Number,n2:Number,n3:Number) => n1<=n2 && n2<=n3
            case _ => throw new IllegalArgumentException("InRangeLELE("+v1+","+v2+","+v3+")")
            }
    }

    override def map(f:Transformation):Expression = {
            val v1 = t1.map(f); 
            val v2 = t2.map(f);
            val v3 = t3.map(f);
            if(v1==t1 && v2==t2 && v3==t3) f(this) else f(InRangeLELE(v1,v2,v3))
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

