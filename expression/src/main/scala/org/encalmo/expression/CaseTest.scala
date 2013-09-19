package org.encalmo.expression

/**
 * CaseTest has key method 'test' invoked by parent Case
 * @author artur.opala
 *
 */
trait CaseTest extends Expression with Auxiliary {

    def test: Option[Boolean]

    def thenUse(e: Expression): Case = e match {
        case s: Symbol => Case(CaseExpression(s), this)
        case _ => Case(CaseExpression(e), this)
    }

    def operator: Seq[String]

    /**
     * Returns AndCaseTest
     */
    def &&(t: CaseTest): CaseTest = AndCaseTest(this, t)
    def and(t: CaseTest): CaseTest = AndCaseTest(this, t)

    /**
     * Returns AndCaseTest with NegCaseTest of argument t
     */
    def &&!(t: CaseTest): CaseTest = AndCaseTest(this, NegCaseTest(t))

    /**
     * Returns OrCaseTest
     */
    def ||(t: CaseTest): CaseTest = OrCaseTest(this, t)
    def or(t: CaseTest): CaseTest = OrCaseTest(this, t)

    /**
     * Returns OrCaseTest with NegCaseTest of argument t
     */
    def ||!(t: CaseTest): CaseTest = OrCaseTest(this, NegCaseTest(t))

    /**
     * Returns NegCaseTest
     */
    override def unary_!(): CaseTest = NegCaseTest(this)
    def unary_not(): CaseTest = NegCaseTest(this)

}

/**
 * CaseTest never returning true
 */
object Never extends CaseTest {

    override def operator: Seq[String] = Seq()

    override def test: Option[Boolean] = Some(false)

    override def face = "(false)"
}

/**
 * CaseTest always returning true
 */
object Always extends CaseTest {

    override def operator: Seq[String] = Seq()

    override def test: Option[Boolean] = Some(true)

    override def face = "(true)"
}

/**
 * IsZero tests if given expression evaluates to zero
 * @author artur.opala
 */
case class IsZero(e: Expression) extends CaseTest {

    override val children = Seq(e)

    override def operator: Seq[String] = Seq("", "= 0")

    override def test: Option[Boolean] = e.eval() match {
        case v: Value if v == ZERO => Some(true)
        case _: Value => Some(false)
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val ve = e.map(f)
        if (ve eq e) f(this) else f(IsZero(ve))
    }

    override def face = "(" + e.face + " == 0)"

}

/**
 * IsNotZero tests if given expression not evaluates to zero
 * @author artur.opala
 */
case class IsNotZero(e: Expression) extends CaseTest {

    override val children = Seq(e)

    override def operator: Seq[String] = Seq("", "!= 0")

    override def test: Option[Boolean] = e.eval() match {
        case v: Value if v == ZERO => Some(false)
        case _: Value => Some(true)
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val ve = e.map(f)
        if (ve eq e) f(this) else f(IsNotZero(ve))
    }

    override def face = "(" + e.face + " != 0)"

}

/**
 * IsEqual tests if given expressions are equals after evaluation
 * @author artur.opala
 */
case class IsEqualTo(t1: Expression, t2: Expression) extends CaseTest {

    override val children = Seq(t1, t2)

    override def operator: Seq[String] = Seq("", "=", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval()) match {
        case (v1: Value, v2: Value) => v1 === v2
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val vl = t1.map(f)
        val vr = t2.map(f)
        if ((vl eq t1) && (vr eq t2)) f(this) else {
            if(vl == vr) f(Always) else f(IsEqualTo(vl, vr))
        }
    }

    override def face = "(" + t1.face + " == " + t2.face + ")"

}

/**
 * GreaterThan test
 * @author artur.opala
 */
case class IsGreaterThan(t1: Expression, t2: Expression) extends CaseTest {

    override val children = Seq(t1, t2)

    override def operator: Seq[String] = Seq("", ">", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval()) match {
        case (v1: Value, v2: Value) => v1 > v2
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val vl = t1.map(f)
        val vr = t2.map(f)
        if ((vl eq t1) && (vr eq t2)) f(this) else f(IsGreaterThan(vl, vr))
    }

    override def face = "(" + t1.face + " > " + t2.face + ")"

}

/**
 * GreaterThan test
 * @author artur.opala
 */
case class IsGreaterThanOrEqualTo(t1: Expression, t2: Expression) extends CaseTest {

    override val children = Seq(t1, t2)

    override def operator: Seq[String] = Seq("", ">=", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval()) match {
        case (v1: Value, v2: Value) => v1 >= v2
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val vl = t1.map(f)
        val vr = t2.map(f)
        if ((vl eq t1) && (vr eq t2)) f(this) else f(IsGreaterThanOrEqualTo(vl, vr))
    }

    override def face = "(" + t1.face + " >= " + t2.face + ")"

}

/**
 * LowerThan test
 * @author artur.opala
 */
case class IsLowerThan(t1: Expression, t2: Expression) extends CaseTest {

    override val children = Seq(t1, t2)

    override def operator: Seq[String] = Seq("", "<", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval()) match {
        case (v1: Value, v2: Value) => v1 < v2
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val vl = t1.map(f)
        val vr = t2.map(f)
        if ((vl eq t1) && (vr eq t2)) f(this) else f(IsLowerThan(vl, vr))
    }

    override def face = "(" + t1.face + " < " + t2.face + ")"

}

/**
 * LowerOrEqualThan test
 * @author artur.opala
 */
case class IsLessThanOrEqualTo(t1: Expression, t2: Expression) extends CaseTest {

    override val children = Seq(t1, t2)

    override def operator: Seq[String] = Seq("", "<=", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval()) match {
        case (v1: Value, v2: Value) => v1 <= v2
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val vl = t1.map(f)
        val vr = t2.map(f)
        if ((vl eq t1) && (vr eq t2)) f(this) else f(IsLessThanOrEqualTo(vl, vr))
    }

    override def face = "(" + t1.face + " <= " + t2.face + ")"

}

/**
 * InRangeLEL test
 * @author artur.opala
 */
case class IsInRangeLessOrEqualAndLessThan(t1: Expression, t2: Expression, t3: Expression) extends CaseTest {

    override val children = Seq(t1, t2, t3)

    override def operator: Seq[String] = Seq("", "<=", "<", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval(), t3.eval()) match {
        case (v1: Value, v2: Value, v3: Value) => v1 <= v2 & v2 < v3
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val v1 = t1.map(f)
        val v2 = t2.map(f)
        val v3 = t3.map(f)
        if ((v1 eq t1) && (v2 eq t2) && (v3 eq t3)) f(this) else f(IsInRangeLessOrEqualAndLessThan(v1, v2, v3))
    }

    override def face = "(" + t1.face + " <= " + t2.face + " < " + t3.face + ")"

}

/**
 * InRangeLLE test
 * @author artur.opala
 */
case class IsInRangeLessAndLessOrEqual(t1: Expression, t2: Expression, t3: Expression) extends CaseTest {

    override val children = Seq(t1, t2, t3)

    override def operator: Seq[String] = Seq("", "<", "<=", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval(), t3.eval()) match {
        case (v1: Value, v2: Value, v3: Value) => v1 < v2 & v2 <= v3
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val v1 = t1.map(f)
        val v2 = t2.map(f)
        val v3 = t3.map(f)
        if ((v1 eq t1) && (v2 eq t2) && (v3 eq t3)) f(this) else f(IsInRangeLessAndLessOrEqual(v1, v2, v3))
    }

    override def face = "(" + t1.face + " < " + t2.face + " <= " + t3.face + ")"

}

/**
 * InRangeLL test
 * @author artur.opala
 */
case class IsInRangeLessAndLess(t1: Expression, t2: Expression, t3: Expression) extends CaseTest {

    override val children = Seq(t1, t2, t3)

    override def operator: Seq[String] = Seq("", "<", "<", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval(), t3.eval()) match {
        case (v1: Value, v2: Value, v3: Value) => v1 < v2 & v2 < v3
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val v1 = t1.map(f)
        val v2 = t2.map(f)
        val v3 = t3.map(f)
        if ((v1 eq t1) && (v2 eq t2) && (v3 eq t3)) f(this) else f(IsInRangeLessAndLess(v1, v2, v3))
    }

    override def face = "(" + t1.face + " < " + t2.face + " < " + t3.face + ")"

}

/**
 * InRangeLELE test
 * @author artur.opala
 */
case class IsInRangeLessOrEqualAndLessOrEqual(t1: Expression, t2: Expression, t3: Expression) extends CaseTest {

    override val children = Seq(t1, t2, t3)

    override def operator: Seq[String] = Seq("", "<=", "<=", "")

    override def test: Option[Boolean] = (t1.eval(), t2.eval(), t3.eval()) match {
        case (v1: Value, v2: Value, v3: Value) => v1 <= v2 & v2 <= v3
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val v1 = t1.map(f)
        val v2 = t2.map(f)
        val v3 = t3.map(f)
        if ((v1 eq t1) && (v2 eq t2) && (v3 eq t3)) f(this) else f(IsInRangeLessOrEqualAndLessOrEqual(v1, v2, v3))
    }

    override def face = "(" + t1.face + " <= " + t2.face + " <= " + t3.face + ")"

}

/**
 * AndCaseTest tests if both nested cases returns true
 * @author artur.opala
 */
case class AndCaseTest(t1: CaseTest, t2: CaseTest) extends CaseTest {

    override val children = Seq(t1, t2)

    override def operator: Seq[String] = Seq("", "&", "")

    override def test: Option[Boolean] = (t1.test, t2.test) match {
        case (Some(b1), Some(b2)) => Some(b1 & b2)
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val vl = t1.map(f)
        val vr = t2.map(f)
        if ((vl eq t1) && (vr eq t2)) f(this) else f(AndCaseTest(vl.asInstanceOf[CaseTest], vr.asInstanceOf[CaseTest]))
    }

    override def face = "(" + t1.face + " & " + t2.face + ")"

}

/**
 * OrCaseTest tests if one or both nested cases returns true
 * @author artur.opala
 */
case class OrCaseTest(t1: CaseTest, t2: CaseTest) extends CaseTest {

    override val children = Seq(t1, t2)

    override def operator: Seq[String] = Seq("", "|", "")

    override def test: Option[Boolean] = (t1.test, t2.test) match {
        case (Some(b1), Some(b2)) => Some(b1 | b2)
        case _ => None
    }

    override def map(f: Transformation): Expression = {
        val vl = t1.map(f)
        val vr = t2.map(f)
        if ((vl eq t1) && (vr eq t2)) f(this) else f(OrCaseTest(vl.asInstanceOf[CaseTest], vr.asInstanceOf[CaseTest]))
    }

    override def face = "(" + t1.face + " | " + t2.face + ")"

}

/**
 * NegCaseTest tests if nested case returns false
 * @author artur.opala
 */
case class NegCaseTest(t: CaseTest) extends CaseTest {

    override val children = Seq(t)

    override def operator: Seq[String] = Seq("!", "")

    override def test: Option[Boolean] = t.test.map(!_)

    override def map(f: Transformation): Expression = {
        val vt = t.map(f)
        if (vt eq t) f(this) else f(NegCaseTest(vt.asInstanceOf[CaseTest]))
    }

    override def face = "(!" + t.face + ")"

}

