package org.encalmo.expression

/**
 * CaseTest has key method 'test' invoked by parent Case
 * @author artur.opala
 *
 */
trait CaseTest extends Expression with Auxiliary {
  
  def test:Boolean
  
  def then(e:Expression):Case = e match {
    case s:Symbol => Case(s,this)
    case _ => Case(e,this)
  }
  
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
	
	override def test:Boolean = false
}

/**
 * CaseTest always returning true
 */
object Always extends CaseTest {
	
	override def test:Boolean = true
}

/**
 * IsZero tests if given expression evaluates to zero
 * @author artur.opala
 */
case class IsZero(e:Expression) extends CaseTest {
	
  override def test:Boolean = zero.eq(e.eval)
  
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
	
  override def test:Boolean = zero.ne(e.eval)
  
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
	
  override def test:Boolean = (t1==t2)
  
  override def map(f:Transformation):Expression = {
	  val vl = t1.map(f); 
	  val vr = t2.map(f);
	  if(vl==t1 && vr==t2) f(this) else f(Equals(vl,vr))
  }
	
}

/**
 * AndCaseTest tests if both nested cases returns true
 * @author artur.opala
 */
case class AndCaseTest(t1:CaseTest,t2:CaseTest) extends CaseTest{
	
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
	
  override def test:Boolean = !t.test
  
   override def map(f:Transformation):Expression = {
	  val vt = t.map(f); 
	  if(vt==t) f(this) else f(NegCaseTest(vt.asInstanceOf[CaseTest]))
  }
	
}

