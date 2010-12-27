package org.encalmo.expression

/**
 * Selection is a composite expression consisting of one default expression 
 * and number of test cases. Each test case consist of expression and test.
 * Selection evaluates to the expression of case which test first returns true, 
 * or to the default expression if neither case succeeds.
 * @author artur.opala
 *
 */
case class Selection(e:Expression,cases:Seq[Case]) extends Expression with Auxiliary {

	/**
	 * Examines cases. Returns expression of case which test first returns true, 
	 * or default expression if neither case succeeds.
	 * @return
	 */
  def select:Expression = {
	  for(cas:Case <- cases){
	 	  if(cas.test)return cas.e
	  }
	  e
  }
  
    /**
	 * Examines test cases. Evaluates expression of case which test first returns true, 
	 * or default expression if neither case succeeds.
	 * @return
	 */
  override def eval:Expression = {
    for(cas:Case <- cases){
    	if(cas.test)return cas.e.eval
    }
    e.eval
  }
  
  /**
   * Maps default expression and all cases
   */
  final override def map(f:Transformation):Expression = {
	  val ve = e.map(f)
	  val m:Seq[Case] = cases.map(c => {
	 	  val vc =c.map(f)
	 	  if(vc.isInstanceOf[Case]){
	 	 	  vc.asInstanceOf[Case]
	 	  }else{
	 	 	  EmptyCase
	 	  }
	  })
	  if(ve==e && cases.zip(m).forall(t => t._1 eq t._2)){
	 	  f(this)
	  }else{
	 	  f(Selection(ve,m))
	  }
  }
  
  /**
   * Returns new selection with appended case
   */
  override def or(c:Case):Selection = Selection(e,cases.:+(c))

}