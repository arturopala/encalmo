package org.encalmo.expression

/**
 * Selection is a composite expression consisting of one default expression 
 * and number of test cases. Each test case consist of expression and test.
 * Selection evaluates to the expression of case which test first returns true, 
 * or to the default expression if neither case succeeds.
 * @author artur.opala
 *
 */
case class Selection(ce:CaseExpression,cases:Seq[Case]) extends Expression with Auxiliary {
    
    override val children = {
    	if(ce!=null){
    		cases.:+(ce)
		}else{
			cases
		}
	}
	
	/** Return true if only one choice exists */
	def isSingle:Boolean = (ce.expr==Unknown && cases.size==1) || (ce.expr!=Unknown && cases.isEmpty)

	/**
	 * Examines cases. Returns expression of case which test first returns true, 
	 * or default expression if neither case succeeds.
	 * @return
	 */
	  def select:Expression = {
		  for(cas:Case <- cases){
		 	  if(cas.test)return cas.ce.expr
		  }
		  ce.expr match {
		  	case Unknown => this
		  	case _ => ce.expr
		  }
	  }
	  
	  /** Trims this selection to one element */
	  def trim:Expression = {
	  	  if(ce.expr==Unknown && cases.size==1) return this
		  for(cas:Case <- cases){
		 	  if(cas.test) return {
		 	  	Selection(CaseExpression(),Seq(cas))
		 	  }
		  }
		  ce.expr match {
		  	case Unknown => this
		  	case _ => ce.expr
		  }
	  }
  
    /**
	 * Examines test cases. Evaluates expression of case which test first returns true, 
	 * or default expression if neither case succeeds.
	 * @return
	 */
  override def eval:Expression = {
    for(cas:Case <- cases){
    	if(cas.test)return cas.ce.eval
    }
    ce.expr match {
	  	case Unknown => this
	  	case _ => ce.eval
	  }
  }
  
  /**
   * Maps only test expressions
   */
  final override def map(f:Transformation):Expression = {
	  val m:Seq[Case] = cases.map(c => {
	 	  val vc =c.mapAll(f)
	 	  if(vc.isInstanceOf[Case]){
	 	 	  vc.asInstanceOf[Case]
	 	  }else{
	 	 	  EmptyCase
	 	  }
	  })
	  if(cases.zip(m).forall(t => t._1 eq t._2)){
	 	  f(this)
	  }else{
	 	  f(Selection(ce,m))
	  }
  }
  
  /**
   * Returns new selection with appended case
   */
  override def or(c:Case):Selection = Selection(ce,cases.:+(c))

}