package org.encalmo.expression

/**
 * Case is a part of the Selection. 
 * If case test evaluates to true then expression is used by parent Selection.
 * @author artur.opala
 */
case class Case(e:Expression,t:CaseTest) extends Expression with Auxiliary {
	
  def test:Boolean = t.test
  
  final override def eval():Expression = {
	  val ev = e eval; 
	  if(ev.ne(e)) Case(ev,t) else this
  }
  
  final override def map(f:Transformation):Expression = {
	  val ve = e.map(f);
	  val vt = t.map(f);
	  if(ve==e && vt==t) f(this) else {
	 	  if(vt.isInstanceOf[CaseTest]){
	 	 	  f(Case(ve,vt.asInstanceOf[CaseTest]))
	 	  }else{
	 	 	  f(Case(ve,Never))
	 	  }
	  }
  }
	
}

/**
 * Special purpose empty case
 * @author artur.opala
 */
object EmptyCase extends Case(Void,Never)