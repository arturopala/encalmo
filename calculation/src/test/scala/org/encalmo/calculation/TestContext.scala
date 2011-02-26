package org.encalmo.calculation

import org.encalmo.expression._

@org.junit.Ignore
class TestContext(id:String) extends MapContext() {
	
	import BasicSymbols._
	
	val p1:Symbol = p|1
	val p2:Symbol = p!2
	val p3:Symbol = p|(3,b)
	
	val e_a:Expression = 2*p1+2*p2
	val e_b:Expression = p1*p2
	val e_c:Expression = hypot(p1,p2)
	val e_d:Expression = 2*org.encalmo.expression.min(a,b,c)
	val e_e:Expression = p1*p2 or (IsZero(p3) then p1^p2)
	
	this(a) = e_a
	this(b) = e_b
	this(c) = e_c
	this(d) = e_d
	this(e) = e_e
	
}

object TestContext {
	
	def apply(id:String) = new TestContext(id)
}