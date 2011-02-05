package org.encalmo.calculation

import org.encalmo.expression._

@org.junit.Ignore
class TestContext2(id:String,coeff:Double) extends DefaultContext() {
	
	import BasicSymbols._
	
	val a:Symbol = BasicSymbols.a|id
	val b:Symbol = BasicSymbols.b|id
	val c:Symbol = BasicSymbols.c|id
	val d:Symbol = BasicSymbols.d|id
	val e:Symbol = BasicSymbols.e|id
	
	val yy:Expression = y/coeff
	val e_a:Expression = 2*x+2*yy
	val e_b:Expression = x*yy
	val e_c:Expression = hypot(x,yy)
	val e_d:Expression = 2*org.encalmo.expression.min(a,b,c)
	val e_e:Expression = x*yy or (IsZero(z) then x^yy)
	
	put (a, e_a)
	put (b, e_b)
	put (c, e_c)
	put (d, e_d)
	put (e, e_e)
	
}

object TestContext2 {
	
	def apply(id:String,coeff:Double) = new TestContext2(id,coeff)
}