package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._

class Kwadrat(id:Option[String],a:Expression) extends Calculation(id) {

	import ShapeSymbols._
	
	this(A) = a*a
    this(Iz) = (a^4)/12
    this(Iy) = (a^4)/12
    this(Imin) = Iz
    this(imin) = sqrt(Imin/A)

}

object Kwadrat {
	
	def apply(id:String,a:Expression) = new Kwadrat(Option(id),a)
	
}