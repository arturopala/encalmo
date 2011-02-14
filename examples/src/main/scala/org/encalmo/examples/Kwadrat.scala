package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._

object ShapeSymbols extends SymbolConfigurator {

    import BasicSymbols._
    val dictionary, contextId = "geometry"
    
    val A = symbol(BasicSymbols.A) is "pole powierzchni" unit "m²"
    val Iz = symbol(BasicSymbols.I|"z") is "moduł bezwładności względem osi Z" unit "m4"
    val Iy = symbol(BasicSymbols.I|"y") is "moduł bezwładności względem osi Y" unit "m4"
    val Imin = symbol(BasicSymbols.I|"min") is "mniejszy z modułów bezwładności" unit "m4"
    val imin = symbol(BasicSymbols.i|"min") is "promień bezwładności" unit "m"
        
}

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