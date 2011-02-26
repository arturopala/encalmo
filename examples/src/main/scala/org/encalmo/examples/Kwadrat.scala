package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

class Kwadrat(id:String,a:Expression) extends Section(id) {

	import SectionSymbols._
	
	this(A) = a*a
    this(Iz) = (a^4)/12
    this(Iy) = (a^4)/12
    this(Imin) = Iz
    this(imin) = sqrt(Imin/A)
    
    def info = NumSection(TextToTranslate("Rectangle",SectionSymbols.dictionary)
	)

}

object Kwadrat {
	
	def apply(id:String,a:Expression) = new Kwadrat(id,a)
	
}