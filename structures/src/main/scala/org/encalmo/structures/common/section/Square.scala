package org.encalmo.structures.common.section

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

class Square(id:String,a:Expression) extends Section(id) {

	import SectionSymbols._
	
	this(A) = a*a
    this(Iz) = (a^4)/12
    this(Iy) = (a^4)/12
    this(Imin) = Iz
    this(imin) = sqrt(Imin/A)
    
    def info = NumSection(TextToTranslate("Square",SectionSymbols.dictionary)
	)

}

object Square {
	
	def apply(id:String,a:Expression) = new Square(id,a)
	
}
