package org.encalmo.structures.common.section

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document.{NumSection, Text}

class Square(name:String,a:Expression) extends Section(name) {
	
	A := a*a
    Iz := (a^4)/12
    Iy := (a^4)/12
    Imin := Iz
    imin := sqrt(Imin/A)
    
    def info = NumSection(Text("Square",sectionDict))

}

object Square {
	
	def apply(id:String,a:Expression) = new Square(id,a)
	
}
