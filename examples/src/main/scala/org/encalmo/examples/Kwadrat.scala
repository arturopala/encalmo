package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._

class Kwadrat(id:Option[String],a:Expression) extends Calculation(id) {
	
	val A = Kwadrat.A
    val Iz = Kwadrat.Iz
    val Iy = Kwadrat.Iy
    val Imin = Kwadrat.Imin
    val imin = Kwadrat.imin
	
	this(A) = a*a
    this(Iz) = (a^4)/12
    this(Iy) = (a^4)/12
    this(Imin) = Iz
    this(imin) = sqrt(Imin/A)

}

object Kwadrat {
	
	def apply(id:String,a:Expression) = new Kwadrat(Option(id),a)
	
	val A = BasicSymbols.A is "pole powierzchni" unit "m²"
    val Iz = BasicSymbols.I|"z" is "moduł bezwładności względem osi Z" unit "m4"
    val Iy = BasicSymbols.I|"y" is "moduł bezwładności względem osi Y" unit "m4"
    val Imin = BasicSymbols.I|"min" is "mniejszy z modułów bezwładności" unit "m4"
    val imin = BasicSymbols.i|"min" is "promień bezwładności" unit "m"
	
}