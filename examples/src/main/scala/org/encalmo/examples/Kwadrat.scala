package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._

case class Kwadrat(override val id:String,a:Expression) extends Calculation(id) {
	
	val A = Kwadrat.A at id
    val Iz = Kwadrat.Iz at id
    val Iy = Kwadrat.Iy at id
    val Imin = Kwadrat.Imin at id
    val imin = Kwadrat.imin at id
	
	this(A) = a*a
    this(Iz) = (a^4)/12
    this(Iy) = (a^4)/12
    this(Imin) = Iz
    this(imin) = sqrt(Imin/A)

}

object Kwadrat {
	
	val A = BasicSymbols.A is "pole powierzchni" unit "m²"
    val Iz = BasicSymbols.I|"z" is "moduł bezwładności względem osi Z" unit "m4"
    val Iy = BasicSymbols.I|"y" is "moduł bezwładności względem osi Y" unit "m4"
    val Imin = BasicSymbols.I|"min" is "mniejszy z modułów bezwładności" unit "m4"
    val imin = BasicSymbols.i|"min" is "promień bezwładności" unit "m"
	
}