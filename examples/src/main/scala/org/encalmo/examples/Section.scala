package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._


object SectionSymbols extends SymbolConfigurator {

    import BasicSymbols._
    val dictionary, contextId = "section"
    
    val A = symbol(BasicSymbols.A) unit "m²"
    val h = symbol("h") unit "m"
    val b = symbol("b") unit "m"
    val Iz = symbol(BasicSymbols.I|"z") unit "m4"
    val Iy = symbol(BasicSymbols.I|"y") unit "m4"
    val iz = symbol(BasicSymbols.i|"z") unit "m"
    val iy = symbol(BasicSymbols.i|"y") unit "m"
    val Imin = symbol(BasicSymbols.I|"min") unit "m4"
    val imin = symbol(BasicSymbols.i|"min") unit "m"
    val Wz = symbol(BasicSymbols.W|("z")) unit "m4"
    val Wy = symbol(BasicSymbols.W|("y")) unit "m4"
    val Wzd = symbol(BasicSymbols.W|("z","d")) unit "m4"
    val Wyd = symbol(BasicSymbols.W|("y","d")) unit "m4"
    val Wzg = symbol(BasicSymbols.W|("z","g")) unit "m4"
    val Wyg = symbol(BasicSymbols.W|("y","g")) unit "m4"
    val Wzpl = symbol(BasicSymbols.W|"z,pl") unit "m4"
    val Wypl = symbol(BasicSymbols.W|"y,pl") unit "m4"
    val Iomega = symbol(BasicSymbols.I|BasicSymbols.omega) unit "m6"
    val It = symbol(BasicSymbols.I|BasicSymbols.t) unit "m4"
    val m = symbol(BasicSymbols.m) unit "kg/m"
    val u = symbol(BasicSymbols.u) unit "m2/m"
        
}

object SectionExpressions extends MapContext {
	
	import SectionSymbols._
	
	this(Imin) = min(Iz,Iy)
	this(imin) = min(iy,iz)

}

/** Section trait */
abstract class Section(id:String) extends Calculation(Option(id)){

	import SectionSymbols._
	
	this add SectionExpressions
	
	def info:DocumentComponent

}