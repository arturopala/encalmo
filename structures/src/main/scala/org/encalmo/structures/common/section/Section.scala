package org.encalmo.structures.common.section

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._


object SectionSymbols extends SymbolConfigurator {

    import BasicSymbols._
    val dictionary, contextId = "section"
    
    //Dimensions
    val h = symbol("h") unit "mm"
    val b = symbol("b") unit "mm"
    //Area
    val A = symbol(BasicSymbols.A) unit "cm2"
    val AVz = symbol(BasicSymbols.A|"Vz") unit "cm2"
    //Surface
    val AL = symbol(BasicSymbols.A|"L") unit "m2/m"
    val AG = symbol(BasicSymbols.A|"G") unit "m2/kg"
    //Section properties
    val Iz = symbol(BasicSymbols.I|"z") unit "cm4"
    val Iy = symbol(BasicSymbols.I|"y") unit "cm4"
    val iz = symbol(BasicSymbols.i|"z") unit "cm"
    val iy = symbol(BasicSymbols.i|"y") unit "cm"
    val Imin = symbol(BasicSymbols.I|"min") unit "cm4"
    val imin = symbol(BasicSymbols.i|"min") unit "cm"
    val Wz = symbol(BasicSymbols.W|("z")) unit "cm3"
    val Wy = symbol(BasicSymbols.W|("y")) unit "cm3"
    val Wzd = symbol(BasicSymbols.W|("z","d")) unit "cm3"
    val Wyd = symbol(BasicSymbols.W|("y","d")) unit "cm3"
    val Wzg = symbol(BasicSymbols.W|("z","g")) unit "cm3"
    val Wyg = symbol(BasicSymbols.W|("y","g")) unit "cm3"
    val Wzpl = symbol(BasicSymbols.W|"z,pl") unit "cm3"
    val Wypl = symbol(BasicSymbols.W|"y,pl") unit "cm3"
    val Iomega = symbol(BasicSymbols.I|BasicSymbols.omega) unit "cm6"
    val It = symbol(BasicSymbols.I|BasicSymbols.t) unit "cm4"
    //Mass
    val m = symbol(BasicSymbols.m) unit "kg/m"
    //Fire resistance coefficients
    val f1 = symbol(BasicSymbols.f|1)
    val f2 = symbol(BasicSymbols.f|2)
    val f3 = symbol(BasicSymbols.f|3)
    val f4 = symbol(BasicSymbols.f|4)
    
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
	
	def descriptionRef:String = ""
	def info:DocumentComponent

}
