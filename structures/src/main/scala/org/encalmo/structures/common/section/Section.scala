package org.encalmo.structures.common.section

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._


trait SectionSymbols extends SymbolConfigurator {

    val sectionDict = "section"

    val ID = symbol("ID").makeNonPrintable dict sectionDict
    //Dimensions
    val h = symbol("h") unit "mm" dict sectionDict
    val b = symbol("b") unit "mm" dict sectionDict
    //Area
    val A = symbol(BasicSymbols.A) unit "cm2" dict sectionDict
    val AVz = symbol(BasicSymbols.A|"Vz") unit "cm2" dict sectionDict
    //Surface
    val AL = symbol(BasicSymbols.A|"L") unit "m2/m" dict sectionDict
    val AG = symbol(BasicSymbols.A|"G") unit "m2/kg" dict sectionDict
    //Section properties
    val Iz = symbol(BasicSymbols.I|"z") unit "cm4" dict sectionDict
    val Iy = symbol(BasicSymbols.I|"y") unit "cm4" dict sectionDict
    val Iyz = symbol(BasicSymbols.I|"yz") unit "cm4" dict sectionDict
    val iz = symbol(BasicSymbols.i|"z") unit "cm" dict sectionDict
    val iy = symbol(BasicSymbols.i|"y") unit "cm" dict sectionDict
    val Imin = symbol(BasicSymbols.I|"min") unit "cm4" dict sectionDict
    val imin = symbol(BasicSymbols.i|"min") unit "cm" dict sectionDict
    val Wz = symbol(BasicSymbols.W| "z") unit "cm3" dict sectionDict
    val Wy = symbol(BasicSymbols.W| "y") unit "cm3" dict sectionDict
    val Wzd = symbol(BasicSymbols.W|("z","d")) unit "cm3" dict sectionDict
    val Wyd = symbol(BasicSymbols.W|("y","d")) unit "cm3" dict sectionDict
    val Wzg = symbol(BasicSymbols.W|("z","g")) unit "cm3" dict sectionDict
    val Wyg = symbol(BasicSymbols.W|("y","g")) unit "cm3" dict sectionDict
    val Wzpl = symbol(BasicSymbols.W|"z,pl") unit "cm3" dict sectionDict
    val Wypl = symbol(BasicSymbols.W|"y,pl") unit "cm3" dict sectionDict
    val Iomega = symbol(BasicSymbols.I|BasicSymbols.omega) unit "cm6" dict sectionDict
    val It = symbol(BasicSymbols.I|BasicSymbols.t) unit "cm4" dict sectionDict
    val alpha = symbol(BasicSymbols.alpha) unit SI.deg dict sectionDict
    val Iu = symbol(BasicSymbols.I|"u") unit "cm4" dict sectionDict
    val Iv = symbol(BasicSymbols.I|"v") unit "cm4" dict sectionDict
    val iu = symbol(BasicSymbols.i|"u") unit "cm" dict sectionDict
    val iv = symbol(BasicSymbols.i|"v") unit "cm" dict sectionDict
    //Mass
    val m = symbol(BasicSymbols.m) unit "kg/m" dict sectionDict
    
}

/** Section trait */
abstract class Section(val name:String) extends MapContext with SectionSymbols {
	
	def descriptionRef:String = ""
	def info:DocumentComponent

    ID := text(name)

    Imin := min(Iz,Iy)
    imin := min(iy,iz)

    Iu := Iy
    Iv := Iz
    iu := iy
    iv := iz
    alpha := ZERO

    def label = this(ID)

}
