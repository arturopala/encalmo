package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._

object ShapeSymbols extends SymbolConfigurator {

    import BasicSymbols._
    val dictionary, contextId = "shape"
    
    val A = symbol(BasicSymbols.A) unit "m²"
    val Iz = symbol(BasicSymbols.I|"z") unit "m4"
    val Iy = symbol(BasicSymbols.I|"y") unit "m4"
    val Imin = symbol(BasicSymbols.I|"min") unit "m4"
    val imin = symbol(BasicSymbols.i|"min") unit "m"
    val Wzd = symbol(BasicSymbols.W|("z","d")) unit "m4"
    val Wyd = symbol(BasicSymbols.W|("y","d")) unit "m4"
    val Wzg = symbol(BasicSymbols.W|("z","g")) unit "m4"
    val Wyg = symbol(BasicSymbols.W|("y","g")) unit "m4"
        
}

object ShapeExpressions extends MapContext {
	
	import ShapeSymbols._

}

/** Section's shape trait */
trait Shape {

	import ShapeSymbols._

}