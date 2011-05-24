package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** RectangularSilos symbols */
object RectangularSilosSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "rectangularSilos"
	
	//geometry
	val b1 = symbol(BasicSymbols.b|1) unit "m"
    val b2 = symbol(BasicSymbols.b|2) unit "m"
    val h1 = symbol(BasicSymbols.h|1) unit "m"
    val h2 = symbol(BasicSymbols.h|2) unit "m"
	
}

/** RectangularSilos context */
object RectangularSilosExpressions extends MapContext {

	import RectangularSilosSymbols._
	
	//this(hc) = h-hp
	
	// end of context initialization
	lock

}

class RectangularSilos(
	vb1:Expression,
	vb2:Expression,
	vh1:Expression, 
	vh2:Expression, 
	particulateSolid:ParticulateSolid,
	vD:Expression, 
	concrete:Concrete,
	reinforcingSteel:ReinforcingSteel
)
extends Calculation {

	import RectangularSilosSymbols._
	import ParticulateSolidSymbols._
	import ConcreteSymbols.{fcd,fck,fctm}
	
	particulateSolid(ParticulateSolidSymbols.D) = vD
	
	this add RectangularSilosExpressions
	this add particulateSolid
	this add concrete
	this add reinforcingSteel
	
	this(b1) = vb1
	this(b2) = vb2
	this(h1) = vh1
	this(h2) = vh2
	
	def inputGeometry = NumSection(TextToTranslate("_inputGeometry",RectangularSilosSymbols.dictionary),
		Evaluate(Seq(b1,b2,h1,h2),this)
	)
	
}	
