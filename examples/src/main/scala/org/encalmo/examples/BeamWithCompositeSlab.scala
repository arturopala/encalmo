package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Composite slab with profiled steel sheeting symbols */
object BeamOfCompositeSlabSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "compositeSlabWithProfiledSheeting"
	
	
}

/** Composite slab with profiled steel sheeting context */
object BeamOfCompositeSlabExpressions extends MapContext {

	//import BeamOfCompositeSlabSymbols.{}
	//import CompositeSlabWithProfiledSheetingSymbols.{}
	//import ProfiledSteelSheetSymbols.{}
	import SteelSymbols.{E,fypd}
    //import ConcreteSymbols.{}
    //import ActionsSymbols.{}
	
	
	
	// end of context initialization
	lock

}

class BeamOfCompositeSlab(
	length:Expression,
	shape:SteelBeamShape, 
	steel:Steel,
	slab:CompositeSlabWithProfiledSheeting
)
extends Calculation {

	//import BeamOfCompositeSlabSymbols.{}
	//import CompositeSlabWithProfiledSheetingSymbols.{}
	//import ProfiledSteelSheetSymbols.{}
	import SteelSymbols.{E,fypd}
    //import ConcreteSymbols.{}
    //import ActionsSymbols.{}

	this add BeamOfCompositeSlabExpressions
	this add shape
	this add steel
	this add slab
	
	
	def info = NumSection(TextToTranslate("BeamOfCompositeSlab",BeamOfCompositeSlabSymbols.dictionary)
	)
	
	def LOAD1 = Evaluate(Seq(),this)
	
	def ULS1 = NumSection(TextToTranslate("ULS","eurocode")
	)
	
	def SLS1 = NumSection(TextToTranslate("SLS","eurocode")
	)
	
	def LOAD2 = Evaluate(Seq(),this)
	
	def ULS2 = NumSection(TextToTranslate("ULS","eurocode")
	)
	
	def SLS2 = NumSection(TextToTranslate("SLS","eurocode")
	)
	
}	
