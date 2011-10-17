package org.encalmo.structures.eurocode.concrete

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** ReinforcingSteel symbols */
object ReinforcingSteelSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "reinforcing_steel"
	
	val CLASS = symbol("CLASS").makeNonPrintable
	val Es = symbol(BasicSymbols.E|"s") unit "Pa"
	val Gs = symbol(BasicSymbols.G|"s") unit "Pa"
	val fyk = symbol(f|"y,k") unit "Pa"
	val fymax = symbol(f|"y,max") unit "Pa"
	val fyd = symbol(f|"y,d") unit "Pa"
	val ft = symbol(f|"t") unit "Pa"
	val fR = symbol(f|"R") unit "Pa"
	val gammaS = symbol(gamma|"S")
	val epsiuk = symbol(BasicSymbols.epsiv|"u,k")
	val epsiu = symbol(BasicSymbols.epsiv|"u")
	val gammas = symbol(gamma|"s") unit "N/m3"
	
}

/** Common ReinforcingSteel expressions */
object ReinforcingSteelExpressions extends MapContext {

	import ReinforcingSteelSymbols._
	
	this(Es) = 200E9
	this(gammas) = 78.5E3
	this(fyd) = fyk/gammaS
	this(epsiu) = epsiuk/gammaS
	
	lock
}

/** ReinforcingSteel context class */
class ReinforcingSteel(id:String,data:Context) extends Calculation(Option(id)) {

	import ReinforcingSteelSymbols._
	
	def info = NumSection(TextToTranslate("ReinforcingSteel",dictionary),id,"EN 10080",
		Evaluate(Seq(fyk,gammaS,fyd,Es),this)
	)
	
	this add ReinforcingSteelExpressions
	this add data
	
	this(CLASS) = text(id)
	this(gammaS) = 1.15
}

/** ReinforcingSteel library */
object ReinforcingSteel {
	
	import ReinforcingSteelSymbols._
	
	def apply(s:String):ReinforcingSteel = map.get(s).map(x => x()).getOrElse(throw new IllegalStateException)
	
	val map = Map[String,()=>ReinforcingSteel](
		"B500SP" -> B500SP _
	)

	def B500SP = new ReinforcingSteel("B500SP",data_B500SP)
	
	private lazy val data_B500SP = new MapContext {
		this(fyk) = 500E6
		this(ft) = 575E6
		this(epsiuk) = 0.08
		lock
	}
}
