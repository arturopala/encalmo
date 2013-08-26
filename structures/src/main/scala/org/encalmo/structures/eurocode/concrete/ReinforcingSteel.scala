package org.encalmo.structures.eurocode.concrete

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

/** ReinforcingSteel symbols */
object ReinforcingSteelSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "reinforcing_steel"
	
	val CLASS = symbol("CLASS").makeNonPrintable
	val Es = symbol(BasicSymbols.E|"s") unit SI.GPa
	val Gs = symbol(BasicSymbols.G|"s") unit SI.GPa
	val fyk = symbol(f|"y,k") unit SI.MPa
	val fymax = symbol(f|"y,max") unit SI.MPa
	val fyd = symbol(f|"y,d") unit SI.MPa
	val ft = symbol(f|"t") unit SI.MPa
	val fR = symbol(f|"R") unit SI.MPa
	val gammaS = symbol(gamma|"S")
	val epsiuk = symbol(BasicSymbols.epsiv|"u,k")
	val epsiu = symbol(BasicSymbols.epsiv|"u")
	val gammas = symbol(gamma|"s") unit "kN/m3"
	
}

/** Common ReinforcingSteel expressions */
object ReinforcingSteelExpressions extends MapContext {

	import ReinforcingSteelSymbols._
	
	this(Es) = 200
	this(gammas) = 78.5
	this(fyd) = fyk/gammaS
	this(epsiu) = epsiuk/gammaS
	
	lock()
}

/** ReinforcingSteel context class */
class ReinforcingSteel(name:String, data:Context) extends Calculation(name) {

	import ReinforcingSteelSymbols._
	
	def info = NumSection(TextToTranslate("ReinforcingSteel",dictionary),name,"EN 10080",
		Evaluate(fyk,gammaS,fyd,Es)
	)
	
	this add ReinforcingSteelExpressions
	this add data
	
	this(CLASS) = text(name)
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
		this(fyk) = 500
		this(ft) = 575
		this(epsiuk) = 0.08
		lock()
	}
}
