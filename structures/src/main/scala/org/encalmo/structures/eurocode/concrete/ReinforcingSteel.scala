package org.encalmo.structures.eurocode.concrete

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

/** ReinforcingSteel symbols */
trait ReinforcingSteelSymbols extends SymbolConfigurator {

	import BasicSymbols._
	
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

/** ReinforcingSteel context class */
class ReinforcingSteel(name:String, p_fyk: Int, p_ft: Int, p_epsiuk: Double, standard: String) extends MapContext("reinforcing_steel") with ReinforcingSteelSymbols {

    fyk := p_fyk
    ft := p_ft
    epsiuk := p_epsiuk

    Es := 200
    gammas := 78.5
    fyd := fyk/gammaS
    epsiu := epsiuk/gammaS

	CLASS := text(name)
	gammaS := 1.15

    def label = this(CLASS)

    def info = NumSection(Text("ReinforcingSteel",dictionary),name,standard,
        Evaluate(fyk,gammaS,fyd,Es)
    )
}

/** ReinforcingSteel library */
object ReinforcingSteel extends Catalog[ReinforcingSteel]("Reinforcing Steel"){
	
	override val map = Map[String,()=>ReinforcingSteel](
		"B500SP" -> B500SP _
	)

	def B500SP = new ReinforcingSteel("B500SP",500,575,0.08,"EN 10080")
}
