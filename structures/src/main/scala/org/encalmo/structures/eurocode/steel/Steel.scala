package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.ContextFactory
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.document._
import org.encalmo.expression.sqrt

/** Steel symbols */
trait SteelSymbols extends SymbolConfigurator {

	import BasicSymbols._
	
	val CLASS = symbol("CLASS").makeNonPrintable
	val E = symbol(BasicSymbols.E) unit SI.GPa //Moduł sprężystości
	val G = symbol(BasicSymbols.G) unit SI.GPa //Moduł sprężystości przy ścinaniu
	val fy = symbol(f|"y") unit SI.MPa         //Charakterystyczna granica plastyczności stali
	val fyd = symbol(f|"y,d") unit SI.MPa      //Obliczeniowa granica plastyczności stali
	val fu = symbol(f|"u") unit SI.MPa         //Wytrzymałość na rozciąganie
	val gammaM0 = symbol(gamma|"M,0")          //Współczynnik materiałowy dla stali przy weryfikacji nośności przekroju poprzecznego, wg 6.1(1)
	val gammaM1 = symbol(gamma|"M,1")          //Współczynnik materiałowy dla stali przy ocenie stateczności, wg 6.1(1)
	val gammaV = symbol(gamma|"V")             //Odkształcenie odpowiadające granicy plastyczności
	val gammas = symbol(gamma|"s") unit "kN/m3"//Ciężar objętościowy stali
	val epsiy = symbol(BasicSymbols.epsiv|"y") //Odkształcenie odpowiadające granicy plastyczności
	val epsiu = symbol(BasicSymbols.epsiv|"u") //Odkształcenie odpowiadające wytrzymałości na rozciąganie
	val epsi = symbol(BasicSymbols.epsi)       //Współczynnik korekcyjny klasy stali
}

/** Steel context class */
class Steel(name:String, proof_strength: Int, tensile_strength: Int,  standard: String) extends MapContext("steel") with SteelSymbols {

    fy := proof_strength
    fu := tensile_strength

    E      := 210
    gammas := 78.5
    fyd    := fy/gammaM0
    epsiy  := fy/E
    epsi   := sqrt(Number(235,SI.MPa)/fy)

	CLASS   := text(name)
	gammaM0 := 1
	gammaM1 := 1
	gammaV  := 1.25

    def info = NumSection(TextToTranslate("Steel",dictionary),name,standard,
        Evaluate(fy,gammaM0,fyd,E)
    )

	def label = this(CLASS)
}

/** Steel library */
object Steel extends Catalog[Steel]("Steel") {
	
	override val map = Map[String,()=>Steel](
		"S235" -> S235 _,
		"S275" -> S275 _,
		"S355" -> S355 _,
		"S450" -> S450 _,
		"S280 GD" -> S280GD _,
        "S350 GD" -> S350GD _
	)

	def S235 = new Steel("S235",235,360,"EN 10025-2")
	def S275 = new Steel("S275",275,430,"EN 10025-2")
	def S355 = new Steel("S355",355,510,"EN 10025-2")
	def S450 = new Steel("S450",440,550,"EN 10025-2")
	def S280GD = new Steel("S280 GD",280,360,"EN 10326")
	def S350GD = new Steel("S350 GD",350,420,"EN 10326")

}
