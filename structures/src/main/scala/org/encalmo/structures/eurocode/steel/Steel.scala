package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Steel symbols */
object SteelSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "steel"
	
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

/** Common steel expressions */
object SteelExpressions extends MapContext {

	import SteelSymbols._
	
	E      := 210
	gammas := 78.5
	fyd    := fy/gammaM0
	epsiy  := fy/E
	epsi   := sqrt(Number(235,SI.MPa)/fy)
	lock()
}

/** Steel context class */
class Steel(id:String,data:Context) extends Calculation(Option(id)) {

	import SteelSymbols._
	
	def info = NumSection(TextToTranslate("Steel",dictionary),id,"EN 10025-2",
		Evaluate(fy,gammaM0,fyd,E)
	)
	
	this add SteelExpressions
	this add data
	
	CLASS   := text(id)
	gammaM0 := 1
	gammaM1 := 1
	gammaV  := 1.25
	
	override def label = this(CLASS)
}

/** Steel library */
object Steel {
	
	import SteelSymbols._
	
	def apply(s:String):Steel = map.get(s).map(x => x()).getOrElse(throw new IllegalStateException)
	
	val map = Map[String,()=>Steel](
		"S235" -> S235 _,
		"S275" -> S275 _,
		"S355" -> S355 _,
		"S450" -> S450 _,
		"S280 GD" -> S280GD _
	)

	def S235 = new Steel("S235",data_S235)
	def S275 = new Steel("S275",data_S275)
	def S355 = new Steel("S355",data_S355)
	def S450 = new Steel("S450",data_S450)
	def S280GD = new Steel("S280 GD",data_S280GD)
	def S350GD = new Steel("S350 GD",data_S350GD)
	
	private lazy val data_S235 = new MapContext {
		fy := 235
		fu := 360
		lock()
	}
	
	private lazy val data_S275 = new MapContext {
		fy := 275
		fu := 430
		lock()
	}
	
	private lazy val data_S355 = new MapContext {
		fy := 355
		fu := 510
		lock()
	}
	
	private lazy val data_S450 = new MapContext {
		fy := 440
		fu := 550
		lock()
	}
	
	private lazy val data_S280GD = new MapContext {
		E  := 210
		fy := 280
		lock()
	}
	
	private lazy val data_S350GD = new MapContext {
        E  := 210
        fy := 350
        lock()
    }
}
