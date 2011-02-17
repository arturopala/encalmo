package org.encalmo.examples

import org.encalmo.expression._
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
	val E = symbol(BasicSymbols.E) unit "Pa"
	val G = symbol(BasicSymbols.G) unit "Pa"
	val fyp = symbol(f|"yp") unit "Pa"
	val fypd = symbol(f|"yp,d") unit "Pa"
	val fyb = symbol(f|"yb") unit "Pa"
	val fu = symbol(f|"u") unit "Pa"
	val gammaM0 = symbol(gamma|"M,0")
	val gammaM1 = symbol(gamma|"M,1")
	val gammas = symbol(gamma|"s") unit "N/m3"
	val epsiy = symbol(BasicSymbols.epsiv|"y")
	val epsiu = symbol(BasicSymbols.epsiv|"u")
}

/** Common steel expressions */
object SteelExpressions extends MapContext {

	import SteelSymbols._
	
	this(E) = 210E9
	this(gammas) = 78.5E3
	this(fypd) = fyp/gammaM0
	this(fyb) = fyp
	this(epsiy) = fyp/E
	lock
}

/** Steel context class */
class Steel(id:String,data:Context) extends Calculation(Option(id)) {

	import SteelSymbols._
	
	def info = NumSection(TextToTranslate("Steel",dictionary),id,"EN 10025-2",
		Evaluate(Seq(fyp,gammaM0,fypd,E),this)
	)
	
	this add SteelExpressions
	this add data
	this(CLASS) = text(id)
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
	
	private lazy val data_S235 = new MapContext {
		this(fyp) = 235E6
		this(fu) = 360E6
		lock
	}
	
	private lazy val data_S275 = new MapContext {
		this(fyp) = 275E6
		this(fu) = 430E6
		lock
	}
	
	private lazy val data_S355 = new MapContext {
		this(fyp) = 355E6
		this(fu) = 510E6
		lock
	}
	
	private lazy val data_S450 = new MapContext {
		this(fyp) = 440E6
		this(fu) = 550E6
		lock
	}
	
	private lazy val data_S280GD = new MapContext {
		this(E) = 210E9
		this(fyp) = 280E6
		lock
	}
}