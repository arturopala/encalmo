package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Concrete related symbols */
object ConcreteSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "concrete"
	
	val CLASS = symbol("CLASS").makeNonPrintable
	val fck = symbol(f|"ck") unit "Pa"
	val fckcube = symbol(f|"ck,cube") unit "Pa"
	val fcm = symbol(f|"cm") unit "Pa"
	val fctm = symbol(f|"ctm") unit "Pa"
	val fctk = symbol(f|"ctk") unit "Pa"
	val fctk095 = symbol(f|"ctk, 0,95") unit "Pa"
	val Ecm = symbol(E|"cm") unit "Pa"
	val epsic1 = symbol(epsiv|"c1") unit "‰"
	val epsicu1 = symbol(epsiv|"cu1") unit "‰"
	val epsic2 = symbol(epsiv|"c2") unit "‰"
	val epsicu2 = symbol(epsiv|"cu2") unit "‰"
	val epsic3 = symbol(epsiv|"c3") unit "‰"
	val epsicu3 = symbol(epsiv|"cu3") unit "‰"
	val n = symbol(BasicSymbols.n)
	val gammaC = symbol(gamma|"C")
	val fcd = symbol(f|"cd") unit "Pa"
	val fctd = symbol(f|"ctd") unit "Pa"
	val gammac = symbol(gamma|"c") unit "N/m3"
	val gammacf = symbol(gamma|"c,f") unit "N/m3"
}

/** Common Concrete expressions */
object ConcreteExpressions extends MapContext {

	import ConcreteSymbols._
	
	this(fcd) = fck/gammaC
	this(fctd) = fctk/gammaC
	this(gammac) = 25000
	this(gammacf) = gammac+1000
}

/** Concrete class */
class Concrete(id:String,data:Context) extends Calculation(Option(id)) {

	import ConcreteSymbols._

	def info = NumSection(TextToTranslate("Concrete",ConcreteSymbols.dictionary),id,
		Evaluate(Seq(fck,gammaC,fcd,fcm,fctk,fctd,Ecm,epsic1,epsicu1),this)
	)
	
	this add ConcreteExpressions
	this add data
	this(CLASS) = text(id)
	
}

/** Concrete classes library */
object Concrete {

	import ConcreteSymbols._

	def C_50_60:Concrete = new Concrete("C50/60",data_C_50_60)

	private lazy val data_C_50_60 = new MapContext{
		this(fck) = 50E6
		this(fckcube) = 60E6
		this(fcm) = 58E6
		this(fctm) = 4.1E6
		this(fctk) = 2.9E6
		this(fctk095) = 5.3E6
		this(Ecm) = 37E9
		this(epsic1) = 2.45
		this(epsicu1) = 3.5
		this(epsic2) = 2.0
		this(epsicu2) = 3.5
		this(epsic3) = 1.75
		this(epsicu3) = 3.5
		this(n) = 2.0
		lock
	}
	
}