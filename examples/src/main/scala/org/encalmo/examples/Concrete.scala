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
	this(gammaC) = 1.5
	
}

class NormalConcrete extends MapContext {

	import ConcreteSymbols._

	this(epsicu1) = 3.5
	this(epsic2) = 2.0
	this(epsicu2) = 3.5
	this(epsic3) = 1.75
	this(epsicu3) = 3.5
	this(n) = 2.0
}

/** Concrete classes library */
object Concrete {

	import ConcreteSymbols._
	
	def apply(s:String):Concrete = map.get(s).map(x => x()).getOrElse(throw new IllegalStateException)
	
	val map = Map[String,()=>Concrete](
		"C12/15" -> C_12_15 _,
		"C16/20" -> C_16_20 _,
		"C20/25" -> C_20_25 _,
		"C25/30" -> C_25_30 _,
		"C30/37" -> C_30_37 _,
		"C35/45" -> C_35_45 _,
		"C40/50" -> C_40_50 _,
		"C45/55" -> C_45_55 _,
		"C50/60" -> C_50_60 _,
		"C55/67" -> C_55_67 _,
		"C60/75" -> C_60_75 _,
		"C70/85" -> C_70_85 _,
		"C80/95" -> C_80_95 _,
		"C90/105" -> C_90_105 _
	)
	
	//normal
	def C_12_15:Concrete = new Concrete("C12/15",data_C_12_15)
	def C_16_20:Concrete = new Concrete("C16/20",data_C_16_20)
	def C_20_25:Concrete = new Concrete("C20/25",data_C_20_25)
	def C_25_30:Concrete = new Concrete("C25/30",data_C_25_30)
	def C_30_37:Concrete = new Concrete("C30/37",data_C_30_37)
	def C_35_45:Concrete = new Concrete("C35/45",data_C_35_45)
	def C_40_50:Concrete = new Concrete("C40/50",data_C_40_50)
	def C_45_55:Concrete = new Concrete("C45/55",data_C_45_55)
	def C_50_60:Concrete = new Concrete("C50/60",data_C_50_60)
	//special
	def C_55_67:Concrete = new Concrete("C57/67",data_C_55_67)
	def C_60_75:Concrete = new Concrete("C60/75",data_C_60_75)
	def C_70_85:Concrete = new Concrete("C70/85",data_C_70_85)
	def C_80_95:Concrete = new Concrete("C80/95",data_C_80_95)
	def C_90_105:Concrete = new Concrete("C50/60",data_C_90_105)
	
	private lazy val data_C_12_15 = new NormalConcrete{
		this(fck) = 12E6
		this(fckcube) = 15E6
		this(fcm) = 20E6
		this(fctm) = 1.6E6
		this(fctk) = 1.1E6
		this(fctk095) = 2.0E6
		this(Ecm) = 27E9
		this(epsic1) = 1.8
		lock
	}
	
	private lazy val data_C_16_20 = new NormalConcrete{
		this(fck) = 16E6
		this(fckcube) = 20E6
		this(fcm) = 24E6
		this(fctm) = 1.9E6
		this(fctk) = 1.3E6
		this(fctk095) = 2.5E6
		this(Ecm) = 29E9
		this(epsic1) = 1.9
		lock
	}
	
	private lazy val data_C_20_25 = new NormalConcrete{
		this(fck) = 20E6
		this(fckcube) = 25E6
		this(fcm) = 28E6
		this(fctm) = 2.2E6
		this(fctk) = 1.5E6
		this(fctk095) = 2.9E6
		this(Ecm) = 30E9
		this(epsic1) = 2
		lock
	}
	
	private lazy val data_C_25_30 = new NormalConcrete{
		this(fck) = 25E6
		this(fckcube) = 30E6
		this(fcm) = 33E6
		this(fctm) = 2.6E6
		this(fctk) = 1.8E6
		this(fctk095) = 3.3E6
		this(Ecm) = 31E9
		this(epsic1) = 2.1
		lock
	}
	
	private lazy val data_C_30_37 = new NormalConcrete{
		this(fck) = 30E6
		this(fckcube) = 37E6
		this(fcm) = 38E6
		this(fctm) = 2.9E6
		this(fctk) = 2.0E6
		this(fctk095) = 3.8E6
		this(Ecm) = 32E9
		this(epsic1) = 2.2
		lock
	}
	
	private lazy val data_C_35_45 = new NormalConcrete{
		this(fck) = 35E6
		this(fckcube) = 45E6
		this(fcm) = 43E6
		this(fctm) = 3.2E6
		this(fctk) = 2.2E6
		this(fctk095) = 4.2E6
		this(Ecm) = 34E9
		this(epsic1) = 2.25
		lock
	}
	
	private lazy val data_C_40_50 = new NormalConcrete{
		this(fck) = 40E6
		this(fckcube) = 50E6
		this(fcm) = 48E6
		this(fctm) = 3.5E6
		this(fctk) = 2.5E6
		this(fctk095) = 4.6E6
		this(Ecm) = 35E9
		this(epsic1) = 2.3
		lock
	}
	
	private lazy val data_C_45_55 = new NormalConcrete{
		this(fck) = 45E6
		this(fckcube) = 55E6
		this(fcm) = 53E6
		this(fctm) = 3.8E6
		this(fctk) = 2.7E6
		this(fctk095) = 4.9E6
		this(Ecm) = 36E9
		this(epsic1) = 2.4
		lock
	}
	
	private lazy val data_C_50_60 = new NormalConcrete{
		this(fck) = 50E6
		this(fckcube) = 60E6
		this(fcm) = 58E6
		this(fctm) = 4.1E6
		this(fctk) = 2.9E6
		this(fctk095) = 5.3E6
		this(Ecm) = 37E9
		this(epsic1) = 2.45
		lock
	}
	
	private lazy val data_C_55_67 = new MapContext{
		this(fck) = 55E6
		this(fckcube) = 67E6
		this(fcm) = 63E6
		this(fctm) = 4.2E6
		this(fctk) = 3E6
		this(fctk095) = 5.5E6
		this(Ecm) = 38E9
		this(epsic1) = 2.5
		this(epsicu1) = 3.2
		this(epsic2) = 2.2
		this(epsicu2) = 3.1
		this(n) = 1.75
		this(epsic3) = 1.8
		this(epsicu3) = 3.1
		lock
	}
	
	private lazy val data_C_60_75 = new MapContext{
		this(fck) = 60E6
		this(fckcube) = 75E6
		this(fcm) = 68E6
		this(fctm) = 4.4E6
		this(fctk) = 3.1E6
		this(fctk095) = 5.7E6
		this(Ecm) = 39E9
		this(epsic1) = 2.6
		this(epsicu1) = 3.0
		this(epsic2) = 2.3
		this(epsicu2) = 2.9
		this(n) = 1.6
		this(epsic3) = 1.9
		this(epsicu3) = 2.9
		lock
	}
	
	private lazy val data_C_70_85 = new MapContext{
		this(fck) = 70E6
		this(fckcube) = 85E6
		this(fcm) = 78E6
		this(fctm) = 4.6E6
		this(fctk) = 3.2E6
		this(fctk095) = 6.0E6
		this(Ecm) = 41E9
		this(epsic1) = 2.7
		this(epsicu1) = 2.8
		this(epsic2) = 2.4
		this(epsicu2) = 2.7
		this(n) = 1.45
		this(epsic3) = 2.0
		this(epsicu3) = 2.7
		lock
	}
	
	private lazy val data_C_80_95 = new MapContext{
		this(fck) = 80E6
		this(fckcube) = 95E6
		this(fcm) = 88E6
		this(fctm) = 4.8E6
		this(fctk) = 3.4E6
		this(fctk095) = 6.3E6
		this(Ecm) = 42E9
		this(epsic1) = 2.8
		this(epsicu1) = 2.8
		this(epsic2) = 2.5
		this(epsicu2) = 2.6
		this(n) = 1.4
		this(epsic3) = 2.2
		this(epsicu3) = 2.6
		lock
	}
	
	private lazy val data_C_90_105 = new MapContext{
		this(fck) = 90E6
		this(fckcube) = 105E6
		this(fcm) = 98E6
		this(fctm) = 5.0E6
		this(fctk) = 3.5E6
		this(fctk095) = 6.6E6
		this(Ecm) = 44E9
		this(epsic1) = 2.8
		this(epsicu1) = 2.8
		this(epsic2) = 2.6
		this(epsicu2) = 2.6
		this(n) = 1.4
		this(epsic3) = 2.3
		this(epsicu3) = 2.6
		lock
	}
	
}