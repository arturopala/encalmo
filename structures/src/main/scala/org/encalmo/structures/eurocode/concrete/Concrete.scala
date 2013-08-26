package org.encalmo.structures.eurocode.concrete

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.expression.min
import org.encalmo.expression.cbrt
import org.encalmo.expression.sqrt

/** Concrete related symbols */
object ConcreteSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "concrete"
	
	val CLASS = symbol("CLASS").makeNonPrintable
	val fck = symbol(f|"ck") unit SI.MPa
	val fckcube = symbol(f|"ck,cube") unit SI.MPa
	val fcm = symbol(f|"cm") unit SI.MPa
	val fctm = symbol(f|"ctm") unit SI.MPa
	val fctk = symbol(f|"ctk") unit SI.MPa
	val fctk095 = symbol(f|"ctk, 0,95") unit SI.MPa
	val Ecm = symbol(E|"cm") unit SI.GPa
	val epsic1 = symbol(epsiv|"c1") unit "‰"
	val epsicu1 = symbol(epsiv|"cu1") unit "‰"
	val epsic2 = symbol(epsiv|"c2") unit "‰"
	val epsicu2 = symbol(epsiv|"cu2") unit "‰"
	val epsic3 = symbol(epsiv|"c3") unit "‰"
	val epsicu3 = symbol(epsiv|"cu3") unit "‰"
	val nc = symbol(BasicSymbols.n|c)
	val gammaC = symbol(gamma|"C")
	val fcd = symbol(f|"cd") unit SI.MPa
	val fctd = symbol(f|"ctd") unit SI.MPa
	val gammac = symbol(gamma|"c") unit "kN/m3"
	val gammacf = symbol(gamma|"c,f") unit "kN/m3"
	val vc = symbol(BasicSymbols.v|c)
	
	val epsics = epsiv|"cs" is "Odkształcenie skurczowe" unit SI.permille
	val epsicsd = epsiv|"csd" is "Odkształcenie skurczowe spowodowane wysychaniem betonu" args(t,t|s) unit SI.permille
	val epsicsdinf = epsiv|"csd,∞" is "Końcowe odkształcenie skurczowe spowodowane wysychaniem betonu" unit SI.permille
    val epsicsa = epsiv|"csa" is "Odkształcenie skurczowe spowodowane skurczem autogenicznym betonu" unit SI.permille
    val epsicsainf = epsiv|"csa,∞" is "Końcowe odkształcenie skurczowe spowodowane skurczem autogenicznym betonu" unit SI.permille
    val betasc = beta|"sc" is "Współczynnik skurczowy zależny od rodzaju cementu"
    val betads = beta|"ds" is "Funkcja przyrostu skurczu w czasie t-ts" args(t,t|s)
	val betaas = beta|"as" is "Funkcja przyrostu skurczu autogenicznego w czasie t" args(t)
    val betaRH = beta|"RH" is "Współczynnik skurczowy zależny od wilgotności względnej powietrza"
    val RH = "RH" is "Wilgotność względna powietrza"
    val dtimec = "t-ts" is "Liczba dni"
    val timec0 = "t0" is "Wiek betonu w chwili przyłożenia obciążenia"
    val ho = h|o is "Miarodajny wymiar przekroju elementu" unit SI.mm
    val phit = phi args(t,t|0) is "Współczynnik pełzania betonu"
    val phitinf = phi args ("∞",t) is "Końcowy współczynnik pełzania betonu"
    val betac = beta|"c" is "Funkcja przyrostu pełzania po przyłożeniu obciążenia"
    val pfiRH = phi|"RH" is ""
    val betafcm = beta|"fcm" is ""
    val betaH = beta|"H" is "Współczynnik zależny od wilgotności względnej powietrza"
    val betat0 = beta|"t0" is ""
    val Eceff = symbol(E|"c,eff") unit SI.GPa is "Efektywny sieczny moduł sprężystości betonu z uwzględnieniem czasu trwania obciążenia"
}

/** Common Concrete expressions */
object ConcreteExpressions extends MapContext {

	import ConcreteSymbols._
	
	this(fcd) = fck/gammaC
	this(fctd) = fctk/gammaC
	this(gammac) = 25 unit SI.kN/SI.m3
	this(gammacf) = gammac+Number(1,SI.kN/SI.m3)
	this(vc) = 0.6*(1-fck/Number(250,SI.MPa))
	this(betasc) = 5
	this(betaRH) = 1.55*(1-((RH/100)^3))
	this(epsicsdinf) = ((160+(betasc*((90-fcm).nounit)))*1E-3*betaRH)
	this(betads) = ((dtimec)/(0.035*((ho.nounit)^2)+dtimec))^0.5
	this(epsicsd) = epsicsdinf*betads
	this(epsicsainf) = 2.5*(fck.nounit-10)*1E-3
	this(epsicsainf) = 2.5*(fck.nounit-10)*1E-3
	this(betaas) = 1-(EUL^(-0.2*sqrt(dtimec)))
	this(epsicsa) = epsicsainf*betaas
	this(epsics) = epsicsd+epsicsa
	this(pfiRH) = (1 + (1-RH/100)/(0.1*cbrt(ho.nounit)*((35/fcm.nounit)^0.7)))*((35/fcm.nounit)^0.2)
	this(betafcm) = 16.8/sqrt(fcm.nounit)
	this(betat0) = 1/(0.1+(timec0^0.2))
	this(phitinf) = pfiRH*betafcm*betat0
	this(betaH) = min(1.5*(1+((0.012*RH)^18))*ho.nounit,1500)
	this(betac) = (dtimec/(betaH+dtimec))^0.3
	this(phit) = phitinf*betac
	this(Eceff) = Ecm/(1+phit)
}

/** Concrete class */
class Concrete(name:String,data:Context) extends Calculation(name) {

	import ConcreteSymbols._

	def info = NumSection(TextToTranslate("Concrete",ConcreteSymbols.dictionary),name,
		Evaluate(fck,gammaC,fcd,fcm,fctk,fctd,Ecm,epsic1,epsicu1)
	)
	
	def skurcz  = Evaluate(betasc,RH,betaRH,epsicsdinf,ho,dtimec,betads,epsicsd,epsicsainf,betaas,epsicsa,epsics,pfiRH,betafcm,betat0,phitinf,betaH,betac,phit)
	
	this add ConcreteExpressions
	this add data
	
	this(CLASS) = text(name)
	this(gammaC) = 1.5

	override def label = this(CLASS)

}

class NormalConcrete extends MapContext {

	import ConcreteSymbols._

	this(epsicu1) = 3.5
	this(epsic2) = 2.0
	this(epsicu2) = 3.5
	this(epsic3) = 1.75
	this(epsicu3) = 3.5
	this(nc) = 2.0
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
		this(fck) = 12
		this(fckcube) = 15
		this(fcm) = 20
		this(fctm) = 1.6
		this(fctk) = 1.1
		this(fctk095) = 2.0
		this(Ecm) = 27
		this(epsic1) = 1.8
		lock()
	}
	
	private lazy val data_C_16_20 = new NormalConcrete{
		this(fck) = 16
		this(fckcube) = 20
		this(fcm) = 24
		this(fctm) = 1.9
		this(fctk) = 1.3
		this(fctk095) = 2.5
		this(Ecm) = 29
		this(epsic1) = 1.9
		lock()
	}
	
	private lazy val data_C_20_25 = new NormalConcrete{
		this(fck) = 20
		this(fckcube) = 25
		this(fcm) = 28
		this(fctm) = 2.2
		this(fctk) = 1.5
		this(fctk095) = 2.9
		this(Ecm) = 30
		this(epsic1) = 2
		lock()
	}
	
	private lazy val data_C_25_30 = new NormalConcrete{
		this(fck) = 25
		this(fckcube) = 30
		this(fcm) = 33
		this(fctm) = 2.6
		this(fctk) = 1.8
		this(fctk095) = 3.3
		this(Ecm) = 31
		this(epsic1) = 2.1
		lock()
	}
	
	private lazy val data_C_30_37 = new NormalConcrete{
		this(fck) = 30
		this(fckcube) = 37
		this(fcm) = 38
		this(fctm) = 2.9
		this(fctk) = 2.0
		this(fctk095) = 3.8
		this(Ecm) = 32
		this(epsic1) = 2.2
		lock()
	}
	
	private lazy val data_C_35_45 = new NormalConcrete{
		this(fck) = 35
		this(fckcube) = 45
		this(fcm) = 43
		this(fctm) = 3.2
		this(fctk) = 2.2
		this(fctk095) = 4.2
		this(Ecm) = 34
		this(epsic1) = 2.25
		lock()
	}
	
	private lazy val data_C_40_50 = new NormalConcrete{
		this(fck) = 40
		this(fckcube) = 50
		this(fcm) = 48
		this(fctm) = 3.5
		this(fctk) = 2.5
		this(fctk095) = 4.6
		this(Ecm) = 35
		this(epsic1) = 2.3
		lock()
	}
	
	private lazy val data_C_45_55 = new NormalConcrete{
		this(fck) = 45
		this(fckcube) = 55
		this(fcm) = 53
		this(fctm) = 3.8
		this(fctk) = 2.7
		this(fctk095) = 4.9
		this(Ecm) = 36
		this(epsic1) = 2.4
		lock()
	}
	
	private lazy val data_C_50_60 = new NormalConcrete{
		this(fck) = 50
		this(fckcube) = 60
		this(fcm) = 58
		this(fctm) = 4.1
		this(fctk) = 2.9
		this(fctk095) = 5.3
		this(Ecm) = 37
		this(epsic1) = 2.45
		lock()
	}
	
	private lazy val data_C_55_67 = new MapContext{
		this(fck) = 55
		this(fckcube) = 67
		this(fcm) = 63
		this(fctm) = 4.2
		this(fctk) = 3
		this(fctk095) = 5.5
		this(Ecm) = 38
		this(epsic1) = 2.5
		this(epsicu1) = 3.2
		this(epsic2) = 2.2
		this(epsicu2) = 3.1
		this(nc) = 1.75
		this(epsic3) = 1.8
		this(epsicu3) = 3.1
		lock()
	}
	
	private lazy val data_C_60_75 = new MapContext{
		this(fck) = 60
		this(fckcube) = 75
		this(fcm) = 68
		this(fctm) = 4.4
		this(fctk) = 3.1
		this(fctk095) = 5.7
		this(Ecm) = 39
		this(epsic1) = 2.6
		this(epsicu1) = 3.0
		this(epsic2) = 2.3
		this(epsicu2) = 2.9
		this(nc) = 1.6
		this(epsic3) = 1.9
		this(epsicu3) = 2.9
		lock()
	}
	
	private lazy val data_C_70_85 = new MapContext{
		this(fck) = 70
		this(fckcube) = 85
		this(fcm) = 78
		this(fctm) = 4.6
		this(fctk) = 3.2
		this(fctk095) = 6.0
		this(Ecm) = 41
		this(epsic1) = 2.7
		this(epsicu1) = 2.8
		this(epsic2) = 2.4
		this(epsicu2) = 2.7
		this(nc) = 1.45
		this(epsic3) = 2.0
		this(epsicu3) = 2.7
		lock()
	}
	
	private lazy val data_C_80_95 = new MapContext{
		this(fck) = 80
		this(fckcube) = 95
		this(fcm) = 88
		this(fctm) = 4.8
		this(fctk) = 3.4
		this(fctk095) = 6.3
		this(Ecm) = 42
		this(epsic1) = 2.8
		this(epsicu1) = 2.8
		this(epsic2) = 2.5
		this(epsicu2) = 2.6
		this(nc) = 1.4
		this(epsic3) = 2.2
		this(epsicu3) = 2.6
		lock()
	}
	
	private lazy val data_C_90_105 = new MapContext{
		this(fck) = 90
		this(fckcube) = 105
		this(fcm) = 98
		this(fctm) = 5.0
		this(fctk) = 3.5
		this(fctk095) = 6.6
		this(Ecm) = 44
		this(epsic1) = 2.8
		this(epsicu1) = 2.8
		this(epsic2) = 2.6
		this(epsicu2) = 2.6
		this(nc) = 1.4
		this(epsic3) = 2.3
		this(epsicu3) = 2.6
		lock()
	}
	
}
