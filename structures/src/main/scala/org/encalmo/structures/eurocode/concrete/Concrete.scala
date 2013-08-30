package org.encalmo.structures.eurocode.concrete

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.expression.min
import org.encalmo.expression.cbrt
import org.encalmo.expression.sqrt

/** Concrete symbols */
trait ConcreteSymbols extends SymbolConfigurator {

	import BasicSymbols._
	
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
	
	val epsics = symbol(epsiv|"cs") is "Odkształcenie skurczowe" unit SI.permille
	val epsicsd = symbol(epsiv|"csd") is "Odkształcenie skurczowe spowodowane wysychaniem betonu" args(t,t|s) unit SI.permille
	val epsicsdinf = symbol(epsiv|"csd,∞") is "Końcowe odkształcenie skurczowe spowodowane wysychaniem betonu" unit SI.permille
    val epsicsa = symbol(epsiv|"csa") is "Odkształcenie skurczowe spowodowane skurczem autogenicznym betonu" unit SI.permille
    val epsicsainf = symbol(epsiv|"csa,∞") is "Końcowe odkształcenie skurczowe spowodowane skurczem autogenicznym betonu" unit SI.permille
    val betasc = symbol(beta|"sc") is "Współczynnik skurczowy zależny od rodzaju cementu"
    val betads = symbol(beta|"ds") is "Funkcja przyrostu skurczu w czasie t-ts" args(t,t|s)
	val betaas = symbol(beta|"as") is "Funkcja przyrostu skurczu autogenicznego w czasie t" args(t)
    val betaRH = symbol(beta|"RH") is "Współczynnik skurczowy zależny od wilgotności względnej powietrza"
    val RH = symbol("RH") is "Wilgotność względna powietrza"
    val dtimec = symbol("t-ts") is "Liczba dni"
    val timec0 = symbol(t|0) is "Wiek betonu w chwili przyłożenia obciążenia"
    val ho =  symbol(h|o) is "Miarodajny wymiar przekroju elementu" unit SI.mm
    val phit = symbol(phi) args(t,t|0) is "Współczynnik pełzania betonu"
    val phitinf = symbol(phi) args ("∞",t) is "Końcowy współczynnik pełzania betonu"
    val betac = symbol(beta|"c") is "Funkcja przyrostu pełzania po przyłożeniu obciążenia"
    val pfiRH = symbol(phi|"RH") is ""
    val betafcm = symbol(beta|"fcm") is ""
    val betaH = symbol(beta|"H") is "Współczynnik zależny od wilgotności względnej powietrza"
    val betat0 = symbol(beta|"t0") is ""
    val Eceff = symbol(E|"c,eff") unit SI.GPa is "Efektywny sieczny moduł sprężystości betonu z uwzględnieniem czasu trwania obciążenia"
}

/** Concrete class */
class Concrete(
    name:String,
    p_fck: Double,
    p_fckcube: Double,
    p_fcm: Double,
    p_fctm: Double,
    p_fctk: Double,
    p_fctk095: Double,
    p_Ecm: Double,
    p_epsic1: Double,
    p_epsicu1: Double,
    p_epsic2: Double,
    p_epsicu2: Double,
    p_nc: Double,
    p_epsic3: Double,
    p_epsicu3: Double
) extends MapContext("concrete") with ConcreteSymbols {

    fck := p_fck
    fckcube := p_fckcube
    fcm := p_fcm
    fctm := p_fctm
    fctk := p_fctk
    fctk095 := p_fctk095
    Ecm := p_Ecm
    epsic1 := p_epsic1
    epsicu1 := p_epsicu1
    epsic2 := p_epsic2
    epsicu2 := p_epsicu2
    nc := p_nc
    epsic3 := p_epsic3
    epsicu3 := p_epsicu3

    CLASS := text(name)
    gammaC := 1.5
    fcd := fck/gammaC
    fctd := fctk/gammaC
    gammac := 25 unit SI.kN/SI.m3
    gammacf := gammac+Number(1,SI.kN/SI.m3)
    vc := 0.6*(1-fck/Number(250,SI.MPa))
    betasc := 5
    betaRH := 1.55*(1-((RH/100)^3))
    epsicsdinf := ((160+(betasc*((90-fcm).nounit)))*1E-3*betaRH)
    betads := ((dtimec)/(0.035*((ho.nounit)^2)+dtimec))^0.5
    epsicsd := epsicsdinf*betads
    epsicsainf := 2.5*(fck.nounit-10)*1E-3
    epsicsainf := 2.5*(fck.nounit-10)*1E-3
    betaas := 1-(EUL^(-0.2*sqrt(dtimec)))
    epsicsa := epsicsainf*betaas
    epsics := epsicsd+epsicsa
    pfiRH := (1 + (1-RH/100)/(0.1*cbrt(ho.nounit)*((35/fcm.nounit)^0.7)))*((35/fcm.nounit)^0.2)
    betafcm := 16.8/sqrt(fcm.nounit)
    betat0 := 1/(0.1+(timec0^0.2))
    phitinf := pfiRH*betafcm*betat0
    betaH := min(1.5*(1+((0.012*RH)^18))*ho.nounit,1500)
    betac := (dtimec/(betaH+dtimec))^0.3
    phit := phitinf*betac
    Eceff := Ecm/(1+phit)

	def info = NumSection(TextToTranslate("Concrete",dictionary),name,
		Evaluate(fck,gammaC,fcd,fcm,fctk,fctd,Ecm,epsic1,epsicu1)
	)
	
	def skurcz  = Evaluate(betasc,RH,betaRH,epsicsdinf,ho,dtimec,betads,epsicsd,epsicsainf,betaas,epsicsa,epsics,pfiRH,betafcm,betat0,phitinf,betaH,betac,phit)

	def label = this(CLASS)

}

class NormalConcrete(
    name:String,
    p_fck: Double,
    p_fckcube: Double,
    p_fcm: Double,
    p_fctm: Double,
    p_fctk: Double,
    p_fctk095: Double,
    p_Ecm: Double,
    p_epsic1: Double
) extends Concrete(name,p_fck,p_fckcube,p_fcm,p_fctm,p_fctk,p_fctk095,p_Ecm,p_epsic1,3.5,2.0,3.5,2.0,1.75,3.5)

/** Concrete library */
object Concrete extends Catalog[Concrete]("Concrete")  {
	
	override val map = Map[String,()=>Concrete](
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
	def C_12_15:Concrete = new NormalConcrete(name = "C12/15",p_fck = 12,p_fckcube = 15,p_fcm = 20,p_fctm = 1.6,p_fctk = 1.1,p_fctk095 = 2.0,p_Ecm = 27,p_epsic1 = 1.8)
	def C_16_20:Concrete = new NormalConcrete(name = "C16/20",p_fck = 16,p_fckcube = 20,p_fcm = 24,p_fctm = 1.9,p_fctk = 1.3,p_fctk095 = 2.5,p_Ecm = 29,p_epsic1 = 1.9)
	def C_20_25:Concrete = new NormalConcrete(name = "C20/25",p_fck = 20,p_fckcube = 25,p_fcm = 28,p_fctm = 2.2,p_fctk = 1.5,p_fctk095 = 2.9,p_Ecm = 30,p_epsic1 = 2)
	def C_25_30:Concrete = new NormalConcrete(name = "C25/30",p_fck = 25,p_fckcube = 30,p_fcm = 33,p_fctm = 2.6,p_fctk = 1.8,p_fctk095 = 3.3,p_Ecm = 31,p_epsic1 = 2.1)
	def C_30_37:Concrete = new NormalConcrete(name = "C30/37",p_fck = 30,p_fckcube = 37,p_fcm = 38,p_fctm = 2.9,p_fctk = 2.0,p_fctk095 = 3.8,p_Ecm = 32,p_epsic1 = 2.2)
	def C_35_45:Concrete = new NormalConcrete(name = "C35/45",p_fck = 35,p_fckcube = 45,p_fcm = 43,p_fctm = 3.2,p_fctk = 2.2,p_fctk095 = 4.2,p_Ecm = 34,p_epsic1 = 2.25)
	def C_40_50:Concrete = new NormalConcrete(name = "C40/50",p_fck = 40,p_fckcube = 50,p_fcm = 48,p_fctm = 3.5,p_fctk = 2.5,p_fctk095 = 4.6,p_Ecm = 35,p_epsic1 = 2.3)
	def C_45_55:Concrete = new NormalConcrete(name = "C45/55",p_fck = 45,p_fckcube = 55,p_fcm = 53,p_fctm = 3.8,p_fctk = 2.7,p_fctk095 = 4.9,p_Ecm = 36,p_epsic1 = 2.4)
	def C_50_60:Concrete = new NormalConcrete(name = "C50/60",p_fck = 50,p_fckcube = 60,p_fcm = 58,p_fctm = 4.1,p_fctk = 2.9,p_fctk095 = 5.3,p_Ecm = 37,p_epsic1 = 2.45)
	//special
	def C_55_67:Concrete = new Concrete(name = "C57/67",p_fck = 55,p_fckcube = 67,p_fcm = 63,p_fctm = 4.2,p_fctk = 3,p_fctk095 = 5.5,p_Ecm = 38,p_epsic1 = 2.5,p_epsicu1 = 3.2,p_epsic2 = 2.2,p_epsicu2 = 3.1,p_nc = 1.75,p_epsic3 = 1.8,p_epsicu3 = 3.1)
	def C_60_75:Concrete = new Concrete(name = "C60/75",p_fck = 60,p_fckcube = 75,p_fcm = 68,p_fctm = 4.4,p_fctk = 3.1,p_fctk095 = 5.7,p_Ecm = 39,p_epsic1 = 2.6,p_epsicu1 = 3.0,p_epsic2 = 2.3,p_epsicu2 = 2.9,p_nc = 1.6,p_epsic3 = 1.9,p_epsicu3 = 2.9)
	def C_70_85:Concrete = new Concrete(name = "C70/85",p_fck = 70,p_fckcube = 85,p_fcm = 78,p_fctm = 4.6,p_fctk = 3.2,p_fctk095 = 6.0,p_Ecm = 41,p_epsic1 = 2.7,p_epsicu1 = 2.8,p_epsic2 = 2.4,p_epsicu2 = 2.7,p_nc = 1.45,p_epsic3 = 2.0,p_epsicu3 = 2.7)
	def C_80_95:Concrete = new Concrete(name = "C80/95",p_fck = 80,p_fckcube = 95,p_fcm = 88,p_fctm = 4.8,p_fctk = 3.4,p_fctk095 = 6.3,p_Ecm = 42,p_epsic1 = 2.8,p_epsicu1 = 2.8,p_epsic2 = 2.5,p_epsicu2 = 2.6,p_nc = 1.4,p_epsic3 = 2.2,p_epsicu3 = 2.6)
	def C_90_105:Concrete = new Concrete(name ="C50/60",p_fck = 90,p_fckcube = 105,p_fcm = 98,p_fctm = 5.0,p_fctk = 3.5,p_fctk095 = 6.6,p_Ecm = 44,p_epsic1 = 2.8,p_epsicu1 = 2.8,p_epsic2 = 2.6,p_epsicu2 = 2.6,p_nc = 1.4,p_epsic3 = 2.3,p_epsicu3 = 2.6)
	
}
