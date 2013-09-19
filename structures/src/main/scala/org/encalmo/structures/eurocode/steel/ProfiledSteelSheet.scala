package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Calculation
import org.encalmo.document._
import org.encalmo.structures.eurocode.actions._
import org.encalmo.expression.min
import org.encalmo.expression.sin
import org.encalmo.expression.IsGreaterThan
import org.encalmo.expression.sqrt
import org.encalmo.expression.IsInRangeLessAndLessOrEqual

trait ProfiledSteelSheetSymbols extends SymbolConfigurator {

    import BasicSymbols._
	
	val ID = symbol("ID").makeNonPrintable
	val hp = symbol(BasicSymbols.h|"p") unit "mm"
	val hw = symbol(BasicSymbols.h|"w") unit "mm"
	val br = symbol(BasicSymbols.b|"r") unit "mm"
	val bs = symbol(BasicSymbols.b|"s") unit "mm"
	val bo = symbol(BasicSymbols.b|"o") unit "mm"
	val bb = symbol(BasicSymbols.b|"b") unit "mm"
	val t = symbol(BasicSymbols.t) unit "mm"
	val r = symbol(BasicSymbols.r) unit "mm"
    val m = symbol(BasicSymbols.m) unit "kg/m2" acc 0.01
	val Iminus = symbol(I!"-") unit "cm4/m"
	val Iplus = symbol(I!"+") unit "cm4/m"
	val Wminus = symbol(W!"-") unit "cm3/m"
	val Wplus = symbol(W!"+") unit "cm3/m"
	val Ap = symbol(A|p) unit "cm2/m"
	val eminus = symbol(e!"-") unit "mm"
	val eplus = symbol(e!"+") unit "mm"
	val ep = symbol(e|p) unit "mm"
	val epd = symbol(e|("p","d")) unit "mm"
	val Gcck = symbol(G|"cc,k") unit "kN/m2"
	val Gccd = symbol(G|"cc,d") unit "kN/m2"
	val lambdaw = symbol(lambda|w) over "─"
	val sw = symbol(s|w) unit "mm"
	val Phi = symbol(BasicSymbols.Phi) unit "°"
	val tcor = symbol(BasicSymbols.t|"cor") unit "mm"
	val fbv = symbol(f|"bv") unit SI.MPa
    val MRdm = symbol(M|("Rd","-")) unit "kNm/m"
    val MRdp = symbol(M|("Rd","+")) unit "kNm/m"
	val VwRd = symbol(V|"w,Rd") unit "kN/m"
	val VbRd = symbol(V|"b,Rd") unit "kN"
	val VplRd = symbol(V|"pl,Rd") unit "kN"
	val Rw1Rd = symbol(R|"w1,Rd") unit "kN"
	val RwRd = symbol(R|"w,Rd") unit "kN/m"
	val alpha = symbol(BasicSymbols.alpha)
	val la = symbol(BasicSymbols.l|"a") unit "m"
	val fyb = symbol(BasicSymbols.f|"yb") unit SI.MPa
	val fypd = symbol(BasicSymbols.f|"yp,d") unit SI.MPa

}

class ProfiledSteelSheet(
    name:String,
    val steel:Steel,
    p_hp: Double,
    p_br: Double,
    p_bs: Double,
    p_bo: Double,
    p_bb: Double,
    p_t: Double,
    p_r: Double,
    p_Iminus: Double,
    p_Iplus: Double,
    p_Ap: Double,
    p_eminus: Double,
    p_eplus: Double,
    p_ep: Double,
    p_Phi: Double,
    p_m: Double
)
extends Calculation(name,"profiledSteelSheet") with ProfiledSteelSheetSymbols with ActionsSymbols {

	import steel.{fy,fyd,E,gammaM0,gammaM1,gammas}

	this add steel
	
	ID := text(name)
	fyb := steel(fy)
	fypd := steel(fyd)

    hp := p_hp
    br := p_br
    bs := p_bs
    bo := p_bo
    bb := p_bb
    t := p_t
    r := p_r
    Iminus := p_Iminus
    Iplus := p_Iplus
    Ap := p_Ap
    eminus := p_eminus
    eplus := p_eplus
    ep := p_ep
    Phi := p_Phi
    m := p_m

    epd := hp-ep
    Wminus := Iminus/eminus
    Wplus := Iplus/eplus
    MRdm := (Wminus*fyd)/gammaM0
    MRdp := (Wplus*fyd)/gammaM0
    hw := hp-t
    sw := hp/sin(Phi)
    tcor := t - (0.08 unit SI.mm)
    lambdaw := 0.346*(sw/tcor)*sqrt(fyb/E)
    Gcck := m*GRAV
    Gccd := Gcck*gammaG
    fbv := (0.58*fyb) unless (IsInRangeLessAndLessOrEqual(0.83,lambdaw,1.40) thenUse (0.48*fyb)/lambdaw) unless (IsGreaterThan(lambdaw,1.40) thenUse ((0.67*fyb)/(lambdaw^2)))
    VbRd := ((hw/sin(Phi))*tcor*fbv)/gammaM0
    VplRd := ((hw/sin(Phi))*tcor*(fyb/sqrt(3)))/gammaM0
    VwRd := (2*min(VbRd,VplRd))/bs
    Rw1Rd := alpha*(tcor^2)*sqrt(fyb*E)*(1-0.1*sqrt(r/tcor))*(0.5+sqrt((0.02*la)/tcor))*(2.4+((Phi/(Number(90,SI.deg)))^2))*(1/gammaM1)
    RwRd := (2*Rw1Rd)/bs
	
	override def label = this(ID)

    def info = NumSection(Text("ProfiledSteelSheet",dictionary),name,
		Evaluate(t,tcor,hp,br,bs,bo,bb,r,Ap,Iminus,Iplus,eminus,eplus,ep,epd,Wminus,Wplus)
	)
	
	def web = Evaluate(Phi,sw,tcor,lambdaw)
	
	def shearForce = Evaluate(fyb,fbv,VbRd,VplRd,VwRd)
	
}

object ProfiledSteelSheet extends Catalog[ProfiledSteelSheet]("Profiled Steel Sheet") {

    override val map = Map[String,()=>ProfiledSteelSheet](
        "FLORSTROP T59 Z 0.75" -> FLORSTROP_T59_Z_075 _,
        "FLORSTROP T59 Z 0.88" -> FLORSTROP_T59_Z_088 _,
        "FLORSTROP T59 Z 1.0" -> FLORSTROP_T59_Z_100 _,
        "FLORSTROP T59 Z 1.25" -> FLORSTROP_T59_Z_125 _,
        "COFRAPLUS 60 1.0" -> COFRAPLUS_60_100 _,
        "COFRAPLUS 60 0.75" ->  COFRAPLUS_60_75 _,
        "COFRAPLUS 60 0.88" ->  COFRAPLUS_60_88 _,
        "COFRAPLUS 60 1.25" ->  COFRAPLUS_60_125 _
    )

    def FLORSTROP_T59_Z_075 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 0.75", steel = Steel.S280GD,p_m = 12.03,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 0.75,p_r = 5,p_Iminus = 34.87,p_Iplus = 54.42,p_Ap = 12.53,p_eminus = 30.6,p_eplus = 40.3,p_ep = 36.4,p_Phi = 75)
    def FLORSTROP_T59_Z_088 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 0.88", steel = Steel.S280GD,p_m = 14.12,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 0.88,p_r = 5,p_Iminus = 42.65,p_Iplus = 63.66,p_Ap = 15.04,p_eminus = 31.6,p_eplus = 40.6,p_ep = 36.4,p_Phi = 75)
    def FLORSTROP_T59_Z_100 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 1.0",  steel = Steel.S280GD,p_m = 16.05,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 1.0, p_r = 5,p_Iminus = 49.61,p_Iplus = 73.23,p_Ap = 17.35,p_eminus = 32.7,p_eplus = 40.6,p_ep = 36.4,p_Phi = 75)
    def FLORSTROP_T59_Z_125 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 1.25", steel = Steel.S280GD,p_m = 20.05,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 1.25,p_r = 5,p_Iminus = 65.84,p_Iplus = 96.48,p_Ap = 22.17,p_eminus = 34.6,p_eplus = 40.6,p_ep = 36.4,p_Phi = 75)
    def COFRAPLUS_60_75    =  new ProfiledSteelSheet(name = "COFRAPLUS 60 0.75",    steel = Steel.S350GD,p_m = 8.53, p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 1.0, p_r = 5,p_Iminus = 60.08,p_Iplus = 55.12,p_Ap = 10.29,p_eminus = 33.3,p_eplus = 35.2,p_ep = 33.3,p_Phi = 72)
    def COFRAPLUS_60_88    =  new ProfiledSteelSheet(name = "COFRAPLUS 60 0.88",    steel = Steel.S350GD,p_m = 10,   p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 1.0, p_r = 5,p_Iminus = 60.08,p_Iplus = 65.21,p_Ap = 12.17,p_eminus = 33.3,p_eplus = 35.2,p_ep = 33.3,p_Phi = 72)
    def COFRAPLUS_60_100    = new ProfiledSteelSheet(name = "COFRAPLUS 60 1.0",     steel = Steel.S350GD,p_m = 11.37,p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 1.0, p_r = 5,p_Iminus = 60.08,p_Iplus = 74.53,p_Ap = 13.91,p_eminus = 33.3,p_eplus = 35.2,p_ep = 33.3,p_Phi = 72)
    def COFRAPLUS_60_125    = new ProfiledSteelSheet(name = "COFRAPLUS 60 1.25",    steel = Steel.S350GD,p_m = 14.22,p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 1.0, p_r = 5,p_Iminus = 60.08,p_Iplus = 93.94,p_Ap = 17.57,p_eminus = 33.3,p_eplus = 35.2,p_ep = 33.3,p_Phi = 72)

}
