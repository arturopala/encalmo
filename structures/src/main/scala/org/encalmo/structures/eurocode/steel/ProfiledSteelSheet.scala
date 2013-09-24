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
    val mass = symbol(BasicSymbols.m|BasicSymbols.s) unit "kg/m2" acc 0.01
    val Iy = symbol(I|"y") unit "cm4/m"
	val Iminus = symbol(I|("eff","─")) unit "cm4/m"
	val Iplus = symbol(I|("eff","+")) unit "cm4/m"
	val Wminus = symbol(W|("eff","─")) unit "cm3/m"
	val Wplus = symbol(W|("eff","+")) unit "cm3/m"
	val Ap = symbol(A|p) unit "cm2/m"
    val zetaf = symbol(BasicSymbols.zeta|f)
    val zetaw = symbol(BasicSymbols.zeta|w)
	val ec = symbol(e|c) unit "mm" acc 0.01
	val et = symbol(e|t) unit "mm" acc 0.01
	val eplus = symbol(e!"+") unit "mm" acc 0.01
	val eminus = symbol(e!"-") unit "mm" acc 0.01
    val ewt = symbol(e|"w,t") unit "mm" acc 0.01
    val ewc = symbol(e|"w,c") unit "mm" acc 0.01
    val swt = symbol(s|"w,t") unit "mm" acc 0.01
    val swc = symbol(s|"w,c") unit "mm" acc 0.01
	val Gcck = symbol(G|"cc,k") unit "kN/m2"
	val Gccd = symbol(G|"cc,d") unit "kN/m2"
	val lambdaw = symbol(lambda|w) over "─"
	val sw = symbol(s|w) unit "mm"
	val Phi = symbol(BasicSymbols.Phi) unit "°" acc 0.1
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

    val m = symbol(BasicSymbols.m) unit "MPa"
    val k = symbol(BasicSymbols.k) unit "MPa"

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
    p_mass: Double,
    p_zetaf: Double,
    p_zetaw: Double,
    p_m: Double,
    p_k: Double,
    p_image: String
)
extends Calculation(name,"profiledSteelSheet") with ProfiledSteelSheetSymbols with ActionsSymbols {

	import steel.{fy,fyd,E,gammaM0,gammaM1}

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
    mass := p_mass
    m := p_m
    k := p_k

    zetaf := p_zetaf
    zetaw := p_zetaw

    tcor := t - (0.08 unit SI.mm)
    hw := hp-t
    Phi := arctan((2*hw)/abs(bs-br-bb))
    sw := hw/sin(Phi)
    et := (br*hw+sw*hw)/(br+bb+2*sw)
    ec := hp - et
    ewt := (br*hw)/(br+bb)
    ewc := hw - ewt
    swt := sw * (ewt/hw)
    swc := sw * (ewc/hw)
    eplus := ((1-zetaw)*br*hw+sw*hw-zetaf*swc*(ewt+ewc/2))/((1-zetaw)*br+bb+2*(sw-zetaf*swc))
    eminus := (br*hw+sw*hw-zetaf*swt*(ewt/2))/(br+bb+2*(sw-zetaf*swc))

    Iy := (br*tcor*sq(ec))/bs+(bb*tcor*sq(et))/bs+2*((cube(hw)*tcor)/(12*sin(Phi)))/bs+2*(sw*tcor*sq(hw/2-et))/bs
    Iplus := ((1-zetaw)*br*tcor*sq(hw-eplus))/bs + (bb*tcor*sq(eplus))/bs + 2*((cube(hw)*tcor)/(12*sin(Phi)))/bs + 2*(sw*tcor*sq(hw/2-eplus))/bs + (-2*((cube(zetaf*ewc)*tcor)/(12*sin(Phi)))/bs) + (-2*(zetaf*swc*tcor*sq((hw-eplus)/2))/bs)
    Iminus := (br*tcor*sq(hw-eminus))/bs + (bb*tcor*sq(eminus))/bs + 2*((cube(hw)*tcor)/(12*sin(Phi)))/bs + 2*(sw*tcor*sq(hw/2-eminus))/bs + (-2*((cube(zetaf*ewt)*tcor)/(12*sin(Phi)))/bs) + (-2*(zetaf*swt*tcor*sq(eminus/2))/bs)

    Wplus := Iplus/eplus
    Wminus := Iminus/eminus
    MRdm := (Wminus*fyd)/gammaM0
    MRdp := (Wplus*fyd)/gammaM0
    lambdaw := 0.346*(sw/tcor)*sqrt(fyb/E)
    Gcck := mass*GRAV
    Gccd := Gcck*gammaG
    fbv := (0.58*fyb) unless (IsInRangeLessAndLessOrEqual(0.83,lambdaw,1.40) thenUse (0.48*fyb)/lambdaw) unless (IsGreaterThan(lambdaw,1.40) thenUse ((0.67*fyb)/(lambdaw^2)))
    VbRd := ((hw/sin(Phi))*tcor*fbv)/gammaM0
    VplRd := ((hw/sin(Phi))*tcor*(fyb/sqrt(3)))/gammaM0
    VwRd := (2*min(VbRd,VplRd))/bs
    Rw1Rd := alpha*(tcor^2)*sqrt(fyb*E)*(1-0.1*sqrt(r/tcor))*(0.5+sqrt((0.02*la)/tcor))*(2.4+((Phi/ Number(90, SI.deg))^2))*(1/gammaM1)
    RwRd := (2*Rw1Rd)/bs
	
	override def label = this(ID)

    def info = NumSection(Text("ProfiledSteelSheet",dictionary),name,
        Image(p_image),
        NumSection("Geometria",
		    Evaluate(t,tcor,hp,hw,br,bs,bo,bb,r,Ap,Phi,sw,mass)),
        "Efektywne charakterystyki przekroju blachy zostały wyznaczone dla stanu częściowego uplastycznienia podczas zginania środników w przęśle i na podporze oraz półki ściskanej w przęśle.",
        NumSection("Wyznaczenie położenia osi obojętnych",
            Evaluate(et,ec,ewt,ewc,swt,swc,eplus,eminus)),
        NumSection("Momenty bezwładności i wskaźniki zginania",
            Evaluate(Iy,Iplus,Wplus,Iminus,Wminus))
	)
	
	def shear = NumSection("Nośność blachy fałdowej na ścinanie wg PN-EN 1993-1-3 pkt. 6.1.5",
        Evaluate(lambdaw,fyb,fbv,VbRd,VplRd,VwRd))
	
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

    val cofraplus60 = "http://www.kontirom.ro/upload/cofra1.jpg"
    val florstrop59 = "http://www.florex-sa.pl/images/wym_FLORSTROP.gif"

    def FLORSTROP_T59_Z_075 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 0.75", steel = Steel.S280GD,p_mass = 12.03,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 0.75,p_r = 5,p_Iminus = 34.87,p_Iplus = 54.42,p_Ap = 12.53,p_eminus = 30.6,p_eplus = 40.3,p_ep = 36.4,p_zetaf = 0.8, p_zetaw = 0.1,p_m = 103,p_k = 0.19,p_image = florstrop59)
    def FLORSTROP_T59_Z_088 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 0.88", steel = Steel.S280GD,p_mass = 14.12,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 0.88,p_r = 5,p_Iminus = 42.65,p_Iplus = 63.66,p_Ap = 15.04,p_eminus = 31.6,p_eplus = 40.6,p_ep = 36.4,p_zetaf = 0.8, p_zetaw = 0.1,p_m = 103,p_k = 0.19,p_image = florstrop59)
    def FLORSTROP_T59_Z_100 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 1.0",  steel = Steel.S280GD,p_mass = 16.05,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 1.0, p_r = 5,p_Iminus = 49.61,p_Iplus = 73.23,p_Ap = 17.35,p_eminus = 32.7,p_eplus = 40.6,p_ep = 36.4,p_zetaf = 0.8, p_zetaw = 0.1,p_m = 103,p_k = 0.19,p_image = florstrop59)
    def FLORSTROP_T59_Z_125 = new ProfiledSteelSheet(name = "FLORSTROP T59 Z 1.25", steel = Steel.S280GD,p_mass = 20.05,p_hp = 59,p_br = 44.6,p_bs = 140,p_bo = 95.4,p_bb = 127,p_t = 1.25,p_r = 5,p_Iminus = 65.84,p_Iplus = 96.48,p_Ap = 22.17,p_eminus = 34.6,p_eplus = 40.6,p_ep = 36.4,p_zetaf = 0.8, p_zetaw = 0.1,p_m = 103,p_k = 0.19,p_image = florstrop59)

    def COFRAPLUS_60_75    =  new ProfiledSteelSheet(name = "COFRAPLUS 60 0.75",    steel = Steel.S350GD,p_mass = 8.53, p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 0.75, p_r = 5,p_Iminus = 42.6,p_Iplus = 55.12,p_Ap = 10.29,p_eminus = 33.3,p_eplus = 33.3,p_ep = 39.55,p_zetaf = 0.7, p_zetaw = 0.45,p_m = 92.5,p_k = 0.056,p_image = cofraplus60)
    def COFRAPLUS_60_88    =  new ProfiledSteelSheet(name = "COFRAPLUS 60 0.88",    steel = Steel.S350GD,p_mass = 10,   p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 0.88, p_r = 5,p_Iminus = 50.2,p_Iplus = 65.21,p_Ap = 12.17,p_eminus = 33.3,p_eplus = 33.3,p_ep = 39.55,p_zetaf = 0.7, p_zetaw = 0.45,p_m = 92.5,p_k = 0.056,p_image = cofraplus60)
    def COFRAPLUS_60_100    = new ProfiledSteelSheet(name = "COFRAPLUS 60 1.0",     steel = Steel.S350GD,p_mass = 11.37,p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 1.0, p_r = 5,p_Iminus = 57.2,p_Iplus = 74.53,p_Ap = 13.91,p_eminus = 33.3,p_eplus = 33.3,p_ep = 39.55,p_zetaf = 0.7, p_zetaw = 0.45,p_m = 92.5,p_k = 0.056,p_image = cofraplus60)
    def COFRAPLUS_60_125    = new ProfiledSteelSheet(name = "COFRAPLUS 60 1.25",    steel = Steel.S350GD,p_mass = 14.22,p_hp = 58,p_br = 106, p_bs = 207,p_bo = 81.5,p_bb = 62, p_t = 1.25, p_r = 5,p_Iminus = 72.1,p_Iplus = 93.94,p_Ap = 17.57,p_eminus = 33.3,p_eplus = 33.3,p_ep = 39.55,p_zetaf = 0.7, p_zetaw = 0.45,p_m = 92.5,p_k = 0.056,p_image = cofraplus60)

}
