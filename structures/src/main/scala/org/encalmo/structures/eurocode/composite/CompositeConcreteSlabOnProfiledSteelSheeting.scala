package org.encalmo.structures.eurocode.composite

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Calculation
import org.encalmo.document._
import org.encalmo.structures.eurocode.steel.ProfiledSteelSheet
import org.encalmo.structures.eurocode.concrete.Concrete
import org.encalmo.structures.eurocode.concrete.ReinforcingSteel
import org.encalmo.structures.eurocode.actions.ActionsSymbols
import org.encalmo.structures.common.statics.ContinuousBeam_5_LinearLoad
import org.encalmo.expression.abs
import org.encalmo.expression.sin
import org.encalmo.expression.sqrt
import org.encalmo.expression.max

/** Composite slab with profiled steel sheeting symbols */
trait CompositeConcreteSlabOnProfiledSteelSheetingSymbols extends SymbolConfigurator {

	import BasicSymbols._
	
	val ls = symbol(BasicSymbols.l|BasicSymbols.s) unit SI.m
	val nsp = symbol(BasicSymbols.n|"span")
	val nsup = symbol(BasicSymbols.n|"sup")
	/** height of the slab */
	val h = symbol(BasicSymbols.h) unit SI.mm
	val hc = symbol(BasicSymbols.h|c) unit SI.mm
	val Qcfk1 = symbol(Q|"cf,k1") unit "kN/m"
	val Qcfk = symbol(Q|"cf,k") unit "kN/m2"
	val Qcfd = symbol(Q|"cf,d") unit "kN/m2"
	val Qmk = symbol(Q|"m,k") unit "kN/m2"
	val Qmd = symbol(Q|"m,d") unit "kN/m2"
	val Qk1 = symbol(Q|"k|m") unit "kN/m2"
    val Qd1 = symbol(Q|"d|m") unit "kN/m2"
	val deltasm = symbol(BasicSymbols.delta|"s|m") unit SI.mm acc 0.1
	val deltam = symbol(BasicSymbols.delta|"m") unit SI.mm acc 0.1
	val deltae = symbol(BasicSymbols.delta|"e") unit SI.mm acc 0.1
	val deltamax = symbol(BasicSymbols.delta|"max") unit SI.mm acc 0.1
    val MEdmm = symbol(M|("Ed|m","-")) unit "kNm/m"
    val MEdmp = symbol(M|("Ed|m","+")) unit "kNm/m"
    val MEkm1 = symbol(M|("Ek,1|m")) unit "kNm/m"
    val MEkm2 = symbol(M|("Ek,2|m")) unit "kNm/m"
    val VEdm = symbol(V|"Ed|m") unit "kN/m"
    val VEdm1 = symbol(V|"Ed,1|m") unit "kN/m"
    val VEdm2 = symbol(V|"Ed,2|m") unit "kN/m"
    val FEdm = symbol(F|"Ed|m") unit "kN/m"
    val betav = symbol(BasicSymbols.beta|"v")
    val ss = symbol(BasicSymbols.s|"s") unit SI.m
    
    val Gck = symbol(G|"c,k") unit "kN/m2"
    val Gcd = symbol(G|"c,d") unit "kN/m2"
    val Gsk = symbol(G|"s,k") unit "kN/m2"
    val Gsd = symbol(G|"s,d") unit "kN/m2"
    val qk = symbol(q|"k") unit "kN/m2"
    val qd = symbol(q|"d") unit "kN/m2"
    val Fk = symbol(F|"k") unit "kN"
    val Fd = symbol(F|"d") unit "kN"
    val SigmaQk = symbol(("ΣQ")|"k") unit "kN/m2"
    val SigmaQd = symbol(("ΣQ")|"d") unit "kN/m2"
    val SigmaGk = symbol(("ΣG")|"k") unit "kN/m2"
    val SigmaGd = symbol(("ΣG")|"d") unit "kN/m2"
    val Qk2 = symbol(Q|"k|e") unit "kN/m2"
    val Qd2 = symbol(Q|"d|e") unit "kN/m2"
    val DeltaQk = symbol(("ΔQ")|"k") unit "kN/m2"
    val DeltaQd = symbol(("ΔQ")|"d") unit "kN/m2"
    
    val MEdep = symbol(M|("Ed|e","+")) unit "kNm/m"
    val MEdem = symbol(M|("Ed|e","-")) unit "kNm/m"
    val xpl = symbol(x|"pl") unit SI.mm acc 0.1
    val dp = symbol(BasicSymbols.d|"p") unit SI.mm acc 0.1
	val Np = symbol(BasicSymbols.N|"p") unit "kN/m"
	val z = symbol(BasicSymbols.z) unit SI.mm acc 0.1
	val MplRd = symbol(BasicSymbols.M|"pl,Rd") unit "kNm/m"
	val VEde = symbol(BasicSymbols.V|"Ed|e") unit "kN/m"
	val Ls = symbol(BasicSymbols.L|"s") unit SI.m
	val gammaVs = symbol(BasicSymbols.gamma|"Vs")
	val V1Rd = symbol(BasicSymbols.V|"1,Rd") unit "kN/m"
	val ap = symbol(BasicSymbols.a|"p") unit SI.m
	val bp = symbol(BasicSymbols.b|"p") unit SI.m
	val cp = symbol(BasicSymbols.c|"p") unit SI.m
	val vmin = symbol(BasicSymbols.v|"min") unit "MPa"
	val dv = symbol(BasicSymbols.d|"v") unit SI.mm acc 0.1
	val kv = symbol(BasicSymbols.k|"v")
	val VRdc = symbol(BasicSymbols.V|"Rd,c") unit "kN/m"
    val VRdc2 = symbol(BasicSymbols.V|"Rd,c,2") unit "kN/m"
	val VpRd = symbol(BasicSymbols.V|"p,Rd") unit "kN"
	val Qvd = symbol(BasicSymbols.Q|"v,d") unit "kN"
	val Asmin = symbol(BasicSymbols.A|"s,min") unit "mm2/m"
	val dmesh = symbol(BasicSymbols.d|"mesh") unit SI.mm
	val sd = symbol(BasicSymbols.s|"d") unit SI.mm acc 1
	val sdmax = symbol(BasicSymbols.s|"d,max") unit SI.mm acc 1
	val Eceff = symbol(BasicSymbols.E|"c,eff") unit "GPa"
	val nE = symbol(BasicSymbols.n|"E")
	val e0 = symbol(BasicSymbols.e|"0") unit SI.mm acc 0.01
	val I0 = symbol(BasicSymbols.I|"0") unit "cm4/m"
	val W0 = symbol(BasicSymbols.W|"0") unit "cm3/m"
	val Mk = symbol(BasicSymbols.M|"k") unit "kNm/m"
	val sigmactplus = symbol(BasicSymbols.sigma|("ct","+")) unit "MPa"
	val fyrd = symbol(BasicSymbols.f|"yr,d") unit "MPa"
    val la = symbol(BasicSymbols.l|"a") unit "m"
}

class CompositeConcreteSlabOnProfiledSteelSheeting(
    name: String,
	height: Expression,
	length: Expression,
	spans: Expression,
	val sheet: ProfiledSteelSheet,
	val concrete: Concrete,
	val reinforcingSteel: ReinforcingSteel,
    p_gammaG: Expression,
    p_gammaQ: Expression,
    p_Gsk: Expression,
    p_qk: Expression,
    p_Fk: Expression,
    p_dmesh: Expression,
    p_sd: Expression,
    p_ss: Expression, //szerokość podparcia  //support width
	p_nsup: Expression = ZERO
)
extends Calculation(name, "compositeSlabWithProfiledSheeting") with CompositeConcreteSlabOnProfiledSteelSheetingSymbols with ActionsSymbols {

	import sheet.{Gcck,hp,bs,bo,bb,Gccd,alpha,Iplus,Iminus,Ap,ec,eplus,t,br,MRdm,MRdp,VwRd,hw,r,Rw1Rd,Phi,RwRd,k,m}
    import sheet.steel.{E,fyd,gammaM0}
    import concrete.{fcd,fck,fctm,gammac,gammacf,Ecm}

	this add sheet
	this add concrete
	this add reinforcingSteel
	
	ls := length
	nsp := spans
	nsup := p_nsup
	h := height
	fyrd := reinforcingSteel(reinforcingSteel.fyd)

    this.gammaG := p_gammaG
    this.gammaQ := p_gammaQ
    sheet(sheet.gammaG) = p_gammaG
    sheet(sheet.gammaQ) = p_gammaQ

    Gsk := p_Gsk
    qk := p_qk
    Fk := p_Fk
    dmesh := p_dmesh
    sd := p_sd
    ss := p_ss
	
	val beamULS = new ContinuousBeam_5_LinearLoad("ULS",ls/(1+nsup),Qd1)
	val beamSLS1 = new ContinuousBeam_5_LinearLoad("SLS1",ls/(1+nsup),Qk1)
	val beamSLS2 = new ContinuousBeam_5_LinearLoad("SLS2",ls,Gcck + Qcfk)

    this add beamULS
    this add beamSLS1
    this add beamSLS2
	
	//faza montazu - ULS
	MEdmm := beamULS.Mmin
	MEdmp := beamULS.Mmax
	VEdm := beamULS.Tmax
	VEdm1 := beamULS.TRmax1
	VEdm2 := beamULS.TRmax2
	
	MEkm1 := beamSLS2.Mmax
	MEkm2 := beamSLS1.Mmax

    hc := h-hp

    val check1 = check(t >= Number(0.7,SI.mm),"Warunek minimalnej grubości blachy EN 1994-1-1 3.5(2)")
    val check2 = check((br/bs)<=0.6,"Warunek geometrii fałd blachy EN 1994-1-1 9.1.1(2)")
    val check3 = check(h>=Number(90,SI.mm),"Warunek minimalnej grubości całkowitej płyty EN 1994-1-1 9.2.1(2)")
    val check4 = check(hc>=Number(50,SI.mm),"Warunek minimalnej grubości płyty nad fałdami blachy EN 1994-1-1 9.2.1(2)")

    //faza montazu - LOAD
    Qcfk1 := (bs*hc+0.5*(bo+bb)*hp)*gammacf
    Qcfk := Qcfk1/bs
    Qcfd := Qcfk*gammaQ
    Qmk := max(0.75 unit "kN/m2",0.1*Qcfk)
    Qmd := gammaQ*Qmk
    Qk1 := Gcck + Qcfk + Qmk
    Qd1 := Gccd + Qcfd + Qmd

    val ULS1M = require(abs(MEdmm/MRdm) <= 1,"Warunek nośności na zginanie na podporze w fazie montażu")
    val ULS2M = require(abs(MEdmp/MRdp) <= 1,"Warunek nośności na zginanie w przęśle w fazie montażu")

    FEdm := abs(VEdm1)+abs(VEdm2)
    alpha := 0.15
    betav := (abs(abs(VEdm1)-abs(VEdm2))/(abs(VEdm1)+abs(VEdm2))) ## "6.1.7.3(3)"
    la := rangeChoiceLELE(betav,ss,0.2,((betav-0.2)/0.1)*((10E-3-ss)/0.1),0.3,10E-3)
    sheet(sheet.la) = this(la)

    val ULS3M = require(abs(VEdm/VwRd)<=1,"Warunek nośności na ścinanie przy podporze w fazie montażu")
    val ULS4M = require(abs(VEdm)<=abs(0.5*VwRd),"Warunek braku interakcji ścinania i zginania nad podporą w fazie montażu")
	val ULS5M = check((r/t)<10,"Warunek geometrii profilu blachy 6.17a")
	val ULS6M = check((hw/t)<(200*sin(Phi)),"Warunek smukłości maksymalnej środnika profilu blachy 6.17b")
	val ULS7M = check(Number(45,SI.deg) < Phi < Number(90,SI.deg),"Warunek kąta " +
        "pochylenia środnika 6.17c")
	val ULS8M = require(abs(FEdm/RwRd)<=1,"Warunek nośności na miejscową siłą poprzeczną (6.28b)")
	val ULS9M = require(abs(MEdmm/MRdm)+abs(FEdm/RwRd)<=1.25,"Sprawdzenie interakcji momentu zginającego i obciążenia lokalnego nad podporą w fazie montażu (6.28c)")
	
    //faza montazu - SLS
    deltasm := 0.08*(MEkm1*sq(ls/(1+nsup)))/(E*avg(Iplus,Iminus))
    deltam := 0.08*(MEkm2*sq(ls/(1+nsup)))/(E*avg(Iplus,Iminus))
	val SLS1M = require(deltasm<=(h/10),SI.mm,"Warunek 1 ugięcia maksymalnego blachy w fazie montażu EN 1994-1-1 9.3.2(2)")
	val SLS2M = require(deltasm<=(ls/(1+nsup)/180),SI.mm,"Warunek 2 ugięcia maksymalnego blachy w fazie montażu EN 1994-1-1 9.6(2)")
	val SLS3M = require(deltam<=(ls/(1+nsup)/150),SI.mm,"Warunek 3 ugięcia maksymalnego blachy w fazie montażu EN 1993-1-1 NA.22 7.2.1(1)B")

    //faza eksploatacji - LOAD
    Gck := Qcfk*(gammac/gammacf)
    Gcd := Gck*gammaG
    Gsd := Gsk*gammaG
    qd := qk*gammaQ
    SigmaQk := qk
    SigmaGk := Gcck+Gck+Gsk
    SigmaQd := qd
    SigmaGd := Gccd+Gcd+Gsd
    Qk2 := SigmaGk + SigmaQk
    Qd2 := SigmaGd + SigmaQd
    Fd := Fk*gammaQ
    DeltaQk := Gsk+qk
    DeltaQd := Gsd+qd

    //faza eksploatacji - ULS
    //zginanie
    MEdep := (Qd2*(ls^2))/8
    xpl := (Ap*fyd)/(0.85*fcd)
    dp := h-eplus
    Np := Ap*fyd
    z := dp-0.5*xpl
    MplRd := Np*z
	val ULS1E = check(xpl<=hc,SI.mm,"Sprawdzenie usytuowania osi obojętnej w przekroju płyty")
	val ULS2E = require(abs(MEdep/MplRd)<=1,"Warunek nośności na zginanie płyty w fazie eksploatacji (PN-EN 1994-1-1 9.7.2)")
	
    //rozwarstwienie
    VEde := 0.5*Qd2*ls*0.9
    Ls := ls/4
    gammaVs := 1.25
    V1Rd := (dp*((m*Ap)/Ls+k))/gammaVs
	val ULS3E = require(abs(VEde/V1Rd)<=1,"Warunek nośności na rozwarstwienie płyty w fazie eksploatacji (PN-EN 1994-1-1 9.7.3)")
	
    //ścinanie poprzeczne
    ap := 0.1
    bp := 0.1
    cp := 2*(ap+bp)+4*(dp-hc)+2*PI*hc
    dv := dp
    kv := min(sqrt(1+(Number(200,SI.mm)/dv)),2.0)
    vmin := 0.035*sqrt(kv^3)*sqrt(fck).set(SI.MPa)
    VRdc := (vmin*dv*bo)/bs
    VRdc2 := (0.5*dv*fcd*(0.6*(1-(fck/Number(250,SI.MPa))))).set("kN/m")
	val ULS4E = require(abs(VEde/VRdc)<=1,"Warunek 1 nośności na ścinanie poprzeczne płyty w fazie eksploatacji PN-EN 1992-1-1 6.2.2)")
    val ULS5E = require(abs(VEde/VRdc2)<=1,"Warunek 2 nośności na ścinanie poprzeczne płyty w fazie eksploatacji PN-EN 1992-1-1 6.2.2)")

    //przebicie
    VpRd := vmin*cp*dp
    Qvd := Fd
    val ULS6E = require(abs(Qvd/VpRd)<=1,"Sprawdzenie nośności na przebicie płyty w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.6(1) i PN-EN 1992-1-1 pkt.6.4.4")
	
    //minimalne zbrojenie nad podpora
    Asmin := 0.002*hc
    sdmax := (PI*(dmesh^2))/(4*Asmin)
    val SLS1E = require(sd<=sdmax,"Warunek minimalnego zbrojenia płyty na zarysowanie nad podporą")

    //zarysowanie betonu w przesle
    Eceff := (Ecm/2) ## "5.4.2.2(11)"
    nE := E/Eceff
    e0 := (nE*Ap*eplus + hc*(hc/2 + hp) + (bo/bs)*hp*(hp/2)) / (nE*Ap+hc+(bo/bs)*hp)
    I0 := Iplus + Ap*sq(e0-eplus) + cb(hc)/(12*nE) + (hc*sq(e0-h+hc/2))/nE + (bo*cb(hp))/(bs*12*nE) + (((bo*hp)/bs)*sq(e0-hp/2))/nE
    W0 := I0/e0
    Mk := (DeltaQk*(ls^2))/8
    sigmactplus := Mk/(W0*nE)
    val SLS2E = require(abs(sigmactplus)<=fctm,"Warunek braku zarysowania betonu w przęśle płyty od obciążeń przyłożonych po zespoleniu")

    //ugiecia w fazie eksploatacji
    deltae := (5*DeltaQk*(ls^4))/(384*I0*E)
    deltamax := deltasm+deltae
    val SLS3E = require(deltamax<=ls/250,SI.mm,"Sprawdzenie ugięcia całkowitego blachy w fazie eksploatacji")



	
	def info = NumSection(Text("CompositeSlabWithProfiledSheeting",dictionary),
		Evaluate(ls,nsp,h,hc,nsup),
        Check(check1,check2,check3,check4)
	)
	
	def LOAD1 = Evaluate(gammaG,gammaQ,Gcck,Gccd,Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Qk1,Qd1)
	
	def ULS1 = NumSection(Text("ULS","eurocode"),
		NumSection("Sprawdzenie nośności na zginanie w fazie montażu wg PN-EN 1993-1-3 pkt. 6.1.4.1",
			Evaluate(MEdmm,MRdm),
			Check(ULS1M), //Warunek nośności na zginanie na podporze
			Evaluate(MEdmp,MRdp),
            Check(ULS2M) //Warunek nośności na zginanie w przęśle
		),
        sheet.shear,
		NumSection("Sprawdzenie nośności na ścinanie w fazie montażu wg PN-EN 1993-1-3 pkt. 6.1.5",
			Evaluate(VEdm),
            Check(ULS3M,ULS4M)//Warunek nośności na ścinanie przy podporze w fazie montażu
		),
		NumSection("Sprawdzenie obciążenia miejscowego siłą poprzeczną nad podporą pośrednią wg PN-EN 1993-1-3 pkt. 6.1.7.3(1)",
			Check(ULS5M,ULS6M,ULS7M),
			Evaluate(VEdm1,VEdm2,alpha,betav,ss,la,Rw1Rd,RwRd,FEdm),
			Check(ULS8M)//Warunek nośności na miejscową siłą poprzeczną (6.28b)
		),
		NumSection("Sprawdzenie interakcji momentu zginającego i obciążenia lokalnego nad podporą wg PN-EN 1993-1-3 pkt. 6.1.11(1)",
			Check(ULS9M)
		)
	)
	
	def SLS1 = NumSection(Text("SLS","eurocode"),
		Evaluate(MEkm1,deltasm,MEkm2,deltam),
		Check(SLS1M,SLS2M,SLS3M)
	)
	
	def LOAD2 = Evaluate(gammaG,gammaQ,Gck,Gcd,Gsk,Gsd,qk,qd,SigmaQk,SigmaQd,SigmaGk,SigmaGd,Qk2,Qd2,DeltaQk,DeltaQd,Fk,Fd)
	
	def ULS2 = NumSection(Text("ULS","eurocode"),
		NumSection("Sprawdzenie nośności na zginanie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.2",
			Evaluate(MEdep,xpl),
			Check(ULS1E),
			Evaluate(dp,Np,z,MplRd),
			Check(ULS2E)
		),
		NumSection("Sprawdzenie nośności na rozwarstwienie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.3",
			Evaluate(VEde,m,k,gammaVs,Ls,V1Rd),
			Check(ULS3E)
		),
		NumSection("Sprawdzenie nośności na ścinanie poprzeczne w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.5(1) i PN-EN 1992-1-1 pkt. 6.2.2",
			Evaluate(dv,kv,vmin,VRdc),
			Check(ULS4E,ULS5E)
		),
		NumSection("Sprawdzenie nośności na przebicie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.6(1) i PN-EN 1992-1-1 pkt.6.4.4",
			Evaluate(ap,bp,cp,VpRd,Qvd),
            Check(ULS6E)
		)
	)
	
	def SLS2 = NumSection(Text("SLS","eurocode"),
		NumSection("Sprawdzenie braku zarysowania betonu na podporami wg PN-EN 1994-1-1 pkt. 9.8.1",
			Evaluate(Asmin,dmesh,sd,sdmax),
            Check(SLS1E)
		),
		NumSection("Sprawdzenie braku zarysowania betonu w przęśle od obciążeń przyłożonych po zespoleniu",
			Evaluate(Eceff,nE,e0,I0,W0,Mk,sigmactplus),
            Check(SLS2E)
		),
		NumSection("Sprawdzenie ugięcia całkowitego",
			Evaluate(deltae,deltamax),
            Check(SLS3E)
		)
	)
	
}	
