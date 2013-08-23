package org.encalmo.structures.eurocode.composite

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._
import org.encalmo.structures.eurocode.steel.ProfiledSteelSheet
import org.encalmo.structures.eurocode.concrete.Concrete
import org.encalmo.structures.eurocode.concrete.ReinforcingSteel
import org.encalmo.structures.eurocode.steel.ProfiledSteelSheetSymbols
import org.encalmo.structures.eurocode.concrete.ConcreteSymbols
import org.encalmo.structures.eurocode.actions.ActionsSymbols
import org.encalmo.structures.eurocode.steel.SteelSymbols
import org.encalmo.structures.common.statics.ContinuousBeamSymbols
import org.encalmo.structures.eurocode.concrete.ReinforcingSteelSymbols
import org.encalmo.structures.common.statics.ContinuousBeam_5_LinearLoad

/** Composite slab with profiled steel sheeting symbols */
object CompositeSlabWithProfiledSheetingSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "compositeSlabWithProfiledSheeting"
	
	val l = symbol(BasicSymbols.l) unit SI.m
	val nsp = symbol(BasicSymbols.n|"sp")
	/** height of the slab */
	val h = symbol(BasicSymbols.h) unit SI.cm
	val hc = symbol(BasicSymbols.h|c) unit SI.cm
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
    val xpl = symbol(x|"pl") unit SI.cm
    val dp = symbol(BasicSymbols.d|"p") unit SI.cm
	val Np = symbol(BasicSymbols.N|"p") unit "kN/m"
	val z = symbol(BasicSymbols.z) unit SI.cm
	val MplRd = symbol(BasicSymbols.M|"pl,Rd") unit "kNm/m"
	val VEde = symbol(BasicSymbols.V|"Ed|e") unit "kN/m"
	val mV = symbol(BasicSymbols.m|"V") unit "MPa"
	val kV = symbol(BasicSymbols.k|"V") unit "MPa"
	val Ls = symbol(BasicSymbols.L|"s") unit SI.m
	val gammaVs = symbol(BasicSymbols.gamma|"Vs")
	val V1Rd = symbol(BasicSymbols.V|"1,Rd") unit "kN/m"
	val ap = symbol(BasicSymbols.a|"p") unit SI.m
	val bp = symbol(BasicSymbols.b|"p") unit SI.m
	val cp = symbol(BasicSymbols.c|"p") unit SI.m
	val vmin = symbol(BasicSymbols.v|"min") unit "MPa"
	val dv = symbol(BasicSymbols.d|"v") unit SI.cm
	val kv = symbol(BasicSymbols.k|"v")
	val VRdc = symbol(BasicSymbols.V|"Rd,c") unit "kN/m"
	val VpRd = symbol(BasicSymbols.V|"p,Rd") unit "kN"
	val Qvd = symbol(BasicSymbols.Q|"v,d") unit "kN"
	val Asmin = symbol(BasicSymbols.A|"s,min") unit "m2/m"
	val dmesh = symbol(BasicSymbols.d|"mesh") unit "mm"
	val sd = symbol(BasicSymbols.s|"d") unit SI.m
	val sdmax = symbol(BasicSymbols.s|"d,max") unit SI.m
	val Eceff2 = symbol(BasicSymbols.E|"ceff") unit "GPa"
	val nE = symbol(BasicSymbols.n|"E")
	val e0 = symbol(BasicSymbols.e|"0") unit SI.cm
	val I0 = symbol(BasicSymbols.I|"0") unit "cm4/m"
	val W0 = symbol(BasicSymbols.W|"0") unit "cm3/m"
	val Mk = symbol(BasicSymbols.M|"k") unit "kNm/m"
	val sigmactplus = symbol(BasicSymbols.sigma|("ct","+")) unit "MPa"
	val fyrd = symbol(BasicSymbols.f|"yr,d") unit "MPa"
}

/** Composite slab with profiled steel sheeting context */
object CompositeSlabWithProfiledSheetingExpressions extends MapContext {

	import CompositeSlabWithProfiledSheetingSymbols._
	import ProfiledSteelSheetSymbols._
	import SteelSymbols.{E,fyd}
    import ConcreteSymbols._
    import ActionsSymbols._
	
	hc := h-hp
	
	//faza montazu - LOAD
	Qcfk1 := (bs*hc+0.5*(bo+bb)*hp)*gammacf
	Qcfk := Qcfk1/bs
	Qcfd := Qcfk*gammaQ
	Qmk := max(0.75 unit "kN/m2",0.1*Qcfk)
	Qmd := gammaQ*Qmk
	Qk1 := Gcck + Qcfk + Qmk
	Qd1 := Gccd + Qcfd + Qmd

	FEdm := abs(VEdm1)+abs(VEdm2)
	alpha := 0.15
	betav := abs(abs(VEdm1)-abs(VEdm2))/(abs(VEdm1)+abs(VEdm2))
	ss := 180 unit SI.cm
	la := rangeChoiceLELE(betav,ss,0.2,((betav-0.2)/0.1)*((10E-3-ss)/0.1),0.3,10E-3)
	
	//faza montazu - SLS
	deltasm := 0.08*(MEkm1*(l^2))/(E*Iplus)
	deltam := 0.08*(MEkm2*(l^2))/(E*Iplus)
	
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
	MEdep := (Qd2*(l^2))/8
	MEdep := (Qd2*(l^2))/8
	xpl := (Ap*fyd)/(0.85*fcd)
	dp := ep+hc
	Np := Ap*fyd
	z := dp-0.5*xpl
	MplRd := Np*z
	VEde := 0.5*Qd2*l
	mV := 103 unit SI.MPa
	kV := 0.19 unit SI.MPa
	Ls := l/4
	gammaVs := 1.25
	V1Rd := (dp*(((mV*Ap)/(Ls))+kV))/gammaVs
	//przebicie
	ap := 0.1
	bp := 0.1
	cp := 2*(ap+bp)+4*(dp-hc)+2*PI*hc
	dv := dp
	kv := sqrt(1+(200/(dv*1000))).nounit
	vmin := 35*sqrt(kv^3)*(sqrt(fck).setunit(SI.MPa))
	VpRd := vmin*cp*dp
	VRdc := vmin*dv
	Qvd := Fd
	//zarysowanie betonu nad podpora
	Asmin := 0.002*hc*1
	sdmax := (PI*(dmesh^2))/(4*Asmin)//*1E-6
	//ugiecia w fazie eksploatacji
	Eceff2 := Ecm/2
	nE := E/Eceff2
	e0 := (Ap*(h-epd)+(1/nE)*1*hc*(hc/2)+(1/nE)*(bo/bs)*hp*(h-(hp/2)))/(Ap+((1*hc)/nE)+(1*bo*hp)/(bs*nE))
	I0 := Iplus+Ap*((h-e0-epd)^2)+((1*(hc^3))/(nE*12))+((1*hc)/nE)*((e0-hc/2)^2)+(1*bo*(hp^3))/(nE*bs*12)+(1*bo*hp)/(nE*bs)*((h-e0-(hp/2))^2)
	W0 := I0/(h-e0)
	Mk := (DeltaQk*(l^2))/8
	sigmactplus := Mk/(W0*nE)
	deltae := (5*DeltaQk*(l^4))/(384*I0*E)
	deltamax := deltasm+deltae
	
	// end of context initialization
	lock

}

class CompositeSlabWithProfiledSheeting(
	height:Expression,
	length:Expression,
	spans:Expression, 
	sheet:ProfiledSteelSheet, 
	concrete:Concrete,
	reinforcingSteel:ReinforcingSteel
)
extends Calculation {

	import CompositeSlabWithProfiledSheetingSymbols._
	import ProfiledSteelSheetSymbols._
    import ConcreteSymbols.{fcd,fck,fctm}
    import ActionsSymbols._

	val Beam = ContinuousBeamSymbols

	this add CompositeSlabWithProfiledSheetingExpressions
	this add sheet
	this add concrete
	this add reinforcingSteel
	
	l := length
	nsp := spans
	h := height
	fyrd := reinforcingSteel(ReinforcingSteelSymbols.fyd)
	
	val beamULS = new ContinuousBeam_5_LinearLoad("ULS",l,Qd1)
	val beamSLS1 = new ContinuousBeam_5_LinearLoad("SLS1",l,Qk1)
	val beamSLS2 = new ContinuousBeam_5_LinearLoad("SLS2",l,Gcck + Qcfk)
	
	//faza montazu - ULS
	MEdmm := beamULS(Beam.Mmin)
	MEdmp := beamULS(Beam.Mmax)
	VEdm := beamULS(Beam.Tmax)
	VEdm1 := beamULS(Beam.TRmax1)
	VEdm2 := beamULS(Beam.TRmax2)
	
	MEkm1 := beamSLS2(Beam.Mmax)
	MEkm2 := beamSLS1(Beam.Mmax)
	
	def info = NumSection(TextToTranslate("CompositeSlabWithProfiledSheeting",CompositeSlabWithProfiledSheetingSymbols.dictionary),
		Evaluate(l,nsp,h,hc),
		AssertionGE("EN 1994-1-1 3.5(2)",this,t, Number(0.7,SI.mm)),
		AssertionLE("EN 1994-1-1 9.1.1(2)",this,br/bs,0.6),
		AssertionGE("EN 1994-1-1 9.2.1(2)",this,h,Number(90,SI.mm)),
		AssertionGE("EN 1994-1-1 9.2.1(2)",this,hc,Number(50,SI.mm))
	)
	
	def LOAD1 = Evaluate(gammaG,gammaQ,Gcck,Gccd,Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Qk1,Qd1)
	
	def ULS1 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Sprawdzenie nośności na zginanie w fazie montażu",
			Evaluate(MEdmm,MRdm),
			AssertionLE("nośności na zginanie",this,abs(MEdmm/MRdm),1),
			Evaluate(MEdmp,MRdp),
			AssertionLE("nośności na zginanie",this,abs(MEdmp/MRdp),1)
		),
		NumSection("Sprawdzenie nośności na ścinanie w fazie montażu",
			sheet.web,
			sheet.shearForce,
			Evaluate(VEdm,VwRd),
			AssertionLE("nośności na ścinanie",this,abs(VEdm/VwRd),1),
			AssertionLE("braku interakcji ścinania i zginania",this,abs(VEdm),0.5*VwRd)
		),
		NumSection("Sprawdzenie obciążenia miejscowego siłą poprzeczną nad podporą pośrednią wg PN-EN 1993-1-3",
			AssertionL("6.1.7.3(1)",this,r/t,10),
			AssertionL("6.1.7.3(1)",this,hw/t,200*sin(Phi)),
			AssertionRangeLL("6.1.7.3(1)",this,45,Phi,90),
			Evaluate(VEdm1,VEdm2,alpha,betav,ss,la,Rw1Rd,RwRd,FEdm),
			AssertionLE("nośności na miejscową siłą poprzeczną",this,abs(FEdm/RwRd),1)
		),
		NumSection("Sprawdzenie interakcji momentu zginającego i obciążenia lokalnego nad podporą wg PN-EN 1993-1-3",
			AssertionL("6.1.11(1)",this,abs(MEdmm/MRdm)+abs(FEdm/RwRd),1.25)
		)
	)
	
	def SLS1 = NumSection(TextToTranslate("SLS","eurocode"),
		Evaluate(MEkm1,deltasm,MEkm2,deltam),
		AssertionLE("EN 1994-1-1 9.3.2(2)",this,deltasm,h/10),
		AssertionLE("EN 1994-1-1 9.6(2)",this,deltasm,l/180),
		AssertionLE("EN 1993-1-1 NA.22 7.2.1(1)B",this,deltam,l/150)
	)
	
	def LOAD2 = Evaluate(gammaG,gammaQ,Gck,Gcd,Gsk,Gsd,qk,qd,SigmaQk,SigmaQd,SigmaGk,SigmaGd,Qk2,Qd2,DeltaQk,DeltaQd,Fk,Fd)
	
	def ULS2 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Sprawdzenie nośności na zginanie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.2",
			Evaluate(MEdep,xpl),
			AssertionLE("usytuowania osi obojętnej",this,xpl,hc),
			Evaluate(dp,Np,z,MplRd),
			AssertionLE("nośności na zginanie w fazie eksploatacji",this,abs(MEdep/MplRd),1)
		),
		NumSection("Sprawdzenie nośności na rozwarstwienie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.3",
			Evaluate(VEde,mV,kV,gammaVs,Ls,V1Rd),
			AssertionLE("nośności na rozwarstwienie",this,abs(VEde/V1Rd),1)
		),
		NumSection("Sprawdzenie nośności na ścinanie poprzeczne w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.5(1) i PN-EN 1992-1-1 pkt. 6.2.2",
			Evaluate(dv,kv,vmin,VRdc),
			AssertionLE("nośności na ścinanie",this,abs(VEde/VRdc),1),
			AssertionLE("EN 1992-1-1 (6.5)",this,VEde,(0.5*dv*fcd*(0.6*(1-(fck/Number(250,SI.MPa))))).setunit("kN/m"))
		),
		NumSection("Sprawdzenie nośności na przebicie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.6(1) i PN-EN 1992-1-1 pkt.6.4.4",
			Evaluate(ap,bp,cp,VpRd,Qvd),
			AssertionLE("nośności na przebicie",this,abs(Qvd/VpRd),1)
		)
	)
	
	def SLS2 = NumSection(TextToTranslate("SLS","eurocode"),
		NumSection("Sprawdzenie braku zarysowania betonu na podporami wg PN-EN 1994-1-1 pkt. 9.8.1",
			Evaluate(Asmin,dmesh,sd,sdmax),
			AssertionLE("minimalnego zbrojenia na zarysowanie nad podporą",this,sd,sdmax)
		),
		NumSection("Sprawdzenie braku zarysowania betonu w przęśle od obciążeń przyłożonych po zespoleniu wg PN-EN 1994-1-1 pkt. 9.8.2(3)",
			Evaluate(Eceff2,nE,e0,I0,W0,Mk,sigmactplus),
			AssertionLE("braku zarysowania przekroju",this,sigmactplus,fctm)
		),
		NumSection("Sprawdzenie ugięcia całkowitego",
			Evaluate(deltae,deltamax),
			AssertionLE("ugięcia całkowitego",this,deltamax,l/250)
		)
	)
	
}	
