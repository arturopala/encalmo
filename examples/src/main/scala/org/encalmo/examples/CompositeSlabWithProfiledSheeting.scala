package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Composite slab with profiled steel sheeting symbols */
object CompositeSlabWithProfiledSheetingSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "compositeSlabWithProfiledSheeting"
	
	val l = symbol(BasicSymbols.l) unit "m"
	val n = symbol(BasicSymbols.n)
	val h = symbol(BasicSymbols.h) unit "m"
	val hc = symbol(BasicSymbols.h|c) unit "m"
	val Qcfk1 = symbol(Q|"cf,k1") unit "N/m"
	val Qcfk = symbol(Q|"cf,k") unit "N/m2"
	val Qcfd = symbol(Q|"cf,d") unit "N/m2"
	val Qmk = symbol(Q|"m,k") unit "N/m2"
	val Qmd = symbol(Q|"m,d") unit "N/m2"
	val Qk1 = symbol(Q|"k|m") unit "N/m2"
    val Qd1 = symbol(Q|"d|m") unit "N/m2"
	val deltasm = symbol(BasicSymbols.delta|"s|m") unit "m"
	val deltam = symbol(BasicSymbols.delta|"m") unit "m"
    val MEdmm = symbol(M|("Ed|m","-")) unit "Nm/m"
    val MEdmp = symbol(M|("Ed|m","+")) unit "Nm/m"
    val MEkm1 = symbol(M|("Ek,1|m")) unit "Nm/m"
    val MEkm2 = symbol(M|("Ek,2|m")) unit "Nm/m"
    val VEdm = symbol(V|"Ed|m") unit "N/m"
    val VEdm1 = symbol(V|"Ed,1|m") unit "N/m"
    val VEdm2 = symbol(V|"Ed,2|m") unit "N/m"
    val FEdm = symbol(F|"Ed|m") unit "N/m"
    val betav = symbol(BasicSymbols.beta|"v")
    val ss = symbol(BasicSymbols.s|"s") unit "m"
    
    val Gck = symbol(G|"c,k") unit "N/m2"
    val Gcd = symbol(G|"c,d") unit "N/m2"
    val Gsk = symbol(G|"s,k") unit "N/m2"
    val Gsd = symbol(G|"s,d") unit "N/m2"
    val qk = symbol(q|"k") unit "N/m2"
    val qd = symbol(q|"d") unit "N/m2"
    val Fk = symbol(F|"k") unit "N"
    val Fd = symbol(F|"d") unit "N"
    val SigmaQk = symbol(("ΣQ")|"k") unit "N/m2"
    val SigmaQd = symbol(("ΣQ")|"d") unit "N/m2"
    val SigmaGk = symbol(("ΣG")|"k") unit "N/m2"
    val SigmaGd = symbol(("ΣG")|"d") unit "N/m2"
    val Qk2 = symbol(Q|"k|e") unit "N/m2"
    val Qd2 = symbol(Q|"d|e") unit "N/m2"
    val DeltaQk = symbol(("ΔQ")|"k") unit "N/m2"
    val DeltaQd = symbol(("ΔQ")|"d") unit "N/m2"
    
    val MEdep = symbol(M|("Ed|e","+")) unit "Nm/m"
    val MEdem = symbol(M|("Ed|e","-")) unit "Nm/m"
    val xpl = symbol(x|"pl") unit "m"
    val dp = symbol(BasicSymbols.d|"p") unit "m"
	val Np = symbol(BasicSymbols.N|"p") unit "N/m"
	val z = symbol(BasicSymbols.z) unit "m"
	val MplRd = symbol(BasicSymbols.M|"pl,Rd") unit "Nm/m"
	val VEde = symbol(BasicSymbols.V|"Ed|e") unit "N/m"
	val mV = symbol(BasicSymbols.m|"V") unit "Pa"
	val kV = symbol(BasicSymbols.k|"V") unit "Pa"
	val Ls = symbol(BasicSymbols.L|"s") unit "m"
	val gammaVs = symbol(BasicSymbols.gamma|"Vs")
	val V1Rd = symbol(BasicSymbols.V|"1,Rd") unit "N/m"
	val ap = symbol(BasicSymbols.a|"p") unit "m"
	val bp = symbol(BasicSymbols.b|"p") unit "m"
	val cp = symbol(BasicSymbols.c|"p") unit "m"
	val vmin = symbol(BasicSymbols.v|"min") unit "Pa"
	val dv = symbol(BasicSymbols.d|"v") unit "m"
	val kv = symbol(BasicSymbols.k|"v")
	val VRdc = symbol(BasicSymbols.V|"Rd,c") unit "N/m"
	val VpRd = symbol(BasicSymbols.V|"p,Rd") unit "N"
	val Qvd = symbol(BasicSymbols.Q|"v,d") unit "N"
	val Asmin = symbol(BasicSymbols.A|"s,min") unit "m2/m"
	val dmesh = symbol(BasicSymbols.d|"mesh") unit "mm"
	val sd = symbol(BasicSymbols.s|"d") unit "m"
	val sdmax = symbol(BasicSymbols.s|"d,max") unit "m"
	val Eceff = symbol(BasicSymbols.E|"c,eff") unit "Pa"
	val nE = symbol(BasicSymbols.n|"E")
}

/** Composite slab with profiled steel sheeting context */
object CompositeSlabWithProfiledSheetingExpressions extends MapContext {

	import CompositeSlabWithProfiledSheetingSymbols._
	import ProfiledSteelSheetSymbols._
	import SteelSymbols.{E,fypd}
    import ConcreteSymbols._
    import ActionsSymbols._
	
	this(hc) = h-hp
	
	//faza montazu - LOAD
	this(Qcfk1) = (bs*hc+0.5*(bo+bb)*hp)*gammacf
	this(Qcfk) = Qcfk1/bs
	this(Qcfd) = Qcfk*gammaQ
	this(Qmk) = max(0.75E3,0.1*Qcfk)
	this(Qmd) = gammaQ*Qmk
	this(Qk1) = Gcck + Qcfk + Qmk
	this(Qd1) = Gccd + Qcfd + Qmd

	this(FEdm) = abs(VEdm1)+abs(VEdm2)
	this(alpha) = 0.15
	this(betav) = abs(abs(VEdm1)-abs(VEdm2))/(abs(VEdm1)+abs(VEdm2))
	this(ss) = 180E-3
	this(la) = rangeChoiceLELE(betav,ss,0.2,((betav-0.2)/0.1)*((10E-3-ss)/0.1),0.3,10E-3)
	
	//faza montazu - SLS
	this(deltasm) = 0.08*(MEkm1*(l^2))/(E*Iplus)
	this(deltam) = 0.08*(MEkm2*(l^2))/(E*Iplus)
	
	//faza eksploatacji - LOAD
	this(Gck) = Qcfk*(gammac/gammacf)
	this(Gcd) = Gck*gammaG
	this(Gsd) = Gsk*gammaG
	this(qd) = qk*gammaQ
	this(SigmaQk) = qk
	this(SigmaGk) = Gcck+Gck+Gsk
	this(SigmaQd) = qd
	this(SigmaGd) = Gccd+Gcd+Gsd
	this(Qk2) = SigmaGk + SigmaQk
	this(Qd2) = SigmaGd + SigmaQd
	this(Fd) = Fk*gammaQ
	this(DeltaQk) = Qk2-Qk1
	this(DeltaQd) = Qd2-Qd1
	
	//faza eksploatacji - ULS
	this(MEdep) = (Qd2*(l^2))/8
	this(MEdep) = (Qd2*(l^2))/8
	this(xpl) = (Ap*fypd)/(0.85*fcd)
	this(dp) = ep+hc
	this(Np) = Ap*fypd
	this(z) = dp-0.5*xpl
	this(MplRd) = Np*z
	this(VEde) = 0.5*Qd2*l
	this(mV) = 103E6
	this(kV) = 0.19E6
	this(Ls) = l/4
	this(gammaVs) = 1.25
	this(V1Rd) = (dp*(((mV*Ap)/(Ls))+kV))/gammaVs
	//przebicie
	this(ap) = 0.1
	this(bp) = 0.1
	this(cp) = 2*(ap+bp)+4*(dp-hc)+2*PI*hc
	this(dv) = dp
	this(kv) = sqrt(1+(200/(dv*1000)))
	this(vmin) = 35*sqrt(kv^3)*sqrt(fck)
	this(VpRd) = vmin*cp*dp
	this(VRdc) = vmin*dv
	this(Qvd) = Fd
	//zarysowanie betonu nad podpora
	this(Asmin) = 0.002*hc*1
	this(sdmax) = (PI*(dmesh^2))/(4*Asmin)*1E-6
	//ugiecia w fazie eksploatacji
	this(Eceff) = Ecm/2
	this(nE) = E/Eceff
	
	// end of context initialization
	lock

}

class CompositeSlabWithProfiledSheeting(
	height:Expression,
	length:Expression,
	spans:Expression, 
	sheet:ProfiledSteelSheet, 
	concrete:Concrete
)
extends Calculation {

	import CompositeSlabWithProfiledSheetingSymbols._
	import ProfiledSteelSheetSymbols._
    import ConcreteSymbols.{fcd,fck}
    import ActionsSymbols._

	val Beam = ContinuousBeamSymbols

	this add CompositeSlabWithProfiledSheetingExpressions
	this add sheet
	this add concrete
	
	this(l) = length
	this(BasicSymbols.n) = spans
	this(h) = height
	
	val beamULS = new ContinuousBeam_5_LinearLoad(null,l,Qd1)
	val beamSLS1 = new ContinuousBeam_5_LinearLoad(null,l,Qk1)
	val beamSLS2 = new ContinuousBeam_5_LinearLoad(null,l,Gcck + Qcfk)
	
	//faza montazu - ULS
	this(MEdmm) = beamULS(Beam.Mmin)
	this(MEdmp) = beamULS(Beam.Mmax)
	this(VEdm) = beamULS(Beam.Tmax)
	this(VEdm1) = beamULS(Beam.TRmax1)
	this(VEdm2) = beamULS(Beam.TRmax2)
	
	this(MEkm1) = beamSLS2(Beam.Mmax)
	this(MEkm2) = beamSLS1(Beam.Mmax)
	
	def info = NumSection(TextToTranslate("CompositeSlabWithProfiledSheeting",CompositeSlabWithProfiledSheetingSymbols.dictionary),
		Evaluate(Seq(l,n,h,hc),this),
		AssertionGE("EN 1994-1-1 3.5(2)",this,t,0.7E-3),
		AssertionLE("EN 1994-1-1 9.1.1(2)",this,br/bs,0.6),
		AssertionGE("EN 1994-1-1 9.2.1(2)",this,h,90E-3),
		AssertionGE("EN 1994-1-1 9.2.1(2)",this,hc,50E-3)
	)
	
	def LOAD1 = Evaluate(Seq(gammaG,gammaQ,Gcck,Gccd,Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Qk1,Qd1),this)
	
	def ULS1 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Sprawdzenie nośności na zginanie w fazie montażu",
			Evaluate(Seq(MEdmm,MRdm),this),
			AssertionLE("nośności na zginanie",this,abs(MEdmm/MRdm),1),
			Evaluate(Seq(MEdmp,MRdp),this),
			AssertionLE("nośności na zginanie",this,abs(MEdmp/MRdp),1)
		),
		NumSection("Sprawdzenie nośności na ścinanie w fazie montażu",
			sheet.web,
			sheet.shearForce,
			Evaluate(Seq(VEdm,VwRd),this),
			AssertionLE("nośności na ścinanie",this,abs(VEdm/VwRd),1),
			AssertionLE("braku interakcji ścinania i zginania",this,abs(VEdm),0.5*VwRd)
		),
		NumSection("Sprawdzenie obciążenia miejscowego siłą poprzeczną nad podporą pośrednią wg PN-EN 1993-1-3",
			AssertionL("6.1.7.3(1)",this,r/t,10),
			AssertionL("6.1.7.3(1)",this,hw/t,200*sin(Phi)),
			AssertionRangeLL("6.1.7.3(1)",this,45,Phi,90),
			Evaluate(Seq(VEdm1,VEdm2,alpha,betav,ss,la,Rw1Rd,RwRd,FEdm),this),
			AssertionLE("nośności na miejscową siłą poprzeczną",this,abs(FEdm/RwRd),1)
		),
		NumSection("Sprawdzenie interakcji momentu zginającego i obciążenia lokalnego nad podporą wg PN-EN 1993-1-3",
			AssertionL("6.1.11(1)",this,abs(MEdmm/MRdm)+abs(FEdm/RwRd),1.25)
		)
	)
	
	def SLS1 = NumSection(TextToTranslate("SLS","eurocode"),
		Evaluate(Seq(MEkm1,deltasm,MEkm2,deltam),this),
		AssertionLE("EN 1994-1-1 9.3.2(2)",this,deltasm,h/10),
		AssertionLE("EN 1994-1-1 9.6(2)",this,deltasm,l/180),
		AssertionLE("EN 1993-1-1 NA.22 7.2.1(1)B",this,deltam,l/150)
	)
	
	def LOAD2 = Evaluate(Seq(gammaG,gammaQ,Gck,Gcd,Gsk,Gsd,qk,qd,SigmaQk,SigmaQd,SigmaGk,SigmaGd,Qk2,Qd2,DeltaQk,DeltaQd,Fk,Fd),this)
	
	def ULS2 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Sprawdzenie nośności na zginanie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.2",
			Evaluate(Seq(MEdep,xpl),this),
			AssertionLE("usytuowania osi obojętnej",this,xpl,hc),
			Evaluate(Seq(dp,Np,z,MplRd),this),
			AssertionLE("nośności na zginanie w fazie eksploatacji",this,abs(MEdep/MplRd),1)
		),
		NumSection("Sprawdzenie nośności na rozwarstwienie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.3",
			Evaluate(Seq(VEde,mV,kV,gammaVs,Ls,V1Rd),this),
			AssertionLE("nośności na rozwarstwienie",this,abs(VEde/V1Rd),1)
		),
		NumSection("Sprawdzenie nośności na ścinanie poprzeczne w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.5(1) i PN-EN 1992-1-1 pkt. 6.2.2",
			Evaluate(Seq(dv,kv,vmin,VRdc),this),
			AssertionLE("nośności na ścinanie",this,abs(VEde/VRdc),1),
			AssertionLE("EN 1992-1-1 (6.5)",this,VEde,0.5*dv*fcd*(0.6*(1-(fck/250E6))))
		),
		NumSection("Sprawdzenie nośności na przebicie w fazie eksploatacji wg PN-EN 1994-1-1 pkt. 9.7.6(1) i PN-EN 1992-1-1 pkt.6.4.4",
			Evaluate(Seq(ap,bp,cp,VpRd,Qvd),this),
			AssertionLE("nośności na przebicie",this,abs(Qvd/VpRd),1)
		)
	)
	
	def SLS2 = NumSection(TextToTranslate("SLS","eurocode"),
		NumSection("Kontrola zarysowania betonu na podporami wg PN-EN 1994-1-1 pkt. 9.8.1",
			Evaluate(Seq(Asmin,dmesh,sd,sdmax),this),
			AssertionLE("minimalnego zbrojenia na zarysowanie nad podporą",this,sd,sdmax)
		)
	)
	
}	
