package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Composite slab with profiled steel sheeting symbols */
trait CompositeSlabWithProfiledSheetingSymbols extends SymbolConfigurator {

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
	val Qk1 = symbol(Q|"k,1") unit "N/m2"
    val Qd1 = symbol(Q|"d,1") unit "N/m2"
	val deltas = symbol(BasicSymbols.delta|s) unit "m"
	val delta = symbol(BasicSymbols.delta) unit "m"
    val MEdm = symbol(M|("Ed","-")) unit "Nm/m"
    val MEdp = symbol(M|("Ed","+")) unit "Nm/m"
    val MEk1 = symbol(M|("Ek,1")) unit "Nm/m"
    val MEk2 = symbol(M|("Ek,2")) unit "Nm/m"
    val VEd = symbol(V|"Ed") unit "N/m"
    val VEd1 = symbol(V|"Ed,1") unit "N/m"
    val VEd2 = symbol(V|"Ed,2") unit "N/m"
    val FEd = symbol(F|"Ed") unit "N/m"
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
    val Qk2 = symbol(Q|"k,2") unit "N/m2"
    val Qd2 = symbol(Q|"d,2") unit "N/m2"
    val DeltaQk = symbol(("ΔQ")|"k") unit "N/m2"
    val DeltaQd = symbol(("ΔQ")|"d") unit "N/m2"
	
}

/** Composite slab with profiled steel sheeting context */
class CompositeSlabWithProfiledSheeting(
	id:String,
	height:Expression,
	length:Expression,
	spans:Expression,
	val sheet:ProfiledSteelSheet,
	val concrete:Concrete
) 
extends Calculation(Option(id)) with CompositeSlabWithProfiledSheetingSymbols {

	import ProfiledSteelSheet._
	import Concrete._
	import Steel._
	import Actions._
	
	val Beam = ContinuousBeam
	
	this add sheet
	this add concrete
	
	this(l) = length
	this(n) = spans
	this(h) = height
	this(hc) = h-hp
	this(Qcfk1) = (bs*hc+0.5*(bo+bb)*hp)*gammacf
	this(Qcfk) = Qcfk1/bs
	this(Qcfd) = Qcfk*gammaQ
	this(Qmk) = max(0.75E3,0.1*Qcfk)
	this(Qmd) = gammaQ*Qmk
	this(Qk1) = Gcck + Qcfk + Qmk
	this(Qd1) = Gccd + Qcfd + Qmd
	
	val beamULS = new ContinuousBeam_5_LinearLoad(null,l,Qd1)
	val beamSLS1 = new ContinuousBeam_5_LinearLoad(null,l,Qk1)
	val beamSLS2 = new ContinuousBeam_5_LinearLoad(null,l,Gcck + Qcfk)
	
	//belka 5 przeslowa
	this(MEdm) = beamULS(Beam.Mmin)
	this(MEdp) = beamULS(Beam.Mmax)
	this(VEd) = beamULS(Beam.Tmax)
	this(VEd1) = beamULS(Beam.TRmax1)
	this(VEd2) = beamULS(Beam.TRmax2)
	this(FEd) = abs(VEd1)+abs(VEd2)
	this(alpha) = 0.15
	this(betav) = (abs(VEd1)-abs(VEd2))/(abs(VEd1)+abs(VEd2))
	this(ss) = 180E-3
	this(la) = rangeChoiceLELE(betav,ss,0.2,((betav-0.2)/0.1)*((10E-3-ss)/0.1),0.3,10E-3)
	
	this(MEk1) = beamSLS2(Beam.Mmax)
	this(MEk2) = beamSLS1(Beam.Mmax)
	this(deltas) = 0.08*(MEk1*(l^2))/(E*Iplus)
	this(delta) = 0.08*(MEk2*(l^2))/(E*Iplus)
	
	//faza eksploatacji
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

	def info = NumSection(TextToTranslate("CompositeSlabWithProfiledSheeting",dictionary),id,
		Evaluate(Seq(l,n,h,hc),this),
		AssertionGE("EN 1994-1-1 3.5(2)",this,t,0.7E-3),
		AssertionLE("EN 1994-1-1 9.1.1(2)",this,br/bs,0.6),
		AssertionGE("EN 1994-1-1 9.2.1(2)",this,h,90E-3),
		AssertionGE("EN 1994-1-1 9.2.1(2)",this,hc,50E-3)
	)
	
	def LOAD1 = Evaluate(Seq(gammaG,gammaQ,Gcck,Gccd,Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Qk1,Qd1),this)
	
	def ULS1 = NumSection(TextToTranslate("ULS","eurocode"),id,
		NumSection("Sprawdzenie nośności na zginanie",
			Evaluate(Seq(MEdm,MRdm),this),
			AssertionLE("nośności na zginanie",this,abs(MEdm/MRdm),1),
			Evaluate(Seq(MEdp,MRdp),this),
			AssertionLE("nośności na zginanie",this,abs(MEdp/MRdp),1)
		),
		NumSection("Sprawdzenie nośności na ścinanie",
			Evaluate(Seq(VEd,VEd1,VEd2),this),
			sheet.web,
			sheet.shearForce,
			AssertionLE("nośności na ścinanie",this,abs(VEd/VwRd),1),
			AssertionLE("braku interakcji ścinania i zginania",this,abs(VEd),0.5*VwRd)
		),
		NumSection("Sprawdzenie obciążenia miejscowego siłą poprzeczną nad podporą pośrednią wg PN-EN 1993-1-3",
			AssertionL("6.1.7.3(1)",this,r/t,10),
			AssertionL("6.1.7.3(1)",this,hw/t,200*sin(Phi)),
			AssertionRangeLL("6.1.7.3(1)",this,45,Phi,90),
			Evaluate(Seq(alpha,betav,ss,la,Rw1Rd,RwRd,FEd),this),
			AssertionLE("nośności na miejscową siłą poprzeczną",this,abs(FEd/RwRd),1)
		),
		NumSection("Sprawdzenie interakcji momentu zginającego i obciążenia lokalnego nad podporą wg PN-EN 1993-1-3",
			AssertionL("6.1.11(1)",this,abs(MEdm/MRdm)+abs(FEd/RwRd),1.25)
		)
	)
	
	def SLS1 = NumSection(TextToTranslate("SLS","eurocode"),id,
		Evaluate(Seq(MEk1,deltas,MEk2,delta),this),
		AssertionLE("EN 1994-1-1 9.3.2(2)",this,deltas,h/10),
		AssertionLE("EN 1994-1-1 9.6(2)",this,deltas,l/180),
		AssertionLE("EN 1993-1-1 NA.22 7.2.1(1)B",this,delta,l/150)
	)
	
	def LOAD2 = Evaluate(Seq(gammaG,gammaQ,Gck,Gcd,Gsk,Gsd,qk,qd,SigmaQk,SigmaQd,SigmaGk,SigmaGd,Qk2,Qd2,DeltaQk,DeltaQd,Fk,Fd),this)
	
}

/** Composite slab with profiled steel sheeting library */
object CompositeSlabWithProfiledSheeting extends CompositeSlabWithProfiledSheetingSymbols {
	
	def apply(
		height:Expression,
		length:Expression,
		spans:Expression, 
		sheet:ProfiledSteelSheet, 
		concrete:Concrete) = {
		new CompositeSlabWithProfiledSheeting(null,height,length,spans,sheet,concrete)
	}

}