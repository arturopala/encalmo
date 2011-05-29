package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.calculation.Eval
import org.encalmo.document._

/** RectangularSilos symbols */
object RectangularSilosSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "rectangularSilos"
	
	//input geometry
	lazy val b1 = symbol(BasicSymbols.b|1) unit "m"
    lazy val b2 = symbol(BasicSymbols.b|2) unit "m"
    lazy val b3 = symbol(BasicSymbols.b|3) unit "m"
    lazy val b4 = symbol(BasicSymbols.b|4) unit "m"
    lazy val h1 = symbol(BasicSymbols.h|1) unit "m"
    lazy val h2 = symbol(BasicSymbols.h|2) unit "m"
    lazy val h3 = symbol(BasicSymbols.h|3) unit "m"
    lazy val t = symbol(BasicSymbols.t) unit "m"
    lazy val ts = symbol(BasicSymbols.t|BasicSymbols.s) unit "m"
    lazy val be = symbol(BasicSymbols.b|BasicSymbols.e) unit "m"
    
    //calculated geometry
    lazy val dc = symbol(BasicSymbols.d|BasicSymbols.c) unit "m"
    lazy val A = symbol(BasicSymbols.A) unit "m2"
    lazy val U = symbol(BasicSymbols.U) unit "m"
    lazy val AU = symbol("A/U")
    lazy val beta = symbol(BasicSymbols.beta) unit "°"
    lazy val hh = symbol(BasicSymbols.h|BasicSymbols.h) unit "m"
    lazy val he = symbol(BasicSymbols.h|BasicSymbols.e) unit "m"
    lazy val htp = symbol(BasicSymbols.h|"tp") unit "m"
    lazy val ho = symbol(BasicSymbols.h|"o") unit "m"
    lazy val hc = symbol(BasicSymbols.h|"c") unit "m"
    lazy val hb = symbol(BasicSymbols.h|"b") unit "m"
    lazy val hcdc = symbol("hc/dc")
    
    //volumes
    lazy val V = symbol(BasicSymbols.V) unit "m3"
    lazy val Vh = symbol(BasicSymbols.V|BasicSymbols.h) unit "m3"
    lazy val Vc = symbol(BasicSymbols.V|BasicSymbols.c) unit "m3"
    lazy val Vf = symbol(BasicSymbols.V|BasicSymbols.f) unit "m3"
    
    //weights
    lazy val W = symbol(BasicSymbols.W) unit "t"
    
    //filling symmetrical load
    lazy val z = symbol(BasicSymbols.z) unit "m"
    lazy val phf = symbol(BasicSymbols.p|"hf") args (z) unit "kPa"
    lazy val pwf = symbol(BasicSymbols.p|"wf") unit "kPa"
    lazy val pvf = symbol(BasicSymbols.p|"vf") unit "kPa"
    lazy val pvft = symbol(BasicSymbols.p|"vft") unit "kPa"
    lazy val pho = symbol(BasicSymbols.p|"ho") unit "kPa"
    lazy val YR = symbol(BasicSymbols.Y|"R")
    lazy val zo = symbol(BasicSymbols.z|"o") unit "m"
    lazy val n = symbol(BasicSymbols.n)
    lazy val phf1 = symbol(BasicSymbols.p|"hf,1") unit "kPa"
    lazy val phf2 = symbol(BasicSymbols.p|"hf,2") unit "kPa"
    lazy val phf3 = symbol(BasicSymbols.p|"hf,3") unit "kPa"
    lazy val phft = symbol(BasicSymbols.p|"hft") unit "kPa"
    lazy val zV = symbol(BasicSymbols.z|BasicSymbols.V) unit "m"
    lazy val nfzSk = symbol(BasicSymbols.n|"fzSk") unit "kN/m"
    lazy val nfzSkt = symbol(BasicSymbols.n|"fzSkt") unit "kN/m"
    
    //filling patch load
    lazy val ef = symbol(BasicSymbols.e|"f") unit "m"
    lazy val E = symbol(BasicSymbols.E)
    lazy val Cpf = symbol(BasicSymbols.C|"pf")
    lazy val ppf = symbol(BasicSymbols.p|"pf") unit "kPa"
    lazy val s = symbol(BasicSymbols.s) unit "m"
    lazy val ppfnc = symbol(BasicSymbols.p|"pf,nc") unit "kPa"
    lazy val ppfnc1 = symbol(BasicSymbols.p|"pf,nc,1") unit "kPa"
    lazy val Fpf1 = symbol(BasicSymbols.F|"pf,1") unit "kN/m"
    
    //discharge symmetrical load
	lazy val CS = symbol(BasicSymbols.C|"S")
	lazy val Ch = symbol(BasicSymbols.C|"h")
	lazy val Cw = symbol(BasicSymbols.C|"w")
	lazy val phe = symbol(BasicSymbols.p|"he") unit "kPa"
    lazy val pwe = symbol(BasicSymbols.p|"we") unit "kPa"
    lazy val phet = symbol(BasicSymbols.p|"het") unit "kPa"
    lazy val nezSk = symbol(BasicSymbols.n|"ezSk") unit "kN/m"
    lazy val nezSkt = symbol(BasicSymbols.n|"ezSkt") unit "kN/m"
    
    //discharge patch load
    lazy val Cpe = symbol(BasicSymbols.C|"pe")
    lazy val ppe = symbol(BasicSymbols.p|"pe") unit "kPa"
    lazy val ppenc = symbol(BasicSymbols.p|"pe,nc") unit "kPa"
    lazy val ppenc1 = symbol(BasicSymbols.p|"pe,nc,1") unit "kPa"
    lazy val Fpe1 = symbol(BasicSymbols.F|"pe,1") unit "kN/m"
    
    //loads on silo hoppers
    lazy val x = symbol(BasicSymbols.x)
    lazy val Cb = symbol(BasicSymbols.C|"b")
    lazy val pv = symbol(BasicSymbols.p|"v") args (x) unit "kPa"
    lazy val pnf = symbol(BasicSymbols.p|"nf") args (x) unit "kPa"
    lazy val ptf = symbol(BasicSymbols.p|"tf") args (x) unit "kPa"
    lazy val nh = symbol(BasicSymbols.n|"h")
    lazy val muheff = symbol(BasicSymbols.mu|"heff")
    lazy val Ff = symbol(BasicSymbols.F|"f")
}

/** RectangularSilos context */
object RectangularSilosExpressions extends MapContext {

	import RectangularSilosSymbols._
	import ParticulateSolidSymbols._
	
	//calculated geometry
	this(h3) = h1-ts
	this(b3) = b1-2*t
	this(b4) = b2-2*t
	this(dc) = min(b3,b4)
	this(A) = (b3)*(b4)
	this(U) = 2*b3+2*b4
	this(AU) = A/U
	this(beta) = 90-(fir+5)
	this(hh) = max(b3,b4)/(2*tan(beta))
	this(htp) = dc/(2*tan(fir))
	this(ho) = (dc*tan(fir))/4
	this(he) = be/(2*tan(beta))
	this(hc) = h3-hh+he
	this(hb) = hc+hh
	this(hcdc) = hc/dc
	
	//volumes
	this(Vc) = b3*b4*hc
	this(Vh) = (A*(h3-hc)-(be*be*he))/3
	this(V) = Vc+Vh
	this(Vf) = A*(hh-he)-Vh
	this(W) = V*gammau/GRAV
	
	//filling symmetrical load
	this(zo) = 1/(Km*mum)*AU
	this(n) = -(1+tan(fir))*(1-ho/zo)
	this(pho) = gammau/mu_l*AU
	this(YR) = 1-(((z-ho)/(zo-ho)+1)^n) 
	this(phf) = fx(pho*YR,z)
	this(phf1) = Eval(phf, z -> (ho+((hc-ho)/4)))
	this(phf2) = Eval(phf, z -> (ho+((hc-ho)/2)))
	this(phf3) = Eval(phf, z -> (ho+(3*(hc-ho)/4)))
	this(phft) = Eval(phf, z -> hc)
	this(pwf) = mu_u * phf
	this(zV) = ho - ((1/(n+1))*(zo-ho-(((z+zo-2*ho)^(n+1))/((z-ho)^n))))
	this(pvf) = gammau*zV
	this(pvft) = Eval(pvf, z -> hc)
	this(nfzSk) = mu_u*pho*(z-zV)
	this(nfzSkt) = Eval(nfzSk, z -> hc)
	
	//filling patch load
	this(ef) = dc/4
	this(E) = 2*ef/dc
	this(Cpf) = 0.21*Cop*(1+2*(E^2))*(1-(EUL^(-1.5*(hcdc-1))))
	this(ppf) = Cpf*phf
	this(ppfnc) = 0.36*ppf
	this(ppfnc1) = Eval(ppfnc, z -> h3/2)
	this(s) = (PI*dc)/16
	this(Fpf1) = ppfnc1*s
	
	//discharge symmetrical load
	this(CS) = hcdc-1
	this(Ch) = 1+0.15*CS
	this(Cw) = 1+0.1*CS
	this(phe) = Ch*phf
	this(pwe) = Cw*pwf
	this(phet) = Eval(phe, z -> hc)
	this(nezSk) = Cw*mu_u*pho*(z-zV)
    this(nezSkt) = Eval(nezSk, z -> hc)
	
	//discharge patch load
    this(Cpe) = 0.42*Cop*(1+2*(E^2))*(1-(EUL^(-1.5*(hcdc-1))))
    this(ppe) = Cpe*phe
    this(ppenc) = 0.36*ppe
    this(ppenc1) = Eval(ppenc, z -> h3/2)
    this(Fpe1) = ppenc1*s
    
    //loads on silo hoppers
    this(Cb) = 1
    this(muheff) = (1-Km)/(2*tan(beta))
    this(Ff) = 1-(0.2/(1+(tan(beta)/muheff)))
    this(nh) = 2*(1-0.2)*muheff*cot(beta)
    this(pv) = fx(((gammau*hh)/(nh-1))*((x/hh)-((x/hh)^nh))+pvft*((x/hh)^nh),x)
    this(pnf) = Ff*pv
    this(ptf) = muheff*Ff*pv
	
	// end of context initialization
	lock

}

class RectangularSilos(
	vb1:Expression,
	vb2:Expression,
	vh1:Expression, 
	vh2:Expression, 
	vt:Expression, 
	vts:Expression, 
	vbe:Expression, 
	particulateSolid:ParticulateSolid,
	vD:Expression, 
	concrete:Concrete,
	reinforcingSteel:ReinforcingSteel
)
extends Calculation {

	import RectangularSilosSymbols._
	import ParticulateSolidSymbols._
	import ConcreteSymbols.{fcd,fck,fctm}
	
	particulateSolid(ParticulateSolidSymbols.D) = vD
	
	this add RectangularSilosExpressions
	this add particulateSolid
	this add concrete
	this add reinforcingSteel
	
	this(b1) = vb1
	this(b2) = vb2
	this(h1) = vh1
	this(h2) = vh2
	this(t) = vt
	this(ts) = vts
	this(be) = vbe
	
	//input geometry
	def inputGeometry = NumSection(TextToTranslate("_inputGeometry",RectangularSilosSymbols.dictionary),
		Evaluate(Seq(b1,b2,h1,h2,t),this)
	)
	//input assertions
	
	//calculated geometry
	def calculatedGeometry = NumSection(TextToTranslate("_calculatedGeometry",RectangularSilosSymbols.dictionary),
		Evaluate(Seq(h3,b3,b4,dc,A,U,AU,beta,hh,htp,ho,hc,hb,hcdc),this)
	)
	//volumes
	def volumes = NumSection(TextToTranslate("_volumes",RectangularSilosSymbols.dictionary),
		Evaluate(Seq(Vc,Vh,V,Vf,W),this)
	)
	//filling symmetrical load
	def fillingSymmetricalLoad = NumSection(TextToTranslate("_fillingSymmetricalLoad",RectangularSilosSymbols.dictionary),
		Evaluate(Seq(zo,n,pho,YR,phf,phf1,phf2,phf3,phft,pwf,zV,pvf,pvft,nfzSk,nfzSkt),this)
	)
	//filling patch load
	def fillingPatchLoad = NumSection(TextToTranslate("_fillingPatchLoad",RectangularSilosSymbols.dictionary),
        Evaluate(Seq(ef,E,Cpf,ppf,ppfnc,ppfnc1,s,Fpf1),this)
    )
    //discharge symmetrical load
	def dischargeSymmetricalLoad = NumSection(TextToTranslate("_dischargeSymmetricalLoad",RectangularSilosSymbols.dictionary),
        Evaluate(Seq(CS,Ch,Cw,phe,phet,pwe,nezSk,nezSkt),this)
    )
    //discharge patch load
    def dischargePatchLoad = NumSection(TextToTranslate("_dischargePatchLoad",RectangularSilosSymbols.dictionary),
        Evaluate(Seq(Cpe,ppe,ppenc,ppenc1,Fpe1),this)
    )
    
    //loads on silo hoppers
    def loadsOnSiloHoppers = NumSection(TextToTranslate("_loadsOnSiloHoppers",RectangularSilosSymbols.dictionary),
        Evaluate(Seq(tan(beta)-(1-Km)/(2*mu_l),Cb,muheff,Ff,nh,pv,pnf,ptf),this)
    )
    
}	
