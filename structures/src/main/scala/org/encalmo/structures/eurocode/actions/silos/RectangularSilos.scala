package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.calculation.Eval
import org.encalmo.document._


/** Rectangular silos symbols */
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
    lazy val be = symbol(BasicSymbols.b|BasicSymbols.e) unit "m"
}

/** RectangularSilos expressions */
object RectangularSlenderSilosWithFlatBottomExpressions extends MapContext {

    import SilosSymbols._
	import RectangularSilosSymbols._
	import ParticulateSolidSymbols._
	
	//calculated geometry
	this(h3) = h1-ts
	this(b3) = b1-2*t
	this(b4) = b2-2*t
	this(dc) = min(b3,b4)
	this(A) = (b3)*(b4)
	this(U) = 2*b3+2*b4
	this(hc) = h3
	//volumes
	this(Vc) = b3*b4*hc
	
	//calculated geometry
	this(AU) = A/U
	this(beta) = 90
	this(hh) = 0
	this(htp) = dc/(2*tan(fir))
	this(ho) = (dc*tan(fir))/4
	this(he) = 0
	this(hb) = hc
	this(hcdc) = hc/dc
	
	//volumes
	this(Vh) = 0
	this(V) = Vc+Vh
	this(W) = V*gammau/GRAV
	
	//filling symmetrical load
	//  (1) Maximum normal pressure on vertical wall
	this(zo(1)) = 1/(K_u*mu_l)*AU
	this(YJ(1)) = 1-(EUL^(-z/zo(1)))
    this(pho(1)) = (gammau/mu_l)*AU
	this(phf(1)) = fx(pho(1)*YJ(1),z)
	this(phf1) = Eval(phf(1), z -> hc/4)
	this(phf2) = Eval(phf(1), z -> hc/2)
	this(phf3) = Eval(phf(1), z -> 0.75*hc)
	this(phft) = Eval(phf(1), z -> hc)
	//  (2) Maximum frictional traction on vertical wall
	this(zo(2)) = 1/(K_u*mu_u)*AU
    this(YJ(2)) = 1-(EUL^(-z/zo(2)))
    this(pho(2)) = (gammau/mu_u)*AU
    this(phf(2)) = fx(pho(2)*YJ(2),z)
	this(pwf) = mu_u * phf(2)
    this(nfzSk) = mu_u*pho(2)*(z-zo(2)*YJ(2))
    this(nfzSkt) = Eval(nfzSk, z -> hc)
	//  (3) Maximum vertical load on hopper or silo bottom
	this(zo(3)) = 1/(K_l*mu_l)*AU
    this(YJ(3)) = 1-(EUL^(-z/zo(3)))
    this(pho(3)) = (gammau/mu_l)*AU
	this(pvf) = (pho(3)/K_l)*YJ(3)
	this(pvft) = Eval(pvf, z -> hc)
	
	//filling patch load
	this(ef) = dc/4
	this(E) = 2*ef/dc
	this(Cpf) = 0.21*Cop*(1+2*(E^2))*(1-(EUL^(-1.5*(hcdc-1))))
	this(ppf) = Cpf*phf(1)
	this(ppfnc) = 0.36*ppf
	this(ppfnc1) = Eval(ppfnc, z -> hc/2)
	this(s) = (PI*dc)/16
	this(Fpf1) = ppfnc1*s
	
	//discharge symmetrical load
	this(CS) = hcdc-1
	this(Ch) = 1+0.15*CS
	this(Cw) = 1+0.1*CS
	this(phe) = Ch*phf(1)
	this(pwe) = Cw*pwf
	this(phet) = Eval(phe, z -> hc)
	this(nezSk) = Cw*mu_u*pho(2)*(z-zo(2)*YJ(2))
    this(nezSkt) = Eval(nezSk, z -> hc)
	
	//discharge patch load
    this(Cpe) = 0.42*Cop*(1+2*(E^2))*(1-(EUL^(-1.5*(hcdc-1))))
    this(ppe) = Cpe*phe
    this(ppenc) = 0.36*ppe
    this(ppenc1) = Eval(ppenc, z -> hc/2)
    this(Fpe1) = ppenc1*s
	
	// end of context initialization
	lock

}


/** Rectangular slender silos calculation */
class RectangularSlenderSilosWithFlatBottom(
	vb1:Expression,
	vb2:Expression,
	vh1:Expression, 
	vh2:Expression, 
	vt:Expression, 
	vts:Expression, 
	vbe:Expression, 
	particulateSolid:ParticulateSolid,
	vD:Expression
)
extends Calculation {

	import SilosSymbols._
	import RectangularSilosSymbols._
	import ParticulateSolidSymbols._
	
	particulateSolid(ParticulateSolidSymbols.D) = vD
	
	this add RectangularSlenderSilosWithFlatBottomExpressions
	this add particulateSolid
	
	this(b1) = vb1
	this(b2) = vb2
	this(h1) = vh1
	this(h2) = vh2
	this(t) = vt
	this(ts) = vts
	this(be) = vbe
	
	//input geometry
	def inputGeometry = NumSection(TextToTranslate("_inputGeometry",SilosSymbols.dictionary),
		Evaluate(Seq(b1,b2,h1,h2,t,ts),this)
	)
	//input assertions
	
	//calculated geometry
	def calculatedGeometry = NumSection(TextToTranslate("_calculatedGeometry",SilosSymbols.dictionary),
		Evaluate(Seq(h3,b3,b4,dc,A,U,AU,beta,hh,htp,ho,hc,hb,hcdc),this)
	)
	//volumes
	def volumes = NumSection(TextToTranslate("_volumes",SilosSymbols.dictionary),
		Evaluate(Seq(Vc,Vh,V,W),this)
	)
	//filling symmetrical load
	def fillingSymmetricalLoad = NumSection(TextToTranslate("_fillingSymmetricalLoad",SilosSymbols.dictionary),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_1",SilosSymbols.dictionary),
		Evaluate(Seq(zo(1),pho(1),YJ(1),phf(1),phf1,phf2,phf3,phft),this)
		),
		NumSection(TextToTranslate("_fillingSymmetricalLoad_2",SilosSymbols.dictionary),
        Evaluate(Seq(zo(2),pho(2),YJ(2),phf(2),pwf,nfzSk,nfzSkt),this)
        ),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_3",SilosSymbols.dictionary),
        Evaluate(Seq(zo(3),pho(3),YJ(3),pvf,pvft),this)
        )
	)
	//filling patch load
	def fillingPatchLoad = NumSection(TextToTranslate("_fillingPatchLoad",SilosSymbols.dictionary),
        Evaluate(Seq(ef,E,Cpf,ppf,ppfnc,ppfnc1,s,Fpf1),this)
    )
    //discharge symmetrical load
	def dischargeSymmetricalLoad = NumSection(TextToTranslate("_dischargeSymmetricalLoad",SilosSymbols.dictionary),
        Evaluate(Seq(CS,Ch,Cw,phe,phet,pwe,nezSk,nezSkt),this)
    )
    //discharge patch load
    def dischargePatchLoad = NumSection(TextToTranslate("_dischargePatchLoad",SilosSymbols.dictionary),
        Evaluate(Seq(Cpe,ppe,ppenc,ppenc1,Fpe1),this)
    )
    
    //loads on silo hoppers
    def loadsOnSiloBottom = NumSection(TextToTranslate("_loadsOnSiloBottom",SilosSymbols.dictionary),
        Evaluate(Seq(pvft),this)
    )
    
}	
