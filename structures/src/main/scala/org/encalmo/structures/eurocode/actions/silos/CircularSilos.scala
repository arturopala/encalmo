package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.calculation.Eval
import org.encalmo.document._


/** Circular silos symbols */
object CircularSilosSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "circularSilos"
	
	//input geometry
	lazy val d1 = symbol(BasicSymbols.d|1) unit SI.m
    lazy val h1 = symbol(BasicSymbols.h|1) unit SI.m
    lazy val h2 = symbol(BasicSymbols.h|2) unit SI.m
    lazy val de = symbol(BasicSymbols.d|BasicSymbols.e) unit SI.m
}

/** Thin walled circular silos with steep hopper expressions */
object ThinWalledCircularSlenderSilosWithSteepHopperExpressions extends MapContext {

    import SilosSymbols._
	import CircularSilosSymbols._
	import ParticulateSolidSymbols._
	
	//calculated geometry
	this(dc) = d1-2*t
	this(A) = PI*(dc^2)/4
	this(U) = PI*dc
	this(hh) = h2
	this(AU) = A/U
	this(beta) = arctan((dc-de)/(2*hh))
	this(htp) = (dc*tan(fir))/2
	this(ho) = htp/2
	this(hc) = h1-ho
	this(he) = 0
	this(hb) = hc+hh
	this(hcdc) = hc/dc
	
	//volumes
	this(Vc) = A*h1
	this(Vh) = 1/3d*PI*h2*(((dc/2)^2)+(dc/2)*(de/2)+((de/2)^2))
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
    this(Cb)=1.2
	this(zo(3)) = 1/(K_l*mu_l)*AU
    this(YJ(3)) = 1-(EUL^(-z/zo(3)))
    this(pho(3)) = (gammau/mu_l)*AU
	this(pvf) = (pho(3)/K_l)*YJ(3)
	this(pvft) = Eval(Cb*pvf, z -> hc)
	
	//filling patch load
	this(ef) = dc/4
	this(E) = 2*ef/dc
	this(Cpf) = 0.21*Cop*(1+2*(E^2))*(1-(EUL^(-1.5*(hcdc-1))))
	this(ppf) = Cpf*phf(1)
	this(zp) = min(zo(1),hc/2)
	this(ppfzp) = Eval(ppf, z -> zp)
	this(s) = (PI*dc)/16
	this(Fpf1) = (PI/2)*s*dc*ppfzp
	
	//discharge symmetrical load
	this(Ch) = 1.15
	this(Cw) = 1.1
	this(phe) = Ch*phf(1)
	this(pwe) = Cw*pwf
	this(phet) = Eval(phe, z -> hc)
	this(nezSk) = Cw*mu_u*pho(2)*(z-zo(2)*YJ(2))
    this(nezSkt) = Eval(nezSk, z -> hc)
	
	//discharge patch load
    this(Cpe) = 0.42*Cop*(1+2*(E^2))*(1-(EUL^(-1.5*(hcdc-1))))
    this(ppe) = Cpe*phe
    this(ppezp) = Eval(ppe, z -> zp)
    this(Fpe1) = (PI/2)*s*dc*ppezp
    
    //Loads on silo hoppers and silo bottoms
    
    //filling load on hopper
    this(muheff) = mu_l
    this(Ff) = 1 - (0.2/(1+(tan(beta)/mu_l)))
    this(nh) = 2*(1-0.2)*mu_l*cot(beta)
    this(pv) = fx(((gammau*hh)/(nh-1))*((x/hh)-((x/hh)^nh))+pvft*((x/hh)^nh),x)
    this(pnf) = Ff*pv
    this(ptf) = muheff*Ff*pv
    
    //discharge load on hopper
    this(Fe) = 1 //TODO wzor z normy
    this(pne) = Fe*pv
    this(pte) = muheff*Fe*pv
	
	// end of context initialization
	lock

}

/** Circular slender silos calculation */
class ThinWalledCircularSlenderSilosWithSteepHopper(
	diameter:Expression,
	heightOfChamber:Expression, 
	heightOfHopper:Expression, 
	thicknessOfChamberWall:Expression, 
	thicknessOfHopperWall:Expression, 
	diameterOfOutlet:Expression, 
	particulateSolid:ParticulateSolid,
	wallType:Expression
)
extends Calculation {

	import SilosSymbols._
	import CircularSilosSymbols._
	import ParticulateSolidSymbols._
	
	particulateSolid(ParticulateSolidSymbols.D) = wallType
	
	this add ThinWalledCircularSlenderSilosWithSteepHopperExpressions
	this add particulateSolid
	
	this(d1) = diameter
	this(h1) = heightOfChamber
	this(h2) = heightOfHopper
	this(t) = thicknessOfChamberWall
	this(ts) = thicknessOfHopperWall
	this(de) = diameterOfOutlet
	
	//input geometry
	def inputGeometry = NumSection(TextToTranslate("_inputGeometry",SilosSymbols.dictionary),
		Evaluate(Seq(d1,h1,h2,de,t,ts),this)
	)
	//input assertions
	
	//calculated geometry
	def calculatedGeometry = NumSection(TextToTranslate("_calculatedGeometry",SilosSymbols.dictionary),
		Evaluate(Seq(dc,A,U,AU,beta,hh,htp,ho,hc,hb,hcdc),this)
	)
	//volumes
	def volumes = NumSection(TextToTranslate("_volumes",SilosSymbols.dictionary),
		Evaluate(Seq(Vc,Vh,V,W),this)
	)
	//filling symmetrical load
	def fillingSymmetricalLoad = NumSection(TextToTranslate("_fillingSymmetricalLoad",SilosSymbols.dictionary),"[1991-4] 5.2.1.1",
        NumSection(TextToTranslate("_fillingSymmetricalLoad_1",SilosSymbols.dictionary),
		Evaluate(Seq(zo(1),pho(1),YJ(1),phf(1),phf1,phf2,phf3,phft),this)
		),
		NumSection(TextToTranslate("_fillingSymmetricalLoad_2",SilosSymbols.dictionary),
        Evaluate(Seq(zo(2),pho(2),YJ(2),phf(2),pwf,nfzSk,nfzSkt),this)
        ),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_3",SilosSymbols.dictionary),
        Evaluate(Seq(zo(3),pho(3),YJ(3),pvf),this)
        )
	)
	//filling patch load
	def fillingPatchLoad = NumSection(TextToTranslate("_fillingPatchLoad",SilosSymbols.dictionary),"[1991-4] 5.2.1.2, 5.2.1.4",
        Evaluate(Seq(ef,E,Cpf,ppf,zp,ppfzp,s,Fpf1),this)
    )
    //discharge symmetrical load
	def dischargeSymmetricalLoad = NumSection(TextToTranslate("_dischargeSymmetricalLoad",SilosSymbols.dictionary),"[1991-4] 5.2.2.1",
        Evaluate(Seq(Ch,Cw,phe,phet,pwe,nezSk,nezSkt),this)
    )
    //discharge patch load
    def dischargePatchLoad = NumSection(TextToTranslate("_dischargePatchLoad",SilosSymbols.dictionary),"[1991-4] 5.2.2.2, 5.2.2.4",
        Evaluate(Seq(Cpe,ppe,zp,ppezp,Fpe1),this)
    )
    //filling loads on silo hoppers
    def fillingHopperLoad = NumSection(TextToTranslate("_fillingHopperLoad",SilosSymbols.dictionary),"[1991-4] 6.1.2, 6.3.2",
        AssertionL("leja stromego",this,tan(beta),(1-K_l)/(2*mu_u)),    
        Evaluate(Seq(Cb,pvft,muheff,Ff,nh,pv,pnf,ptf),this)
    )
    //discharge loads on silo hoppers
    def dischargeHopperLoad = NumSection(TextToTranslate("_dischargeHopperLoad",SilosSymbols.dictionary),"[1991-4] 6.3.3", 
        Evaluate(Seq(Fe,pne,pte),this)
    )
    
}	
