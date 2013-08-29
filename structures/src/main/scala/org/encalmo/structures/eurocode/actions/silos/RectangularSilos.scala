package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._
import org.encalmo.document._
import org.encalmo.calculation._

/** Rectangular silos symbols */
trait RectangularSiloSymbols extends SiloSymbols {

	val rectangularSiloDict = "rectangularSilo"
	
	//input geometry
	val b1 = symbol(BasicSymbols.b|1) unit "m" dict rectangularSiloDict
    val b2 = symbol(BasicSymbols.b|2) unit "m" dict rectangularSiloDict
    val b3 = symbol(BasicSymbols.b|3) unit "m" dict rectangularSiloDict
    val b4 = symbol(BasicSymbols.b|4) unit "m" dict rectangularSiloDict
    val h1 = symbol(BasicSymbols.h|1) unit "m" dict rectangularSiloDict
    val h2 = symbol(BasicSymbols.h|2) unit "m" dict rectangularSiloDict
    val h3 = symbol(BasicSymbols.h|3) unit "m" dict rectangularSiloDict
    val be = symbol(BasicSymbols.b|BasicSymbols.e) unit "m" dict rectangularSiloDict
}

/** Rectangular slender silos calculation */
class RectangularSlenderSiloWithFlatBottom(
    name: String,
	vb1:Expression,
	vb2:Expression,
	vh1:Expression, 
	vh2:Expression, 
	vt:Expression, 
	vts:Expression, 
	vbe:Expression, 
	val particulateSolid:ParticulateSolid,
	vD:Expression
)
extends Calculation(name) with RectangularSiloSymbols {

    import particulateSolid._

    particulateSolid(D) = vD

	this add particulateSolid
	
	b1 := vb1
	b2 := vb2
	h1 := vh1
	h2 := vh2
	t := vt
	th := vts
	be := vbe

    //calculated geometry
    h3 := h1-th
    b3 := b1-2*t
    b4 := b2-2*t
    dc := min(b3,b4)
    A := (b3)*(b4)
    U := 2*b3+2*b4
    hc := h3
    //volumes
    Vc := b3*b4*hc

    //calculated geometry
    AU := A/U
    beta := 90
    hh := 0
    htp := dc/(2*tan(fir))
    ho := (dc*tan(fir))/4
    he := 0
    hb := hc
    hcdc := hc/dc

    //volumes
    Vh := 0
    V := Vc+Vh
    W := V*gammau/GRAV

    //filling symmetrical load
    //  (1) Maximum normal pressure on vertical wall
    zo(1) := 1/(K_u*mu_l)*AU
    YJ(1) := 1-(EUL^(-z/zo(1)))
    pho(1) := (gammau/mu_l)*AU
    phf(1) := fx(pho(1)*YJ(1),z)
    phf1 := EvalAt(phf(1), z -> hc/4)
    phf2 := EvalAt(phf(1), z -> hc/2)
    phf3 := EvalAt(phf(1), z -> 0.75*hc)
    phft := EvalAt(phf(1), z -> hc)
    //  (2) Maximum frictional traction on vertical wall
    zo(2) := 1/(K_u*mu_u)*AU
    YJ(2) := 1-(EUL^(-z/zo(2)))
    pho(2) := (gammau/mu_u)*AU
    phf(2) := fx(pho(2)*YJ(2),z)
    pwf := mu_u * phf(2)
    nfzSk := mu_u*pho(2)*(z-zo(2)*YJ(2))
    nfzSkt := EvalAt(nfzSk, z -> hc)
    //  (3) Maximum vertical load on hopper or silo bottom
    zo(3) := 1/(K_l*mu_l)*AU
    YJ(3) := 1-(EUL^(-z/zo(3)))
    pho(3) := (gammau/mu_l)*AU
    pvf := (pho(3)/K_l)*YJ(3)
    pvft := EvalAt(pvf, z -> hc)

    //filling patch load
    ef := dc/4
    Ef := 2*ef/dc
    Cpf := 0.21*Cop*(1+2*(Ef^2))*(1-(EUL^(-1.5*(hcdc-1))))
    ppf := Cpf*phf(1)
    ppfnc := 0.36*ppf
    ppfnc1 := EvalAt(ppfnc, z -> hc/2)
    s := (PI*dc)/16
    Fpf1 := ppfnc1*s*s

    //discharge symmetrical load
    CS := hcdc-1
    Ch := 1+0.15*CS
    Cw := 1+0.1*CS
    phe := Ch*phf(1)
    pwe := Cw*pwf
    phet := EvalAt(phe, z -> hc)
    nezSk := Cw*mu_u*pho(2)*(z-zo(2)*YJ(2))
    nezSkt := EvalAt(nezSk, z -> hc)

    //discharge patch load
    Cpe := 0.42*Cop*(1+2*(Ef^2))*(1-(EUL^(-1.5*(hcdc-1))))
    ppe := Cpe*phe
    ppenc := 0.36*ppe
    ppenc1 := EvalAt(ppenc, z -> hc/2)
    Fpe1 := ppenc1*s*s
	
	//input geometry
	def inputGeometry = NumSection(TextToTranslate("_inputGeometry",dictionary),
		Evaluate(b1,b2,h1,h2,t,th)
	)
	//input assertions
	
	//calculated geometry
	def calculatedGeometry = NumSection(TextToTranslate("_calculatedGeometry",dictionary),
		Evaluate(h3,b3,b4,dc,A,U,AU,beta,hh,htp,ho,hc,hb,hcdc)
	)
	//volumes
	def volumes = NumSection(TextToTranslate("_volumes",dictionary),
		Evaluate(Vc,Vh,V,W)
	)
	//filling symmetrical load
	def fillingSymmetricalLoad = NumSection(TextToTranslate("_fillingSymmetricalLoad",dictionary),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_1",dictionary),
		Evaluate(zo(1),pho(1),YJ(1),phf(1),phf1,phf2,phf3,phft)
		),
		NumSection(TextToTranslate("_fillingSymmetricalLoad_2",dictionary),
        Evaluate(zo(2),pho(2),YJ(2),phf(2),pwf,nfzSk,nfzSkt)
        ),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_3",dictionary),
        Evaluate(zo(3),pho(3),YJ(3),pvf,pvft)
        )
	)
	//filling patch load
	def fillingPatchLoad = NumSection(TextToTranslate("_fillingPatchLoad",dictionary),
        Evaluate(ef,Ef,Cpf,ppf,ppfnc,ppfnc1,s,Fpf1)
    )
    //discharge symmetrical load
	def dischargeSymmetricalLoad = NumSection(TextToTranslate("_dischargeSymmetricalLoad",dictionary),
        Evaluate(CS,Ch,Cw,phe,phet,pwe,nezSk,nezSkt)
    )
    //discharge patch load
    def dischargePatchLoad = NumSection(TextToTranslate("_dischargePatchLoad",dictionary),
        Evaluate(Cpe,ppe,ppenc,ppenc1,Fpe1)
    )
    
    //loads on silo hoppers
    def loadsOnSiloBottom = NumSection(TextToTranslate("_loadsOnSiloBottom",dictionary),
        Evaluate(pvft)
    )
    
}	
