package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** Composite slab with profiled steel sheeting symbols */
object BeamOfCompositeSlabSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "beamOfCompositeSlab"
	
	val l = symbol(BasicSymbols.l) unit "m"
	val gk = symbol(BasicSymbols.g|BasicSymbols.k) unit "N/m"
	val gd = symbol(BasicSymbols.g|BasicSymbols.d) unit "N/m"
	val Qk1 = symbol(Q|"k|m") unit "N/m"
    val Qd1 = symbol(Q|"d|m") unit "N/m"
    val Qk2 = symbol(Q|"k|e") unit "N/m"
    val Qd2 = symbol(Q|"d|e") unit "N/m"
    val eta = symbol(BasicSymbols.eta)
    val Cf1 = symbol("C"|"f,1")
    val Cw1 = symbol("C"|"w,1")
    val Cf2 = symbol("C"|"f,2")
    val Cw2 = symbol("C"|"w,2")
    val Cf3 = symbol("C"|"f,3")
    val Cw3 = symbol("C"|"w,3")
    val C = symbol(BasicSymbols.C)
    val MelRd = symbol(M|"el,Rd") unit "Nm"
    val VplRd = symbol(V|"pl,Rd") unit "N"
    val NcRd = symbol(N|"c,Rd") unit "N"
    val MEdm = symbol(M|("Ed|m")) unit "Nm"
    val VEdm = symbol(V|("Ed|m")) unit "N"
    val MEde = symbol(M|("Ed|e")) unit "Nm"
    val VEde = symbol(V|("Ed|e")) unit "N"
    val ΔMEd = symbol("ΔM"|("Ed")) unit "Nm"
    val ΔVEd = symbol("ΔV"|("Ed")) unit "N"
    val sigmamplus = symbol(BasicSymbols.sigma|("m","+")) unit "Pa"
    val MEdm1 = symbol(M|("Ed,m,1")) unit "Nm"
    val Mkm = symbol(M|("k|m")) unit "Nm"
    val Mkm1 = symbol(M|("k|m,1")) unit "Nm"
    val ΔMk = symbol("ΔM"|("k")) unit "Nm"
    val Mke = symbol(M|("k|e")) unit "Nm"
    val sigmadm1 = symbol(BasicSymbols.sigma|("d|m,1")) unit "Pa"
    val sigmakm1 = symbol(BasicSymbols.sigma|("k|m,1")) unit "Pa"
    val deltam0 = symbol(BasicSymbols.delta|"m,0") unit "m"
    val deltam = symbol(BasicSymbols.delta|"m") unit "m"
    val deltam1 = symbol(BasicSymbols.delta|"m,1") unit "m"
    val ΔQk = symbol(("ΔQ")|"k") unit "N/m"
    val ΔQd = symbol(("ΔQ")|"d") unit "N/m"
    val b0 = symbol(BasicSymbols.b|"0") unit "m"
    val beff = symbol(BasicSymbols.b|"eff") unit "m"
    val bei = symbol(BasicSymbols.b|"ei") unit "m"
    val Ncf = symbol(BasicSymbols.N|"c,f") unit "N"
    val Npla = symbol(BasicSymbols.N|"pl,a") unit "N"
    val N1pla = symbol(BasicSymbols.N|("pl,a","'")) unit "N"
    val x = symbol(BasicSymbols.x) unit "m"
    
}

/** Composite slab with profiled steel sheeting context */
object BeamOfCompositeSlabExpressions extends MapContext {

	val SLAB = CompositeSlabWithProfiledSheetingSymbols

	import BeamOfCompositeSlabSymbols._
	import SteelSymbols.{E,fyd,fy,gammaM0}
	import CompositeSlabWithProfiledSheetingSymbols.{Qcfk1,Qcfk,Qcfd,Qmk,Qmd}
	import ProfiledSteelSheetSymbols.{Gcck,Gccd}
    import ActionsSymbols.{gammaG,gammaQ}
    import SectionSymbols.{m,Wy,A,Iy}
    import IBeamSectionSymbols.{ctw,ctf,AV}
    
    //obciazenia i schemat statyczny w fazie montazu
    this(gk) = 10*m
    this(gd) = gk*gammaG
    this(Qk1) = (Qcfk*SLAB.l)+(Gcck*SLAB.l)+gk+(Qmk*SLAB.l)
    this(Qd1) = (Qcfk*gammaG*SLAB.l)+(Gccd*SLAB.l)+gd+(Qmd*SLAB.l)
    //sprawdzenie statecznosci srodnika
    this(eta) = rangeChoiceLE(fy,1.2,460E6,1.0)
	//klasa przekroju
	this(Cf1)=rangeChoice4LE(ctf,1,9*eta,2,10*eta,3,14*eta,4)
	this(Cw1)=rangeChoice4LE(ctw,1,72*eta,2,83*eta,3,124*eta,4)
	this(C) = max(Cf1,Cw1)
	//nosnosci
	this(MelRd) = (Wy*fy)/gammaM0
	this(VplRd) = (AV*(fy/sqrt(3)))/gammaM0
	this(NcRd) = (A*fy)/gammaM0
	//sily wewnetrzne w fazie montazu
	this(MEdm) = (Qd1*(l^2))/8
	this(VEdm) = (Qd1*l)/2
	this(MEdm1) = ((Qd1-(Qmd*SLAB.l))*(l^2))/8
	this(Mkm) = (Qk1*(l^2))/8
	this(Mkm1) = ((Qk1-(Qmk*SLAB.l))*(l^2))/8
	//naprezenia
	this(sigmamplus) = MEdm/Wy
	this(sigmakm1) = MEdm1/Wy
	this(sigmadm1) = Mkm1/Wy
	//ugiecia
	this(deltam) = (5*Qk1*(l^4))/(384*Iy*E)
	this(deltam1) = (5*(Qk1-(Qmk*SLAB.l))*(l^4))/(384*Iy*E)
	this(deltam0) = round(deltam1,RoundingMode.Step(true,0.01))
	//obciazenia w fazie eksploatacji
	this(Qk2) = (SLAB.qk*SLAB.l)+(SLAB.Gck*SLAB.l)+(Gcck*SLAB.l)+gk+(SLAB.Gsk*SLAB.l)
    this(Qd2) = (SLAB.qd*SLAB.l)+(SLAB.Gcd*SLAB.l)+(Gccd*SLAB.l)+gd+(SLAB.Gsd*SLAB.l)
    this(ΔQk) = (SLAB.qk*SLAB.l)+(SLAB.Gsk*SLAB.l)
	this(ΔQd) = (SLAB.qd*SLAB.l)+(SLAB.Gsd*SLAB.l)
	//sily wewnetrzne w fazie eksploatacji
	this(ΔMEd) = (ΔQd*(l^2))/8
	this(ΔVEd) = (ΔQd*l)/2
	this(MEde) = (Qd2*(l^2))/8
	this(VEde) = (Qd2*l)/2
	this(b0) = 0
	this(bei) = min(l/8,SLAB.l/2)
	this(beff) = b0+2*bei
	
	
	// end of context initialization
	lock

}

class BeamOfCompositeSlab(
	val length:Expression,
	val section:Section, 
	val steel:Steel,
	val slab:CompositeSlabWithProfiledSheeting
)
extends Calculation {

	val SLAB = CompositeSlabWithProfiledSheetingSymbols

	import BeamOfCompositeSlabSymbols._
	import SteelSymbols.{E,fyd,epsi,fy}
	import CompositeSlabWithProfiledSheetingSymbols.{Qcfk1,Qcfk,Qcfd,Qmk,Qmd}
	import ProfiledSteelSheetSymbols.{Gcck,Gccd}
    import ActionsSymbols.{gammaG,gammaQ}
    import IBeamSectionSymbols.{hw,tw,ctf,ctw,AV}

	this add BeamOfCompositeSlabExpressions
	this add section
	this add steel
	this add slab
	
	this(l) = length
	
	
	def info = NumSection(TextToTranslate("BeamOfCompositeSlab",BeamOfCompositeSlabSymbols.dictionary),
		Text(section.id.get),
		Evaluate(Seq(l,SLAB.l,SLAB.h),this),
		section.info,
		steel.info
	)
	
	def LOAD1 = Evaluate(Seq(gammaG,gammaQ,gk,gd,Gcck,Gccd,Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Qk1,Qd1),this)
	
	def ULS1 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Sprawdzenie stateczności środnika wg PN-EN 1993-1-5 pkt. 5.1(2)",
			Evaluate(Seq(hw,epsi,eta),this),
			AssertionL("stateczności środnika",this,hw/tw,(72*epsi)/eta)
		),
		NumSection("Określenie klasy przekroju",
			Evaluate(Seq(ctf,ctw,Cf1,Cw1,C),this),
			AssertionE("dopuszczalności określania nośności wg nośności plastycznej",this,C,1)
		),
		NumSection("Sprawdzenie nośności belki na zginanie w fazie montażu",
			Evaluate(Seq(MelRd,MEdm),this),
			AssertionLE("nośności na zginanie",this,MEdm/MelRd,1),
			Evaluate(Seq(sigmamplus),this),
			AssertionLE("braku uplastycznienia",this,sigmamplus,fy)
		),
		NumSection("Sprawdzenie nośności belki na ścinanie w fazie montażu",
			Evaluate(Seq(AV,VplRd,VEdm),this),
			AssertionLE("nośności na ścinanie",this,VEdm/VplRd,1)
		),
		NumSection("Naprężenia pozostające w belce po fazie montażu",
			Evaluate(Seq(MEdm1,Mkm1,sigmadm1,sigmakm1),this)
		)
	)
	
	def SLS1 = NumSection(TextToTranslate("SLS","eurocode"),
		NumSection("Sprawdzenie ugięć w fazie montażu",
			Evaluate(Seq(deltam1,deltam0,deltam),this),
			AssertionLE("dopuszczalnych ugięć",this,deltam-deltam0,l/250)
		)
	)
	
	def LOAD2 = Evaluate(Seq(Qk2,Qd2,ΔQk,ΔQd),this)
	
	def ULS2 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Siły wewnętrzne w belce w fazie eksploatacji",
			Evaluate(Seq(ΔMEd,ΔVEd,MEde,VEde),this),
			AssertionLE("braku interakcji zginania i ścinania",this,VEde,0.5*VplRd)
		),
		NumSection("Szerokość współpracująca i położenie osi obojętnej",
			Evaluate(Seq(b0,bei,beff),this)
		),
		NumSection("Sprawdzenie nośności belki na zginanie ze ścinaniem poprzecznym",
			Evaluate(Seq(),this)
		)
	)
	
	def SLS2 = NumSection(TextToTranslate("SLS","eurocode")
	)
	
}	
