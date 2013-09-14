package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._
import org.encalmo.calculation.{MapContext,Catalog}
import org.encalmo.document._

// The European Standard EN 1991-4:2006 
// Eurocode 1 — Actions on structures — Part 4: Silos and tanks

/** ParticulateSolid symbols */
trait ParticulateSolidSymbols extends SymbolConfigurator {
	// particulate solid properties
	
	/** Bulk unit weight (lower) */
	val gammal = symbol(BasicSymbols.gamma|"l") unit "kN/m3"
	/** Bulk unit weight (upper) */
    val gammau = symbol(BasicSymbols.gamma|"u") unit "kN/m3"
    /** angle of repose of a particulate solid (conical pile) */
    val fir = symbol(BasicSymbols.phi|"r") unit "°"
    /** Angle of internal friction (mean) */
    val fiim = symbol(BasicSymbols.phi|"im") unit "°"
    /** Angle of internal friction (factor) */
    val afi = symbol(BasicSymbols.a|BasicSymbols.phi)
    /** Lateral pressure ratio (mean) */
    val Km = symbol(BasicSymbols.K|"m") 
    /** Lateral pressure ratio (factor) */
    val aK = symbol(BasicSymbols.a|BasicSymbols.K) 
    /** Coefficient of wall friction (D1) */
    val mum1 = symbol(BasicSymbols.mu|("m","D1")) 
    /** Coefficient of wall friction (D2) */
    val mum2 = symbol(BasicSymbols.mu|("m","D2")) 
    /** Coefficient of wall friction (D3) */
    val mum3 = symbol(BasicSymbols.mu|("m","D3")) 
    /** Coefficient of wall friction */
    val mum = symbol(BasicSymbols.mu|"m") 
    /** Coefficient of wall friction (factor) */
    val amu = symbol(BasicSymbols.a|BasicSymbols.mu) 
    /** Patch load solid reference factor */
    val Cop = symbol(BasicSymbols.C|"op") 
    /** Wall type (1,2,3) */
    val D = symbol(BasicSymbols.D)
    
    // characteristic values
    
    /** Lateral pressure ratio (upper characteristic value) */
    val K_u = symbol(BasicSymbols.K!"u") 
    /** Lateral pressure ratio (lower characteristic value) */
    val K_l = symbol(BasicSymbols.K!"l") 
    /** Coefficient of wall friction (upper characteristic value) */
    val mu_u = symbol(BasicSymbols.mu!"u") 
    /** Coefficient of wall friction (lower characteristic value) */
    val mu_l = symbol(BasicSymbols.mu!"l") 
    /** Angle of internal friction (upper characteristic value) */
    val fi_u = symbol(BasicSymbols.phi|("i","u")) unit "°" acc 0.1
    /** Angle of internal friction (lower characteristic value) */
    val fi_l = symbol(BasicSymbols.phi|("i","l")) unit "°" acc 0.1
   
}

/** 
 * ParticulateSolid context 
 * MUST be provided: D
 */
class ParticulateSolid(
    name:String,
	v_gammal:Expression,
	v_gammau:Expression,
	v_fir:Expression, 
	v_fiim:Expression,
	v_afi:Expression,
	v_Km:Expression, 
	v_aK:Expression, 
	v_mum1:Expression, 
	v_mum2:Expression,
	v_mum3:Expression,
	v_amu:Expression, 
	v_Cop:Expression
)
extends MapContext("particulateSolid") with ParticulateSolidSymbols {
	
	this(gammal) = v_gammal
	this(gammau) = v_gammau
	this(fir) = v_fir
	this(fiim) = v_fiim
	this(afi) = v_afi
	this(Km) = v_Km
	this(aK) = v_aK
	this(mum1) = v_mum1
	this(mum2) = v_mum2
	this(mum3) = v_mum3
	this(amu) = v_amu
	this(Cop) = v_Cop

    // coefficient of wall friction selection
    this(mum) = mapChoice(D,Number(1) -> mum1, Number(2) -> mum2, Number(3) -> mum3)

    // characteristic values expressions
    this(K_u) = Km*aK
    this(K_l) = Km/aK
    this(mu_u) = mum*amu
    this(mu_l) = mum/amu
    this(fi_u) = fiim*afi
    this(fi_l) = fiim/afi
	
	def properties = NumSection(Text("_properties",dictionary),"(",Text(name,dictionary),")",
		Evaluate(gammal,gammau,fir,fiim,afi,Km,aK,mum1,mum2,mum3,amu,Cop)
	)
	
	def characteristicValues = NumSection(Text("_characteristicValues",dictionary),
	    Evaluate(D,mum,K_u,K_l,mu_u,mu_l,fi_u,fi_l)
	)
}	

/** Particulate solid library provided by EN 1991-4:2006 (E) */
object ParticulateSolid extends Catalog[ParticulateSolid]("Particulate Solid") {
	
	override val map = Map[String,()=>ParticulateSolid](
		"Default" -> Default _,
		"Aggregate" -> Aggregate _,
		"Alumina" -> Alumina _,
		"Animal feed mix" -> AnimalFeedMix _,
		"Animal feed pellets" -> AnimalFeedPellets _,
		"Barley" -> Barley _,
		"Cement" -> Cement _,
		"Cement clinker" -> CementClinker _,
		"Coal" -> Coal _,
		"Coal powdered" -> CoalPowdered _,
		"Coke" -> Coke _,
		"Flyash" -> Flyash _,
		"Flour" -> Flour _,
		"Iron ore pellets" -> IronOrePellets _,
		"Lime hydrated" -> LimeHydrated _,
		"Limestone powder" -> LimestonePowder _,
		"Maize" -> Maize _,
		"Phosphate" -> Phosphate _,
		"Potatoes" -> Potatoes _,
		"Sand" -> Sand _,
		"Slag clinkers" -> SlagClinkers _,
		"Coal" -> SoyaBeans _,
		"Soya beans" -> Sugar _,
		"Coal" -> SugarbeetPellets _,
		"Wheat" -> Wheat _
	)
	
	def Default:ParticulateSolid = new ParticulateSolid("Default",6.0,22.0,40,35,1.3,0.50,1.5,0.32,0.39,0.50,1.40,1.0)
	def Aggregate:ParticulateSolid = new ParticulateSolid("Aggregate",17.0,18.0,36,31,1.16,0.52,1.15,0.39,0.49,0.59,1.12,0.4)
	def Alumina:ParticulateSolid = new ParticulateSolid("Alumina",10.0,12.0,36,30,1.22,0.54,1.20,0.41,0.46,0.51,1.07,0.5)
	def AnimalFeedMix:ParticulateSolid = new ParticulateSolid("AnimalFeedMix",5.0,6.0,39,36,1.08,0.45,1.10,0.22,0.30,0.43,1.28,1.0)
	def AnimalFeedPellets:ParticulateSolid = new ParticulateSolid("AnimalFeedPellets",6.5,8.0,37,35,1.06,0.47,1.07,0.23,0.28,0.37,1.20,0.7)
	def Barley:ParticulateSolid = new ParticulateSolid("Barley",7.0,8.0,31,28,1.14,0.59,1.11,0.24,0.33,0.48,1.16,0.5)
	def Cement:ParticulateSolid = new ParticulateSolid("Cement",13.0,16.0,36,30,1.22,0.54,1.20,0.41,0.46,0.51,1.07,0.5)
	def CementClinker:ParticulateSolid = new ParticulateSolid("CementClinker",15.0,18.0,47,40,1.20,0.38,1.31,0.46,0.56,0.62,1.07,0.7)
	def Coal:ParticulateSolid = new ParticulateSolid("Coal",7.0,10.0,36,31,1.16,0.52,1.15,0.44,0.49,0.59,1.12,0.6)
	def CoalPowdered:ParticulateSolid = new ParticulateSolid("CoalPowdered",6.0,8.0,34,27,1.26,0.58,1.20,0.41,0.51,0.56,1.07,0.5)
	def Coke:ParticulateSolid = new ParticulateSolid("Coke",6.5,8.0,36,31,1.16,0.52,1.15,0.49,0.54,0.59,1.12,0.6)
	def Flyash:ParticulateSolid = new ParticulateSolid("Flyash",8.0,15.0,41,35,1.16,0.46,1.20,0.51,0.62,0.72,1.07,0.5)
	def Flour:ParticulateSolid = new ParticulateSolid("Flour",6.5,7.0,45,42,1.06,0.36,1.11,0.24,0.33,0.48,1.16,0.6)
	def IronOrePellets:ParticulateSolid = new ParticulateSolid("IronOrePellets",19.0,22.0,36,31,1.16,0.52,1.15,0.49,0.54,0.59,1.12,0.5)
	def LimeHydrated:ParticulateSolid = new ParticulateSolid("LimeHydrated",6.0,8.0,34,27,1.26,0.58,1.20,0.36,0.41,0.51,1.07,0.6)
	def LimestonePowder:ParticulateSolid = new ParticulateSolid("LimestonePowder",11.0,13.0,36,30,1.22,0.54,1.20,0.41,0.51,0.56,1.07,0.5)
	def Maize:ParticulateSolid = new ParticulateSolid("Maize",7.0,8.0,35,31,1.14,0.53,1.14,0.22,0.36,0.53,1.24,0.9)
	def Phosphate:ParticulateSolid = new ParticulateSolid("Phosphate",16.0,22.0,34,29,1.18,0.56,1.15,0.39,0.49,0.54,1.12,0.5)
	def Potatoes:ParticulateSolid = new ParticulateSolid("Potatoes",6.0,8.0,34,30,1.12,0.54,1.11,0.33,0.38,0.48,1.16,0.5)
	def Sand:ParticulateSolid = new ParticulateSolid("Sand",14.0,16.0,39,36,1.09,0.45,1.11,0.38,0.48,0.57,1.16,0.4)
	def SlagClinkers:ParticulateSolid = new ParticulateSolid("SlagClinkers",10.5,12.0,39,36,1.09,0.45,1.11,0.48,0.57,0.67,1.16,0.6)
	def SoyaBeans:ParticulateSolid = new ParticulateSolid("SoyaBeans",7.0,8.0,29,25,1.16,0.63,1.11,0.24,0.38,0.48,1.16,0.5)
	def Sugar:ParticulateSolid = new ParticulateSolid("Sugar",8.0,9.5,38,32,1.19,0.50,1.20,0.46,0.51,0.56,1.07,0.4)
	def SugarbeetPellets:ParticulateSolid = new ParticulateSolid("SugarbeetPellets",6.5,7.0,36,31,1.16,0.52,1.15,0.35,0.44,0.54,1.12,0.5)
	def Wheat:ParticulateSolid = new ParticulateSolid("Wheat",7.5,9.0,34,30,1.12,0.54,1.11,0.24,0.38,0.57,1.16,0.5)
}
