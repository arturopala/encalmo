package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._
import org.encalmo.structures.eurocode.actions._

object ProfiledSteelSheetSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "profiledSteelSheet"
	
	val ID = symbol("ID").makeNonPrintable
	val hp = symbol(BasicSymbols.h|"p") unit "mm"
	val hw = symbol(BasicSymbols.h|"w") unit "mm"
	val br = symbol(BasicSymbols.b|"r") unit "mm"
	val bs = symbol(BasicSymbols.b|"s") unit "mm"
	val bo = symbol(BasicSymbols.b|"o") unit "mm"
	val bb = symbol(BasicSymbols.b|"b") unit "mm"
	val t = symbol(BasicSymbols.t) unit "mm"
	val r = symbol(BasicSymbols.r) unit "mm"
	val Iminus = symbol(I!"-") unit "cm4/m"
	val Iplus = symbol(I!"+") unit "cm4/m"
	val Wminus = symbol(W!"-") unit "cm3/m"
	val Wplus = symbol(W!"+") unit "cm3/m"
	val Ap = symbol(A|p) unit "cm2/m"
	val eminus = symbol(e!"-") unit "mm"
	val eplus = symbol(e!"+") unit "mm"
	val ep = symbol(e|p) unit "mm"
	val epd = symbol(e|("p","d")) unit "mm"
	val Gcck = symbol(G|"cc,k") unit "kN/m2"
	val Gccd = symbol(G|"cc,d") unit "kN/m2"
	val lambdaw = symbol(lambda|w) over "─"
	val sw = symbol(s|w) unit "mm"
	val Phi = symbol(BasicSymbols.Phi) unit "°"
	val tcor = symbol(BasicSymbols.t|"cor") unit "mm"
	val fbv = symbol(f|"bv") unit SI.MPa
    val MRdm = symbol(M|("Rd","-")) unit "kNm/m"
    val MRdp = symbol(M|("Rd","+")) unit "kNm/m"
	val VwRd = symbol(V|"w,Rd") unit "kN/m"
	val VbRd = symbol(V|"b,Rd") unit "kN"
	val VplRd = symbol(V|"pl,Rd") unit "kN"
	val Rw1Rd = symbol(R|"w1,Rd") unit "kN"
	val RwRd = symbol(R|"w,Rd") unit "kN/m"
	val alpha = symbol(BasicSymbols.alpha)
	val la = symbol(BasicSymbols.l|"a") unit "m"
	val fyb = symbol(BasicSymbols.f|"yb") unit SI.MPa
	val fypd = symbol(BasicSymbols.f|"yp,d") unit SI.MPa

}

class ProfiledSteelSheet(id:String, val steel:Steel, data:Context) 
extends Calculation(Option(id)) {

	import ProfiledSteelSheetSymbols._
	import ActionsSymbols._
	import SteelSymbols.{fy,fyd}
	
	this add ProfiledSteelSheetExpressions
	this add data
	this add steel
	
	ID := text(id)
	fyb := steel(fy)
	fypd := steel(fyd)
	
	override def label = this(ID)
	
    def info = NumSection(TextToTranslate("ProfiledSteelSheet",ProfiledSteelSheetSymbols.dictionary),id,
		Evaluate(t,tcor,hp,br,bs,bo,bb,r,Ap,Iminus,Iplus,eminus,eplus,ep,epd,Wminus,Wplus)
	)
	
	def web = Evaluate(Phi,sw,tcor,lambdaw)
	
	def shearForce = Evaluate(fyb,fbv,VbRd,VplRd,VwRd)
	
}

object ProfiledSteelSheetExpressions extends MapContext {

	import ProfiledSteelSheetSymbols._
	import ActionsSymbols._
	import SteelSymbols.{fyd,E,gammaM0,gammaM1,gammas}
	
	epd := hp-ep
	Wminus := Iminus/eminus
	Wplus := Iplus/eplus
	MRdm := Wminus*fyd
	MRdp := Wplus*fyd
	hw := hp-t
	sw := hp/sin(Phi)
	tcor := t - (0.08 unit SI.mm)
	lambdaw := 0.346*(sw/tcor)*sqrt(fyb/E)
	Gcck := Ap*gammas
	Gccd := Gcck*gammaG
	fbv := (0.58*fyb) or (InRangeLLE(0.83,lambdaw,1.40) thenUse (0.48*fyb)/lambdaw) or (GreaterThan(lambdaw,1.40) thenUse ((0.67*fyb)/(lambdaw^2)))
	VbRd := ((hw/sin(Phi))*tcor*fbv)/gammaM0
	VplRd := ((hw/sin(Phi))*tcor*(fyb/sqrt(3)))/gammaM0
	VwRd := (2*min(VbRd,VplRd))/bs
	Rw1Rd := alpha*(tcor^2)*sqrt(fyb*E)*(1-0.1*sqrt(r/tcor))*(0.5+sqrt((0.02*la)/tcor))*(2.4+((Phi/(Number(90,SI.deg)))^2))*(1/gammaM1)
	RwRd := (2*Rw1Rd)/bs
	
	lock()
}
