package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

object ProfiledSteelSheetSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "profiledSteelSheet"
	
	val ID = symbol("ID").makeNonPrintable
	val hp = symbol(BasicSymbols.h|"p") unit "m"
	val hw = symbol(BasicSymbols.h|"w") unit "m"
	val br = symbol(BasicSymbols.b|"r") unit "m"
	val bs = symbol(BasicSymbols.b|"s") unit "m"
	val bo = symbol(BasicSymbols.b|"o") unit "m"
	val bb = symbol(BasicSymbols.b|"b") unit "m"
	val t = symbol(BasicSymbols.t) unit "m"
	val r = symbol(BasicSymbols.r) unit "m"
	val Iminus = symbol(I!"-") unit "m4/m"
	val Iplus = symbol(I!"+") unit "m4/m"
	val Wminus = symbol(W!"-") unit "m3/m"
	val Wplus = symbol(W!"+") unit "m3/m"
	val Ap = symbol(A|p) unit "m2/m"
	val eminus = symbol(e!"-") unit "m"
	val eplus = symbol(e!"+") unit "m"
	val ep = symbol(e|p) unit "m"
	val epd = symbol(e|("p","d")) unit "m"
	val Gcck = symbol(G|"cc,k") unit "N/m2"
	val Gccd = symbol(G|"cc,d") unit "N/m2"
	val lambdaw = symbol(lambda|w) over "─"
	val sw = symbol(s|w) unit "m"
	val Phi = symbol(BasicSymbols.Phi) unit "°"
	val tcor = symbol(BasicSymbols.t|"cor") unit "m"
	val fbv = symbol(f|"bv") unit "Pa"
    val MRdm = symbol(M|("Rd","-")) unit "Nm/m"
    val MRdp = symbol(M|("Rd","+")) unit "Nm/m"
	val VwRd = symbol(V|"w,Rd") unit "N/m"
	val VbRd = symbol(V|"b,Rd") unit "N"
	val VplRd = symbol(V|"pl,Rd") unit "N"
	val Rw1Rd = symbol(R|"w1,Rd") unit "N"
	val RwRd = symbol(R|"w,Rd") unit "N/m"
	val alpha = symbol(BasicSymbols.alpha)
	val la = symbol(BasicSymbols.l|"a") unit "m"

}

class ProfiledSteelSheet(id:String, val steel:Steel, data:Context) 
extends Calculation(Option(id)) {

	import ProfiledSteelSheetSymbols._
	import ActionsSymbols._
	import SteelSymbols._
	
	this add ProfiledSteelSheetExpressions
	this add data
	this add steel
	
	this(ID) = text(id)
	
    def info = NumSection(TextToTranslate("ProfiledSteelSheet",ProfiledSteelSheetSymbols.dictionary),id,
		Evaluate(Seq(t,tcor,hp,br,bs,bo,bb,r,Ap,Iminus,Iplus,eminus,eplus,ep,epd,Wminus,Wplus),this)
	)
	
	def web = Evaluate(Seq(Phi,sw,tcor,lambdaw),this)
	
	def shearForce = Evaluate(Seq(fyb,fbv,VbRd,VplRd,VwRd),this)
	
}

object ProfiledSteelSheetExpressions extends MapContext {

	import ProfiledSteelSheetSymbols._
	import ActionsSymbols._
	import SteelSymbols._
	
	this(epd) = hp-ep
	this(Wminus) = Iminus/eminus
	this(Wplus) = Iplus/eplus
	this(MRdm) = Wminus*fypd
	this(MRdp) = Wplus*fypd
	this(hw) = hp-t
	this(sw) = hp/sin(Phi)
	this(tcor) = t - 8E-5
	this(lambdaw) = 0.346*(sw/tcor)*sqrt(fyb/E)
	this(Gcck) = Ap*gammas
	this(Gccd) = Gcck*gammaG
	this(fbv) = (0.58*fyb) or (InRangeLLE(0.83,lambdaw,1.40) then (0.48*fyb)/lambdaw) or (GreaterThan(lambdaw,1.40) then ((0.67*fyb)/(lambdaw^2)))
	this(VbRd) = ((hw/sin(Phi))*tcor*fbv)/gammaM0
	this(VplRd) = ((hw/sin(Phi))*tcor*(fyb/sqrt(3)))/gammaM0
	this(VwRd) = (2*min(VbRd,VplRd))/bs
	this(Rw1Rd) = alpha*(tcor^2)*sqrt(fyb*E)*(1-0.1*sqrt(r/tcor))*(0.5+sqrt((0.02*la)/tcor))*(2.4+((Phi/90)^2))*(1/gammaM1)
	this(RwRd) = (2*Rw1Rd)/bs
	
	lock
}

object FLORSTROP {

	import ProfiledSteelSheetSymbols._

	def T59_Z_075 = new ProfiledSteelSheet("FLORSTROP T59 Z 0.75", Steel.S280GD, data_T59_Z_075)
	private lazy val data_T59_Z_075 = new MapContext {
		this(hp) = 59E-3
		this(br) = 44.6E-3
		this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
		this(t) = 0.75E-3
		this(r) = 5E-3
		this(Iminus) = 34.87E-8
        this(Iplus) = 54.42E-8
        this(Ap) = 12.53E-4
        this(eminus) = 3.06E-2
        this(eplus) = 4.03E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
	}
	
	def T59_Z_088 = new ProfiledSteelSheet("FLORSTROP T59 Z 0.88", Steel.S280GD, data_T59_Z_088)
	private lazy val data_T59_Z_088 = new MapContext {
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 0.88E-3
		this(r) = 5E-3
        this(Iminus) = 42.65E-8
        this(Iplus) = 63.66E-8
        this(Ap) = 15.04E-4
        this(eminus) = 3.16E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
    }
	
	def T59_Z_100 = new ProfiledSteelSheet("FLORSTROP T59 Z 1.0", Steel.S280GD, data_T59_Z_100)
	private lazy val data_T59_Z_100 = new MapContext {
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 1.0E-3
		this(r) = 5E-3
        this(Iminus) = 49.61E-8
        this(Iplus) = 73.23E-8
        this(Ap) = 17.35E-4
        this(eminus) = 3.27E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
    }
	
    def T59_Z_125 = new ProfiledSteelSheet("FLORSTROP T59 Z 1.25", Steel.S280GD, data_T59_Z_125)
	private lazy val data_T59_Z_125 = new MapContext {
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 1.25E-3
		this(r) = 5E-3
        this(Iminus) = 65.84E-8
        this(Iplus) = 96.48E-8
        this(Ap) = 22.17E-4
        this(eminus) = 3.46E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
    }

}