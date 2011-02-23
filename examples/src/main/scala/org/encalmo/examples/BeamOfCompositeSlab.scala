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
    val Gk = symbol(G|"k") unit "N/m"
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
    val MEdm1 = symbol(BasicSymbols.M|("Ed|m,1")) unit "Nm"
    val Mkm = symbol(BasicSymbols.M|("k|m")) unit "Nm"
    val Mkm1 = symbol(BasicSymbols.M|("k|m,1")) unit "Nm"
    val ΔMk = symbol("ΔM"|("k")) unit "Nm"
    val Mke = symbol(BasicSymbols.M|("k|e")) unit "Nm"
    val sigmadm1 = symbol(BasicSymbols.sigma|("d|m,1")) unit "Pa"
    val sigmakm1 = symbol(BasicSymbols.sigma|("k|m,1")) unit "Pa"
    val deltam0 = symbol(BasicSymbols.delta|"m,0") unit "m"
    val deltam = symbol(BasicSymbols.delta|"m") unit "m"
    val deltam1 = symbol(BasicSymbols.delta|"m,1") unit "m"
    val deltamax = symbol(BasicSymbols.delta|"max") unit "m"
    val deltae = symbol(BasicSymbols.delta|"e") unit "m"
    val ΔQk = symbol(("ΔQ")|"k") unit "N/m"
    val ΔQd = symbol(("ΔQ")|"d") unit "N/m"
    val b0 = symbol(BasicSymbols.b|"0") unit "m"
    val beff = symbol(BasicSymbols.b|"eff") unit "m"
    val bei = symbol(BasicSymbols.b|"ei") unit "m"
    val Ncf = symbol(BasicSymbols.N|"c,f") unit "N"
    val Npla = symbol(BasicSymbols.N|"pl,a") unit "N"
    val N1pla = symbol(BasicSymbols.N|("pl,a","'")) unit "N"
    val x = symbol(BasicSymbols.x) unit "m"
    val MplRd = symbol(BasicSymbols.M|"pl,Rd") unit "Nm"
    val bn = symbol(BasicSymbols.b|"E") unit "m"
    val Sy = symbol(BasicSymbols.S|"y") unit "m3"
    val z0 = symbol(BasicSymbols.z|"0") unit "m"
    val I1 = symbol(BasicSymbols.I|"1") unit "m4"
    val Wel = symbol(BasicSymbols.W|"el") unit "m3"
    val sigmake = symbol(BasicSymbols.sigma|"k|e") unit "Pa"
    val sigmamax = symbol(BasicSymbols.sigma|"max") unit "Pa"
	val PRd = symbol(BasicSymbols.P|"Rd") unit "N"
	val alpha = symbol(BasicSymbols.alpha)
	val kt = symbol(BasicSymbols.k|"t")
	val ktmax = symbol(BasicSymbols.k|"t,max")
	val nr = symbol(BasicSymbols.n|"r") unit "szt."
	val VEdr = symbol(BasicSymbols.V|"Ed,r") unit "N"
	val Ls = symbol(BasicSymbols.L|"s") unit "m"
	val nf = symbol(BasicSymbols.n|"f")
	val nfprim = symbol(BasicSymbols.n|("f","'")) unit "szt."
	val s = symbol(BasicSymbols.s) unit "m"
	val smax = symbol(BasicSymbols.s|"max") unit "m"
	val smin = symbol(BasicSymbols.s|"min") unit "m"
	val MaRd = symbol(BasicSymbols.M|"a,Rd") unit "Nm"
	val VEdc = symbol(BasicSymbols.V|"Ed,c") unit "N"
	val kphi = symbol(BasicSymbols.k|BasicSymbols.phi)
	val PpbRd = symbol(BasicSymbols.P|"pb,Rd") unit "N"
	val VRdsr = symbol(BasicSymbols.V|"Rd,s,r") unit "N"
	val thetat = symbol(BasicSymbols.theta|"t") unit "°"
	val VRdsc = symbol(BasicSymbols.V|"Rd,s,c") unit "N"
	val bn2 = symbol(BasicSymbols.b|"E,2") unit "m"
    val Sy2 = symbol(BasicSymbols.S|"y,2") unit "m3"
    val z2 = symbol(BasicSymbols.z|"2") unit "m"
    val I2 = symbol(BasicSymbols.I|"2") unit "m4"
	val yw = symbol(BasicSymbols.y|"w") unit "m"
	val f = symbol(BasicSymbols.f) unit "Hz"
	val mS = symbol(BasicSymbols.m|"S") unit "kg/m2"
}

/** Composite slab with profiled steel sheeting context */
object BeamOfCompositeSlabExpressions extends MapContext {

	val SHEET = ProfiledSteelSheetSymbols
	val SLAB = CompositeSlabWithProfiledSheetingSymbols
	val STUD = HeadedStudSymbols

	import BeamOfCompositeSlabSymbols._
	import ConcreteSymbols.{fck,fcd,Ecm,v}
	import SteelSymbols.{E,fyd,fy,fu,gammaM0,gammaV}
	import CompositeSlabWithProfiledSheetingSymbols.{Qcfk1,Qcfk,Qcfd,Qmk,Qmd,hc,Eceff,nE,dmesh,sd,fyrd}
	import ProfiledSteelSheetSymbols.{Gcck,Gccd,hp,t,bo,bs,fypd}
    import ActionsSymbols.{gammaG,gammaQ}
    import SectionSymbols.{m,Wy,A,Iy,b,h,Wypl,AVz}
    import IBeamSectionSymbols.{ctw,ctf,bf,tf}
    
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
	this(VplRd) = (AVz*(fy/sqrt(3)))/gammaM0
	this(NcRd) = (A*fy)/gammaM0
	//sily wewnetrzne w fazie montazu
	this(MEdm) = (Qd1*(l^2))/8
	this(VEdm) = (Qd1*l)/2
	this(MEdm1) = ((Qd1-(Qmd*SLAB.l))*(l^2))/8
	this(Mkm) = (Qk1*(l^2))/8
	this(Mkm1) = ((Qk1-(Qmk*SLAB.l))*(l^2))/8
	this(ΔMk) = (ΔQk*(l^2))/8
	//naprezenia
	this(sigmamplus) = MEdm/Wy
	this(sigmadm1) = MEdm1/Wy
	this(sigmakm1) = Mkm1/Wy
	//ugiecia
	this(deltam) = (5*Qk1*(l^4))/(384*Iy*E)
	this(deltam1) = (5*(Qk1-(Qmk*SLAB.l))*(l^4))/(384*Iy*E)
	this(deltam0) = round(deltam1,RoundingMode.Step(true,0.01))
	//obciazenia w fazie eksploatacji
	this(Qk2) = (SLAB.qk*SLAB.l)+(SLAB.Gck*SLAB.l)+(Gcck*SLAB.l)+gk+(SLAB.Gsk*SLAB.l)
    this(Qd2) = (SLAB.qd*SLAB.l)+(SLAB.Gcd*SLAB.l)+(Gccd*SLAB.l)+gd+(SLAB.Gsd*SLAB.l)
    this(ΔQk) = (SLAB.qk*SLAB.l)+(SLAB.Gsk*SLAB.l)
	this(ΔQd) = (SLAB.qd*SLAB.l)+(SLAB.Gsd*SLAB.l)
	this(Gk) = (SLAB.Gck*SLAB.l)+(Gcck*SLAB.l)+gk+(SLAB.Gsk*SLAB.l)
	//sily wewnetrzne w fazie eksploatacji
	this(ΔMEd) = (ΔQd*(l^2))/8
	this(ΔVEd) = (ΔQd*l)/2
	this(MEde) = (Qd2*(l^2))/8
	this(VEde) = (Qd2*l)/2
	//szerokosc wspolpracujaca
	this(b0) = 0
	this(bei) = min(l/8,SLAB.l/2)
	this(beff) = b0+2*bei
	//rownowaga sil w przekroju
	this(Ncf) = 0.85*fcd*beff*hc
	this(Npla) = A*fy
	this(N1pla) = (A-2*b*tf)*fy
	this(x) = rangeChoiceLE(Ncf,SLAB.h+(Npla-Ncf)/(2*b*fy),Npla,Npla/(beff*fcd))
	this(MplRd) = rangeChoiceLE(x,Npla*(h/2+(SLAB.h-(x/2))),SLAB.h,Npla*(h/2)+Ncf*(hp+(hc/2))-2*(x-SLAB.h)*b*fy*((x-SLAB.h)/2))
	this(bn) = beff/nE
	this(Sy) = bn*hc*(h/2+hp+hc/2)
	this(z0) = Sy/(A+bn*hc)
	this(I1) = Iy+A*(z0^2)+(bn*(hc^3))/12+bn*hc*((hc/2+hp+h/2-z0)^2)
	this(Wel) = I1/(z0+h/2)
	//naprezenia
	this(sigmake) = ΔMk/Wel
	this(sigmamax) = sigmakm1+sigmake
	//nosnosc sworznia
	this(s) = bs
	this(alpha) = rangeChoiceLELE(STUD.hsc/STUD.d,0,3,0.2*((STUD.hsc/STUD.d)+1),4,1)
	this(PRd) = min((0.8*fu*(PI*square(STUD.d))/4)/gammaV,(0.29*alpha*square(STUD.d)*sqrt(fck*Ecm))/gammaV)
	this(ktmax) = rangeChoiceLE(t,0.85,1,1)
	this(kt) = min(ktmax, (0.7*bo)/(sqrt(nr)*hp)*(STUD.hsc/hp-1))
	this(VEdr) = Ncf
	this(Ls) = 0.5*l
	this(nf) = VEdr/(kt*PRd)
	this(nfprim) = floor(Ls/s)
	this(smin) = min(5*STUD.d)
	this(smax) = min(6*SLAB.h,0.8)
	this(MaRd) = (Wypl*fy)/gammaM0
	//sciananie podluzne w plycie zespolonej
	this(VEdc) = 0.5*(kt*PRd)/s
	this(kphi) = min(1+max(b/2,1.5*1.1*STUD.d)/(1.1*STUD.d),6)
	this(PpbRd) = kphi*1.1*STUD.d*SHEET.tcor*fypd
	this(thetat) = 45
	this(VRdsr) = cot(45)*((((PI*square(dmesh/1000))/4*fyrd)/sd)+min(SHEET.Ap*fypd,PpbRd/s))
	this(VRdsc) = v*fcd*sin(thetat)*cos(thetat)*hc
	//ugiecia w fazie eksploatacji
	this(deltae) = (5*ΔQk*(l^4))/(384*I1*E)
	this(deltamax) = deltam1-deltam0+deltae
	//drgania
	this(bn2) = beff*Ecm/E
	this(Sy2) = bn2*hc*(h/2+hp+hc/2)
	this(z2) = Sy2/(A+bn2*hc)
	this(I2) = Iy+A*(z2^2)+(bn2*(hc^3))/12+bn2*hc*((hc/2+hp+h/2-z2)^2)
	this(yw) = (5*Gk*(l^4))/(384*I2*E)
	this(f) = 18/sqrt(yw*1000)
	//unit mass
	this(mS) = Gk/SLAB.l
	
	// end of context initialization
	lock

}

class BeamOfCompositeSlab(
	val length:Expression,
	val section:Section, 
	val steel:Steel,
	val slab:CompositeSlabWithProfiledSheeting,
	val stud:HeadedStud
)
extends Calculation {

	val SLAB = CompositeSlabWithProfiledSheetingSymbols
	val STUD = HeadedStudSymbols

	import BeamOfCompositeSlabSymbols._
	import ConcreteSymbols.{v}
	import SteelSymbols.{E,fyd,epsi,fy}
	import CompositeSlabWithProfiledSheetingSymbols.{Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Eceff,nE}
	import ProfiledSteelSheetSymbols.{Gcck,Gccd,hp,bo,bs}
    import ActionsSymbols.{gammaG,gammaQ}
    import SectionSymbols.{AVz}
    import IBeamSectionSymbols.{hw,tw,ctf,ctw}

	this add BeamOfCompositeSlabExpressions
	this add section
	this add steel
	this add slab
	this add stud
	
	this(l) = length
	this(nr) = 1
	
	def info = NumSection(TextToTranslate("BeamOfCompositeSlab",BeamOfCompositeSlabSymbols.dictionary),
		Text(section.id.get),
		Evaluate(Seq(l,SLAB.l,SLAB.h),this),
		section.info,
		steel.info,
		stud.info
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
			Evaluate(Seq(AVz,VplRd,VEdm),this),
			AssertionLE("nośności na ścinanie",this,VEdm/VplRd,1)
		),
		NumSection("Naprężenia pozostające w belce po fazie montażu",
			Evaluate(Seq(MEdm1,Mkm1,Mkm,sigmadm1,sigmakm1),this)
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
		NumSection("Szerokość współpracująca i położenie osi obojętnej wg PN-EN 1994-1-1 pkt. 5.4.1.2(5)",
			Evaluate(Seq(b0,bei,beff,Ncf,Npla,N1pla,x),this)
		),
		NumSection("Sprawdzenie nośności belki na zginanie bez uwzględniania zwichrzenia zgodnie z PN-EN 1994-1-1 pkt. 6.4.1(1)",
			Evaluate(Seq(MplRd),this),
			AssertionLE("nośności na zginanie",this,MEde/MplRd,1)
		),
		NumSection("Sprawdzenie naprężeń we włóknach skrajnych belki",
			Evaluate(Seq(Eceff,nE,bn,Sy,z0,I1,Wel,ΔMk,sigmake,sigmamax),this),
			AssertionLE("naprężeń dopuszczalnych we włóknach skrajnych",this,sigmamax,1.02*fy)
		),
		NumSection("Sprawdzenie łączników na ścinanie wg PN-EN 1994-1-1 pkt. 6.6",
			Evaluate(Seq(MaRd),this),
			AssertionL("PN-EN 1994-1-1 pkt. 6.6.1.3(3)",this,MplRd/MaRd,2.5),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.5.7(1)",this,STUD.hsc/STUD.d,3),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.5.8(1)",this,STUD.hsc,hp+2*STUD.d),			
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.5.8(2)",this,bo,50E-3),			
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.4.2(3)",this,bo,hp),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.4.2(3)",this,hp,85E-5),
			AssertionL("PN-EN 1994-1-1 pkt. 6.6.4.2(3)",this,STUD.d,20E-3),
			Evaluate(Seq(s,alpha,PRd,ktmax,kt,VEdr,Ls,nf,nfprim,smin,smax),this),
			AssertionL("maksymalnego rozstawu łączników",this,bs,smax),
			AssertionG("minimanego rozstawu łączników",this,bs,smin)
		),
		NumSection("Sprawdzenie ścinania podłużnego w płycie zespolonej wg pkt. 6.6.6",
			Evaluate(Seq(VEdc,kphi,PpbRd,thetat,VRdsr),this),
			AssertionLE("nośności prętów zbrojeniowych na ścinanie podłużne",this,VEdc,VRdsr),
			Evaluate(Seq(v,VRdsc),this),
			AssertionLE("nośności ściskanych krzyżulców betonowych",this,VEdc,VRdsc)
		)
	)
	
	def SLS2 = NumSection(TextToTranslate("SLS","eurocode"),
		NumSection("Sprawdzenie ugięcia maksymalnego",
			Evaluate(Seq(deltae,deltamax),this),
			AssertionLE("ugięcia maksymalnego",this,deltamax,l/250)
		),
		NumSection("Sprawdzenie drgań",
			Evaluate(Seq(bn2,Sy2,z2,I2,Gk,yw,f),this),
			AssertionG("częstotliwości drgań własnych",this,f,3)
		)
	)
	
}	
