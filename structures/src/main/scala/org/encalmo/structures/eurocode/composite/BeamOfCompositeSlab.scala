package org.encalmo.structures.eurocode.composite

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.ContextFactory
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.structures.eurocode.concrete.ConcreteSymbols
import org.encalmo.structures.eurocode.steel.SteelSymbols
import org.encalmo.structures.eurocode.steel.ProfiledSteelSheetSymbols
import org.encalmo.structures.eurocode.actions.ActionsSymbols
import org.encalmo.structures.eurocode.steel.HeadedStudSymbols
import org.encalmo.structures.common.section.SectionSymbols
import org.encalmo.structures.eurocode.steel.HeadedStud
import org.encalmo.structures.eurocode.steel.Steel
import org.encalmo.structures.common.section.IBeamSectionSymbols
import org.encalmo.structures.common.section.Section
import org.encalmo.document.NumSection
import org.encalmo.document.Evaluate
import org.encalmo.document.TextToTranslate
import org.encalmo.document.AssertionL
import org.encalmo.document.AssertionE
import org.encalmo.document.AssertionLE
import org.encalmo.document.AssertionG
import org.encalmo.document.Text

/** Composite slab with profiled steel sheeting symbols */
object BeamOfCompositeSlabSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "beamOfCompositeSlab"
	
	val l = symbol(BasicSymbols.l) unit "m"
	val gk = symbol(BasicSymbols.g|BasicSymbols.k) unit "kN/m"
	val gd = symbol(BasicSymbols.g|BasicSymbols.d) unit "kN/m"
	val Qk1 = symbol(Q|"k|m") unit "kN/m"
    val Qd1 = symbol(Q|"d|m") unit "kN/m"
    val Qk2 = symbol(Q|"k|e") unit "kN/m"
    val Qd2 = symbol(Q|"d|e") unit "kN/m"
    val Gk = symbol(G|"k") unit "kN/m"
    val eta = symbol(BasicSymbols.eta)
    val Cf1 = symbol("C"|"f,1")
    val Cw1 = symbol("C"|"w,1")
    val Cf2 = symbol("C"|"f,2")
    val Cw2 = symbol("C"|"w,2")
    val Cf3 = symbol("C"|"f,3")
    val Cw3 = symbol("C"|"w,3")
    val C = symbol(BasicSymbols.C)
    val MelRd = symbol(M|"el,Rd") unit "kNm"
    val VplRd = symbol(V|"pl,Rd") unit "kN"
    val NcRd = symbol(N|"c,Rd") unit "kN"
    val MEdm = symbol(M|("Ed|m")) unit "kNm"
    val VEdm = symbol(V|("Ed|m")) unit "kN"
    val MEde = symbol(M|("Ed|e")) unit "kNm"
    val VEde = symbol(V|("Ed|e")) unit "kN"
    val ΔMEd = symbol("ΔM"|("Ed")) unit "kNm"
    val ΔVEd = symbol("ΔV"|("Ed")) unit "kN"
    val sigmamplus = symbol(BasicSymbols.sigma|("m","+")) unit "MPa"
    val MEdm1 = symbol(BasicSymbols.M|("Ed|m,1")) unit "kNm"
    val Mkm = symbol(BasicSymbols.M|("k|m")) unit "kNm"
    val Mkm1 = symbol(BasicSymbols.M|("k|m,1")) unit "kNm"
    val ΔMk = symbol("ΔM"|("k")) unit "kNm"
    val Mke = symbol(BasicSymbols.M|("k|e")) unit "kNm"
    val sigmadm1 = symbol(BasicSymbols.sigma|("d|m,1")) unit "MPa"
    val sigmakm1 = symbol(BasicSymbols.sigma|("k|m,1")) unit "MPa"
    val deltam0 = symbol(BasicSymbols.delta|"m,0") unit "mm" acc 1
    val deltam = symbol(BasicSymbols.delta|"m") unit "mm" acc 1
    val deltam1 = symbol(BasicSymbols.delta|"m,1") unit "mm" acc 1
    val deltamax = symbol(BasicSymbols.delta|"max") unit "mm" acc 1
    val deltae = symbol(BasicSymbols.delta|"e") unit "mm" acc 1
    val ΔQk = symbol(("ΔQ")|"k") unit "kN/m"
    val ΔQd = symbol(("ΔQ")|"d") unit "kN/m"
    val b0 = symbol(BasicSymbols.b|"0") unit "m"
    val beff = symbol(BasicSymbols.b|"eff") unit "m"
    val bei = symbol(BasicSymbols.b|"ei") unit "m"
    val Ncf = symbol(BasicSymbols.N|"c,f") unit "kN"
    val Npla = symbol(BasicSymbols.N|"pl,a") unit "kN"
    val N1pla = symbol(BasicSymbols.N|("pl,a","'")) unit "kN"
    val x = symbol(BasicSymbols.x) unit "cm"
    val MplRd = symbol(BasicSymbols.M|"pl,Rd") unit "kNm"
    val bn = symbol(BasicSymbols.b|"E") unit "m"
    val Sy = symbol(BasicSymbols.S|"y") unit "cm3"
    val z0 = symbol(BasicSymbols.z|"0") unit "m"
    val I1 = symbol(BasicSymbols.I|"1") unit "cm4"
    val Wel = symbol(BasicSymbols.W|"el") unit "cm3"
    val sigmake = symbol(BasicSymbols.sigma|"k|e") unit "MPa"
    val sigmamax = symbol(BasicSymbols.sigma|"max") unit "MPa"
	val PRd = symbol(BasicSymbols.P|"Rd") unit "kN"
	val alpha = symbol(BasicSymbols.alpha)
	val kt = symbol(BasicSymbols.k|"t")
	val ktmax = symbol(BasicSymbols.k|"t,max")
	val nr = symbol(BasicSymbols.n|"r")
	val VEdr = symbol(BasicSymbols.V|"Ed,r") unit "kN"
	val Ls = symbol(BasicSymbols.L|"s") unit "m"
	val nf = symbol(BasicSymbols.n|"f")
	val nfprim = symbol(BasicSymbols.n|("f","'"))
	val s = symbol(BasicSymbols.s) unit "m"
	val smax = symbol(BasicSymbols.s|"max") unit "m"
	val smin = symbol(BasicSymbols.s|"min") unit "m"
	val MaRd = symbol(BasicSymbols.M|"a,Rd") unit "kNm"
	val VEdc = symbol(BasicSymbols.V|"Ed,c") unit "kN/m"
	val kphi = symbol(BasicSymbols.k|BasicSymbols.phi)
	val PpbRd = symbol(BasicSymbols.P|"pb,Rd") unit "kN"
	val VRdsr = symbol(BasicSymbols.V|"Rd,s,r") unit "kN/m"
	val thetat = symbol(BasicSymbols.theta|"t") unit "°"
	val VRdsc = symbol(BasicSymbols.V|"Rd,s,c") unit "kN/m"
	val bn2 = symbol(BasicSymbols.b|"E,2") unit "m"
    val Sy2 = symbol(BasicSymbols.S|"y,2") unit "cm3"
    val z2 = symbol(BasicSymbols.z|"2") unit "m"
    val I2 = symbol(BasicSymbols.I|"2") unit "cm4" acc 1
	val yw = symbol(BasicSymbols.y|"w") unit "mm"
	val f = symbol(BasicSymbols.f) unit "Hz"
	val mS = symbol(BasicSymbols.m|"S") unit "kg/m2" acc 1
}

/** Composite slab with profiled steel sheeting context */
object BeamOfCompositeSlabExpressions extends MapContext {

	val SHEET = ProfiledSteelSheetSymbols
	val SLAB = CompositeSlabWithProfiledSheetingSymbols
	val STUD = HeadedStudSymbols

	import BeamOfCompositeSlabSymbols._
	import ConcreteSymbols.{fck,fcd,Ecm,vc}
	import SteelSymbols.{E,fyd,fy,fu,gammaM0,gammaV}
	import CompositeSlabWithProfiledSheetingSymbols.{Qcfk1,Qcfk,Qcfd,Qmk,Qmd,hc,Eceff2,nE,dmesh,sd,fyrd}
	import ProfiledSteelSheetSymbols.{Gcck,Gccd,hp,t,bo,bs,fypd}
    import ActionsSymbols.{gammaG,gammaQ}
    import SectionSymbols.{m,Wy,A,Iy,b,h,Wypl,AVz}
    import IBeamSectionSymbols.{ctw,ctf,bf,tf}
    
    //obciazenia i schemat statyczny w fazie montazu
    gk := 10*m.setunit("N/m")
    gd := gk*gammaG
    Qk1 := (Qcfk*SLAB.l)+(Gcck*SLAB.l)+gk+(Qmk*SLAB.l)
    Qd1 := (Qcfk*gammaG*SLAB.l)+(Gccd*SLAB.l)+gd+(Qmd*SLAB.l)
    //sprawdzenie statecznosci srodnika
    eta := rangeChoiceLE(fy,1.2,460E6,1.0)
	//klasa przekroju
	Cf1 := rangeChoice4LE(ctf,1,9*eta,2,10*eta,3,14*eta,4)
	Cw1 := rangeChoice4LE(ctw,1,72*eta,2,83*eta,3,124*eta,4)
	C := max(Cf1,Cw1)
	//nosnosci
	MelRd := (Wy*fy)/gammaM0
	VplRd := (AVz*(fy/sqrt(3)))/gammaM0
	NcRd := (A*fy)/gammaM0
	//sily wewnetrzne w fazie montazu
	MEdm := (Qd1*(l^2))/8
	VEdm := (Qd1*l)/2
	MEdm1 := ((Qd1-(Qmd*SLAB.l))*(l^2))/8
	Mkm := (Qk1*(l^2))/8
	Mkm1 := ((Qk1-(Qmk*SLAB.l))*(l^2))/8
	ΔMk := (ΔQk*(l^2))/8
	//naprezenia
	sigmamplus := MEdm/Wy
	sigmadm1 := MEdm1/Wy
	sigmakm1 := Mkm1/Wy
	//ugiecia
	deltam := (5*Qk1*(l^4))/(384*Iy*E)
	deltam1 := (5*(Qk1-(Qmk*SLAB.l))*(l^4))/(384*Iy*E)
	deltam0 := round(deltam1,RoundingMode.Step(true,0.01))
	//obciazenia w fazie eksploatacji
	Qk2 := (SLAB.qk*SLAB.l)+(SLAB.Gck*SLAB.l)+(Gcck*SLAB.l)+gk+(SLAB.Gsk*SLAB.l)
    Qd2 := (SLAB.qd*SLAB.l)+(SLAB.Gcd*SLAB.l)+(Gccd*SLAB.l)+gd+(SLAB.Gsd*SLAB.l)
    ΔQk := (SLAB.qk*SLAB.l)+(SLAB.Gsk*SLAB.l)
	ΔQd := (SLAB.qd*SLAB.l)+(SLAB.Gsd*SLAB.l)
	Gk := (SLAB.Gck*SLAB.l)+(Gcck*SLAB.l)+gk+(SLAB.Gsk*SLAB.l)
	//sily wewnetrzne w fazie eksploatacji
	ΔMEd := (ΔQd*(l^2))/8
	ΔVEd := (ΔQd*l)/2
	MEde := (Qd2*(l^2))/8
	VEde := (Qd2*l)/2
	//szerokosc wspolpracujaca
	b0 := 0
	bei := min(l/8,SLAB.l/2)
	beff := b0+2*bei
	//rownowaga sil w przekroju
	Ncf := 0.85*fcd*beff*hc
	Npla := A*fy
	N1pla := (A-2*b*tf)*fy
	x := rangeChoiceLE(Ncf,SLAB.h+(Npla-Ncf)/(2*b*fy),Npla,Npla/(beff*fcd))
	MplRd := rangeChoiceLE(x,Npla*(h/2+(SLAB.h-(x/2))),SLAB.h,Npla*(h/2)+Ncf*(hp+(hc/2))-2*(x-SLAB.h)*b*fy*((x-SLAB.h)/2))
	bn := beff/nE
	Sy := bn*hc*(h/2+hp+hc/2)
	z0 := Sy/(A+bn*hc)
	I1 := Iy+A*(z0^2)+(bn*(hc^3))/12+bn*hc*((hc/2+hp+h/2-z0)^2)
	Wel := I1/(z0+h/2)
	//naprezenia
	sigmake := ΔMk/Wel
	sigmamax := sigmakm1+sigmake
	//nosnosc sworznia
	s := bs
	alpha := rangeChoiceLELE(STUD.hsc/STUD.d,0,3,0.2*((STUD.hsc/STUD.d)+1),4,1)
	PRd := min((0.8*fu*(PI*square(STUD.d))/4)/gammaV,(0.29*alpha*square(STUD.d)*sqrt(fck*Ecm))/gammaV)
	ktmax := rangeChoiceLE(t,0.85,1,1)
	kt := min(ktmax, (0.7*bo)/(sqrt(nr)*hp)*(STUD.hsc/hp-1))
	VEdr := Ncf
	Ls := 0.5*l
	nf := VEdr/(kt*PRd)
	nfprim := floor(Ls/s)
	smin := min(5*STUD.d)
	smax := min(6*SLAB.h,0.8)
	MaRd := (Wypl*fy)/gammaM0
	//sciananie podluzne w plycie zespolonej
	VEdc := 0.5*(kt*PRd)/s
	kphi := min(1+max(b/2,1.5*1.1*STUD.d)/(1.1*STUD.d),6)
	PpbRd := kphi*1.1*STUD.d*SHEET.tcor*fypd
	thetat := 45
	VRdsr := cot(45)*((((PI*square(dmesh/1000))/4*fyrd)/sd)+min(SHEET.Ap*fypd,PpbRd/s))
	VRdsc := vc*fcd*sin(thetat)*cos(thetat)*hc
	//ugiecia w fazie eksploatacji
	deltae := (5*ΔQk*(l^4))/(384*I1*E)
	deltamax := deltam1-deltam0+deltae
	//drgania
	bn2 := beff*Ecm/E
	Sy2 := bn2*hc*(h/2+hp+hc/2)
	z2 := Sy2/(A+bn2*hc)
	I2 := Iy+A*(z2^2)+(bn2*(hc^3))/12+bn2*hc*((hc/2+hp+h/2-z2)^2)
	yw := (5*Gk*(l^4))/(384*I2*E)
	f := 18/sqrt(yw).nounit
	//unit mass
	mS := ((SLAB.Gck*SLAB.l)+(Gcck*SLAB.l)+gk)/(GRAV*SLAB.l)
	
	// end of context initialization
	lock()

}

class BeamOfCompositeSlab(
    name: String,
	val length:Expression,
	val section:Section, 
	val steel:Steel,
	val slab:CompositeSlabWithProfiledSheeting,
	val stud:HeadedStud
)
extends Calculation(name) {

	val SLAB = CompositeSlabWithProfiledSheetingSymbols
	val STUD = HeadedStudSymbols

	import BeamOfCompositeSlabSymbols._
	import ConcreteSymbols.vc
	import SteelSymbols.{E,fyd,epsi,fy}
	import CompositeSlabWithProfiledSheetingSymbols.{Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Eceff2,nE}
	import ProfiledSteelSheetSymbols.{Gcck,Gccd,hp,bo,bs}
    import ActionsSymbols.{gammaG,gammaQ}
    import SectionSymbols.AVz
    import IBeamSectionSymbols.{hw,tw,ctf,ctw}

	this add BeamOfCompositeSlabExpressions
	this add section
	this add steel
	this add slab
	this add stud
	
	l := length
	nr := 1
	
	def info = NumSection(TextToTranslate("BeamOfCompositeSlab",BeamOfCompositeSlabSymbols.dictionary),
		Text(section.name),
		Evaluate(l,SLAB.l,SLAB.h),
		section.info,
		steel.info,
		stud.info
	)
	
	def LOAD1 = Evaluate(gammaG,gammaQ,gk,gd,Gcck,Gccd,Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Qk1,Qd1)
	
	def ULS1 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Sprawdzenie stateczności środnika wg PN-EN 1993-1-5 pkt. 5.1(2)",
			Evaluate(hw,epsi,eta),
			AssertionL("stateczności środnika",hw/tw,(72*epsi)/eta)
		),
		NumSection("Określenie klasy przekroju",
			Evaluate(ctf,ctw,Cf1,Cw1,C),
			AssertionE("dopuszczalności określania nośności wg nośności plastycznej",C,1)
		),
		NumSection("Sprawdzenie nośności belki na zginanie w fazie montażu",
			Evaluate(MelRd,MEdm),
			AssertionLE("nośności na zginanie",MEdm/MelRd,1),
			Evaluate(sigmamplus),
			AssertionLE("braku uplastycznienia",sigmamplus,fy)
		),
		NumSection("Sprawdzenie nośności belki na ścinanie w fazie montażu",
			Evaluate(AVz,VplRd,VEdm),
			AssertionLE("nośności na ścinanie",VEdm/VplRd,1)
		),
		NumSection("Naprężenia pozostające w belce po fazie montażu",
			Evaluate(MEdm1,Mkm1,Mkm,sigmadm1,sigmakm1)
		)
	)
	
	def SLS1 = NumSection(TextToTranslate("SLS","eurocode"),
		NumSection("Sprawdzenie ugięć w fazie montażu",
			Evaluate(deltam1,deltam0,deltam),
			AssertionLE("dopuszczalnych ugięć",deltam-deltam0,l/250)
		)
	)
	
	def LOAD2 = Evaluate(Qk2,Qd2,ΔQk,ΔQd)
	
	def ULS2 = NumSection(TextToTranslate("ULS","eurocode"),
		NumSection("Siły wewnętrzne w belce w fazie eksploatacji",
			Evaluate(ΔMEd,ΔVEd,MEde,VEde),
			AssertionLE("braku interakcji zginania i ścinania",VEde,0.5*VplRd)
		),
		NumSection("Szerokość współpracująca i położenie osi obojętnej wg PN-EN 1994-1-1 pkt. 5.4.1.2(5)",
			Evaluate(b0,bei,beff,Ncf,Npla,N1pla,x)
		),
		NumSection("Sprawdzenie nośności belki na zginanie bez uwzględniania zwichrzenia zgodnie z PN-EN 1994-1-1 pkt. 6.4.1(1)",
			Evaluate(MplRd),
			AssertionLE("nośności na zginanie",MEde/MplRd,1)
		),
		NumSection("Sprawdzenie naprężeń we włóknach skrajnych belki",
			Evaluate(Eceff2,nE,bn,Sy,z0,I1,Wel,ΔMk,sigmake,sigmamax),
			AssertionLE("naprężeń dopuszczalnych we włóknach skrajnych",sigmamax,1.02*fy)
		),
		NumSection("Sprawdzenie łączników na ścinanie wg PN-EN 1994-1-1 pkt. 6.6",
			Evaluate(MaRd),
			AssertionL("PN-EN 1994-1-1 pkt. 6.6.1.3(3)",MplRd/MaRd,2.5),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.5.7(1)",STUD.hsc/STUD.d,3),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.5.8(1)",STUD.hsc,hp+2*STUD.d),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.5.8(2)",bo,50 unit SI.mm),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.4.2(3)",bo,hp),
			AssertionG("PN-EN 1994-1-1 pkt. 6.6.4.2(3)",hp,85E-5),
			AssertionL("PN-EN 1994-1-1 pkt. 6.6.4.2(3)",STUD.d,20 unit SI.mm),
			Evaluate(s,alpha,PRd,ktmax,kt,VEdr,Ls,nf,nfprim,smin,smax),
			AssertionL("maksymalnego rozstawu łączników",bs,smax),
			AssertionG("minimanego rozstawu łączników",bs,smin)
		),
		NumSection("Sprawdzenie ścinania podłużnego w płycie zespolonej wg pkt. 6.6.6",
			Evaluate(VEdc,kphi,PpbRd,thetat,VRdsr),
			AssertionLE("nośności prętów zbrojeniowych na ścinanie podłużne",VEdc,VRdsr),
			Evaluate(vc,VRdsc),
			AssertionLE("nośności ściskanych krzyżulców betonowych",VEdc,VRdsc)
		)
	)
	
	def SLS2 = NumSection(TextToTranslate("SLS","eurocode"),
		NumSection("Sprawdzenie ugięcia maksymalnego",
			Evaluate(deltae,deltamax),
			AssertionLE("ugięcia maksymalnego",deltamax,l/250)
		),
		NumSection("Sprawdzenie drgań",
			Evaluate(bn2,Sy2,z2,I2,Gk,yw,f),
			AssertionG("częstotliwości drgań własnych",f,3)
		)
	)
	
}	
