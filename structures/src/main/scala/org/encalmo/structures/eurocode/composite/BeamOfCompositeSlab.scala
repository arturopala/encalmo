package org.encalmo.structures.eurocode.composite

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Calculation
import org.encalmo.structures.eurocode.actions.ActionsSymbols
import org.encalmo.structures.eurocode.steel.{IBeamSteelSection, Steel}
import org.encalmo.document._
import org.encalmo.expression.min
import org.encalmo.expression.sin
import org.encalmo.expression.cos
import org.encalmo.expression.sqrt
import org.encalmo.expression.max
import org.encalmo.expression.round
import org.encalmo.expression.cot
import org.encalmo.structures.eurocode.fasteners.HeadedStud
import org.encalmo.expression.min
import org.encalmo.expression.sin
import org.encalmo.expression.cos
import org.encalmo.expression.sqrt
import org.encalmo.expression.max
import org.encalmo.expression.round
import org.encalmo.expression.cot

/** Composite slab with profiled steel sheeting symbols */
trait BeamOfCompositeSlabSymbols extends SymbolConfigurator {

	import BasicSymbols._
	
	val lb = symbol(BasicSymbols.l|BasicSymbols.b) unit "m"
	val gk = symbol(BasicSymbols.g|BasicSymbols.k) unit "kN/m"
	val gd = symbol(BasicSymbols.g|BasicSymbols.d) unit "kN/m"
	val Qk1 = symbol(Q|"k|m") unit "kN/m"
    val Qd1 = symbol(Q|"d|m") unit "kN/m"
    val Qk2 = symbol(Q|"k|e") unit "kN/m"
    val Qd2 = symbol(Q|"d|e") unit "kN/m"
    val Gk = symbol(G|"k") unit "kN/m"
    val eta = symbol(BasicSymbols.eta)
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
	val smax = symbol(BasicSymbols.s|"max") unit "mm"
	val smin = symbol(BasicSymbols.s|"min") unit "mm"
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

class BeamOfCompositeSlab(
    name: String,
	val length:Expression,
	val section:IBeamSteelSection,
	val steel:Steel,
	val slab:CompositeConcreteSlabOnProfiledSteelSheeting,
	val stud:HeadedStud,
    p_gammaG: Expression,
    p_gammaQ: Expression
)
extends Calculation(name, "beamOfCompositeSlab") with BeamOfCompositeSlabSymbols with ActionsSymbols {

    val sheet = slab.sheet

	import slab.concrete.{fck,fcd,Ecm,vc}
	import steel.{E,epsi,fy,fu,gammaM0,gammaV}
	import slab.{Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Eceff,nE,hc,dmesh,sd,fyrd}
	import slab.sheet.{Gcck,Gccd,hp,bo,bs,t,fypd}
    import section.{m,Wy,A,Iy,b,h,Wypl,AVz,hw,tw,ctf,ctw,tf,Cf1,Cw1,C1}

	this add section
	this add steel
	this add slab
	this add stud
	
	lb := length
	nr := 1

    gammaG := p_gammaG
    gammaQ := p_gammaQ
	
    //obciazenia i schemat statyczny w fazie montazu
    gk := m*GRAV
    gd := gk*gammaG
    Qk1 := (Qcfk*slab.ls)+(Gcck*slab.ls)+gk+(Qmk*slab.ls)
    Qd1 := (Qcfk*gammaG*slab.ls)+(Gccd*slab.ls)+gd+(Qmd*slab.ls)
	
    //sprawdzenie statecznosci srodnika
    eta := rangeChoiceLE(fy,1.2,Number(460,SI.MPa),1.0)
	val ULS1M = check((hw/tw)<((72*epsi)/eta),"Sprawdzenie stateczności środnika wg PN-EN 1993-1-5 pkt 5.1(2)")
	val ULS2M = check(C1===1,"Sprawdzenie dopuszczalności określania nośności wg nośności plastycznej")
	
    //nosnosci
    MelRd := (Wy*fy)/gammaM0
    VplRd := (AVz*(fy/sqrt(3)))/gammaM0
    NcRd := (A*fy)/gammaM0
	val ULS3M = require(abs(MEdm/MelRd)<=1,"Warunek nośności belki na zginanie w fazie montażu")
	val ULS5M = require(abs(VEdm/VplRd)<=1,"Warunek nośności belki na ścinanie w fazie montażu")
	
    //sily wewnetrzne w fazie montazu
    MEdm := (Qd1*(lb^2))/8
    VEdm := (Qd1*lb)/2
    MEdm1 := ((Qd1-(Qmd*slab.ls))*(lb^2))/8
    Mkm := (Qk1*(lb^2))/8
    Mkm1 := ((Qk1-(Qmk*slab.ls))*(lb^2))/8
    ΔMk := (ΔQk*(lb^2))/8
	
    //naprezenia
    sigmamplus := MEdm/Wy
    sigmadm1 := MEdm1/Wy
    sigmakm1 := Mkm1/Wy
	val ULS4M = require(sigmamplus<=fy,"Warunek braku uplastycznienia belki w fazie montażu")
	
    //ugiecia
    deltam := (5*Qk1*(lb^4))/(384*Iy*E)
    deltam1 := (5*(Qk1-(Qmk*slab.ls))*(lb^4))/(384*Iy*E)
    deltam0 := round(deltam1,RoundingMode.Step(true,0.01))
	val SLS1M = require((deltam-deltam0)<(lb/250),SI.mm,"Warunek dopuszczalnych ugięć belki w fazie montażu")
	
    //obciazenia w fazie eksploatacji
    Qk2 := (slab.qk*slab.ls)+(slab.Gck*slab.ls)+(Gcck*slab.ls)+gk+(slab.Gsk*slab.ls)
    Qd2 := (slab.qd*slab.ls)+(slab.Gcd*slab.ls)+(Gccd*slab.ls)+gd+(slab.Gsd*slab.ls)
    ΔQk := (slab.qk*slab.ls)+(slab.Gsk*slab.ls)
    ΔQd := (slab.qd*slab.ls)+(slab.Gsd*slab.ls)
    Gk := (slab.Gck*slab.ls)+(Gcck*slab.ls)+gk+(slab.Gsk*slab.ls)
	
    //sily wewnetrzne w fazie eksploatacji
    ΔMEd := (ΔQd*(lb^2))/8
    ΔVEd := (ΔQd*lb)/2
    MEde := (Qd2*(lb^2))/8
    VEde := (Qd2*lb)/2
	val ULS1E = require(VEde<=(0.5*VplRd),"Warunek braku interakcji zginania i ścinania w belce w fazie eksploatacji")
	
    //szerokosc wspolpracujaca
    b0 := 0
    bei := min(lb/8,slab.ls/2)
    beff := b0+2*bei
    //rownowaga sil w przekroju
    Ncf := 0.85*fcd*beff*hc
    Npla := A*fy
    N1pla := (A-2*b*tf)*fy
    x := rangeChoiceLE(Ncf,slab.h+(Npla-Ncf)/(2*b*fy),Npla,Npla/(beff*fcd))
    MplRd := rangeChoiceLE(x,Npla*(h/2+(slab.h-(x/2))),slab.h,Npla*(h/2)+Ncf*(hp+(hc/2))-2*(x-slab.h)*b*fy*((x-slab.h)/2))
    bn := beff/nE
    Sy := bn*hc*(h/2+hp+hc/2)
    z0 := Sy/(A+bn*hc)
    I1 := Iy+A*(z0^2)+(bn*(hc^3))/12+bn*hc*((hc/2+hp+h/2-z0)^2)
    Wel := I1/(z0+h/2)
	val ULS2E = require(abs(MEde/MplRd)<=1,"Warunek nośności belki na zginanie bez uwzględniania zwichrzenia w fazie eksploatacji")
	
    //naprezenia
    sigmake := ΔMk/Wel
    sigmamax := sigmakm1+sigmake
	val ULS3E = require(sigmamax<=(1.02*fy),"Sprawdzenie naprężeń dopuszczalnych we włóknach skrajnych belki w fazie eksploatacji")
	
    //nosnosc sworznia
    s := bs
    alpha := rangeChoiceLELE(stud.hsc/stud.d,0,3,0.2*((stud.hsc/stud.d)+1),4,1)
    PRd := min((0.8*fu*(PI*square(stud.d))/4)/gammaV,(0.29*alpha*square(stud.d)*sqrt(fck*Ecm))/gammaV)
    ktmax := rangeChoiceLE(t,0.85,1,1)
    kt := min(ktmax, (0.7*bo)/(sqrt(nr)*hp)*(stud.hsc/hp-1))
    VEdr := Ncf
    Ls := 0.5*lb
    nf := ceil(VEdr/(kt*PRd))
    nfprim := floor(Ls/s)
    smin := min(5*stud.d)
    smax := min(6*slab.h,800 unit SI.mm)
    MaRd := (Wypl*fy)/gammaM0

	val CHS1 = check((MplRd/MaRd)<2.5,"Warunek PN-EN 1994-1-1 pkt. 6.6.1.3(3)")
	val CHS2 = check((stud.hsc/stud.d)>3,"Warunek PN-EN 1994-1-1 pkt. 6.6.5.7(1)")
	val CHS3 = check(stud.hsc>(hp+2*stud.d),"Warunek PN-EN 1994-1-1 pkt. 6.6.5.8(1)")
	val CHS4 = check(bo>(50 unit SI.mm),"Warunek PN-EN 1994-1-1 pkt. 6.6.5.8(2)")
	val CHS5 = check(bo>hp,"Warunek PN-EN 1994-1-1 pkt. 6.6.4.2(3)")
	val CHS6 = check(hp<=(85 unit SI.mm),"Warunek PN-EN 1994-1-1 pkt. 6.6.4.2(3)")
	val CHS7 = check(stud.d<(20 unit SI.mm),"Warunek PN-EN 1994-1-1 pkt. 6.6.4.2(3)")
	val CHS8 = check(bs<smax,"Warunek maksymalnego rozstawu łączników")
	val CHS9 = check(bs>smin,"Warunek minimanego rozstawu łączników")
	
    //sciananie podluzne w plycie zespolonej
    VEdc := 0.5*(kt*PRd)/s
    kphi := min(1+max(b/2,1.5*1.1*stud.d)/(1.1*stud.d),6)
    PpbRd := kphi*1.1*stud.d*sheet.tcor*fypd
    thetat := 45
    VRdsr := cot(45)*((((PI*square(dmesh))/4*fyrd)/sd)+min(sheet.Ap*fypd,PpbRd/s))
    VRdsc := vc*fcd*sin(thetat)*cos(thetat)*hc
	val ULS4E = require(VEdc<=VRdsr,"Warunek nośności prętów zbrojeniowych na ścinanie podłużne")
	val ULS5E = require(VEdc<=VRdsc,"Warunek nośności ściskanych krzyżulców betonowych")
	
    //ugiecia w fazie eksploatacji
    deltae := (5*ΔQk*(lb^4))/(384*I1*E)
    deltamax := deltam1-deltam0+deltae
	val SLS1E = require(deltamax<=(lb/250),SI.mm,"Warunek ugięcia maksymalnego belki w fazie eksploatacji")
	
    //drgania
    bn2 := beff*Ecm/E
    Sy2 := bn2*hc*(h/2+hp+hc/2)
    z2 := Sy2/(A+bn2*hc)
    I2 := Iy+A*(z2^2)+(bn2*(hc^3))/12+bn2*hc*((hc/2+hp+h/2-z2)^2)
    yw := (5*Gk*(lb^4))/(384*I2*E)
    f := 18/sqrt(yw).nounit
	val SLS2E = require(f>3,"Warunek minimalnej częstotliwości drgań własnych stropu")
	
    //unit mass
    mS := ((slab.Gck*slab.ls)+(Gcck*slab.ls)+gk)/(GRAV*slab.ls)
	
	def info = NumSection(Text("BeamOfCompositeSlab",dictionary),
		Text(section.name),
		Evaluate(lb,slab.ls,slab.h),
		section.info,
		steel.info,
		stud.info
	)
	
	def LOAD1 = Evaluate(gammaG,gammaQ,gk,gd,Gcck,Gccd,Qcfk1,Qcfk,Qcfd,Qmk,Qmd,Qk1,Qd1)
	
	def ULS1 = NumSection(Text("ULS","eurocode"),
		NumSection("Sprawdzenie stateczności środnika wg PN-EN 1993-1-5 pkt 5.1(2)",
			Evaluate(hw,epsi,eta),
			Check(ULS1M)
		),
		NumSection("Określenie klasy przekroju wg PN-EN 1993-1-1 pkt 5.5",
			Evaluate(ctf,ctw,Cf1,Cw1,C1),
			Check(ULS2M)
		),
		NumSection("Sprawdzenie nośności belki na zginanie w fazie montażu",
			Evaluate(MelRd,MEdm),
			Check(ULS3M),
			Evaluate(sigmamplus),
			Check(ULS4M)
		),
		NumSection("Sprawdzenie nośności belki na ścinanie w fazie montażu",
			Evaluate(AVz,VplRd,VEdm),
			Check(ULS5M)
		),
		NumSection("Naprężenia pozostające w belce po fazie montażu",
			Evaluate(MEdm1,Mkm1,Mkm,sigmadm1,sigmakm1)
		)
	)
	
	def SLS1 = NumSection(Text("SLS","eurocode"),
		NumSection("Sprawdzenie ugięć w fazie montażu",
			Evaluate(deltam1,deltam0,deltam),
			Check(SLS1M)
		)
	)
	
	def LOAD2 = Evaluate(Qk2,Qd2,ΔQk,ΔQd)
	
	def ULS2 = NumSection(Text("ULS","eurocode"),
		NumSection("Siły wewnętrzne w belce w fazie eksploatacji",
			Evaluate(ΔMEd,ΔVEd,MEde,VEde),
			Check(ULS1E)
		),
		NumSection("Szerokość współpracująca i położenie osi obojętnej wg PN-EN 1994-1-1 pkt. 5.4.1.2(5)",
			Evaluate(b0,bei,beff,Ncf,Npla,N1pla,x)
		),
		NumSection("Sprawdzenie nośności belki na zginanie bez uwzględniania zwichrzenia zgodnie z PN-EN 1994-1-1 pkt. 6.4.1(1)",
			Evaluate(MplRd),
			Check(ULS2E)
		),
		NumSection("Sprawdzenie naprężeń we włóknach skrajnych belki",
			Evaluate(Eceff,nE,bn,Sy,z0,I1,Wel,ΔMk,sigmake,sigmamax),
			Check(ULS3E)
		),
		NumSection("Sprawdzenie łączników na ścinanie wg PN-EN 1994-1-1 pkt. 6.6",
			Evaluate(MaRd),
			Check(CHS1,CHS2,CHS3,CHS4,CHS5,CHS6,CHS7),
			Evaluate(s,alpha,PRd,ktmax,kt,VEdr,Ls,nf,nfprim,smin,smax),
			Check(CHS8,CHS9)
		),
		NumSection("Sprawdzenie ścinania podłużnego w płycie zespolonej wg pkt. 6.6.6",
			Evaluate(VEdc,kphi,PpbRd,thetat,VRdsr),
			Check(ULS4E),
			Evaluate(vc,VRdsc),
			Check(ULS5E)
		)
	)
	
	def SLS2 = NumSection(Text("SLS","eurocode"),
		NumSection("Sprawdzenie ugięcia maksymalnego",
			Evaluate(deltae,deltamax),
			Check(SLS1E)
		),
		NumSection("Sprawdzenie drgań",
			Evaluate(bn2,Sy2,z2,I2,Gk,yw,f),
			Check(SLS2E)
		)
	)
	
}	
