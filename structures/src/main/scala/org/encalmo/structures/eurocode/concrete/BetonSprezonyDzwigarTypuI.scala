package org.encalmo.structures.eurocode.concrete

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.style.PredefinedStyles
import PredefinedStyles._
import org.encalmo.structures.Worksheet
import org.encalmo.style.PredefinedStyles

class BetonSprezonyDzwigarTypuI extends Worksheet("bks") {
        
    import BasicSymbols._
    import SI.{mm,mm2,cm,cm2,kN,MPa}

    val beton = Concrete.C_40_50
    this add beton

    import beton._
    
    val lr = l|r is "Rozstaw osi dźwigarów" unit SI.m; lr := 5.5
    val ld = l|d is "Rozpiętość dźwigara w osi podpór" unit SI.m; ld := 14
    val leff = l|"eff" is "Długość efektywna dźwigara do obliczeń" unit SI.m; leff := ld
    val hd = h|d is "Wysokość przekroju dźwigara" unit SI.cm acc 1; hd := 86
    
    val phis = phi|s is "Średnica strzemion" unit SI.mm; phis := 6
    
    val phip = phi|p is "Średnica drutu sprężającego" unit SI.mm; phip := 7.5
    val ap = a|p is "Pole przekroju drutu sprężającego" unit SI.mm2; ap := 44.2
    val rhop = rho|p is "Masa drutu sprężającego" unit SI.g/SI.m; rhop := 345
    val dsigp = "Δσ"|p is "Wytrzymałość zmęczeniowa" unit SI.MPa; dsigp := 200
    val Fpk = F|"pk" is "Charakterystyczna siła zrywająca" unit SI.kN; Fpk := 73.8
    val Fp = F|"p" is "Maksymalna siła zrywająca" unit SI.kN; Fp := 93.8
    val Fp01k = F|"p0,1k" is "Maksymalna siła wywołująca odkształcenie plastyczne 0.1%" unit SI.kN; Fp01k := 63.5
    val rop = r|"op" is "Minimalny promień odginania drutu sprężającego" unit SI.mm; rop := 20
    val fpk = f|"pk" is "Charakterystyczna wytrzymałość drutu sprężającego" unit SI.MPa acc 1; fpk := Fpk/ap
    val fp01k = f|"p01k" is "Charakterystyczna granica platyczności drutu sprężającego" unit SI.MPa acc 1; fp01k := Fp01k/ap
    val gammas = gamma|s is "Współczynnik materiałowy dla stali sprężającej"; gammas := 1.15
    val fpd = f|"pd" is "Obliczeniowa granica plastyczności drutu sprężającego" unit SI.MPa acc 1; fpd := fp01k/gammas
    
    val HM = symbol(BasicSymbols.H|"M") is "Wysokość nad poziomem morza dla lokalizacji obiektu";HM := 104
    val sk = symbol(BasicSymbols.s|"k") unit "kN/m2" acc 0.1 is "Wartość charakterystyczna obciążenia gruntu śniegiem";sk := max(0.7,0.007*HM-1.4)
    val mi1 = symbol(BasicSymbols.mu|"1") is "Współczynnik kształtu dachu";mi1 := 0.8
    val Ce = symbol(BasicSymbols.C|"e") is "Współczynnik ekspozycji";Ce := 1.0
    val Ct = symbol(BasicSymbols.C|"t") is "Współczynnik termiczny"; Ct := 1.0
    val qsk = symbol(BasicSymbols.q|"sk") unit "kN/m2" acc 0.1 is "Charakterystyczne obciążenie śniegiem dachu";qsk := mi1*Ce*Ct*sk
    
    val gpk = g|"p,k" is "Charakterystyczny ciężar pokrycia dachu (płyt kanałowych i warstw izolacyjnych)" unit SI.kN/SI.m2; gpk := 3
    val qtk = q|"t,k" is "Charakterytyczne obciążenie technologiczne dźwigara" unit SI.kN/SI.m; qtk := 1.0
    
    val dg = d|"g" is "Maksymalna średnica ziaren kruszywa"; dg := 8 unit SI.mm
    val dxmin = d|"x,min" is "Minimalny poziomy dystans cięgien sprężających"; dxmin := max(dg + (5 unit SI.mm),phip,20 unit SI.mm)
    val dymin = d|"y,min" is "Minimalny pionowy dystans cięgien sprężających"; dymin := max(dg,phip,10 unit SI.mm)
    val dcdev = "Δc"|"dev" is "Dodatek otuliny ze względu na odchyłkę" unit mm acc 1; dcdev := 5
    val cminb = "c"|"min,b" is "Minimalne otulenie ze względu na przyczepność" unit mm acc 1; cminb := 1.5*phip
    val cmindur = "c"|"min,dur" is "Minimalne otulenie ze względu na warunki środowiska XC2 dla konstrukcji klasy S4" unit mm acc 1; cmindur := 25
    val cmin = c|"min" is "Otulenie minimalne zbrojenia" unit mm acc 1; cmin := max(cminb,cmindur,10 unit mm)
    val cnom = c|"nom" is "Nominalna otulenie zbrojenia" unit mm acc 1; cnom := cmin+dcdev
    val cp = c|"p" is "Otulina efektywna cięgien sprężających" unit mm acc 1; cp := cnom + phis
    
    val qd1 = q|"d,max" is "Wstępne obliczeniowe obciążenie dźwigara (maksymalne)" unit SI.kN/SI.m acc 0.1; qd1 := (hd*(15 unit cm)*gammac)*1.35 + gpk*lr*1.35 + qsk*lr*1.5 + qtk*1.5
    val Myd1 = BasicSymbols.M|"yd1" is "Obliczeniowy moment zginający dla wstępnego obliczenia liczby cięgien" unit SI.kNm; Myd1 := qd1*(leff^2)/8
    val z1 = z|1 is "Ramię sił wewnętrznych w przekroju"; z1 := hd*0.75
    val np1 = n|"p1" is "Przyjęta liczba cięgien w strefie rozciąganej" acc 2; np1 := Myd1/((Fp01k/gammas)*z1)
    val np2 = n|"p2" is "Przyjęta liczba cięgien w strefie ściskanej" acc 2; np2 := (np1+1)*(20 unit SI.percent)
    
    val npxmax = n|"pxmax" is "Maksymalna liczba cięgien w rzędzie"; npxmax := 8
    val np1x = n|"p1x" is "Liczba cięgien dolnych w rzędzie"; np1x := min(np1,npxmax)
    val np1y = n|"p1y" is "Liczba pełnych rzędów cięgien dolnych"; np1y := round(np1/np1x,RoundingMode.FLOOR)
    val np1xr = n|"p1xr" is "Liczba cięgien dolnych w niepełnym rzędzie"; np1xr := np1-(np1x*np1y)
    val np2x = n|"p2x" is "Liczba cięgien górnych w rzędzie"; np2x := min(np2,npxmax-2)
    val np2y = n|"p2y" is "Liczba pełnych rzędów cięgiem górnych"; np2y := round(np2/np2x,RoundingMode.FLOOR)
    val np2xr = n|"p2xr" is "Liczba cięgien górnych w niepełnym rzędzie"; np2xr := np2-(np2x*np2y)
    
    val bd = b|d is "Szerokość przekroju dźwigara" unit SI.cm acc 1; bd := max(hd/8,np1x*phip+(np1x-1)*dxmin+2*cnom+2*phis)
    val b1 = b|1 is "Szerokość środnika dźwigara" unit SI.cm acc 1; b1 := max(bd/4,2*phip+dxmin+2*cnom+2*phis)
    val b2 = b|2 is "Szerokość półki dźwigara" unit SI.cm; b2 := (bd-b1)/2
    val h1 = h|1 is "Wysokość półki górnej przekroju" unit SI.cm; h1 := max(np2y*phip+(np2y-1)*dymin+1*cnom+2*phis,7 unit cm)
    val h2 = h|2 is "Wysokość skosu półki górnej przekroju" unit SI.cm acc 1; h2 := b2/2
    val h4 = h|4 is "Wysokość skosu półki dolnej przekroju" unit SI.cm acc 1; h4 := b2/1.2
    val h5 = h|5 is "Wysokość półki dolnej przekroju" unit SI.cm acc 1; h5 := max(np1y*phip+(np1y-1)*dymin+1*cnom+2*phis,10 unit cm)
    val h3 = h|3 is "Wysokość środnika przekroju" unit SI.cm; h3 := hd-h1-h2-h4-h5
    
    val hz1 = h|"z1" is "Wysokość zastępcza półki górnej" unit SI.cm; hz1 := h1+h2/2
    val hz3 = h|"z3" is "Wysokość zastępcza półki dolnej" unit SI.cm; hz3 := h5+h4/2
    val hz2 = h|"z2" is "Wysokość zastępcza środnika" unit SI.cm; hz2 := hd-hz1-hz3
    
    val Ap1 = A|"p1" is "Pole powierzchni cięgien w strefie rozciąganej" unit mm2; Ap1 := np1*ap
    val Ap2 = A|"p2" is "Pole powierzchni cięgien w strefie ściskanej" unit mm2; Ap2 := np2*ap
    val Ap = A|"p" is "Łączne pole powierzchni cięgien" unit mm2; Ap := Ap1+Ap2
    val Acceff = A|"cc,eff" is "Pole powierzchni strefy rozciąganej" unit cm2; Acceff := Ap1*fpd/fcd
    val Act = A|"ct" is "Pole powierzchni strefy ściskanej" unit cm2; Act := 50*Ap1
    val dps = d|"ps" is "Ogległość osi skrajnych cięgien od krawedzi przekroju" unit cm; dps := cp+phip/2
    val dp = d|"p" is "Ogległość osi rzędów cięgien" unit cm; dp := dymin+phip
    
    val yp1 = y|"p1" is "Położenie wypadkowej cięgien dolnych względem dolnej krawędzi przekroju"
    yp1 := dynamic(np1y,np1xr) { cache =>
        (np1x*dps)/np1 + Sum((for (x <- 1 to cache(np1y).toInt-1) yield (np1x*(dps+x*dp))/np1):_*) + cache(np1xr).mapIfNotZero{(np1xr*(dps+ np1y *dp))/np1}
    }
    val yp2 = y|"p2" is "Położenie wypadkowej cięgien górnych względem górnej krawędzi przekroju"
    yp2 := dynamic(np2y,np2xr) { cache =>
        (np2x*dps)/np2 + Sum((for (x <- 1 to cache(np2y).toInt-1) yield (np2x*(dps+x*dp))/np2):_*) + cache(np2xr).mapIfNotZero{(np2xr*(dps+ np2y *dp))/np2}
    }

    val Ac = A|c is "Pole przekroju betonowego" unit SI.cm2; Ac := hz1*bd+hz2*b1+hz3*bd
    val Sc = S|c is "Moment statyczny przekroju betonowego" unit SI.cm3 acc 1; Sc := hz1*bd*(hd-hz1/2) + hz3*bd*hz3/2 + hz2*b1*(hz2/2+hz3)
    val ycd = y|"cd" is "Położenie osi obojętnej przekroju betonowego względem dolnej krawędzi przekroju" unit cm; ycd := Sc/Ac
    val ycg = y|"cg" is "Położenie osi obojętnej przekroju betonowego względem górnej krawędzi przekroju" unit cm; ycg := hd-ycd
    
    val Ic = I|c is "Moment bezwładności przekroju betonowego" unit SI.cm4 acc 1
    Ic := ((bd*(hz1^3))/12) + bd*hz1*((ycg-hz1/2)^2) + ((bd*(hz3^3))/12) + bd*hz3*((ycd-hz3/2)^2) + ((b1*(hz2^3))/12) + b1*hz2*((ycd-(hz3+hz2/2))^2)
    
    val Wcd = W|"cd" is "Wskaźnik zginania włókien dolnych przekroju betonowego" unit SI.cm3 acc 1; Wcd := Ic/ycd
    val Wcg = W|"cg" is "Wskaźnik zginania włókien górnych przekroju betonowego" unit SI.cm3 acc 1; Wcg := Ic/ycg
    
    val alphae = alpha|"e" is "Iloraz modułów Younga stali sprężającej i betonu"; alphae := (200 unit SI.GPa)/Ecm
    val Ap1s = A|"p1s" is "Pole sprowadzone cięgien w strefie rozciąganej" unit mm2 acc 1; Ap1s := (alphae-1)*Ap1
    val Ap2s = A|"p2s" is "Pole sprowadzone cięgien w strefie ściskanej" unit mm2 acc 1; Ap2s := (alphae-1)*Ap2
    val Acs = A|"cs" is "Pole sprowadzone przekroju" unit SI.cm2 acc 0.1; Acs := Ac+Ap1+Ap2
    val Scs = S|"cs" is "Moment statyczny przekroju sprowadzonego względem osi obojętnej przekroju betonowego" unit SI.cm3 acc 1; Scs := Ap1s*(ycd-yp1)-Ap2s*(ycg-yp2)
    val yp = y|p is "Położenie osi obojętnej przekroju sprowadzonego względem osi obojętnej przekroju betonowego" unit cm; yp := Scs/Acs
    val yd = y|"d" is "Położenie osi obojętnej przekroju sprowadzonego względem dolnej krawędzi przekroju" unit cm; yd := ycd-yp
    val yg = y|"g" is "Położenie osi obojętnej przekroju sprowadzonego względem górnej krawędzi przekroju" unit cm; yg := ycg+yp
    val Ics = I|"cs" is "Moment bezwładności przekroju sprowadzonego" unit SI.cm4 acc 1; Ics := round(Ic + Ac*(yp^2) + Ap1s*((yd-yp1)^2) + Ap2s*((yg-yp2)^2),RoundingMode.FLOOR)
    val Wcsd = W|"csd" is "Wskaźnik zginania włókien dolnych przekroju sprowadzonego" unit SI.cm3 acc 1; Wcsd := Ics/yd
    val Wcsg = W|"csg" is "Wskaźnik zginania włókien górnych przekroju sprowadzonego" unit SI.cm3 acc 1; Wcsg := Ics/yg
    
    val gk = g|"k" is "Charakterystyczny ciężar własny dźwigara" unit SI.kN/SI.m; gk := Ac*gammac + (np1+np2)*rhop*GRAV
    val qd = q|"d" is "Obliczeniowe obciążenie dźwigara" unit SI.kN/SI.m acc 0.1; qd := gk*1.35 + gpk*lr*1.35 + qsk*lr*1.5 + qtk*1.5
    val Myd = BasicSymbols.M|"yd" is "Obliczeniowy moment zginający" unit SI.kNm acc 0.1; Myd := qd*(leff^2)/8
    
    val sigma0max = sigma|"0,max" is "Maksymalne naprężenie w sprężanych cięgnach" unit MPa; sigma0max := min(0.8*fpk,0.9*fp01k)
    val P0max = P|"0,max" is "Początkowa wartość siły sprężającej" unit kN acc 0.1; P0max := sigma0max*Ap
    val dPir = "ΔP"|"ir" is "Strata spowodowana relaksacją stali sprężającej dla klasy 2 ciegien" unit kN acc 0.1; dPir := P0max*0.04*0.55
    val Pmo1 = P|"mo1" is "Siła początkowa przed zwolnieniem naciągu" unit kN acc 0.1; Pmo1 := P0max - dPir
    val zcp = z|"cp" is "Mimośród siły sprężającej" unit cm; zcp := (Ap1*(yd-yp1)-Ap2*(yg-yp2))/Ap
    val dPc = "ΔP"|"c" is "Strata od odkształcalności sprężystej betonu" unit kN acc 0.1; dPc := alphae*(Ap/Acs)*((1+(zcp)^2)*(Acs/Ics))*Pmo1
    val Pmo2 = P|"mo2" is "Siła sprężająca po zwolnieniu naciągu" unit kN acc 0.1; Pmo2 := Pmo1 - dPc
    val sigpmo = sigma|"pmo" is "Naprężenie w cięgnach po zwolnieniu naciągu" unit MPa; sigpmo := min(Pmo2/Ap,0.75*fpk,0.85*fp01k)
    
    RH := 50
    dtimec := 1000
    timec0 := 40
    val Uc = U|c is "Obwód przekroju dźwigara" unit cm; Uc := 2*(b1+h1+hypot(h3,b2)+h3+hypot(h4,b2)+h5)
    ho := 2*Ac/Uc
    
    val sigcg = sigma|"cg" is "Naprężenia w betonie wywołane ciężarem własnym dźwigara" unit MPa; sigcg := -(gk*(leff^2)*zcp)/(8*Ics)
    val sigcpo = sigma|"cpo" is "Napreżenia w betonie wywołane działaniem siły sprężajacej" unit MPa; sigcpo := Pmo2/Acs+(Pmo2*(zcp^2))/Ics
    val sigpcs1 = sigma|"p,C+S,1" is "Wartość licznika do obliczenia σp,C+S"; sigpcs1 := (epsics*(200 unit SI.GPa)+alphae*phit*(sigcg+sigcpo))
    val sigpcs2 = sigma|"p,C+S,2" is "Wartość mianownika do obliczenia σp,C+S"; sigpcs2 := (1+alphae*(Ap/Acs)*(1+(zcp^2)*(Acs/Ics))*(1+0.8*phit))
    val sigpcs = sigma|"p,C+S" is "Naprężenie po stratach wywołanych skurczem i pełzaniem betonu" unit MPa
    sigpcs := sigpcs1/sigpcs2
    val dPt = "ΔP"|"t" is "Strata reologiczna wywołana skurczem i pełzaniem betonu" unit kN acc 0.1; dPt := sigpcs*Ap
    val Pmt = P|"mt" is "Siła sprężająca po uwzględnieniu strat reologicznych betonu" unit kN acc 0.1; Pmt := Pmo2 - dPt
    val sigpmt = sigma|"pmt" is "Naprężenia w cięgnach" unit MPa; sigpmt := Pmt/Ap
    
    val Pksup = P|"k,sup" is "Górna wartość siły sprężającej" unit kN; Pksup := 1.1 * Pmt
    val Pkinf = P|"k,inf" is "Dolna wartość siły sprężającej" unit kN; Pkinf := 0.9 * Pmt
    val sigcpd = sigma|"cpd" is "Naprężenia w dolnych włóknach skrajnych przekroju betonowego" unit MPa; sigcpd := Pksup/Acs + (Pksup*zcp)/Wcsd
    val sigcpg = sigma|"cpg" is "Naprężenia w górnych włóknach skrajnych przekroju betonowego" unit MPa; sigcpg := Pksup/Acs - (Pksup*zcp)/Wcsg
    
    val As2 = A|"s2" is "Pole przekroju zbrojenia pasywnego w strefie ściskanej"; As2 := 2*((PI*((20 unit mm)^2))/4)
    val sigp2 = sigma|"p2" is "Naprężenia w górnych cięgnach sprężających" unit MPa; sigp2 := (400 unit MPa) - sigpmo
    val xeff = x|"eff" is "Zasięg strefy ściskanej dla przekroju teowego" unit cm; xeff := (Ap1*fpd - Ap2*sigp2 - 2*b2*hz1*fcd /*- As2*zbrojenie(fyd)*/)/(b1*fcd)
    val d1 = d|"1" is "Odległość od górnej krawędzi przekroju do osi obojętnej dolnych cięgien sprężających" unit cm; d1 := hd-yp1
    val MRd = M|"Rd" is "Nośność dźwigara na zginanie z uwagi na SGN" unit SI.kNm; MRd := 2*b2*hz1*fcd*(d1-(hz1/2)) + b1*xeff*fcd*(d1-(xeff/2)) + (Ap2*sigp2 /*+ As2*zbrojenie(fyd)*/)*(d1-yp2)
    
    val rhol = rho|"l" is ""; rhol := Ap1/(b1*d1)
    val k1 = k|1 is ""; k1 := max((1.6 - (d1.nounit)/100),1)
    val sigcp = sigma|"cp" is "Średnie naprężenie ściskające w betonie" unit MPa; sigcp := min((0.9*Pmt)/Acs,0.2*fcd)
    val Vsd = V|"sd" is "Siła ścinająca w odległości d od podpory wg [1] 5.5.1.2" unit kN; Vsd := qd*(leff)/2 - qd*(d1+(20 unit cm))
    val VRd1 = V|"Rd1" is "Nośność obliczeniowa na ścinanie ze względu na rozciąganie betonu powstające przy ścinaniu w elemencie nie mającym poprzecznego zbrojenia na ścinanie" unit kN
    VRd1 := (0.35*k1*fctd*(1.2+40*rhol)+0.15*sigcp)*b1*d1
    val VRd21 = (V|"Rd2")(1) is "Nośność obliczeniowa na ścinanie ze względu na ściskanie betonu powstające przy ścinaniu w elementach zginanych dla odcinka typu 1" unit kN
    VRd21 := 0.5*vc*fcd*b1*d1
    val VRd22 = (V|"Rd2")(2) is "Nośność obliczeniowa na ścinanie ze względu na ściskanie betonu powstające przy ścinaniu w elementach zginanych dla odcinka typu 2" unit kN
    VRd22 := 0.5*vc*fcd*b1*d1*(1d/(1+1))
    val alphac = alpha|c is "Współczynnik redukcji nośności na ścinanie"; alphac := 1+sigcp/fcd
    val VRd2RED = V|"Rd2,RED" is "Nośność VRd2 w elementach obciążonych dodatkowo siłami ściskającymi dla odcinka typu 1" unit kN; VRd2RED := alphac*VRd21
    val VRd3 = V|"Rd3" is "Nośność obliczeniowa na ścinanie ze względu na rozciąganie poprzecznego zbrojenia na ścinanie" unit kN
    val Asw1 = A|"sw1" is "Przekrój zbrojenia poprzecznego" unit mm2; Asw1 := 2*((PI*(phis^2))/4)
    val s1 = s|"1" is "Rozstaw strzemion zbrojenia poprzecznego" unit cm acc 0.5; s1 := ((Asw1*(210 unit MPa))*d1*1)/Vsd
    
    val qk = q|"k" is "Charakterystyczne obciążenie dźwigara" unit SI.kN/SI.m; qk :=  gk + gpk*lr + qtk
    val Mk = M|"k" is "Charakterystyczny moment zginający dla SGU (bez śniegu)" unit SI.kNm; Mk := qk*(leff^2)/8
    val sigcpk = sigma|"cpk" is "Naprężenia w dolnych włóknach skrajnych przekroju betonowego dla SGU" unit MPa; sigcpk := Pkinf/Acs + (Pkinf*zcp)/Wcsd
    val Mcr = M|"cr" is "Moment rysujący" unit SI.kNm; Mcr := Wcsd*(sigcpk+fctm)
    
    val lbp = l|"bp" is "Długość zakotwienia cięgien" unit cm; lbp := 50 * phip
    val lbpd = l|"bpd" is "obliczeniowa długość zakotwienia cięgien" unit cm; lbpd := 0.9 * lbp
    
    val Vsk1 = V|"sk1" is "Siła ścinająca w punkcie 1" unit kN; Vsk1 := 0.5*qk*(leff-2*(20 unit cm)-2*yd)
    val Scs1 = S|"cs1" is "Moment statyczny części odciętej przekroju w punkcie 1" unit SI.cm3; Scs1 := bd*hz1*(yg-0.5*hz1)+b1*(yg-hz1)*0.5*(yd-hz1)
    val sigx1 = sigma|"x1" is "Naprężenie normalne w punkcie 1" unit MPa; sigx1 := Pkinf/Acs
    val tauxy1 = tau|"xy1" is "Naprężenie styczne w punkcie 1" unit MPa; tauxy1 := Vsk1*Scs1/(Ics*b1)
    val sigt1 = sigma|"t1" is "" unit MPa; sigt1 := sigx1/2-hypot(sigx1/2,tauxy1)
    
    val Vsk2 = V|"sk2" is "Siła ścinająca w punkcie 2" unit kN; Vsk2 := 0.5*qk*(leff-2*(20 unit cm)-2*(hd-hz1))
    val Scs2 = S|"cs2" is "Moment statyczny części odciętej przekroju w punkcie 2" unit SI.cm3; Scs2 := bd*hz1*(0.5*hz3)
    val sigx2 = sigma|"x2" is "Naprężenie w punkcie 2" unit MPa; sigx2 := Pkinf/Acs - (Pkinf*(yd-0.5*hz1)*(yd-hz1))/Ics
    val tauxy2 = tau|"xy2" is "Naprężenie ścinające w punkcie 2" unit MPa; tauxy2 := Vsk2*Scs2/(Ics*b1)
    val sigt2 = sigma|"t2" is "" unit MPa; sigt2 := sigx2/2-hypot(sigx2/2,tauxy2)
    
    val umax = u|"max" is "Ugięcie maksymalne dźwigara" unit mm; umax := leff/250
    val alphak = alpha|"k" is "Współczynnik zależny od rozkładu momentów"; alphak := Quot(5,48)
    val alphap = alpha|"p" is "Współczynnik zależny od trasy cięgna"; alphap := Quot(1,8)
    val u1 = u|"1" is "Ugięcie dźwigara od ciężaru własnego i obciążeń zewnętrznych" unit mm; u1 := alphak*((Mk*(leff^2))/(Eceff*Ics)) - alphap*((Pkinf*zcp*(leff^2))/(Eceff*Ics))
        
    val w1 = w|1 is "Wytężenie dźwigara ze względu na zginanie" acc 0.1 unit SI.percent := 100*(Myd/MRd)
    val w2 = w|2 is "Wytężenie dźwigara ze względu na naprężenia we włóknach dolnych" acc 0.01 unit SI.percent := 100*(sigcpd/(0.7*fcm))
    val w3 = w|3 is "Wytężenie dźwigara ze względu na ścinanie przy podporze" acc 0.1 unit SI.percent := 100*(Vsd/(alphac*VRd22))
    val w4 = w|4 is "Wytężenie dźwigara ze względu na dopuszczalne ugięcie" acc 0.1 unit SI.percent := 100*(u1/umax)
    val w5 = w|5 is "Wytężenie dźwigara ze względu na brak zarysowania" acc 0.1 unit SI.percent := 100*(Mk/Mcr)


    override val document = Document("",
        PredefinedStyles.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z przedmiotu \"Betonowe Konstrukcje Sprężone\". Semestr zimowy 2011/2012."),
        		Section("Autor: Artur Opala, 61315. Prowadzący: dr inż. Aleksy Łodo, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            Section(
                Section(styleTitle,"Sprawdzenie nośności dźwigara dachowego o stałym przekroju z betonu sprężonego w stanie granicznym nośności i użytkowalności."),
                TableOfContents("Spis treści"),
                Section("Normy i literatura",
                	Section(styleComment1,"[1] Norma PN-B-03264 \"Konstrukcje betonowe, żelbetowe i sprężone. Obliczenia statyczne i projektowanie.\"")
                ),
                PageBreak,
                NumSection("Parametry zadania",Evaluate(ld,lr,hd,beton(CLASS),phip,phis)),
                NumSection("Dane materiałów konstrukcyjnych",
                        NumSection("Cięgna sprężające z drutu Y1670C",Evaluate(rhop,ap,dsigp,Fpk,Fp,Fp01k,rop,fpk,fp01k,gammas,fpd)),
                        beton.info/*,
                        zbrojenie.info*/
                ),
                NumSection("Rozmieszczenie cięgien i otulina",Evaluate(dg,dxmin,dymin,cminb,cmindur,cmin,dcdev,cnom,cp)),
                NumSection("Obciążenia"
                        ,NumSection("Obciążenia stałe",Evaluate(gpk)))
                        ,NumSection("Obciążenie śniegiem",Evaluate(HM,sk,mi1,Ce,Ct,qsk))
                        ,NumSection("Obciążenie technologiczne",Evaluate(qtk))
                ,NumSection("Wyznaczenie liczby cięgien sprężających",Evaluate(qd1,Myd1,z1,np1,np2,npxmax,np1x,np1y,np1xr,np2x,np2y,np2xr)),
                NumSection("Geometria przekroju dźwigara",Evaluate(hd,bd,b1,b2,h1,h2,h4,h5,h3,hz1,hz2,hz3)),
                NumSection("Wyznaczenie charakterystyk przekroju dźwigara",Evaluate(Ap1,Ap2,Ap,Acceff,Act,dps,dp,yp1,yp2,Ac,Sc,ycd,ycg,Ic,Wcd,Wcg,alphae,Ap1s,Ap2s,Acs,Scs,yp,yd,yg,Ics,Wcsd,Wcsg))
                ,NumSection("Sprawdzenie stanu granicznego nośności (SGN)"
                    ,NumSection("Rzeczywisty moment zginający od obciążeń zewnętrzych i ciężaru własnego",Evaluate(gk,qd,Myd))
                    ,NumSection("Obliczenie siły sprężającej",Evaluate(sigma0max,P0max),
                            NumSection("Straty doraźne"
                                    ,Evaluate(dPir,Pmo1,zcp,dPc,Pmo2,sigpmo)
                                    /*,AssertionLE("Naprężenia w cięgnach po stratach doraźnych",calc,round(sigpmo,RoundingMode.FLOOR),round(min(0.75*fpk,0.85*fp01k),RoundingMode.CEILING))*/)
                            ,NumSection("Skurcz i pełzanie betonu",Evaluate(betasc,RH,betaRH,epsicsdinf,Uc,ho,betads,epsicsd,epsicsainf,betaas,epsicsa,epsics,pfiRH,betafcm,betat0,phitinf,betaH,betac,phit))
                            ,NumSection("Straty opóźnione",Evaluate(sigcg,sigcpo,sigpcs1,sigpcs2,sigpcs,dPt,Pmt,sigpmt))
                            ,AssertionLE("naprężenia w cięgnach po stratach opóźnionych",sigpmt,0.65*fpk)
                    )
                    ,NumSection("Sprawdzenie naprężeń we włóknach skrajnych przekroju betonowego"
                            ,Evaluate(Pksup,sigcpd,sigcpg)
                            ,AssertionRangeLLE("naprężenia we włóknach dolnych",0,sigcpd,0.7*fcm)
                            ,AssertionRangeLLE("naprężenia we włóknach górnych",0,sigcpg,0.7*fcm))
                    ,NumSection("Sprawdzenie nośności na zginanie"
                            ,Evaluate(sigp2,/*As2,zbrojenie(fyd),*/xeff)
                            ,AssertionG("przekroju teowego",xeff,hz1)
                            ,Evaluate(d1,MRd)
                            ,AssertionLE("stanu granicznego nośności (SGN) ze względu na zginanie",Myd,MRd))
                    ,NumSection("Sprawdzenie nośności na ścinanie"
                            ,Evaluate(Vsd,rhol,k1,sigcp,VRd1,VRd21,alphac,VRd2RED)
                            ,AssertionLE("odcinka ścinanego typu 1",Vsd,VRd1)
                            ,AssertionLE("odcinka ścinanego typu 1",Vsd,VRd2RED)
                            ,Evaluate(VRd22,Asw1,s1)
                            ,AssertionLE("odcinka ścinanego typu 2",Vsd,alphac*VRd22)
                            ,AssertionGE("minimalnego rozstawu strzemion",s1,5 unit cm))
                )
                ,NumSection("Sprawdzenie stanu granicznego użytkowalności (SGU)"
                        ,NumSection("Sprawdzenie możliwości pojawienia się rys prostopadłych do elementu"
                                ,Evaluate(qk,Mk,Pkinf,sigcpk,Mcr)
                                ,AssertionLE("braku zarysowania",Mk,Mcr))
                        ,NumSection("Sprawdzenie możliwości pojawienia się rys ukośnych"
                                ,NumSection("W punkcie 1 na osi obojętnej"
                                        ,Evaluate(lbp,lbpd,Vsk1,Scs1,sigx1,tauxy1,sigt1)
                                        ,AssertionLE("braku zarysowania",-sigt1,fctm))
                                ,NumSection("W punkcie 2 na styku środnika i półki"
                                        ,Evaluate(Scs2,sigx2,tauxy2,sigt2)
                                        ,AssertionLE("braku zarysowania",-sigt2,fctm)))
                        ,NumSection("Sprawdzenie ugięcia dźwigara"
                               ,Evaluate(umax,Eceff,alphak,alphap,u1)
                               ,AssertionLE("ugięcia",u1,umax))
                ),
                NumSection("Podsumowanie", Evaluate(w1,w2,w3,w4,w5))
            ),
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")
        )
    )

}
