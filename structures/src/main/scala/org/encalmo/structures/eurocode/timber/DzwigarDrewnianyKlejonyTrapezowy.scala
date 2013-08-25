package org.encalmo.structures.eurocode.timber

import org.encalmo.expression._
import org.encalmo.document._
import org.encalmo.calculation._
import org.encalmo.structures.Predefined
import org.encalmo.structures.Predefined._
import org.encalmo.structures.CalculationDocument

class DzwigarDrewnianyKlejonyTrapezowy extends CalculationDocument {
    
    import BasicSymbols._
    
    override val name = "kd2-dzwigar"
    
    val LE = Character.LE
    val ARROW = Character.RARROW
    
    val l0 = l|0 is "rozpiętość obliczeniowa" unit "m"
    val h1 = h|1 is "wysokość przekroju (niższa)" unit "m"
    val h2 = h|2 is "wysokość przekroju (wyższa)" unit "m"
    val hm = h|m is "średnia wysokość przekroju" unit "m"
    val b = BasicSymbols.b is "szerokość przekroju" unit "m"
    val alphag = alpha|g is "kąt nachylenia pasa górnego" unit "°"
    val xihb = xi|"h/b" is "stosunek wysokości do szerokości"
    val xilh = xi|"l/h" is "stosunek rozpiętości do wysokości średniej"
    l0 := 20
    b := 0.2
    h1 := 1.0
    h2 := 1.6
    hm := (h1+h2)/2
    alphag := arctan((h2-h1)/l0)
    xihb := h2/b
    xilh := l0/hm
    
    val rhok = rho|"k" is "gęstość charakterystyczna" unit "kg/m3"
    val G0k = G|"0,k" is "ciężar własny dźwigara" unit "kN/m"
    val G1k = G|"1,k" is "obciążenie stałe" unit "kN/m"
    val Qk1 = Q|"k,1" is "obciążenie średniotrwałe" unit "kN/m"
    val Qk2 = Q|"k,2" is "obciążenie krótkotrwałe" unit "kN/m"
    rhok := 430
    G0k := rhok*hm*b*GRAV
    G1k := 1.9
    Qk1 := 2.5
    Qk2 := 2.1
    
    val gamG1 = gamma|"G,1" is "częściowy współczynnik dla oddziaływania stałego"
    val gamQ1 = gamma|"Q,1" is "częściowy współczynnik dla wiodącego oddziaływania zmiennego"
    val gamQi = gamma|"Q,i" is "częściowy współczynnik dla towarzyszących oddziaływań zmiennych"
    val psi0i = Psi|"0,i" is "współczynnik dla wartości kombinacyjnej oddziaływania zmiennego"
    gamG1 := 1.35
    gamQ1 := 1.5
    gamQi := 1.5
    psi0i := 0.6
    
    val Gd = G|d is "obciążenie stałe" unit "kN/m"
    val Qd1 = Q|"d,1" is "obciążenie średniotrwałe" unit "kN/m"
    val Qd2 = Q|"d,2" is "obciążenie krótkotrwałe" unit "kN/m"
    Gd := gamG1*G1k
    Qd1 := gamQ1*Qk1
    Qd2 := gamQ1*Qk2
    
    val qd = q|d is "obciążenie obliczeniowe dla kombinacji podstawowej" unit "kN/m"
    qd := gamG1*(G0k+G1k)+gamQ1*Qk1+psi0i*gamQi*Qk2
    
    val kmod = k|"mod" is """współczynnik modyfikujący efekt czasu trwania obciążenia i zmiany wilgotności materiału,
 przyjęty dla 2 klasy użytkowania i oddziaływania krótkotrwałego z Tab. 3.1 [1]"""
    kmod := 0.9
    
    val gamM = gamma|M is """częściowy współczynnik bezpieczeństwa właściwości materiału, uwzględniający także
niedoskonałości modelowania i odchyłki wymiarowe, przyjęty dla drewna klejonego warstwowo z Tab. 2.3 [1]"""
    gamM := 1.25
    
    val fmk = f|"m,k" is "wytrzymałość charakterystyczna na zginanie" unit "MPa"; fmk := 36
    val ft0k = f|"t,0,k" is "wytrzymałość charakterystyczna na rozciąganie wzdłuż włókien" unit "MPa"; ft0k := 22.5
    val ft90k = f|"t,90,k" is "wytrzymałość charakterystyczna na rozciąganie w poprzek włókien" unit "MPa"; ft90k := 0.5
    val fc0k = f|"c,0,k" is "wytrzymałość charakterystyczna na ściskanie wzdłuż włókien" unit "MPa"; fc0k := 29
    val fc90k = f|"c,90,k" is "wytrzymałość charakterystyczna na ściskanie w poprzek włókien" unit "MPa"; fc90k := 3.3
    val fvk = f|"v,k" is "wytrzymałość charakterystyczna na ścinanie" unit "MPa"; fvk := 3.8
    val E0mean = E|"0,mean" is "średni moduł sprężystości wzdłuż włókien" unit "GPa"; E0mean := 14.7
    val E005 = E|"005,mean" is "5 % kwantyl modułu sprężystości" unit "GPa"; E005 := 11.9
    val E90mean = E|"90,mean" is "średni moduł sprężystości w poprzek włókien" unit "GPa"; E90mean := 0.46
    val Gmean = G|"mean" is "średni moduł odkształcenia postaciowego" unit "GPa"; Gmean := 0.85
    
    val fmd = f|"md" is "wytrzymałość obliczeniowa na zginanie" unit "MPa"; fmd := kmod*fmk/gamM
    val ft0d = f|"t,0d" is "wytrzymałość obliczeniowa na rozciąganie wzdłuż włókien" unit "MPa"; ft0d := kmod*ft0k/gamM
    val ft90d = f|"t,90d" is "wytrzymałość obliczeniowa na rozciąganie w poprzek włókien" unit "MPa"; ft90d := kmod*ft90k/gamM
    val fc0d = f|"c,0d" is "wytrzymałość obliczeniowa na ściskanie wzdłuż włókien" unit "MPa"; fc0d := kmod*fc0k/gamM
    val fc90d = f|"c,90d" is "wytrzymałość obliczeniowa na ściskanie w poprzek włókien" unit "MPa"; fc90d := kmod*fc90k/gamM
    val fvd = f|"vd" is "wytrzymałość obliczeniowa na ścinanie" unit "MPa"; fvd := kmod*fvk/gamM
    val E0d = E|"0,d" is "obliczeniowy moduł sprężystości wzdłuż włókien" unit "MPa"; E0d := E0mean/gamM
    val E90d = E|"90,d" is "obliczeniowy moduł sprężystości w poprzek włókien" unit "MPa"; E90d := E90mean/gamM
    val Gdd = G|"dd" is "obliczeniowy moduł odkształcenia postaciowego" unit "MPa"; Gdd := Gmean/gamM
    
    val sigmam0d = sigma|"m,0,d" is "obliczeniowe naprężenie zginające" unit "MPa"
    val xmax = x|sigma is "miejsce wystąpienia największych naprężeń normalnych w przęśle liczone od podpory A" unit "m"
    xmax := l0*h1/(h1+h2)
    val hmax = h|sigma is "wysokość przekroju w miejscu wystąpienia największych naprężeń normalnych" unit "m"
    hmax := xmax/l0*(h2-h1)+h1
    
    val RA = R|A is "reakcja na podporze A" unit "kN"
    RA := qd*l0/2
    val My = M|"y" is "moment zginający w miejscu wystąpienia największych naprężeń normalnych" unit "kNm"
    My := RA*xmax-qd*(xmax^2)/2
    val Wy = W|"y" is "wskaźnik zginania w miejscu wystąpienia największych naprężeń normalnych" unit "cm3" acc 1
    Wy := b*(hmax^2)/6
    sigmam0d := My/Wy
    
    val kmalpha = k|"m,&alpha;" is "współczynnik redukcyjny, gdy krawędź ściskana jest nachylona"
    kmalpha := 1/(sqrt(1+((fmd/(1.5*fvd)*tan(alphag))^2)+((fmd/(1.5*fc90d)*((tan(alphag))^2))^2)))
    val sigmamalphad = sigma|"m,&alpha;,d" is "obliczeniowe naprężenie zginające pod kątem do włókien na krawędzi nachylonej ściskanej" unit "MPa"
    sigmamalphad := sigmam0d
    val fmalphad = f|"m,&alpha;,d" is "zredukowana wytrzymałość na zginanie na krawędzi nachylonej ściskanej" unit "MPa"
    fmalphad := kmalpha*fmd
    
    val leff = Symbol("&#x2113;")|"eff" is """efektywna długość belki zależna od warunków podparcia i układu obciążenia, 
zgodnie z Tablicą 6.1 [1] dla obciążenia równomiernie rozłożonego i przyłożonego do pasa górnego""" unit "m"
    leff := 0.9*l0+2*hm
    val sigmamcrit = sigma|"m,crit" is "Naprężenie krytyczne przy zginaniu wg wzoru 6.32 [1]" unit "MPa"
    sigmamcrit := ((0.78*(b^2))/(hm*leff))*E005
    val lambdarel = lambda|"rel" is "smukłość względna przy zginaniu"
    lambdarel := sqrt(fmk/sigmamcrit)
    val kcritm = k|"crit,m" is "współczynnik uwzględniający redukcję wytrzymałości ze względu na zwichrowanie elementu"
    kcritm := 1 or (InRangeLLE(0.75,lambdarel,1.4) thenUse (1.56-(0.75*lambdarel))) or (GreaterThan(lambdarel,1.4) thenUse (1/(lambdarel^2)))
    val fmcritd = f|"m,crit,d" is "zredukowana wytrzymałość na zginanie ze względu na zwichrzenie" unit "MPa"
    fmcritd := kcritm*fmd
    
    val taud = tau|d is "obliczeniowe naprężenie ścinające" unit "MPa"
    val Vd = V|d is "maksymalna obliczeniowa siła tnąca" unit "kN"
    val beff = b|"eff" is "efektywna szerokość elementu uwzględniająca wpływ pęknięć" unit "m"
    val kcr = k|"cr" is "współczynnik uwzględniający wpływ pęknięć"
    kcr := 0.67
    beff := kcr*b
    Vd := RA
    taud := (1.5*Vd)/(beff*h1)
    
    val lpod = l|"pod" is "długość oparcia na podporze" unit "m"
    val Aef = A|"ef" is "efektywne pole docisku" unit "m2"
    val Fc90d = F|"c,90,d" is "obliczeniowa siła ściskająca w poprzek włókien" unit "kN"
    val kc90 = k|"c,90" is "współczynnik uwzględniający rozkład obciążenia, możliwość powstania pęknięć oraz stopień odkształcenia przy ściskaniu"
    Fc90d := RA
    lpod := 0.4
    Aef := b*lpod
    kc90 := 1.0
    val sigmac90d = sigma|"c,90,d" is "obliczeniowe naprężenie ściskające w poprzek włókien, w efektywnym polu docisku" unit "MPa"
    sigmac90d := Fc90d/Aef
    
    val wmax = w|"dop" is "dopuszczalne ugięcie końcowe" unit SI.mm acc 1
    val wfin = w|"fin" is "ugięcie końcowe" unit SI.mm acc 1
    val ufinG = u|"fin,G" is "ugięcie końcowe wywołane obciążeniem stałym G" unit SI.mm acc 1
    val ufinQ1 = u|"fin,Q1" is "ugięcie końcowe wywołane obciążeniem zmiennym Q1" unit SI.mm acc 1
    val ufinQi = u|"fin,Qi" is "ugięcie końcowe wywołane obciążeniem zmiennym Qi" unit SI.mm acc 1
    val winst = w|"inst" is "ugięcie chwilowe" unit SI.mm acc 1
    val uinstG = u|"inst,G" is "ugięcie chwilowe wywołane obciążeniem stałym G" unit SI.mm acc 1
    val uinstQ1 = u|"inst,Q1" is "ugięcie chwilowe wywołane obciążeniem zmiennym Q1" unit SI.mm acc 1
    val uinstQi = u|"inst,Qi" is "ugięcie chwilowe wywołane obciążeniem zmiennym Qi" unit SI.mm acc 1
    val wcreep = w|"creep" is "ugięcie wywołane pełzaniem" unit SI.mm acc 1
    val wnetfin = w|"kNet,fin" is "końcowe ugięcie wynikowe" unit SI.mm acc 1
    val kdef = k|"def" is "współczynnik odkształceń, zgodnie z tablicą 3.2 w [1]"
    val psi21 = psi|"2,1" is "współczynnik obciążenia quasi-stałego w kombinacji obciążeń, wg Tablicy A.1.1 w [3]"
    val umG = u|"m,G" is "ugięcie belki swobodnie podpartej wywołane momentem zginającym od obciążeń stałych" unit SI.mm acc 1
    val umQ1 = u|"m,Q,1" is "ugięcie belki swobodnie podpartej wywołane momentem zginającym od obciążeń zmiennych Q1" unit SI.mm acc 1
    val umQi = u|"m,Q,i" is "ugięcie belki swobodnie podpartej wywołane momentem zginającym od obciążeń zmiennych Qi" unit SI.mm acc 1
    val Iy = I|"y" is "moment bezwładności w środku rozpiętości dźwigara" unit SI.cm4 acc 1
    wmax := l0/300
    Iy := (b*(hm^3))/12
    kdef := 0.8
    psi21 := 0.2
    umG := 5d/384*((G0k+G1k)*(l0^4)/(E0mean*Iy))
    uinstG := umG * ((1+19.2*((hm/l0)^2))/(0.15+0.85*(h1/hm)))
    umQ1 := 5d/384*((Qk1)*(l0^4)/(E0mean*Iy))
    uinstQ1 := umQ1 * ((1+19.2*((hm/l0)^2))/(0.15+0.85*(h1/hm)))
    umQi := 5d/384*((Qk2)*(l0^4)/(E0mean*Iy))
    uinstQi := umQi * ((1+19.2*((hm/l0)^2))/(0.15+0.85*(h1/hm)))
    ufinG := uinstG*(1+kdef)
    ufinQ1 := uinstQ1*(1+psi21*kdef)
    ufinQi := uinstQi*(psi0i+psi21*kdef)
    wfin := ufinG+ufinQ1+ufinQi
    
    val psi11 = psi|"1,1" is "współczynnik dla wartości częstej oddziaływania zmiennego"
    val EdA = E|"d,A" is "kombinacja oddziaływań w warunkach pożarowych" unit "kN/m"
    psi11 := 0.2
    EdA := (G0k+G1k)+psi11*Qk1
    
    val t = BasicSymbols.t is "czas oddziaływania pożaru" unit "min"
    val betan = beta|n is """obliczeniowa prędkość zwęglania z uwzględnieniem wpływu na zaokrąglenia narożników oraz szczelin. 
Z tablicy 3.1 w PN-EN 1995-1-2:2008 dla klejonego warstwowo drewna iglastego.""" unit SI.mmpermin
    betan := 0.7
    val dcharn = d|"char,n" is "hipotetyczna głębokość zwęglenia, uwzględniająca wpływ zaokrągleń narożników." unit SI.mm
    t := 30
    dcharn := betan*t
    val Ar = A|r is "powierzchia przekroju pozostałego po 30 min pożaru" unit "m2"
    val pr = p|r is "obwód przekroju pozostałego po 30 min pożaru" unit "m"
    val kfi = k|"fi" is "współczynnik uwzględniający zwiększoną wytrzymałość i sztywność drewna na podstawie tablicy 2.1"
    val kmodmfi = k|"mod,m,fi" is "współczynnik modyfikujący wytrzymałość na zginanie w warunkach pożarowych"
    Ar := (b-(2*dcharn))*(hmax-(2*dcharn))
    pr := 2*(b-(2*dcharn))+2*(hmax-(2*dcharn))
    kfi := 1.15
    kmodmfi := (1-(pr/(200*Ar))).nounit
    val kmodEfi = k|"mod,E,fi" is "współczynnik modyfikujący moduł sprężystości w warunkach pożarowych"
    kmodEfi := (1-(pr/(333*Ar))).nounit
    val gammaMfi = gamma|"M,fi" is "częściowy współczynnik materiałowy w warunkach pożarowych"
    gammaMfi := 1.0
    val fmdfi = f|"m,d,fi" is "wytrzymałość obliczeniowa na zginanie w warunkach pożarowych"
    fmdfi := kmodmfi*((kfi*fmk)/gammaMfi)
    val Edfi = E|"d,fi" is "obliczeniowy moduł sprężystości w warunkach pożarowych" unit "GPa"
    Edfi := kmodEfi*((kfi*E005)/gammaMfi)
    
    val lefffi = Symbol("&#x2113;")|"eff,fi" is """efektywna długość belki zależna od warunków podparcia i układu obciążenia""" unit "m"
    lefffi := 0.9*l0+2*(hm-2*dcharn)
    val sigmamcritfi = sigma|"m,crit,fi" is "Naprężenie krytyczne przy zginaniu wg wzoru 6.32 [1]" unit "MPa"
    sigmamcritfi := ((0.78*((b-2*dcharn)^2))/((hm-2*dcharn)*lefffi))*Edfi
    val lambdarelfi = lambda|"rel,fi" is "smukłość względna przy zginaniu"
    lambdarelfi := sqrt(fmdfi/sigmamcritfi)
    val kcritfi = k|"crit,fi" is "współczynnik uwzględniający redukcję wytrzymałości ze względu na zwichrowanie elementu"
    kcritfi := 1 or (InRangeLLE(0.75,lambdarelfi,1.4) thenUse (1.56-(0.75*lambdarelfi))) or (GreaterThan(lambdarelfi,1.4) thenUse (1/(lambdarelfi^2)))
    val fmcritfi = f|"m,crit,fi" is "zredukowana wytrzymałość na zginanie ze względu na zwichrzenie" unit "MPa"
    fmcritfi := kcritfi*fmdfi
    
    val sigmam0fi = sigma|"m,0,fi" is "obliczeniowe naprężenie zginające" unit "MPa"
    val xmaxfi = x|(sigma+",fi") is "miejsce wystąpienia największych naprężeń normalnych w przęśle liczone od podpory A" unit "m"
    xmaxfi := l0*(h1-2*dcharn)/((h1-2*dcharn)+(h2-2*dcharn))
    val hmaxfi = h|(sigma+",fi") is "wysokość przekroju w miejscu wystąpienia największych naprężeń normalnych" unit "m"
    hmaxfi := xmaxfi/l0*((h2-2*dcharn)-(h1-2*dcharn))+(h1-2*dcharn)
    
    val RAfi = R|"A,fi" is "reakcja na podporze A" unit "kN"
    RAfi := EdA*l0/2
    val Myfi = M|"y,fi" is "moment zginający w miejscu wystąpienia największych naprężeń normalnych" unit "kNm"
    Myfi := RAfi*xmaxfi-EdA*(xmaxfi^2)/2
    val Wyfi = W|"y,fi" is "wskaźnik zginania w miejscu wystąpienia największych naprężeń normalnych" unit "cm3" acc 1
    Wyfi := (b-2*dcharn)*(hmaxfi^2)/6
    sigmam0fi := Myfi/Wyfi
    
    val deff = d|"eff" is "efektywna głębokość zwęglenia" unit "mm"
    val k0 = k|"0" is "Na podstawie tablicy 4.1"
    val d0 = d|"0" is "zgodnie z punktem 4.2.2" unit SI.mm
    k0 := 1.0
    d0 := 7
    deff := dcharn+(k0*d0)
    val kmodmfi2 = k|"mod,m,fi,2" is "współczynnik modyfikujący wytrzymałość na zginanie w warunkach pożarowych"
    val kmodEfi2 = k|"mod,E,fi,2" is "współczynnik modyfikujący moduł sprężystości w warunkach pożarowych"
    kmodmfi2 := 1
    kmodEfi2 := 1
    val fmdfi2 = f|"m,d,fi,2" is "wytrzymałość obliczeniowa na zginanie w warunkach pożarowych" unit "MPa"
    fmdfi2 := kmodmfi2*((kfi*fmk)/gammaMfi)
    val Edfi2 = E|"d,fi,2" is "obliczeniowy moduł sprężystości w warunkach pożarowych" unit "GPa"
    Edfi2 := kmodEfi2*((kfi*E005)/gammaMfi)
    
    val lefffi2 = Symbol("&#x2113;")|"eff,fi,2" is """efektywna długość belki zależna od warunków podparcia i układu obciążenia""" unit "m"
    lefffi2 := 0.9*l0+2*(hm-2*deff)
    val sigmamcritfi2 = sigma|"m,crit,fi,2" is "Naprężenie krytyczne przy zginaniu wg wzoru 6.32 [1]" unit "MPa"
    sigmamcritfi2 := ((0.78*((b-2*deff)^2))/((hm-2*deff)*lefffi2))*Edfi2
    val lambdarelfi2 = lambda|"rel,fi,2" is "smukłość względna przy zginaniu"
    lambdarelfi2 := sqrt(fmdfi2/sigmamcritfi2)
    val kcritfi2 = k|"crit,fi,2" is "współczynnik uwzględniający redukcję wytrzymałości ze względu na zwichrowanie elementu"
    kcritfi2 := 1 or (InRangeLLE(0.75,lambdarelfi2,1.4) thenUse (1.56-(0.75*lambdarelfi2))) or (GreaterThan(lambdarelfi2,1.4) thenUse (1/(lambdarelfi2^2)))
    val fmcritfi2 = f|"m,crit,fi,2" is "zredukowana wytrzymałość na zginanie ze względu na zwichrzenie" unit "MPa"
    fmcritfi2 := kcritfi2*fmdfi2
    
    val sigmam0fi2 = sigma|"m,0,fi,2" is "obliczeniowe naprężenie zginające" unit "MPa"
    val xmaxfi2 = x|(sigma+",fi,2") is "miejsce wystąpienia największych naprężeń normalnych w przęśle liczone od podpory A" unit "m"
    xmaxfi2 := l0*(h1-2*deff)/((h1-2*deff)+(h2-2*deff))
    val hmaxfi2 = h|(sigma+",fi,2") is "wysokość przekroju w miejscu wystąpienia największych naprężeń normalnych" unit "m"
    hmaxfi2 := xmaxfi2/l0*((h2-2*deff)-(h1-2*deff))+(h1-2*deff)
    
    val RAfi2 = R|"A,fi,2" is "reakcja na podporze A" unit "kN"
    RAfi2 := EdA*l0/2
    val Myfi2 = M|"y,fi,2" is "moment zginający w miejscu wystąpienia największych naprężeń normalnych" unit "kNm"
    Myfi2 := RAfi*xmaxfi2-EdA*(xmaxfi2^2)/2
    val Wyfi2 = W|"y,fi,2" is "wskaźnik zginania w miejscu wystąpienia największych naprężeń normalnych" unit "cm3" acc 1
    Wyfi2 := (b-2*deff)*(hmaxfi2^2)/6
    sigmam0fi2 := Myfi2/Wyfi2
      
    override val doc = Document("",
        Predefined.stylesConfig,
        Chapter("",
            Section("Ćwiczenie projektowe nr 2 z \"Konstrukcji Drewnianych\". Autor: Artur Opala"),
            Section(""),
            Section(styleTitle,"Dźwigar trapezowy z drewna klejonego warstwowo."),
            NumSection("Zadanie",
                Section(styleComment,"Drewno klejone warstwowo kombinowane klasy GL36c."),
                Section(styleComment,"Konstrukcja w 2 klasie użytkowania wg normy [1] pkt. 2.3.1.3."),
                Section(styleComment,"Klasa odporności ogniowej R30 (konstrukcja dachu)."),
                NumSection("kNormy i literatura",
                	Section(styleComment," [1] Norma PN-EN 1995-1-1:2010 \"Eurokod 5. Projektowanie konstrukcji drewnianych. Część 1-1: Postanowienia ogólne. Reguły ogólne i reguły dotyczące budynków\""),
                	Section(styleComment," [2] Norma PN-EN 1194:2000")
                )
            ),
            NumSection("Dane do obliczeń",
                NumSection("Geometria dźwigara",Evaluate(l0,h1,h2,hm,b,alphag,xihb,xilh)),
                NumSection("Obciążenia charakterystyczne",Evaluate(rhok,G0k,G1k,Qk1,Qk2)),
                NumSection("Współczynniki częściowe dla oddziaływań",Evaluate(gamG1,gamQ1,gamQi,psi0i)),
                NumSection("Kombinacje obciążeń",Evaluate(qd)),
                NumSection("Współczynnik ",Symb(kmod),Evaluate(kmod)),
                NumSection("Współczynnik materiałowy ",Symb(gamM),Evaluate(gamM)),
                NumSection("Wytrzymałości i moduły charakterystyczne wg PN-EN 1194:2000",
                        Evaluate(fmk,ft0k,ft90k,fc0k,fc90k,fvk,E0mean,E005,E90mean,Gmean)),
                NumSection("Wytrzymałości i moduły obliczeniowe",
                        Evaluate(fmd,ft0d,ft90d,fc0d,fc90d,fvd,E0d,E90d,Gdd))
            ),
            NumSection("Sprawdzenie stanu granicznego nośności SGN wg PN-EN 1995-1-1",
                NumSection("Sprawdzenie naprężeń maksymalnych od zginania",
                        Evaluate(xmax,hmax,RA,My,Wy,sigmam0d),
                        AssertionLE("6.11", this, sigmam0d, fmd)),
                NumSection("Uwzględnienie zmiennego przekroju dźwigara",
                        Evaluate(kmalpha),
                        Evaluate(fmalphad),
                        AssertionLE("6.38", this, sigmam0d, fmalphad)),
                NumSection("Sprawdzenie stateczności przekroju",
                        Evaluate(leff,sigmamcrit,lambdarel),
                        Evaluate(kcritm),
                        Evaluate(fmcritd),
                        AssertionLE("6.33", this, sigmam0d, fmcritd)),
                NumSection("Sprawdzenie nośności na ścinanie",
                        Evaluate(Vd,kcr,beff,taud),
                        AssertionLE("6.13", this, taud, fvd)),
                NumSection("Sprawdzenie nośności na docisk w poprzek włókien na podporze",
                        Evaluate(lpod,Aef,Fc90d,sigmac90d,kc90),
                        AssertionLE("6.3", this, sigmac90d, kc90*fc90d))
            ),
            NumSection("Sprawdzenie stanu granicznego użytkowania SGU wg PN-EN 1995-1-1",
                NumSection("Obliczenie ugięć",
                        Evaluate(wmax,kdef,psi21,Iy,umG,uinstG,umQ1,uinstQ1,umQi,uinstQi,ufinG,ufinQ1,ufinQi,wfin),
                        AssertionLE("SGU", this, wfin, wmax))
            ),
            NumSection("Sprawdzenie nośności w warunkach pożarowych wg PN-EN 1995-1-2",
                Section(style1,"Oczekiwana klasa odporności ogniowej R30."),
                NumSection("Dane wejściowe",Evaluate(psi11,EdA)),
                NumSection("Obliczenie nośności metodą zredukowanych właściwości",
                        Evaluate(t,betan,dcharn,Ar,pr,kfi,kmodmfi,kmodEfi,gammaMfi,fmdfi,Edfi,lefffi,sigmamcritfi,lambdarelfi,kcritfi,fmcritfi),
                        Evaluate(xmaxfi,hmaxfi,RAfi,Myfi,Wyfi,sigmam0fi),
                        AssertionLE("SGN w trakcie pożaru", this, sigmam0fi, kcritfi*fmdfi)),
                NumSection("Obliczenie nośności metodą zredukowanego przekroju",
                        Evaluate(k0,d0,deff,kmodmfi2,kmodEfi2,gammaMfi,fmdfi2,Edfi2,lefffi2,sigmamcritfi2,lambdarelfi2,kcritfi2,fmcritfi2),
                        Evaluate(xmaxfi2,hmaxfi2,RAfi2,Myfi2,Wyfi2,sigmam0fi2),
                        AssertionLE("SGN w trakcie pożaru", this, sigmam0fi2, kcritfi2*fmdfi2))
            ),
			Section(style1.marginTop(30),""),
			Section("Koniec obliczeń.")
        )
    )

}
