package org.encalmo.structures.eurocode.timber

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.document.StylesConfigSymbols._
import org.encalmo.structures.Predefined
import org.encalmo.structures.Predefined._

class DzwigarDrewnianyKlejonyTrapezowy {
    
    import BasicSymbols._
    
    val LE = Character.LE
    val ARROW = Character.RARROW
    
    val calc = Calculation("1")
    
    val l0 = l|0 is "rozpiętość obliczeniowa" unit "m"
    val h1 = h|1 is "wysokość przekroju (niższa)" unit "m"
    val h2 = h|2 is "wysokość przekroju (wyższa)" unit "m"
    val hm = h|m is "średnia wysokość przekroju" unit "m"
    val b = BasicSymbols.b is "szerokość przekroju" unit "m"
    val alphag = alpha|g is "kąt nachylenia pasa górnego" unit "°"
    val xihb = xi|"h/b" is "stosunek wysokości do szerokości"
    val xilh = xi|"l/h" is "stosunek rozpiętości do wysokości średniej"
    calc(l0) = 20
    calc(b) = 0.2
    calc(h1) = 1.0
    calc(h2) = 1.6
    calc(hm) = (h1+h2)/2
    calc(alphag) = arctan((h2-h1)/l0)
    calc(xihb) = h2/b
    calc(xilh) = l0/hm
    
    val rhok = rho|"k" is "gęstość charakterystyczna" unit "kg/m³"
    val G0k = G|"0,k" is "ciężar własny dźwigara" unit "N/m"
    val G1k = G|"1,k" is "obciążenie stałe" unit "N/m"
    val Qk1 = Q|"k,1" is "obciążenie średniotrwałe" unit "N/m"
    val Qk2 = Q|"k,2" is "obciążenie krótkotrwałe" unit "N/m"
    calc(rhok) = 430
    calc(G0k) = rhok*hm*b*10
    calc(G1k) = 1900
    calc(Qk1) = 2500
    calc(Qk2) = 2100
    
    val gamG1 = gamma|"G,1" is "częściowy współczynnik dla oddziaływania stałego"
    val gamQ1 = gamma|"Q,1" is "częściowy współczynnik dla wiodącego oddziaływania zmiennego"
    val gamQi = gamma|"Q,i" is "częściowy współczynnik dla towarzyszących oddziaływań zmiennych"
    val psi0i = Psi|"0,i" is "współczynnik dla wartości kombinacyjnej oddziaływania zmiennego"
    calc(gamG1) = 1.35
    calc(gamQ1) = 1.5
    calc(gamQi) = 1.5
    calc(psi0i) = 0.6
    
    val Gd = G|d is "obciążenie stałe" unit "N/m"
    val Qd1 = Q|"d,1" is "obciążenie średniotrwałe" unit "N/m"
    val Qd2 = Q|"d,2" is "obciążenie krótkotrwałe" unit "N/m"
    calc(Gd) = gamG1*G1k
    calc(Qd1) = gamQ1*Qk1
    calc(Qd2) = gamQ1*Qk2
    
    val qd = q|d is "obciążenie obliczeniowe dla kombinacji podstawowej" unit "N/m"
    calc(qd) = gamG1*(G0k+G1k)+gamQ1*Qk1+psi0i*gamQi*Qk2
    
    val kmod = k|"mod" is """współczynnik modyfikujący efekt czasu trwania obciążenia i zmiany wilgotności materiału,
 przyjęty dla 2 klasy użytkowania i oddziaływania krótkotrwałego z Tab. 3.1 [1]"""
    calc(kmod) = 0.9
    
    val gamM = gamma|M is """częściowy współczynnik bezpieczeństwa właściwości materiału, uwzględniający także
niedoskonałości modelowania i odchyłki wymiarowe, przyjęty dla drewna klejonego warstwowo z Tab. 2.3 [1]"""
    calc(gamM) = 1.25
    
    val fmk = f|"m,k" is "wytrzymałość charakterystyczna na zginanie" unit "Pa"; calc(fmk) = 36000000
    val ft0k = f|"t,0,k" is "wytrzymałość charakterystyczna na rozciąganie wzdłuż włókien" unit "Pa"; calc(ft0k) = 22500000
    val ft90k = f|"t,90,k" is "wytrzymałość charakterystyczna na rozciąganie w poprzek włókien" unit "Pa"; calc(ft90k) = 500000
    val fc0k = f|"c,0,k" is "wytrzymałość charakterystyczna na ściskanie wzdłuż włókien" unit "Pa"; calc(fc0k) = 29000000
    val fc90k = f|"c,90,k" is "wytrzymałość charakterystyczna na ściskanie w poprzek włókien" unit "Pa"; calc(fc90k) = 3300000
    val fvk = f|"v,k" is "wytrzymałość charakterystyczna na ścinanie" unit "Pa"; calc(fvk) = 3800000
    val E0mean = E|"0,mean" is "średni moduł sprężystości wzdłuż włókien" unit "Pa"; calc(E0mean) = 14700000000l
    val E005 = E|"0,mean" is "5 % kwantyl modułu sprężystości" unit "Pa"; calc(E005) = 11900000000l
    val E90mean = E|"90,mean" is "średni moduł sprężystości w poprzek włókien" unit "Pa"; calc(E90mean) = 460000000
    val Gmean = G|"mean" is "średni moduł odkształcenia postaciowego" unit "Pa"; calc(Gmean) = 850000000
    
    val fmd = f|"md" is "wytrzymałość obliczeniowa na zginanie" unit "Pa"; calc(fmd) = kmod*fmk/gamM
    val ft0d = f|"t,0d" is "wytrzymałość obliczeniowa na rozciąganie wzdłuż włókien" unit "Pa"; calc(ft0d) = kmod*ft0k/gamM
    val ft90d = f|"t,90d" is "wytrzymałość obliczeniowa na rozciąganie w poprzek włókien" unit "Pa"; calc(ft90d) = kmod*ft90k/gamM
    val fc0d = f|"c,0d" is "wytrzymałość obliczeniowa na ściskanie wzdłuż włókien" unit "Pa"; calc(fc0d) = kmod*fc0k/gamM
    val fc90d = f|"c,90d" is "wytrzymałość obliczeniowa na ściskanie w poprzek włókien" unit "Pa"; calc(fc90d) = kmod*fc90k/gamM
    val fvd = f|"vd" is "wytrzymałość obliczeniowa na ścinanie" unit "Pa"; calc(fvd) = kmod*fvk/gamM
    val E0d = E|"0,d" is "obliczeniowy moduł sprężystości wzdłuż włókien" unit "Pa"; calc(E0d) = E0mean/gamM
    val E90d = E|"90,d" is "obliczeniowy moduł sprężystości w poprzek włókien" unit "Pa"; calc(E90d) = E90mean/gamM
    val Gdd = G|"d" is "obliczeniowy moduł odkształcenia postaciowego" unit "Pa"; calc(Gdd) = Gmean/gamM
    
    val sigmam0d = sigma|"m,0,d" is "obliczeniowe naprężenie zginające" unit "Pa"
    val xmax = x|sigma is "miejsce wystąpienia największych naprężeń normalnych w przęśle liczone od podpory A" unit "m"
    calc(xmax) = l0*h1/(h1+h2)
    val hmax = h|sigma is "wysokość przekroju w miejscu wystąpienia największych naprężeń normalnych" unit "m"
    calc(hmax) = xmax/l0*(h2-h1)+h1
    
    val RA = R|A is "reakcja na podporze A" unit "N"
    calc(RA) = qd*l0/2
    val My = M|"y" is "moment zginający w miejscu wystąpienia największych naprężeń normalnych" unit "Nm"
    calc(My) = RA*xmax-qd*(xmax^2)/2
    val Wy = W|"y" is "wskaźnik zginania w miejscu wystąpienia największych naprężeń normalnych" unit "m³"
    calc(Wy) = b*(hmax^2)/6
    calc(sigmam0d) = My/Wy
    
    val kmalpha = k|"m,&alpha;" is "współczynnik redukcyjny, gdy krawędź ściskana jest nachylona"
    calc(kmalpha) = 1/(sqrt(1+((fmd/(1.5*fvd)*tan(alphag))^2)+((fmd/(1.5*fc90d)*((tan(alphag))^2))^2)))
    val sigmamalphad = sigma|"m,&alpha;,d" is "obliczeniowe naprężenie zginające pod kątem do włókien na krawędzi nachylonej ściskanej" unit "Pa"
    calc(sigmamalphad) = sigmam0d
    val fmalphad = f|"m,&alpha;,d" is "zredukowana wytrzymałość na zginanie na krawędzi nachylonej ściskanej" unit "Pa"
    calc(fmalphad) = kmalpha*fmd
    
    val leff = Symbol("&#x2113;")|"eff" is """efektywna długość belki zależna od warunków podparcia i układu obciążenia, 
zgodnie z Tablicą 6.1 [1] dla obciążenia równomiernie rozłożonego i przyłożonego do pasa górnego""" unit "m"
    calc(leff) = 0.9*l0+2*hm
    val sigmamcrit = sigma|"m,crit" is "naprężenie krytyczne przy zginaniu wg wzoru 6.32 [1]" unit "Pa"
    calc(sigmamcrit) = ((0.78*(b^2))/(hm*leff))*E005
    val lambdarel = lambda|"rel" is "smukłość względna przy zginaniu"
    calc(lambdarel) = sqrt(fmk/sigmamcrit)
    val kcritm = k|"crit,m" is "współczynnik uwzględniający redukcję wytrzymałości ze względu na zwichrowanie elementu"
    calc(kcritm) = 1 or (InRangeLLE(0.75,lambdarel,1.4) then (1.56-(0.75*lambdarel))) or (GreaterThan(lambdarel,1.4) then (1/(lambdarel^2)))
    val fmcritd = f|"m,crit,d" is "zredukowana wytrzymałość na zginanie ze względu na zwichrzenie" unit "Pa"
    calc(fmcritd) = kcritm*fmd
    
    val taud = tau|d is "obliczeniowe naprężenie ścinające" unit "Pa"
    val Vd = V|d is "maksymalna obliczeniowa siła tnąca" unit "N"
    val beff = b|"eff" is "efektywna szerokość elementu uwzględniająca wpływ pęknięć" unit "m"
    val kcr = k|"cr" is "współczynnik uwzględniający wpływ pęknięć"
    calc(kcr) = 0.67
    calc(beff) = kcr*b
    calc(Vd) = RA
    calc(taud) = (1.5*Vd)/(beff*h1)
    
    val lpod = l|"pod" is "długość oparcia na podporze" unit "m"
    val Aef = A|"ef" is "efektywne pole docisku" unit "m²"
    val Fc90d = F|"c,90,d" is "obliczeniowa siła ściskająca w poprzek włókien" unit "N"
    val kc90 = k|"c,90" is "współczynnik uwzględniający rozkład obciążenia, możliwość powstania pęknięć oraz stopień odkształcenia przy ściskaniu"
    calc(Fc90d) = RA
    calc(lpod) = 0.4
    calc(Aef) = b*lpod
    calc(kc90) = 1.0
    val sigmac90d = sigma|"c,90,d" is "obliczeniowe naprężenie ściskające w poprzek włókien, w efektywnym polu docisku" unit "Pa"
    calc(sigmac90d) = Fc90d/Aef
    
    val wmax = w|"dop" is "dopuszczalne ugięcie końcowe" unit "m"
    val wfin = w|"fin" is "ugięcie końcowe" unit "m"
    val ufinG = u|"fin,G" is "ugięcie końcowe wywołane obciążeniem stałym G" unit "m"
    val ufinQ1 = u|"fin,Q1" is "ugięcie końcowe wywołane obciążeniem zmiennym Q1" unit "m"
    val ufinQi = u|"fin,Qi" is "ugięcie końcowe wywołane obciążeniem zmiennym Qi" unit "m"
    val winst = w|"inst" is "ugięcie chwilowe" unit "m"
    val uinstG = u|"inst,G" is "ugięcie chwilowe wywołane obciążeniem stałym G" unit "m"
    val uinstQ1 = u|"inst,Q1" is "ugięcie chwilowe wywołane obciążeniem zmiennym Q1" unit "m"
    val uinstQi = u|"inst,Qi" is "ugięcie chwilowe wywołane obciążeniem zmiennym Qi" unit "m"
    val wcreep = w|"creep" is "ugięcie wywołane pełzaniem" unit "m"
    val wnetfin = w|"net,fin" is "końcowe ugięcie wynikowe" unit "m"
    val kdef = k|"def" is "współczynnik odkształceń, zgodnie z tablicą 3.2 w [1]"
    val psi21 = psi|"2,1" is "współczynnik obciążenia quasi-stałego w kombinacji obciążeń, wg Tablicy A.1.1 w [3]"
    val umG = u|"m,G" is "ugięcie belki swobodnie podpartej wywołane momentem zginającym od obciążeń stałych"
    val umQ1 = u|"m,Q,1" is "ugięcie belki swobodnie podpartej wywołane momentem zginającym od obciążeń zmiennych Q1"
    val umQi = u|"m,Q,i" is "ugięcie belki swobodnie podpartej wywołane momentem zginającym od obciążeń zmiennych Qi"
    val Iy = I|"y" is "moment bezwładności w środku rozpiętości dźwigara" unit "m4"
    calc(wmax) = l0/300
    calc(Iy) = (b*(hm^3))/12
    calc(kdef) = 0.8
    calc(psi21) = 0.2
    calc(umG) = 5d/384*((G0k+G1k)*(l0^4)/(E0mean*Iy))
    calc(uinstG) = umG * ((1+19.2*((hm/l0)^2))/(0.15+0.85*(h1/hm)))
    calc(umQ1) = 5d/384*((Qk1)*(l0^4)/(E0mean*Iy))
    calc(uinstQ1) = umQ1 * ((1+19.2*((hm/l0)^2))/(0.15+0.85*(h1/hm)))
    calc(umQi) = 5d/384*((Qk2)*(l0^4)/(E0mean*Iy))
    calc(uinstQi) = umQi * ((1+19.2*((hm/l0)^2))/(0.15+0.85*(h1/hm)))
    calc(ufinG) = uinstG*(1+kdef)
    calc(ufinQ1) = uinstQ1*(1+psi21*kdef)
    calc(ufinQi) = uinstQi*(psi0i+psi21*kdef)
    calc(wfin) = ufinG+ufinQ1+ufinQi
    
    val psi11 = psi|"1,1" is "współczynnik dla wartości częstej oddziaływania zmiennego"
    val EdA = E|"d,A" is "kombinacja oddziaływań w warunkach pożarowych" unit "N/m"
    calc(psi11) = 0.2
    calc(EdA) = (G0k+G1k)+psi11*Qk1
    
    val t = BasicSymbols.t is "czas oddziaływania pożaru" unit "min"
    val betan = beta|n is """obliczeniowa prędkość zwęglania z uwzględnieniem wpływu na zaokrąglenia narożników oraz szczelin. 
Z tablicy 3.1 w PN-EN 1995-1-2:2008 dla klejonego warstwowo drewna iglastego.""" unit "mm/min"
    calc(betan) = 0.7
    val dcharn = d|"char,n" is "hipotetyczna głębokość zwęglenia, uwzględniająca wpływ zaokrągleń narożników." unit "m"
    calc(t) = 30
    calc(dcharn) = (betan*t)/1000
    val Ar = A|r is "powierzchia przekroju pozostałego po 30 min pożaru" unit "m²"
    val pr = p|r is "obwód przekroju pozostałego po 30 min pożaru" unit "m"
    val kfi = k|"fi" is "współczynnik uwzględniający zwiększoną wytrzymałość i sztywność drewna na podstawie tablicy 2.1"
    val kmodmfi = k|"mod,m,fi" is "współczynnik modyfikujący wytrzymałość na zginanie w warunkach pożarowych"
    calc(Ar) = (b-(2*dcharn))*(hmax-(2*dcharn))
    calc(pr) = 2*(b-(2*dcharn))+2*(hmax-(2*dcharn))
    calc(kfi) = 1.15
    calc(kmodmfi) = 1-(pr/(200*Ar))
    val kmodEfi = k|"mod,E,fi" is "współczynnik modyfikujący moduł sprężystości w warunkach pożarowych"
    calc(kmodEfi) = 1-(pr/(333*Ar))
    val gammaMfi = gamma|"M,fi" is "częściowy współczynnik materiałowy w warunkach pożarowych"
    calc(gammaMfi) = 1.0
    val fmdfi = f|"m,d,fi" is "wytrzymałość obliczeniowa na zginanie w warunkach pożarowych"
    calc(fmdfi) = kmodmfi*((kfi*fmk)/gammaMfi)
    val Edfi = E|"d,fi" is "obliczeniowy moduł sprężystości w warunkach pożarowych" unit "Pa"
    calc(Edfi) = kmodEfi*((kfi*E005)/gammaMfi)
    
    val lefffi = Symbol("&#x2113;")|"eff,fi" is """efektywna długość belki zależna od warunków podparcia i układu obciążenia""" unit "m"
    calc(lefffi) = 0.9*l0+2*(hm-2*dcharn)
    val sigmamcritfi = sigma|"m,crit,fi" is "naprężenie krytyczne przy zginaniu wg wzoru 6.32 [1]" unit "Pa"
    calc(sigmamcritfi) = ((0.78*((b-2*dcharn)^2))/((hm-2*dcharn)*lefffi))*Edfi
    val lambdarelfi = lambda|"rel,fi" is "smukłość względna przy zginaniu"
    calc(lambdarelfi) = sqrt(fmdfi/sigmamcritfi)
    val kcritfi = k|"crit,fi" is "współczynnik uwzględniający redukcję wytrzymałości ze względu na zwichrowanie elementu"
    calc(kcritfi) = 1 or (InRangeLLE(0.75,lambdarelfi,1.4) then (1.56-(0.75*lambdarelfi))) or (GreaterThan(lambdarelfi,1.4) then (1/(lambdarelfi^2)))
    val fmcritfi = f|"m,crit,fi" is "zredukowana wytrzymałość na zginanie ze względu na zwichrzenie" unit "Pa"
    calc(fmcritfi) = kcritfi*fmdfi
    
    val sigmam0fi = sigma|"m,0,fi" is "obliczeniowe naprężenie zginające" unit "Pa"
    val xmaxfi = x|(sigma+",fi") is "miejsce wystąpienia największych naprężeń normalnych w przęśle liczone od podpory A" unit "m"
    calc(xmaxfi) = l0*(h1-2*dcharn)/((h1-2*dcharn)+(h2-2*dcharn))
    val hmaxfi = h|(sigma+",fi") is "wysokość przekroju w miejscu wystąpienia największych naprężeń normalnych" unit "m"
    calc(hmaxfi) = xmaxfi/l0*((h2-2*dcharn)-(h1-2*dcharn))+(h1-2*dcharn)
    
    val RAfi = R|"A,fi" is "reakcja na podporze A" unit "N"
    calc(RAfi) = EdA*l0/2
    val Myfi = M|"y,fi" is "moment zginający w miejscu wystąpienia największych naprężeń normalnych" unit "Nm"
    calc(Myfi) = RAfi*xmaxfi-EdA*(xmaxfi^2)/2
    val Wyfi = W|"y,fi" is "wskaźnik zginania w miejscu wystąpienia największych naprężeń normalnych" unit "m³"
    calc(Wyfi) = (b-2*dcharn)*(hmaxfi^2)/6
    calc(sigmam0fi) = Myfi/Wyfi
    
    val deff = d|"eff" is "efektywna głębokość zwęglenia" unit "m"
    val k0 = k|"0" is "na podstawie tablicy 4.1"
    val d0 = d|"0" is "zgodnie z punktem 4.2.2"
    calc(k0) = 1.0
    calc(d0) = 0.007
    calc(deff) = dcharn+(k0*d0)
    val kmodmfi2 = k|"mod,m,fi,2" is "współczynnik modyfikujący wytrzymałość na zginanie w warunkach pożarowych"
    val kmodEfi2 = k|"mod,E,fi,2" is "współczynnik modyfikujący moduł sprężystości w warunkach pożarowych"
    calc(kmodmfi2) = 1
    calc(kmodEfi2) = 1
    val fmdfi2 = f|"m,d,fi,2" is "wytrzymałość obliczeniowa na zginanie w warunkach pożarowych" unit "Pa"
    calc(fmdfi2) = kmodmfi2*((kfi*fmk)/gammaMfi)
    val Edfi2 = E|"d,fi,2" is "obliczeniowy moduł sprężystości w warunkach pożarowych" unit "Pa"
    calc(Edfi2) = kmodEfi2*((kfi*E005)/gammaMfi)
    
    val lefffi2 = Symbol("&#x2113;")|"eff,fi,2" is """efektywna długość belki zależna od warunków podparcia i układu obciążenia""" unit "m"
    calc(lefffi2) = 0.9*l0+2*(hm-2*deff)
    val sigmamcritfi2 = sigma|"m,crit,fi,2" is "naprężenie krytyczne przy zginaniu wg wzoru 6.32 [1]" unit "Pa"
    calc(sigmamcritfi2) = ((0.78*((b-2*deff)^2))/((hm-2*deff)*lefffi2))*Edfi2
    val lambdarelfi2 = lambda|"rel,fi,2" is "smukłość względna przy zginaniu"
    calc(lambdarelfi2) = sqrt(fmdfi2/sigmamcritfi2)
    val kcritfi2 = k|"crit,fi,2" is "współczynnik uwzględniający redukcję wytrzymałości ze względu na zwichrowanie elementu"
    calc(kcritfi2) = 1 or (InRangeLLE(0.75,lambdarelfi2,1.4) then (1.56-(0.75*lambdarelfi2))) or (GreaterThan(lambdarelfi2,1.4) then (1/(lambdarelfi2^2)))
    val fmcritfi2 = f|"m,crit,fi,2" is "zredukowana wytrzymałość na zginanie ze względu na zwichrzenie" unit "Pa"
    calc(fmcritfi2) = kcritfi2*fmdfi2
    
    val sigmam0fi2 = sigma|"m,0,fi,2" is "obliczeniowe naprężenie zginające" unit "Pa"
    val xmaxfi2 = x|(sigma+",fi,2") is "miejsce wystąpienia największych naprężeń normalnych w przęśle liczone od podpory A" unit "m"
    calc(xmaxfi2) = l0*(h1-2*deff)/((h1-2*deff)+(h2-2*deff))
    val hmaxfi2 = h|(sigma+",fi,2") is "wysokość przekroju w miejscu wystąpienia największych naprężeń normalnych" unit "m"
    calc(hmaxfi2) = xmaxfi2/l0*((h2-2*deff)-(h1-2*deff))+(h1-2*deff)
    
    val RAfi2 = R|"A,fi,2" is "reakcja na podporze A" unit "N"
    calc(RAfi2) = EdA*l0/2
    val Myfi2 = M|"y,fi,2" is "moment zginający w miejscu wystąpienia największych naprężeń normalnych" unit "Nm"
    calc(Myfi2) = RAfi*xmaxfi2-EdA*(xmaxfi2^2)/2
    val Wyfi2 = W|"y,fi,2" is "wskaźnik zginania w miejscu wystąpienia największych naprężeń normalnych" unit "m³"
    calc(Wyfi2) = (b-2*deff)*(hmaxfi2^2)/6
    calc(sigmam0fi2) = Myfi2/Wyfi2
      
    val doc1 = Document("",
        Predefined.stylesConfig,
        Chapter("",
            Section("Ćwiczenie projektowe nr 2 z \"Konstrukcji Drewnianych\". Autor: Artur Opala 61315. Wrocław 2010/2011"),
            Section(""),
            NumSection("Zadanie",
                Section(styleComment,"Dźwigar trapezowy z drewna klejonego warstwowo."),
                Section(styleComment,"Drewno klejone warstwowo kombinowane klasy GL36c."),
                Section(styleComment,"Konstrukcja w 2 klasie użytkowania wg normy [1] pkt. 2.3.1.3."),
                Section(styleComment,"Klasa odporności ogniowej R30 (konstrukcja dachu)."),
                NumSection("Normy i literatura",
                	Section(styleComment," [1] Norma PN-EN 1995-1-1:2010 \"Eurokod 5. Projektowanie konstrukcji drewnianych. Część 1-1: Postanowienia ogólne. Reguły ogólne i reguły dotyczące budynków\""),
                	Section(styleComment," [2] Norma PN-EN 1194:2000")
                )
            ),
            NumSection("Dane do obliczeń",
                NumSection("Geometria dźwigara",Evaluate(calc,l0,h1,h2,hm,b,alphag,xihb,xilh)),
                NumSection("Obciążenia charakterystyczne",Evaluate(calc,rhok,G0k,G1k,Qk1,Qk2)),
                NumSection("Współczynniki częściowe dla oddziaływań",Evaluate(calc,gamG1,gamQ1,gamQi,psi0i)),
                NumSection("Kombinacje obciążeń",Evaluate(calc,qd)),
                NumSection("Współczynnik ",Symb(kmod),Evaluate(calc,kmod)),
                NumSection("Współczynnik materiałowy ",Symb(gamM),Evaluate(calc,gamM)),
                NumSection("Wytrzymałości i moduły charakterystyczne wg PN-EN 1194:2000",
                        Evaluate(calc,fmk,ft0k,ft90k,fc0k,fc90k,fvk,E0mean,E005,E90mean,Gmean)),
                NumSection("Wytrzymałości i moduły obliczeniowe",
                        Evaluate(calc,fmd,ft0d,ft90d,fc0d,fc90d,fvd,E0d,E90d,Gdd))
            ),
            NumSection("Sprawdzenie stanu granicznego nośności SGN wg PN-EN 1995-1-1",
                NumSection("Sprawdzenie naprężeń maksymalnych od zginania",
                        Evaluate(calc,xmax,hmax,RA,My,Wy,sigmam0d),
                        Section(styleWarunek,"Warunek 6.11 jest spełniony: ",
                                Symb(sigmam0d),LE,Symb(fmd),ARROW,Result(calc,sigmam0d),"Pa",LE,Result(calc,fmd),"Pa")),
                NumSection("Uwzględnienie zmiennego przekroju dźwigara",
                        Evaluate(calc,kmalpha),
                        Evaluate(calc,fmalphad),
                        Section(styleWarunek,"Warunek 6.38 jest spełniony: ",
                                Symb(sigmam0d),LE,Symb(fmalphad),ARROW,Result(calc,sigmam0d),"Pa",LE,Result(calc,fmalphad),"Pa")),
                NumSection("Sprawdzenie stateczności przekroju",
                        Evaluate(calc,leff,sigmamcrit,lambdarel),
                        Evaluate(calc,kcritm),
                        Evaluate(calc,fmcritd),
                        Section(styleWarunek,"Warunek 6.33 jest spełniony: ",
                                Symb(sigmam0d),LE,Symb(fmcritd),ARROW,Result(calc,sigmam0d),"Pa",LE,Result(calc,fmcritd),"Pa")),
                NumSection("Sprawdzenie nośności na ścinanie",
                        Evaluate(calc,Vd,kcr,beff,taud),
                        Section(styleWarunek,"Warunek 6.13 jest spełniony: ",
                                Symb(taud),LE,Symb(fvd),ARROW,Result(calc,taud),"Pa",LE,Result(calc,fvd),"Pa")),
                NumSection("Sprawdzenie nośności na docisk w poprzek włókien na podporze",
                        Evaluate(calc,lpod,Aef,Fc90d,sigmac90d,kc90),
                        Section(styleWarunek,"Warunek 6.3 jest spełniony: ",
                                Symb(sigmac90d),LE,Symb(kc90),Symb(fc90d),ARROW,Result(calc,sigmac90d),"Pa",LE,Result(calc,kc90*fc90d),"Pa"))
            ),
            NumSection("Sprawdzenie stanu granicznego nośności SGN wg PN-EN 1995-1-1",
                NumSection("Obliczenie ugięć",
                        Evaluate(calc,wmax,kdef,psi21,Iy,umG,uinstG,umQ1,uinstQ1,umQi,uinstQi,ufinG,ufinQ1,ufinQi,wfin),
                        Section(styleWarunek,"Warunek ugięć jest spełniony: ",
                                Symb(wfin),LE,Symb(wmax),ARROW,Result(calc,wfin),"m",LE,Result(calc,wmax),"m"))
            ),
            NumSection("Sprawdzenie nośności w warunkach pożarowych wg PN-EN 1995-1-2",
                Section(style1,"Oczekiwana klasa odporności ogniowej R30."),
                NumSection("Dane wejściowe",Evaluate(calc,psi11,EdA)),
                NumSection("Obliczenie nośności metodą zredukowanych właściwości",
                        Evaluate(calc,t,betan,dcharn,Ar,pr,kfi,kmodmfi,kmodEfi,gammaMfi,fmdfi,Edfi,lefffi,sigmamcritfi,lambdarelfi,kcritfi,fmcritfi),
                        Evaluate(calc,xmaxfi,hmaxfi,RAfi,Myfi,Wyfi,sigmam0fi),
                        Section(styleWarunek,"Warunek SGN jest spełniony: ",
                                Symb(sigmam0fi),LE,Symb(kcritfi),Symb(fmdfi),ARROW,Result(calc,sigmam0fi),"Pa",LE,Result(calc,kcritfi*fmdfi),"Pa")),
                NumSection("Obliczenie nośności metodą zredukowanego przekroju",
                        Evaluate(calc,k0,d0,deff,kmodmfi2,kmodEfi2,gammaMfi,fmdfi2,Edfi2,lefffi2,sigmamcritfi2,lambdarelfi2,kcritfi2,fmcritfi2),
                        Evaluate(calc,xmaxfi2,hmaxfi2,RAfi2,Myfi2,Wyfi2,sigmam0fi2),
                        Section(styleWarunek,"Warunek SGN jest spełniony: ",
                                Symb(sigmam0fi2),LE,Symb(kcritfi2),Symb(fmdfi2),ARROW,Result(calc,sigmam0fi2),"Pa",LE,Result(calc,kcritfi2*fmdfi2),"Pa"))
            ),
			Section(style1.marginTop(30),""),
			Section("Koniec obliczeń.")
        )
    )
    
    @Test def printPdf:Unit = {
        val layout = Predefined.layout
        val output:XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open
        XslFoTextDocumentPrinter.print(doc1,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/kd1-dzwigar.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/kd1-dzwigar.pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc1,o)
        o.printConsole
    }

}
