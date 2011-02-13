package org.encalmo.examples

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.document.StylesConfigSymbols._
import org.encalmo.examples.Predefined._

class SlupDrewnianyZlozonyWkrety {
    
    import BasicSymbols._
    
    val calc = Calculation()
    val calc2 = Calculation("2")
    val calc3 = Calculation()
    
    calc add calc2
    calc add calc3
    
    val l0 = l|0 is "rozpiętość obliczeniowa" unit "m"
    calc2(l0) = 3.8
    val Fcd = BasicSymbols.F|"c,d" is "siła ściskająca osiowo" unit "N"
    calc2(Fcd) = 68000
    val mic = mu|c is "współczynnik długości wyboczeniowej"
    calc2(mic) = 1
    val daneWejsciowe = Seq(l0,Fcd,mic)
    
    val b1 = BasicSymbols.b|1 is "szerokość przekroju półki" unit "m"
    calc(b1) = 0.063
    val h1 = BasicSymbols.h|1 is "wysokość przekroju półki" unit "m"
    calc(h1) = 0.063
    val b2 = BasicSymbols.b|2 is "szerokość przekroju środnika" unit "m"
    calc(b2) = 0.032
    val h2 = BasicSymbols.h|2 is "wysokość przekroju środnika" unit "m"
    calc(h2) = 0.16
    val b = BasicSymbols.b is "szerokość całkowita przekroju złożonego" unit "m"
    calc(b) = b2+2*b1
    val h = BasicSymbols.h is "wysokość całkowita przekroju złożonego" unit "m"
    calc(h) = h2
    val az = BasicSymbols.a|"z" is "odległość środka ciężkości półki od środka ciężkości przekroju złożonego, wzdłuż osi Z" unit "m"
    calc(az) = (h-h1)/2
    val ay = BasicSymbols.a|"y" is "odległość środka ciężkości półki od środka ciężkości przekroju złożonego, wzdłuż osi Y" unit "m"
    calc(ay) = (b-b1)/2
    val przyjetaGeometria = Seq(b1,h1,b2,h2,b,h)
    
    val A1 = BasicSymbols.A|1 is "pole powierzchni segmentu półki" unit "m²"
    calc(A1) = b1*h1
    val A2 = BasicSymbols.A|2 is "pole powierzchni środnika" unit "m²"
    calc(A2) = b2*h2
    val A = BasicSymbols.A is "całkowite pole powierzchni przekroju" unit "m²"
    calc(A) = A2+4*A1
    val Iz1 = BasicSymbols.I|"z,1" is "moduł bezwładności segmentu półki względem osi Z" unit "m4"
    calc(Iz1) = (b1*(h1^3))/12
    val Iy1 = BasicSymbols.I|"y,1" is "moduł bezwładności segmentu półki względem osi Y" unit "m4"
    calc(Iy1) = (h1*(b1^3))/12
    val Iz2 = BasicSymbols.I|"z,2" is "moduł bezwładności środnika względem osi Z" unit "m4"
    calc(Iz2) = (b2*(h2^3))/12
    val Iy2 = BasicSymbols.I|"y,2" is "moduł bezwładności segmentu półki względem osi Y" unit "m4"
    calc(Iy2) = (h2*(b2^3))/12
    val Iz = BasicSymbols.I|"y" is "moduł bezwładności przekroju złożonego względem osi Z" unit "m4"
    calc(Iz) = Iz2+4*(Iz1+A1*(ay^2))
    val Iy = BasicSymbols.I|"z" is "moduł bezwładności przekroju złożonego względem osi Y" unit "m4"
    calc(Iy) = Iy2+4*(Iy1+A1*(az^2))
    val Imin = BasicSymbols.I|"min" is "mniejszy z modułów bezwładności" unit "m4"
    calc(Imin) = min(Iz,Iy)
    val imin = BasicSymbols.i|"min" is "promień bezwładności" unit "m"
    calc(imin) = sqrt(Imin/A)
    val wlasciwosciGeometryczne = Seq(ay,az,A1,A2,A,Iz1,Iy1,Iz2,Iy2,Iz,Iy,Imin,imin)
    
    val d = BasicSymbols.d is "przyjęta średnica wewnętrzna trzpienia dla wkręta samowiercącego SPAX&reg; T-STAR T40(8mm)" unit "mm"
    calc(d) = 5.6
    val deff = BasicSymbols.d|"ef" is "przyjęta efektywna średnica trzpienia wkręta" unit "mm"
    calc(deff) = 6.0
    val s1min = s|"1,min" is "minimalny rozstaw wkrętów w szeregu wzdłuż włókien" unit "mm"
    calc(s1min) = (5+5)*d
    val s1 = s|"1" is "przyjęty rozstaw wkrętów w szeregu wzdłuż włókien " unit "mm"
    calc(s1) = 60
    val nh = n|"h" is "przyjęta ilość wkrętów w rzędzie na szerokości półki" unit "szt."
    calc(nh) = 1
    val np = n|"p" is "ilość płaszczyzn styku środnika z półką wg B.1.3(1) [1]"
    calc(np) = 2
    val s1eff = s|"1,eff" is "efektywny rozstaw wkrętów w szeregu wzdłuż włókien" unit "mm"
    calc(s1eff) = s1/(nh*np)
    val s2 = s|"2" is "minimalny rozstaw wkrętów w poprzek włókien wg tablicy 8.2 [1]" unit "mm"
    calc(s2) = 5*d
    val s3c = s|"3,c" is "minimalna odległość wkręta od końca nieobciążonego" unit "mm"
    calc(s3c) = 10*d
    val s3t = s|"3,t" is "minimalna odległość wkręta od końca obciążonego" unit "mm"
    calc(s3t) = 15*d
    val s4c = s|"4,c" is "minimalna odległość wkręta od boku nieobciążonego" unit "mm"
    calc(s4c) = 5*d
    val s4t = s|"4,t" is "minimalna odległość wkręta od boku obciążonego" unit "mm"
    calc(s4t) = 5*d
    val t2 = t|"2" is "minimalna długość zakotwienia wkręta w elemencie od strony ostrza" unit "mm"
    calc(t2) = 6*d
    val lw = l|"w" is "wymagana długość wkręta" unit "mm"
    calc(lw) = round((b1*1000)+t2,RoundingMode.Step(true,10))
    val nw = n|"w" is "całkowita liczba wkrętów w słupie" unit "szt."
    calc(nw) = 4*nh*(round((l0*1000-2*s3c)/s1)+1)
    val przyjeteLaczniki = Seq(d,deff,s1min,s1,nh,np,s1eff,s2,s3c,s3t,s4c,s4t,t2,lw,nw)
    
    val kmod = k|"mod" is """współczynnik modyfikujący efekt czasu trwania obciążenia i zmiany wilgotności materiału,
 przyjęty dla 2 klasy użytkowania i oddziaływania długotrwałego z Tab. 3.1 [1]"""
    calc3(kmod) = 0.7
    val gamM = gamma|M is """częściowy współczynnik bezpieczeństwa właściwości materiału, uwzględniający także
 niedoskonałości modelowania i odchyłki wymiarowe, przyjęty dla drewna litego z Tab. 2.3 [1]"""
    calc3(gamM) = 1.3
    val wspolczynnikiCzesciowe = Seq(kmod,gamM)
    
    val fmk = f|"m,k" is "wytrzymałość charakterystyczna na zginanie" unit "Pa"
    calc3(fmk) = 27000000
    val ft0k = f|"t,0,k" is "wytrzymałość charakterystyczna na rozciąganie wzdłuż włókien" unit "Pa"
    calc3(ft0k) = 16000000
    val ft90k = f|"t,90,k" is "wytrzymałość charakterystyczna na rozciąganie w poprzek włókien" unit "Pa"
    calc3(ft90k) = 400000
    val fc0k = f|"c,0,k" is "wytrzymałość charakterystyczna na ściskanie wzdłuż włókien" unit "Pa"
    calc3(fc0k) = 22000000
    val fc90k = f|"c,90,k" is "wytrzymałość charakterystyczna na ściskanie w poprzek włókien" unit "Pa"
    calc3(fc90k) = 2600000
    val fvk = f|"v,k" is "wytrzymałość charakterystyczna na ścinanie" unit "Pa"
    calc3(fvk) = 4000000
    val E0mean = E|"0,mean" is "średni moduł sprężystości wzdłuż włókien" unit "Pa"
    calc3(E0mean) = 11500000000l
    val E005 = E|"0,05" is "5 % kwantyl modułu sprężystości" unit "Pa" 
    calc3(E005) = 7700000000l
    val E90mean = E|"90,mean" is "średni moduł sprężystości w poprzek włókien" unit "Pa" 
    calc3(E90mean) = 380000000
    val Gmean = G|"mean" is "średni moduł odkształcenia postaciowego" unit "Pa" 
    calc3(Gmean) = 720000000
    val rhok = rho|"k" is "gęstość charakterystyczna" unit "kg/m³"
    calc3(rhok) = 370
    val rhom = rho|"m" is "gęstość średnia" unit "kg/m³"
    calc3(rhom) = 450
    val wlasciwosciMechaniczneCharakterystyczne = Seq(fmk,ft0k,ft90k,fc0k,fc90k,fvk,E0mean,E005,E90mean,Gmean,rhok,rhom)
    
    val fmd = f|"md" is "wytrzymałość obliczeniowa na zginanie" unit "Pa"
    calc3(fmd) = kmod*fmk/gamM
    val ft0d = f|"t,0,d" is "wytrzymałość obliczeniowa na rozciąganie wzdłuż włókien" unit "Pa"
    calc3(ft0d) = kmod*ft0k/gamM
    val ft90d = f|"t,90,d" is "wytrzymałość obliczeniowa na rozciąganie w poprzek włókien" unit "Pa"
    calc3(ft90d) = kmod*ft90k/gamM
    val fc0d = f|"c,0,d" is "wytrzymałość obliczeniowa na ściskanie wzdłuż włókien" unit "Pa"
    calc3(fc0d) = kmod*fc0k/gamM
    val fc90d = f|"c,90,d" is "wytrzymałość obliczeniowa na ściskanie w poprzek włókien" unit "Pa"
    calc3(fc90d) = kmod*fc90k/gamM
    val fvd = f|"vd" is "wytrzymałość obliczeniowa na ścinanie" unit "Pa"
    calc3(fvd) = kmod*fvk/gamM
    val E0d = E|"0,d" is "obliczeniowy moduł sprężystości wzdłuż włókien" unit "Pa"
    calc3(E0d) = E0mean/gamM
    val E90d = E|"90,d" is "obliczeniowy moduł sprężystości w poprzek włókien" unit "Pa"
    calc3(E90d) = E90mean/gamM
    val Gdd = G|"d" is "obliczeniowy moduł odkształcenia postaciowego" unit "Pa"
    calc3(Gdd) = Gmean/gamM
    val wlasciwosciMechaniczneObliczeniowe = Seq(fmd,ft0d,ft90d,fc0d,fc90d,fvd,E0d,E90d,Gdd)
    
    val lc = l|c is "długość wyboczeniowa" unit "m"
    calc2(lc) = mic*l0
    val betac = beta|c is "współczynnik prostoliniowości elementów wg 6.29 [1]"
    calc2(betac) = 0.2
    
    val Kser = K|"ser" is "moduł podatności łączników mechanicznych dla stanu granicznego nośności (SGN) wg tablicy 7.1 [1]" unit "N/m"
    calc(Kser) = ((rhom^1.5)*deff*0.001)/23
    val Ku = K|"u" is "moduł podatności łączników mechanicznych dla stanu granicznego użytkowalności (SGU) wg 2.1 [1]" unit "N/m"
    calc(Ku) = (2*Kser)/3
    val gamma1SGN = (gamma|"1") is "współczynnik zmniejszający moment bezwładności półek ze względu na podatność połączeń dla SGN wg B.5 [1]"
    calc(gamma1SGN) = (1+(((PI^2)*E0mean*A1*(s1eff*0.001))/(Ku*(l0^2)*1000000)))^(-1)
    val gamma1SGU = (gamma|"1")!"SGU" is "współczynnik zmniejszający moment bezwładności półek ze względu na podatność połączeń dla SGU wg B.5 [1]"
    calc(gamma1SGU) = (1+(((PI^2)*E0mean*A1*(s1eff*0.001))/(Kser*(l0^2)*1000000)))^(-1)
    val EIzeffSGN = (Symbol("(EI)")|"z,ef") is "sztywność zastępcza względem osi Z dla SGN wg B.1 [1]" unit "Nm²" 
    calc(EIzeffSGN) = E0mean*(Iz2+4*(Iz1+gamma1SGN*A1*(ay^2)))
    val EIzeffSGU = (Symbol("(EI)")|"z,ef")!"SGU" is "sztywność zastępcza względem osi Z dla SGU wg B.1 [1]" unit "Nm²" 
    calc(EIzeffSGU) = E0mean*(Iz2+4*(Iz1+gamma1SGU*A1*(ay^2)))
    val EIyeffSGN = (Symbol("(EI)")|"y,ef") is "sztywność zastępcza względem osi Y dla SGN wg B.1 [1]" unit "Nm²" 
    calc(EIyeffSGN) = E0mean*(Iy2+4*(Iy1+gamma1SGN*A1*(az^2)))
    val EIyeffSGU = (Symbol("(EI)")|"y,ef")!"SGU" is "sztywność zastępcza względem osi Y dla SGU wg B.1 [1]" unit "Nm²" 
    calc(EIyeffSGU) = E0mean*(Iy2+4*(Iy1+gamma1SGU*A1*(az^2)))
    val EIeffSGN = (Symbol("(EI)")|"ef") is "sztywność zastępcza dla SGN" unit "Nm²" 
    calc(EIeffSGN) = min(EIzeffSGN,EIyeffSGN)
    val EIeffSGU = (Symbol("(EI)")|"ef")!"SGU" is "sztywność zastępcza dla SGU" unit "Nm²" 
    calc(EIeffSGU) = min(EIzeffSGU,EIyeffSGU)
    val xiEI = xi|"(EI)" is "stosunek sztywności zastępczych względem osi Y/Z" unit "%"
    calc(xiEI) = (EIyeffSGN*100)/EIzeffSGN
    val sztywnoscZastepcza = Seq(Kser,Ku,gamma1SGN/*,gamma1SGU*/,EIzeffSGN,EIyeffSGN,EIeffSGN/*,EIzeffSGU,EIyeffSGU,EIeffSGU*/,xiEI)
    
    val Ief = I|"ef" is "efektywny moduł bezwładności wg C.4 [1]" unit "m4"
    calc(Ief) = EIeffSGN/E0mean
    val lambdaeff = lambda|"ef" is "smukłość efektywna wg C.3 [1]"
    calc(lambdaeff) = lc*sqrt(A/Ief)
    val lambdarel = lambda|"rel" is "smukłość względna wg 6.22 [1]"
    calc(lambdarel) = (lambdaeff/PI)*sqrt(fc0k/E005)
    val kmin = k|"min" is "współczynnik pomocniczy do obliczenia współczynnika wyboczeniowego wg 6.27 [1]"
    calc(kmin) = 0.5*(1+betac*(lambdarel-0.3)+(lambdarel^2))
    val kc = k|c is "współczynnik wyboczeniowy wg 6.25 [1]"
    calc(kc) = 1/(kmin+sqrt((kmin^2)-(lambdarel^2)))
    val sigmac0d = sigma|"c,0,d" is "obliczeniowe naprężenie ściskające wzdłuż włókien wg C.2 [1]" unit "Pa"
    calc(sigmac0d) = Fcd/A
    val NRc = N|"R,c" is "nośność obliczeniowa na ściskanie" unit "N"
    calc(NRc) = fc0d*A
    val Fmax = F|"max" is "maksymalna osiowa siła ściskająca" unit "N"
    calc(Fmax) = kc*NRc
    val xiS = BasicSymbols.xi|"c" is "wytężenie słupa na ściskanie" unit "%"
    calc(xiS) = (Fcd*100)/Fmax
    val nosnoscObliczeniowa = Seq(lc,betac,Ief,lambdaeff,lambdarel,kmin,kc,sigmac0d,NRc,Fmax,xiS)

    val Vd = V|"d" is "siła ścinająca wg C.5 [1]" unit "N"
    calc(Vd) = Fcd/(120*kc) or (InRange(30,lambdaeff,60) then (Fcd/(3600*kc))) or (GreaterThan(lambdaeff,60) then (Fcd/(60*kc)))
    val tau2max = tau|"2,max" is "maksymalne naprężenia ścinające w środniku wg B.9 [1]" unit "Pa"
    calc(tau2max) = E0mean*(gamma1SGN*A1*az+0.5*b2*(h^2))/(b2*EIeffSGN)*Vd
    val nosnoscScinanie = Seq(Vd,tau2max)
    
    val Fi = F|"i" is "obciążenie łącznika wg B.10 [1]" unit "N"
    calc(Fi) = ((gamma1SGN*E0mean*A1*ay*(s1eff*0.001))/EIeffSGN)*Vd
    val kef = k|"ef" is "współczynnik efektywnej liczby łączników w szeregu z tablicy 8.1 [1]"
    calc(kef) = 0.85
    val betal = beta|l is "stosunek wytrzymałości charakterystycznych na docisk łącznika do elementów złącza"
    calc(betal) = 1
    val fhk = f|"h,k" is "wytrzymałość charakterystyczna na docisk w elemencie drewnianym wg 8.16 [1]" unit "Pa"
    calc(fhk) = 0.082*(1-0.01*deff)*rhok*1000000
    val MyRk = M|"y,Rk" is "moment charakterystyczny uplastycznienia wkręta wg 2.2.1.a [2]" unit "Nm"
    calc(MyRk) = 75*(d^2.6)*1000
    val FaxRk = F|"ax,Rk" is "nośność charakterystyczna łącznika na wyciąganie"
    calc(FaxRk) = 0
    val FvRk = F|"v,Rk" is "nośność charakterystyczna łącznika odniesiona do jednej płaszczyzny ścinania wg 8.6 [1]" unit "N"
    val FvRka = F|"v,Rk,a" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia a" unit "N"
    val FvRkb = F|"v,Rk,b" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia b" unit "N"
    val FvRkc = F|"v,Rk,c" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia c" unit "N"
    val FvRkd = F|"v,Rk,d" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia d" unit "N"
    val FvRke = F|"v,Rk,e" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia e" unit "N"
    val FvRkf = F|"v,Rk,f" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia f" unit "N"
    calc(FvRka) = fhk*b1*(deff*0.001)
    calc(FvRkb) = fhk*b2*(deff*0.001)
    calc(FvRkc) = (fhk*b1*(deff*0.001)/(1+betal))*(sqrt(betal+2*(betal^2)*(1+b2/b1+((b2/b1)^2))+(betal^3)*((b2/b1)^2))-betal*(1+(b2/b1)))+FaxRk/4
    calc(FvRkd) = 1.05*(fhk*b1*(deff*0.001)/(2+betal))*(sqrt(2*betal*(1+betal)+((4*betal*(2+betal)*MyRk)/(fhk*deff*0.001*(b1^2))))-betal)+FaxRk/4
    calc(FvRke) = 1.05*(fhk*b2*(deff*0.001)/(1+2*betal))*(sqrt(2*(betal^2)*(1+betal)+((4*betal*(1+2*betal)*MyRk)/(fhk*deff*0.001*(b2^2))))-betal)+FaxRk/4
    calc(FvRkf) = 1.15*sqrt((2*betal)/(1+betal))*sqrt(2*MyRk*fhk*deff*0.001)+FaxRk/4
    calc(FvRk) = min(FvRka,FvRkb,FvRkc,FvRkd,FvRke,FvRkf)
    val FvRd = F|"v,Rd" is "nośność obliczeniowa łącznika" unit "N"
    calc(FvRd) = (kmod*FvRk)/gamM
    
    val nosnoscLacznikow = Seq(Fi,kef,betal,fhk,MyRk,FvRka,FvRkb,FvRkc,FvRkd,FvRke,FvRkf,FvRk,FvRd)
    
    val lambdac = lambda|"c" is "smukłość słupa"
    calc2(lambdac) = lc/imin
    val lambdarel2 = lambda|("rel","'") is "smukłość względna słupa"
    calc2(lambdarel2) = (lambdac/PI)*sqrt(fc0k/E005)
    val kmin2 = k|("min","'") is "współczynnik pomocniczy do obliczenia współczynnika wyboczeniowego"
    calc2(kmin2) = 0.5*(1+betac*(lambdarel2-0.3)+(lambdarel2^2))
    val kc2 = k|("c","'") is "współczynnik wyboczeniowy"
    calc2(kc2) = 1/(kmin2+sqrt((kmin2^2)-(lambdarel2^2)))
    val NRc2 = N|("R,c","'") is "nośność obliczeniowa na ściskanie" unit "N"
    calc2(NRc2) = fc0d*A
    val Fmax2 = F|("max","'") is "maksymalna osiowa siła ściskająca" unit "N"
    calc2(Fmax2) = kc2*NRc2
    val deltaJW = delta|"1" is "strata nośności maksymalnej słupa spowodowana zastosowaniem łączników podatnych" unit "%"
    calc2(deltaJW) = ((Fmax2-Fmax)/Fmax2)*100
    
    val nosnoscSlupaNiepodatnie = Seq(lambdac,lambdarel2,kmin2,kc2,NRc2,Fmax2,deltaJW)
    
    val arec = a|"rec" is "długość boku słupa kwadratowego" unit "m"
    val Sec2 = Kwadrat("rec",arec)
    val calc4 = Calculation("slup kwadratowy jednorodny")
    calc4(arec) = 0.138
    calc4(A) = Sec2.A
    calc4(imin) = Sec2.imin
    calc4 add calc2
    calc4 add calc3
    calc4 add Sec2
    val Aof1 = calc(A)
    val Fmaxof1 = calc(Fmax)
    val xi2 = xi|"2" is "stosunek nośności na ściskanie słupa kwadratowego i zaprojektowanego" unit "%"
    calc4(xi2) = (Fmax2/Fmaxof1)*100
    val delta2 = delta|"2" is "zmiana pola przekroju słupa wielogałęziowego w stosunku do słupa kwadratowego o zbliżonej niegorszej nośności" unit "%"
    calc4(delta2) = ((Aof1-Sec2.A)/Aof1)*100
    
    val przekrojKwadratowy = Seq(arec,Sec2.A,Sec2.Imin,Sec2.imin,lambdac,lambdarel2,kmin2,kc2,NRc2,Fmax2,xi2,delta2)
    
    val doc1 = Document(Predefined.style1,"",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe nr 1 z \"Konstrukcji Drewnianych\". Semestr zimowy 2010/2011."),
        		Section("Autor: Artur Opala, album nr 61315. Prowadzący: dr inż. Tomasz Nowak, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            NumSection("Zadanie",
                Section(styleComment,"Słup wielogałęziowy z drewna litego klasy C27 łączonego na wkręty SPAX&reg; T-STAR T40(8mm)."),
                Section(styleComment,"Obciążenie siłą osiową, zamocowanie obustronnie przegubowe."),
                Section(styleComment,"Konstrukcja w 2 klasie użytkowania wg normy [1] pkt. 2.3.1.3."),
                NumSection("Normy i literatura",
                	Section(styleComment," [1] Norma PN-EN 1995-1-1:2010 \"Eurokod 5. Projektowanie konstrukcji drewnianych. Część 1-1: Postanowienia ogólne. Reguły ogólne i reguły dotyczące budynków\""),
                	Section(styleComment," [2] Aprobata ITB AT-15-7343/2007 \"Wkręty samowiercące SPAX&reg; do konstrukcji drewnianych\""),
                	Section(styleComment," [3] Norma PN-EN 338:2009")
                ),
                NumSection("Dane wejściowe",Evaluate(calc,daneWejsciowe:_*))
           ),
           NumSection("Dane do obliczeń",
                NumSection("Przyjęte wymiary przekroju słupa",Evaluate(calc,przyjetaGeometria:_*)),
                NumSection("Przyjęte łączniki",Evaluate(calc,przyjeteLaczniki:_*)),
                NumSection("Właściwości geometryczne przekroju",Evaluate(calc,wlasciwosciGeometryczne:_*)),
                NumSection("Współczynniki",Evaluate(calc,wspolczynnikiCzesciowe:_*)),
                NumSection("Właściwości mechaniczne charakterystyczne dla drewna litego klasy C27 wg [3]",Evaluate(calc,wlasciwosciMechaniczneCharakterystyczne:_*)),
                NumSection("Właściwości mechaniczne obliczeniowe dla 2 klasy użytkowania i obciążeń długotrwałych",Evaluate(calc,wlasciwosciMechaniczneObliczeniowe:_*))),
           NumSection("Sprawdzenie stanów granicznych nośności wg PN-EN 1995-1-1",
                NumSection("Obliczenie sztywności zastępczej",Evaluate(calc,sztywnoscZastepcza:_*)),
                NumSection("Sprawdzenie nośności na ściskanie",Evaluate(calc,nosnoscObliczeniowa:_*)),
                AssertionLE("C.1 [1]",calc,sigmac0d,kc*fc0d),
				NumSection("Sprawdzenie nośności środnika na ścinanie",Evaluate(calc,nosnoscScinanie:_*)),
				AssertionLE("6.13 [1]",calc,tau2max,fvd),
				NumSection("Sprawdzenie nośności łączników (wkrętów)",Evaluate(calc,nosnoscLacznikow:_*)),
				AssertionLE("nośności wkręta",calc,Fi,FvRd)
			),
			NumSection("Badania porównawcze",
				NumSection("Porównanie ze słupem wielogałęziowym z elementów połączonych niepodatnie (klejonych)",Evaluate(calc,nosnoscSlupaNiepodatnie:_*)),
				NumSection("Porównanie ze słupem jednorodnym kwadratowym o zbliżonej nośności na ściskanie",Evaluate(calc4,przekrojKwadratowy:_*)),
				NumSection("Wnioski",
					Section(styleComment,"Porównanie zaprojektowanego słupa ze słupem klejonym wykazało ",Result(calc,round(deltaJW,RoundingMode.HALF)),
					"% utratę nośności ze względu na podatność łączników."),
					Section(styleComment,"Porównanie zaprojektowanego słupa ze słupem jednorodnym kwadratowym o zbliżonej nośności wykazało ",Result(calc4,round(delta2,RoundingMode.HALF)),
					"% stratę na przekroju słupa wielogałęziowego. Zastosowanie słupa o przekroju kwadratowym przy zadanym obciążeniu byłoby bardziej uzasadnione.")
				)
			),
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")
        )
    )
    
    @Test def printPdf:Unit = {
        val layout = Predefined.layout
        val output:XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open
        XslFoTextDocumentPrinter.print(doc1,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/kd2-slup.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/kd2-slup.pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc1,o)
        o.printConsole
    }

}