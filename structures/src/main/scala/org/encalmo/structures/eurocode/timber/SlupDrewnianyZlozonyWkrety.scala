package org.encalmo.structures.eurocode.timber

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.structures.{Worksheet, Predefined}
import org.encalmo.structures.Predefined._
import org.encalmo.structures.common.section.Square
import org.encalmo.structures.common.section.SectionSymbols

class SlupDrewnianyZlozonyWkrety extends Worksheet("kd1-slup") {

    import BasicSymbols._

    val calc2 = Calculation("2")
    val calc3 = Calculation()
    
    this add calc2
    this add calc3
    
    val l0 = l|0 is "rozpiętość obliczeniowa" unit "cm"
    calc2(l0) = 380
    val Fcd = BasicSymbols.F|"c,d" is "siła ściskająca osiowo" unit "kN"
    calc2(Fcd) = 68
    val mic = mu|c is "współczynnik długości wyboczeniowej"
    calc2(mic) = 1
    val daneWejsciowe = Seq(l0,Fcd,mic)
    
    val b1 = BasicSymbols.b|1 is "szerokość przekroju półki" unit SI.cm
    b1 := 6.3
    val h1 = BasicSymbols.h|1 is "wysokość przekroju półki" unit SI.cm
    h1 := 6.3
    val b2 = BasicSymbols.b|2 is "szerokość przekroju środnika" unit SI.cm
    b2 := 3.2
    val h2 = BasicSymbols.h|2 is "wysokość przekroju środnika" unit SI.cm
    h2 := 16
    val b = BasicSymbols.b is "szerokość całkowita przekroju złożonego" unit SI.cm
    b := b2+2*b1
    val h = BasicSymbols.h is "wysokość całkowita przekroju złożonego" unit SI.cm
    h := h2
    val az = BasicSymbols.a|"z" is "odległość środka ciężkości półki od środka ciężkości przekroju złożonego, wzdłuż osi Z" unit SI.cm
    az := (h-h1)/2
    val ay = BasicSymbols.a|"y" is "odległość środka ciężkości półki od środka ciężkości przekroju złożonego, wzdłuż osi Y" unit SI.cm
    ay := (b-b1)/2
    val przyjetaGeometria = Seq(b1,h1,b2,h2,b,h)
    
    val A1 = BasicSymbols.A|1 is "pole powierzchni segmentu półki" unit "cm2"
    A1 := b1*h1
    val A2 = BasicSymbols.A|2 is "pole powierzchni środnika" unit "cm2"
    A2 := b2*h2
    val As = BasicSymbols.A|s is "całkowite pole powierzchni przekroju" unit "cm2"
    As := A2+4*A1
    val Iz1 = BasicSymbols.I|"z,1" is "moduł bezwładności segmentu półki względem osi Z" unit "cm4" acc 1
    Iz1 := (b1*(h1^3))/12
    val Iy1 = BasicSymbols.I|"y,1" is "moduł bezwładności segmentu półki względem osi Y" unit "cm4" acc 1
    Iy1 := (h1*(b1^3))/12
    val Iz2 = BasicSymbols.I|"z,2" is "moduł bezwładności środnika względem osi Z" unit "cm4" acc 1
    Iz2 := (b2*(h2^3))/12
    val Iy2 = BasicSymbols.I|"y,2" is "moduł bezwładności segmentu półki względem osi Y" unit "cm4" acc 1
    Iy2 := (h2*(b2^3))/12
    val Iz = BasicSymbols.I|"y" is "moduł bezwładności przekroju złożonego względem osi Z" unit "cm4" acc 1
    Iz := Iz2+4*(Iz1+A1*(ay^2))
    val Iy = BasicSymbols.I|"z" is "moduł bezwładności przekroju złożonego względem osi Y" unit "cm4" acc 1
    Iy := Iy2+4*(Iy1+A1*(az^2))
    val Imin = BasicSymbols.I|"min" is "mniejszy z modułów bezwładności" unit "cm4" acc 1
    Imin := min(Iz,Iy)
    val imin = BasicSymbols.i|"min" is "promień bezwładności" unit "cm"
    imin := sqrt(Imin/As)
    val wlasciwosciGeometryczne = Seq(ay,az,A1,A2,As,Iz1,Iy1,Iz2,Iy2,Iz,Iy,Imin,imin)
    
    val d = BasicSymbols.d is "przyjęta średnica wewnętrzna trzpienia dla wkręta samowiercącego SPAX&reg; T-STAR T40(8mm)" unit "mm"
    d := 5.6
    val deff = BasicSymbols.d|"ef" is "przyjęta efektywna średnica trzpienia wkręta" unit "mm"
    deff := 6.0
    val s1min = s|"1,min" is "minimalny rozstaw wkrętów w szeregu wzdłuż włókien" unit "mm"
    s1min := (5+5)*d
    val s1 = s|"1" is "przyjęty rozstaw wkrętów w szeregu wzdłuż włókien " unit "mm"
    s1 := 60
    val nh = n|"h" is "przyjęta ilość wkrętów w rzędzie na szerokości półki"
    nh := 1
    val np = n|"p" is "ilość płaszczyzn styku środnika z półką wg B.1.3(1) [1]"
    np := 2
    val s1eff = s|"1,eff" is "efektywny rozstaw wkrętów w szeregu wzdłuż włókien" unit "mm"
    s1eff := s1/(nh*np)
    val s2 = s|"2" is "minimalny rozstaw wkrętów w poprzek włókien wg tablicy 8.2 [1]" unit "mm"
    s2 := 5*d
    val s3c = s|"3,c" is "minimalna odległość wkręta od końca nieobciążonego" unit "mm"
    s3c := 10*d
    val s3t = s|"3,t" is "minimalna odległość wkręta od końca obciążonego" unit "mm"
    s3t := 15*d
    val s4c = s|"4,c" is "minimalna odległość wkręta od boku nieobciążonego" unit "mm"
    s4c := 5*d
    val s4t = s|"4,t" is "minimalna odległość wkręta od boku obciążonego" unit "mm"
    s4t := 5*d
    val t2 = t|"2" is "minimalna długość zakotwienia wkręta w elemencie od strony ostrza" unit "mm"
    t2 := 6*d
    val lw = l|"w" is "wymagana długość wkręta" unit "mm"
    lw := round(b1+t2,RoundingMode.Step(true,10))
    val nw = n|"w" is "całkowita liczba wkrętów w słupie"
    nw := 4*nh* ceil((l0 - 2 * s3c) / s1)
    val przyjeteLaczniki = Seq(d,deff,s1min,s1,nh,np,s1eff,s2,s3c,s3t,s4c,s4t,t2,lw,nw)
    
    val kmod = k|"mod" is """współczynnik modyfikujący efekt czasu trwania obciążenia i zmiany wilgotności materiału,
 przyjęty dla 2 klasy użytkowania i oddziaływania długotrwałego z Tab. 3.1 [1]"""
    calc3(kmod) = 0.7
    val gamM = gamma|M is """częściowy współczynnik bezpieczeństwa właściwości materiału, uwzględniający także
 niedoskonałości modelowania i odchyłki wymiarowe, przyjęty dla drewna litego z Tab. 2.3 [1]"""
    calc3(gamM) = 1.3
    val wspolczynnikiCzesciowe = Seq(kmod,gamM)
    
    val fmk = f|"m,k" is "wytrzymałość charakterystyczna na zginanie" unit "MPa"
    calc3(fmk) = 27
    val ft0k = f|"t,0,k" is "wytrzymałość charakterystyczna na rozciąganie wzdłuż włókien" unit "MPa"
    calc3(ft0k) = 16
    val ft90k = f|"t,90,k" is "wytrzymałość charakterystyczna na rozciąganie w poprzek włókien" unit "MPa"
    calc3(ft90k) = 0.4
    val fc0k = f|"c,0,k" is "wytrzymałość charakterystyczna na ściskanie wzdłuż włókien" unit "MPa"
    calc3(fc0k) = 22
    val fc90k = f|"c,90,k" is "wytrzymałość charakterystyczna na ściskanie w poprzek włókien" unit "MPa"
    calc3(fc90k) = 2.6
    val fvk = f|"v,k" is "wytrzymałość charakterystyczna na ścinanie" unit "MPa"
    calc3(fvk) = 4
    val E0mean = E|"0,mean" is "średni moduł sprężystości wzdłuż włókien" unit "GPa"
    calc3(E0mean) = 11.5
    val E005 = E|"0,05" is "5 % kwantyl modułu sprężystości" unit "GPa"
    calc3(E005) = 7.7
    val E90mean = E|"90,mean" is "średni moduł sprężystości w poprzek włókien" unit "GPa"
    calc3(E90mean) = 0.38
    val Gmean = G|"mean" is "średni moduł odkształcenia postaciowego" unit "GPa"
    calc3(Gmean) = 0.72
    val rhok = rho|"k" is "gęstość charakterystyczna" unit "kg/m3"
    calc3(rhok) = 370
    val rhom = rho|"m" is "gęstość średnia" unit "kg/m3"
    calc3(rhom) = 450
    val wlasciwosciMechaniczneCharakterystyczne = Seq(fmk,ft0k,ft90k,fc0k,fc90k,fvk,E0mean,E005,E90mean,Gmean,rhok,rhom)
    
    val fmd = f|"md" is "wytrzymałość obliczeniowa na zginanie" unit "MPa"
    calc3(fmd) = kmod*fmk/gamM
    val ft0d = f|"t,0,d" is "wytrzymałość obliczeniowa na rozciąganie wzdłuż włókien" unit "MPa"
    calc3(ft0d) = kmod*ft0k/gamM
    val ft90d = f|"t,90,d" is "wytrzymałość obliczeniowa na rozciąganie w poprzek włókien" unit "MPa"
    calc3(ft90d) = kmod*ft90k/gamM
    val fc0d = f|"c,0,d" is "wytrzymałość obliczeniowa na ściskanie wzdłuż włókien" unit "MPa"
    calc3(fc0d) = kmod*fc0k/gamM
    val fc90d = f|"c,90,d" is "wytrzymałość obliczeniowa na ściskanie w poprzek włókien" unit "MPa"
    calc3(fc90d) = kmod*fc90k/gamM
    val fvd = f|"vd" is "wytrzymałość obliczeniowa na ścinanie" unit "MPa"
    calc3(fvd) = kmod*fvk/gamM
    val E0d = E|"0,d" is "obliczeniowy moduł sprężystości wzdłuż włókien" unit "GPa"
    calc3(E0d) = E0mean/gamM
    val E90d = E|"90,d" is "obliczeniowy moduł sprężystości w poprzek włókien" unit "GPa"
    calc3(E90d) = E90mean/gamM
    val Gdd = G|"d" is "obliczeniowy moduł odkształcenia postaciowego" unit "GPa"
    calc3(Gdd) = Gmean/gamM
    val wlasciwosciMechaniczneObliczeniowe = Seq(fmd,ft0d,ft90d,fc0d,fc90d,fvd,E0d,E90d,Gdd)
    
    val lc = l|c is "długość wyboczeniowa" unit "cm"
    calc2(lc) = mic*l0
    val betac = beta|c is "współczynnik prostoliniowości elementów wg 6.29 [1]"
    calc2(betac) = 0.2
    
    val Kser = K|"ser" is "moduł podatności łączników mechanicznych dla stanu granicznego nośności (SGN) wg tablicy 7.1 [1]" unit "N/mm"
    Kser := (((rhom).nounit^1.5)*(deff).nounit)/23
    val Ku = K|"u" is "moduł podatności łączników mechanicznych dla stanu granicznego użytkowalności (SGU) wg 2.1 [1]" unit "N/mm"
    Ku := (2*Kser)/3
    val gamma1SGN = (gamma|"1") is "współczynnik zmniejszający moment bezwładności półek ze względu na podatność połączeń dla SGN wg B.5 [1]"
    gamma1SGN := (1+(((PI^2)*E0mean*A1*(s1eff))/(Ku*(l0^2))))^(-1)
    val gamma1SGU = (gamma|"1")!"SGU" is "współczynnik zmniejszający moment bezwładności półek ze względu na podatność połączeń dla SGU wg B.5 [1]"
    gamma1SGU := (1+(((PI^2)*E0mean*A1*(s1eff))/(Kser*(l0^2))))^(-1)
    val EIzeffSGN = (Symbol("(EI)")|"z,ef") is "sztywność zastępcza względem osi Z dla SGN wg B.1 [1]" unit SI.kN*SI.m2
    EIzeffSGN := E0mean*(Iz2+4*(Iz1+gamma1SGN*A1*(ay^2)))
    val EIzeffSGU = (Symbol("(EI)")|"z,ef")!"SGU" is "sztywność zastępcza względem osi Z dla SGU wg B.1 [1]" unit SI.kN*SI.m2
    EIzeffSGU := E0mean*(Iz2+4*(Iz1+gamma1SGU*A1*(ay^2)))
    val EIyeffSGN = (Symbol("(EI)")|"y,ef") is "sztywność zastępcza względem osi Y dla SGN wg B.1 [1]" unit SI.kN*SI.m2
    EIyeffSGN := E0mean*(Iy2+4*(Iy1+gamma1SGN*A1*(az^2)))
    val EIyeffSGU = (Symbol("(EI)")|"y,ef")!"SGU" is "sztywność zastępcza względem osi Y dla SGU wg B.1 [1]" unit SI.kN*SI.m2
    EIyeffSGU := E0mean*(Iy2+4*(Iy1+gamma1SGU*A1*(az^2)))
    val EIeffSGN = (Symbol("(EI)")|"ef") is "sztywność zastępcza dla SGN" unit SI.kN*SI.m2
    EIeffSGN := min(EIzeffSGN,EIyeffSGN)
    val EIeffSGU = (Symbol("(EI)")|"ef")!"SGU" is "sztywność zastępcza dla SGU" unit SI.kN*SI.m2
    EIeffSGU := min(EIzeffSGU,EIyeffSGU)
    val xiEI = xi|"(EI)" is "stosunek sztywności zastępczych względem osi Y/Z" unit "%"
    xiEI := (EIyeffSGN/EIzeffSGN)*100
    val sztywnoscZastepcza = Seq(Kser,Ku,gamma1SGN/*,gamma1SGU*/,EIzeffSGN,EIyeffSGN,EIeffSGN/*,EIzeffSGU,EIyeffSGU,EIeffSGU*/,xiEI)
    
    val Ief = I|"ef" is "efektywny moduł bezwładności wg C.4 [1]" unit "cm4" acc 1
    Ief := EIeffSGN/E0mean
    val lambdaeff = lambda|"ef" is "smukłość efektywna wg C.3 [1]"
    lambdaeff := lc*sqrt(As/Ief)
    val lambdarel = lambda|"rel" is "smukłość względna wg 6.22 [1]"
    lambdarel := ((lambdaeff/PI)*sqrt(fc0k/E005)).nounit
    val kmin = k|"min" is "współczynnik pomocniczy do obliczenia współczynnika wyboczeniowego wg 6.27 [1]"
    kmin := 0.5*(1+betac*(lambdarel-0.3)+(lambdarel^2))
    val kc = k|c is "współczynnik wyboczeniowy wg 6.25 [1]"
    kc := 1/(kmin+sqrt((kmin^2)-(lambdarel^2)))
    val sigmac0d = sigma|"c,0,d" is "obliczeniowe naprężenie ściskające wzdłuż włókien wg C.2 [1]" unit "MPa"
    sigmac0d := Fcd/As
    val NRc = N|"R,c" is "nośność obliczeniowa na ściskanie" unit "kN"
    NRc := fc0d*As
    val Fmax = F|"max" is "maksymalna osiowa siła ściskająca" unit "kN"
    Fmax := kc*NRc
    val xiS = BasicSymbols.xi|"c" is "wytężenie słupa na ściskanie" unit "%"
    xiS := (Fcd/Fmax)*100
    val nosnoscObliczeniowa = Seq(lc,betac,Ief,lambdaeff,lambdarel,kmin,kc,sigmac0d,NRc,Fmax,xiS)

    val Vd = V|"d" is "siła ścinająca wg C.5 [1]" unit "kN"
    Vd := Fcd/(120*kc) unless (InRangeLLE(30,lambdaeff,60) thenUse (Fcd/(3600*kc))) unless (GreaterThan(lambdaeff,60) thenUse (Fcd/(60*kc)))
    val tau2max = tau|"2,max" is "maksymalne naprężenia ścinające w środniku wg B.9 [1]" unit "MPa"
    tau2max := E0mean*(gamma1SGN*A1*az+0.5*b2*(h^2))/(b2*EIeffSGN)*Vd
    val nosnoscScinanie = Seq(Vd,tau2max)
    
    val Fi = F|"i" is "obciążenie łącznika wg B.10 [1]" unit "kN"
    Fi := ((gamma1SGN*E0mean*A1*ay* s1eff)/EIeffSGN)*Vd
    val kef = k|"ef" is "współczynnik efektywnej liczby łączników w szeregu z tablicy 8.1 [1]"
    kef := 0.85
    val betal = beta|l is "stosunek wytrzymałości charakterystycznych na docisk łącznika do elementów złącza"
    betal := 1
    val fhk = f|"h,k" is "wytrzymałość charakterystyczna na docisk w elemencie drewnianym wg 8.16 [1]" unit "N/mm2"
    fhk := 0.082*(1-0.01* deff.nounit)* rhok.nounit
    val MyRk = M|"y,Rk" is "moment charakterystyczny uplastycznienia wkręta wg 2.2.1.a [2]" unit SI.N*SI.mm
    MyRk := 75*(d.nounit^2.6)
    val FaxRk = F|"ax,Rk" is "nośność charakterystyczna łącznika na wyciąganie" unit "kN"
    FaxRk := 0
    val FvRk = F|"v,Rk" is "nośność charakterystyczna łącznika odniesiona do jednej płaszczyzny ścinania wg 8.6 [1]" unit "kN"
    val FvRka = F|"v,Rk,a" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia a" unit "kN"
    val FvRkb = F|"v,Rk,b" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia b" unit "kN"
    val FvRkc = F|"v,Rk,c" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia c" unit "kN"
    val FvRkd = F|"v,Rk,d" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia d" unit "kN"
    val FvRke = F|"v,Rk,e" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia e" unit "kN"
    val FvRkf = F|"v,Rk,f" is "nośność charakterystyczna łącznika jednociętego dla schematu zniszczenia f" unit "kN"
    FvRka := fhk*b1* deff
    FvRkb := fhk*b2* deff
    FvRkc := (fhk*b1* deff /(1+betal))*(sqrt(betal+2*(betal^2)*(1+b2/b1+((b2/b1)^2))+(betal^3)*((b2/b1)^2))-betal*(1+(b2/b1)))+FaxRk/4
    FvRkd := 1.05*(fhk*b1* deff /(2+betal))*(sqrt(2*betal*(1+betal)+((4*betal*(2+betal)*MyRk)/(fhk*deff*(b1^2))))-betal)+FaxRk/4
    FvRke := 1.05*(fhk*b2* deff /(1+2*betal))*(sqrt(2*(betal^2)*(1+betal)+((4*betal*(1+2*betal)*MyRk)/(fhk*deff*(b2^2))))-betal)+FaxRk/4
    FvRkf := (1.15*sqrt((2*betal)/(1+betal))*sqrt(2*MyRk*fhk*deff)+FaxRk/4).nounit
    FvRk := min(FvRka,FvRkb,FvRkc,FvRkd,FvRke,FvRkf)
    val FvRd = F|"v,Rd" is "nośność obliczeniowa łącznika" unit "kN"
    FvRd := (kmod*FvRk)/gamM
    
    val nosnoscLacznikow = Seq(Fi,kef,betal,fhk,MyRk,FvRka,FvRkb,FvRkc,FvRkd,FvRke,FvRkf,FvRk,FvRd)
    
    val lambdac = lambda|"c" is "smukłość słupa"
    calc2(lambdac) = lc/imin
    val lambdarel2 = lambda|("rel","'") is "smukłość względna słupa"
    calc2(lambdarel2) = ((lambdac/PI)*sqrt(fc0k/E005)).nounit
    val kmin2 = k|("min","'") is "współczynnik pomocniczy do obliczenia współczynnika wyboczeniowego"
    calc2(kmin2) = 0.5*(1+betac*(lambdarel2-0.3)+(lambdarel2^2))
    val kc2 = k|("c","'") is "współczynnik wyboczeniowy"
    calc2(kc2) = 1/(kmin2+sqrt((kmin2^2)-(lambdarel2^2)))
    val NRc2 = N|("R,c","'") is "nośność obliczeniowa na ściskanie" unit "kN"
    calc2(NRc2) = fc0d*As
    val Fmax2 = F|("max","'") is "maksymalna osiowa siła ściskająca" unit "kN"
    calc2(Fmax2) = kc2*NRc2
    val deltaJW = delta|"1" is "strata nośności maksymalnej słupa spowodowana zastosowaniem łączników podatnych" unit "%"
    calc2(deltaJW) = ((Fmax2-Fmax)/Fmax2)*100
    
    val nosnoscSlupaNiepodatnie = Seq(lambdac,lambdarel2,kmin2,kc2,NRc2,Fmax2,deltaJW)
    
    val arec = a|"rec" is "długość boku słupa kwadratowego" unit "m"
    val Sec2 = Square("rec",arec)
    val calc4 = Calculation("slup kwadratowy jednorodny")
    calc4(arec) = 0.138
    calc4(As) = Sec2.A
    calc4(imin) = Sec2.imin
    calc4 add calc2
    calc4 add calc3
    calc4 add Sec2
    val Aof1 = this(As)
    val Fmaxof1 = this(Fmax)
    val xi2 = xi|"2" is "stosunek nośności na ściskanie słupa kwadratowego i zaprojektowanego" unit "%"
    calc4(xi2) = (Fmax2/Fmaxof1)*100
    val delta2 = delta|"2" is "zmiana pola przekroju słupa wielogałęziowego w stosunku do słupa kwadratowego o zbliżonej niegorszej nośności" unit "%"
    calc4(delta2) = ((Aof1-Sec2.A)/Aof1)*100
    
    val przekrojKwadratowy = Seq(arec,Sec2.A,Sec2.Imin,Sec2.imin,lambdac,lambdarel2,kmin2,kc2,NRc2,Fmax2,xi2,delta2)

    override val document = Document("",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe nr 1 z \"Konstrukcji Drewnianych\". Semestr zimowy 2010/2011."),
        		Section("Autor: Artur Opala. Prowadzący: dr inż. Tomasz Nowak, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            Section(styleTitle,"Słup wielogałęziowy z drewna litego klasy C27 łączonego na wkręty SPAX&reg; T-STAR T40(8mm)."),
            NumSection("Zadanie",
                Section(styleComment,"Obciążenie siłą osiową, zamocowanie obustronnie przegubowe."),
                Section(styleComment,"Konstrukcja w 2 klasie użytkowania wg normy [1] pkt. 2.3.1.3."),
                NumSection("Normy i literatura",
                	Section(styleComment," [1] Norma PN-EN 1995-1-1:2010 \"Eurokod 5. Projektowanie konstrukcji drewnianych. Część 1-1: Postanowienia ogólne. Reguły ogólne i reguły dotyczące budynków\""),
                	Section(styleComment," [2] Aprobata ITB AT-15-7343/2007 \"Wkręty samowiercące SPAX&reg; do konstrukcji drewnianych\""),
                	Section(styleComment," [3] Norma PN-EN 338:2009")
                ),
                NumSection("Dane wejściowe",Evaluate(daneWejsciowe:_*))
           ),
           NumSection("Dane do obliczeń",
                NumSection("Przyjęte wymiary przekroju słupa",Evaluate(przyjetaGeometria:_*)),
                NumSection("Przyjęte łączniki",Evaluate(przyjeteLaczniki:_*)),
                NumSection("Właściwości geometryczne przekroju",Evaluate(wlasciwosciGeometryczne:_*)),
                NumSection("Współczynniki",Evaluate(wspolczynnikiCzesciowe:_*)),
                NumSection("Właściwości mechaniczne charakterystyczne dla drewna litego klasy C27 wg [3]",Evaluate(wlasciwosciMechaniczneCharakterystyczne:_*)),
                NumSection("Właściwości mechaniczne obliczeniowe dla 2 klasy użytkowania i obciążeń długotrwałych",Evaluate(wlasciwosciMechaniczneObliczeniowe:_*))),
           NumSection("Sprawdzenie stanów granicznych nośności wg PN-EN 1995-1-1",
                NumSection("Obliczenie sztywności zastępczej",Evaluate(sztywnoscZastepcza:_*)),
                NumSection("Sprawdzenie nośności na ściskanie",Evaluate(nosnoscObliczeniowa:_*)),
                AssertionLE("C.1 [1]",sigmac0d,kc*fc0d),
				NumSection("Sprawdzenie nośności środnika na ścinanie",Evaluate(nosnoscScinanie:_*)),
				AssertionLE("6.13 [1]",tau2max,fvd),
				NumSection("Sprawdzenie nośności łączników (wkrętów)",Evaluate(nosnoscLacznikow:_*)),
				AssertionLE("nośności wkręta",Fi,FvRd)
			),
			NumSection("Obliczenia porównawcze",
				NumSection("Porównanie ze słupem wielogałęziowym z elementów połączonych niepodatnie (klejonych)",Evaluate(nosnoscSlupaNiepodatnie:_*)),
				NumSection("Porównanie ze słupem jednorodnym kwadratowym o zbliżonej nośności na ściskanie",Evaluate(przekrojKwadratowy:_*)(calc4)),
				NumSection("Wnioski",
					Section(styleComment,"Porównanie zaprojektowanego słupa ze słupem klejonym wykazało ",Result(round(deltaJW)),
					" utratę nośności ze względu na podatność łączników."),
					Section(styleComment,"Porównanie zaprojektowanego słupa ze słupem jednorodnym kwadratowym o zbliżonej nośności wykazało ",Result(round(delta2,RoundingMode.HALF))(calc4),
					" stratę na przekroju słupa wielogałęziowego. Zastosowanie słupa o przekroju kwadratowym przy zadanym obciążeniu byłoby bardziej uzasadnione.")
				)
			),
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")
        )
    )

}
