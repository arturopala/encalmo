package org.encalmo.examples

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.document.StylesConfigSymbols._
import org.encalmo.examples.Predefined._

class SlupDrewnianyZlozonyWkrety extends AssertionsForJUnit {
    
    import BasicSymbols._
    
    val calc = Calculation("1")
    
    val description = """Słup wielogałęziowy z drewna litego klasy C27 łączonego na wkręty typu 'SPAX T-STAR plus' z łbem stożkowym. Obciążenie siłą osiową, zamocowanie 
obustronnie przegubowe. Konstrukcja w 2 klasie użytkowania wg normy [1] pkt. 2.3.1.3."""
    
    val l0 = l|0 is "rozpiętość obliczeniowa" unit "m"
    calc(l0) = 3.8
    val S = BasicSymbols.S is "siła ściskająca osiowo" unit "N"
    calc(S) = 68000
    val daneWejsciowe = Seq(l0,S)
    
    val b1 = BasicSymbols.b|1 is "szerokość przekroju półki" unit "m"
    calc(b1) = 0.06
    val h1 = BasicSymbols.h|1 is "wysokość przekroju półki" unit "m"
    calc(h1) = 0.1
    val b2 = BasicSymbols.b|2 is "szerokość przekroju środnika" unit "m"
    calc(b2) = 0.06
    val h2 = BasicSymbols.h|2 is "wysokość przekroju środnika" unit "m"
    calc(h2) = 0.24
    val b = BasicSymbols.b is "szerokość całkowita przekroju złożonego" unit "m"
    calc(b) = b2+2*b1
    val h = BasicSymbols.h is "wysokość całkowita przekroju złożonego" unit "m"
    calc(h) = h2
    val ay = BasicSymbols.a|"y" is "odległość środka ciężkości półki od środka ciężkości przekroju złożonego wzdłuż osi Y" unit "m"
    calc(ay) = (h-h1)/2
    val ax = BasicSymbols.a|"x" is "odległość środka ciężkości półki od środka ciężkości przekroju złożonego wzdłuż osi X" unit "m"
    calc(ax) = (b-b1)/2
    val przyjetaGeometria = Seq(b1,h1,b2,h2,b,h)
    
    val A1 = BasicSymbols.A|1 is "pole powierzchni segmentu półki" unit "m²"
    calc(A1) = b1*h1
    val A2 = BasicSymbols.A|2 is "pole powierzchni środnika" unit "m²"
    calc(A2) = b2*h2
    val A = BasicSymbols.A is "całkowite pole powierzchni przekroju" unit "m²"
    calc(A) = A2+4*A1
    val Iy1 = BasicSymbols.I|"y,1" is "moduł bezwładności segmentu półki względem osi Y" unit "m4"
    calc(Iy1) = (b1*(h1^3))/12
    val Ix1 = BasicSymbols.I|"x,1" is "moduł bezwładności segmentu półki względem osi X" unit "m4"
    calc(Ix1) = (h1*(b1^3))/12
    val Iy2 = BasicSymbols.I|"y,2" is "moduł bezwładności środnika względem osi Y" unit "m4"
    calc(Iy2) = (b2*(h2^3))/12
    val Ix2 = BasicSymbols.I|"x,2" is "moduł bezwładności segmentu półki względem osi X" unit "m4"
    calc(Ix2) = (h2*(b2^3))/12
    val wlasciwosciGeometryczne = Seq(ay,ax,A1,A2,A,Iy1,Ix1,Iy2,Ix2)
    
    val kmod = k|"mod" is """współczynnik modyfikujący efekt czasu trwania obciążenia i zmiany wilgotności materiału,
 przyjęty dla 2 klasy użytkowania i oddziaływania długotrwałego z Tab. 3.1 [1]"""
    calc(kmod) = 0.7
    val gamM = gamma|M is """częściowy współczynnik bezpieczeństwa właściwości materiału, uwzględniający także
 niedoskonałości modelowania i odchyłki wymiarowe, przyjęty dla drewna litego z Tab. 2.3 [1]"""
    calc(gamM) = 1.3
    val wspolczynnikiCzesciowe = Seq(kmod,gamM)
    
    val fmk = f|"m,k" is "wytrzymałość charakterystyczna na zginanie" unit "Pa"
    calc(fmk) = 27000000
    val ft0k = f|"t,0,k" is "wytrzymałość charakterystyczna na rozciąganie wzdłuż włókien" unit "Pa"
    calc(ft0k) = 16000000
    val ft90k = f|"t,90,k" is "wytrzymałość charakterystyczna na rozciąganie w poprzek włókien" unit "Pa"
    calc(ft90k) = 400000
    val fc0k = f|"c,0,k" is "wytrzymałość charakterystyczna na ściskanie wzdłuż włókien" unit "Pa"
    calc(fc0k) = 22000000
    val fc90k = f|"c,90,k" is "wytrzymałość charakterystyczna na ściskanie w poprzek włókien" unit "Pa"
    calc(fc90k) = 2600000
    val fvk = f|"v,k" is "wytrzymałość charakterystyczna na ścinanie" unit "Pa"
    calc(fvk) = 4000000
    val E0mean = E|"0,mean" is "średni moduł sprężystości wzdłuż włókien" unit "Pa"
    calc(E0mean) = 11500000000l
    val E005 = E|"0,mean" is "5 % kwantyl modułu sprężystości" unit "Pa" 
    calc(E005) = 7700000000l
    val E90mean = E|"90,mean" is "średni moduł sprężystości w poprzek włókien" unit "Pa" 
    calc(E90mean) = 380000000
    val Gmean = G|"mean" is "średni moduł odkształcenia postaciowego" unit "Pa" 
    calc(Gmean) = 720000000
    val rhok = rho|"k" is "gęstość charakterystyczna" unit "kg/m³"
    calc(rhok) = 370
    val rhom = rho|"m" is "gęstość średnia" unit "kg/m³"
    calc(rhom) = 450
    val wlasciwosciMechaniczneCharakterystyczne = Seq(fmk,ft0k,ft90k,fc0k,fc90k,fvk,E0mean,E005,E90mean,Gmean,rhok,rhom)
    
    val fmd = f|"md" is "wytrzymałość obliczeniowa na zginanie" unit "Pa"
    calc(fmd) = kmod*fmk/gamM
    val ft0d = f|"t,0,d" is "wytrzymałość obliczeniowa na rozciąganie wzdłuż włókien" unit "Pa"
    calc(ft0d) = kmod*ft0k/gamM
    val ft90d = f|"t,90,d" is "wytrzymałość obliczeniowa na rozciąganie w poprzek włókien" unit "Pa"
    calc(ft90d) = kmod*ft90k/gamM
    val fc0d = f|"c,0,d" is "wytrzymałość obliczeniowa na ściskanie wzdłuż włókien" unit "Pa"
    calc(fc0d) = kmod*fc0k/gamM
    val fc90d = f|"c,90,d" is "wytrzymałość obliczeniowa na ściskanie w poprzek włókien" unit "Pa"
    calc(fc90d) = kmod*fc90k/gamM
    val fvd = f|"vd" is "wytrzymałość obliczeniowa na ścinanie" unit "Pa"
    calc(fvd) = kmod*fvk/gamM
    val E0d = E|"0,d" is "obliczeniowy moduł sprężystości wzdłuż włókien" unit "Pa"
    calc(E0d) = E0mean/gamM
    val E90d = E|"90,d" is "obliczeniowy moduł sprężystości w poprzek włókien" unit "Pa"
    calc(E90d) = E90mean/gamM
    val Gdd = G|"d" is "obliczeniowy moduł odkształcenia postaciowego" unit "Pa"
    calc(Gdd) = Gmean/gamM
    val wlasciwosciMechaniczneObliczeniowe = Seq(fmd,ft0d,ft90d,fc0d,fc90d,fvd,E0d,E90d,Gdd)
    
    val dmax = BasicSymbols.d|"max" is "maksymalna średnica trzpienia wkręta wg 8.19 [1]" unit "mm"
    calc(dmax) = min(b1,b2)/14*1000
    val d = BasicSymbols.d is "przyjęta średnica trzpienia wkręta" unit "mm"
    calc(d) = 4
    val deff = BasicSymbols.d|"eff" is "efektywna średnica trzpienia wkręta" unit "mm"
    calc(deff) = 1.1*d
    val s1 = s|1 is "przyjęty rozstaw wkrętów w szeregu wzdłuż włókien" unit "mm"
    calc(s1) = (5+5)*d
    val s2 = s|1 is "minimalny rozstaw wkrętów w poprzek włókien" unit "mm"
    calc(s2) = 5*d
    val s3c = s|"3,c" is "minimalna odległość wkręta od końca nieobciążonego" unit "mm"
    calc(s3c) = 10*d
    val s3t = s|"3,t" is "minimalna odległość wkręta od końca obciążonego" unit "mm"
    calc(s3t) = 15*d
    val s4c = s|"4,c" is "minimalna odległość wkręta od boku nieobciążonego" unit "mm"
    calc(s4c) = 5*d
    val s4t = s|"4,t" is "minimalna odległość wkręta od boku obciążonego" unit "mm"
    calc(s4t) = 5*d
    val t2 = t|"2" is "minimalna długość zakotwienia łącznika" unit "mm"
    calc(t2) = 6*d
    val lw = l|"w" is "minimalna długość łącznika" unit "mm"
    calc(lw) = (b1*1000)+t2
    val przyjeteLaczniki = Seq(dmax,d,deff,s1,s2,s3c,s3t,s4c,s4t,t2,lw)
    
    val Kser = K|"ser" is "moduł podatności łączników mechanicznych dla stanu granicznego nośności (SGN)" unit "N/m"
    calc(Kser) = ((rhom^1.5)*deff*0.001)/23
    val Ku = K|"u" is "moduł podatności łączników mechanicznych dla stanu granicznego użytkowalności (SGU)" unit "N/m"
    calc(Ku) = (2*Kser)/3
    val gamma1SGN = (gamma|"1")!"SGN" is "współczynnik zmniejszający moment bezwładności półek ze względu na podatność połączeń dla SGN"
    calc(gamma1SGN) = (1+(((PI^2)*E0mean*A1*(s1*0.001))/(Kser*(l0^2)*1000000)))^(-1)
    val gamma1SGU = (gamma|"1")!"SGU" is "współczynnik zmniejszający moment bezwładności półek ze względu na podatność połączeń dla SGU"
    calc(gamma1SGU) = (1+(((PI^2)*E0mean*A1*(s1*0.001))/(Ku*(l0^2)*1000000)))^(-1)
    val EIyeffSGN = (Symbol("(EI)")|"y,eff")!"SGN" is "Sztywnośc zastępcza względem osi Y dla SGN" unit "Nm²" 
    calc(EIyeffSGN) = E0mean*(Iy2+4*(Iy1+gamma1SGN*A1*(ay^2)))
    val EIyeffSGU = (Symbol("(EI)")|"y,eff")!"SGU" is "Sztywnośc zastępcza względem osi Y dla SGU" unit "Nm²" 
    calc(EIyeffSGU) = E0mean*(Iy2+4*(Iy1+gamma1SGU*A1*(ay^2)))
    val EIxeffSGN = (Symbol("(EI)")|"x,eff")!"SGN" is "Sztywnośc zastępcza względem osi X dla SGN" unit "Nm²" 
    calc(EIxeffSGN) = E0mean*(Ix2+4*(Ix1+gamma1SGN*A1*(ax^2)))
    val EIxeffSGU = (Symbol("(EI)")|"x,eff")!"SGU" is "Sztywnośc zastępcza względem osi X dla SGU" unit "Nm²" 
    calc(EIxeffSGU) = E0mean*(Ix2+4*(Ix1+gamma1SGU*A1*(ax^2)))
    val EIeffSGN = (Symbol("(EI)")|"eff")!"SGN" is "Sztywnośc zastępcza dla SGN" unit "Nm²" 
    calc(EIeffSGN) = min(EIyeffSGN,EIxeffSGN)
    val EIeffSGU = (Symbol("(EI)")|"eff")!"SGU" is "Sztywnośc zastępcza dla SGU" unit "Nm²" 
    calc(EIeffSGU) = min(EIyeffSGU,EIxeffSGU)
    val sztywnoscZastepcza = Seq(Kser,Ku,gamma1SGN,gamma1SGU,EIyeffSGN,EIxeffSGN,EIeffSGN,EIyeffSGU,EIxeffSGU,EIeffSGU)
    
    
    
    
    val LE = "&nbsp;&le;&nbsp;"
    val ARROW = "&nbsp;&rArr;&nbsp;"
    
    val doc1 = Document(Predefined.style1,"",
        Predefined.stylesConfig,
        Chapter("",
            Section("Ćwiczenie projektowe nr 1 z \"Konstrukcji Drewnianych\". Autor: Artur Opala 61315. Wrocław 2010/2011"),
            Section(""),
            NumSection("Dane do projektowania",
                Section(style1.fontItalic.fontSmaller.marginLeft(20),Text(description)),
                NumSection("Dane wejściowe",Evaluate(calc,daneWejsciowe:_*)),
                NumSection("Przyjęte wymiary przekroju słupa",Evaluate(calc,przyjetaGeometria:_*)),
                NumSection("Właściwości geometryczne przekroju",Evaluate(calc,wlasciwosciGeometryczne:_*)),
                NumSection("Przyjęte łączniki",Evaluate(calc,przyjeteLaczniki:_*)),
                NumSection("Współczynniki",Evaluate(calc,wspolczynnikiCzesciowe:_*)),
                NumSection("Właściwości mechaniczne charakterystyczne dla drewna litego klasy C27",Evaluate(calc,wlasciwosciMechaniczneCharakterystyczne:_*)),
                NumSection("Właściwości mechaniczne obliczeniowe dla 2 klasy użytkowania i obciążeń długotrwałych",Evaluate(calc,wlasciwosciMechaniczneObliczeniowe:_*))),
           NumSection("Sprawdzenie stanów granicznych nośności wg PN-EN 1995-1-1",     
                NumSection("Obliczenie sztywności zastępczej",Evaluate(calc,sztywnoscZastepcza:_*))
//                NumSection("Współczynniki częściowe dla oddziaływań",Evaluate(calc,gamG1,gamQ1,gamQi,psi0i)),
//                NumSection("Kombinacje obciążeń",Evaluate(calc,qd)),
//                NumSection("Współczynnik ",Symb(kmod),Evaluate(calc,kmod)),
//                NumSection("Współczynnik materiałowy ",Symb(gamM),Evaluate(calc,gamM)),
//                NumSection("Wytrzymałości i moduły charakterystyczne wg PN-EN 1194:2000",
//                        Evaluate(calc,fmk,ft0k,ft90k,fc0k,fc90k,fvk,E0mean,E005,E90mean,Gmean)),
//                NumSection("Wytrzymałości i moduły obliczeniowe",
//                        Evaluate(calc,fmd,ft0d,ft90d,fc0d,fc90d,fvd,E0d,E90d,Gdd))
            )/*,
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
            )*/
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