package org.encalmo.structures.eurocode.concrete

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.style.PredefinedStyles
import PredefinedStyles._
import org.encalmo.structures.eurocode.actions.ActionsSymbols
import org.encalmo.structures.Worksheet
import org.encalmo.style.PredefinedStyles

class DomeWorksheet extends Worksheet("dome") {
        
    import BasicSymbols._
    import SI.{mm}
    
    val concrete = Concrete.C_30_37
    val reinforcement = ReinforcingSteel.B500SP

    this add concrete
    this add reinforcement

    import concrete._
    import reinforcement.{fyd}
    
    val r = BasicSymbols.r unit SI.m is "Promień rzutu kopuły"
	val R = BasicSymbols.R unit SI.m is "Promień kopuły"
	val H = BasicSymbols.H unit SI.m is "Strzałka kopuły"
	val h = BasicSymbols.h unit SI.m acc 0.01 is "Grubość powłoki"
	val φ = BasicSymbols.phiv unit SI.deg is "Kąt pionowy"
	val Θ = BasicSymbols.theta unit SI.deg is "Kąt poziomy"
	val ζ = BasicSymbols.zeta acc 1 is "Stosunek rozpiętości kopuły do grubości powłoki"
	
	ζ := (2*r)/h
	
	val r0 = BasicSymbols.r|0 unit SI.m is "Promień otworu kopuły"
	val φ0 = BasicSymbols.phiv|0 unit SI.deg is "Kąt otworu kopuły"
	val φ1 = BasicSymbols.phiv|1 unit SI.deg is "Kąt kopuły"
	val φ2 = BasicSymbols.phiv|2 unit SI.deg is "Kąt linii przejściowej"
	
	φ2 := 52 unit SI.deg
	
	val g = BasicSymbols.g unit "kN/m2" is "Obliczeniowy ciężar własny kopuły"
	val s = BasicSymbols.s unit "kN/m2" is "Obliczeniowe obciążenie śniegiem"
	val w = BasicSymbols.w unit SI.kPa is "Obliczeniowe parcie wiatru"
	
	val phis = phi|s is "Średnica prętów zbrojeniowych" unit SI.mm := 8
	
    val dcdev = "Δc"|"dev" is "Dodatek otuliny ze względu na odchyłkę" unit mm acc 1 := 5
    val cminb = "c"|"min,b" is "Minimalne otulenie ze względu na przyczepność" unit mm acc 1 := 1.5*phis
    val cmindur = "c"|"min,dur" is "Minimalne otulenie ze względu na warunki środowiska XC2 dla konstrukcji klasy S4" unit mm acc 1 := 25
    val cmin = c|"min" is "Otulenie minimalne zbrojenia" unit mm acc 1 := max(cminb,cmindur,10 unit mm)
    val cnom = c|"nom" is "Nominalna otulenie zbrojenia" unit mm acc 1 := cmin+dcdev
	
	r := 10 unit SI.m
	r0 := 1 unit SI.m
	φ1 := 90 unit SI.deg
	h := 2*cnom+2*phis
	
	R := r/sin(φ1)
	H := R*(1-cos(φ1))
	φ0 := arcsin(r0/R)
	g := gammac*h*1.35
    
    val sk = symbol(BasicSymbols.s|"k") unit "kN/m2" acc 0.1 is "Wartość charakterystyczna obciążenia gruntu śniegiem"
    val HM = symbol(BasicSymbols.H|"M") is "Wysokość nad poziomem morza dla lokalizacji obiektu"
    val mi1 = symbol(BasicSymbols.mu|"1") is "Współczynnik kształtu dachu"
    val Ce = symbol(BasicSymbols.C|"e") is "Współczynnik ekspozycji"
    val Ct = symbol(BasicSymbols.C|"t") is "Współczynnik termiczny"
    val sr = symbol(BasicSymbols.s|"r") unit "kN/m2" acc 0.1 is "Charakterystyczne obciążenie śniegiem powłoki"
    
    //obciazenie sniegiem
    HM := 140 unit SI.m
    sk := max(0.7,(0.007*HM-1.4).nounit)
    mi1 := 0.8
    Ce := 0.8
    Ct := 1.0
    sr := mi1*Ce*Ct*sk
    s := sr*1.5
    
    val vbo = symbol(BasicSymbols.v|"b,o") unit "m/s" acc 0.1 is "Bazowa prędkość wiatru dla strefy I"
    val qbo = symbol(BasicSymbols.q|"b,o") unit SI.kPa acc 0.1 is "Ciśnienie prędkości wiatru dla strefy I"
    val vmz = symbol(BasicSymbols.v|"m") args(z) unit "m/s" acc 0.1 is "Średnia prędkość wiatru dla II kategorii terenu"
    val crz = symbol(BasicSymbols.c|"r") acc 0.01 args(z) is "Współczynnik chropowatości terenu"
    val coz = symbol(BasicSymbols.c|"o") acc 0.01 args(z) is "Współczynnik rzeźby terenu"
    val cez = symbol(BasicSymbols.c|"e") acc 0.01 args(z) is "Współczynnik ekspozycji"
    val qpz = symbol(BasicSymbols.q|"p") args(z) unit SI.kPa is "Szczytowe ciśnienie prędkości wiatru"
    val we = symbol(BasicSymbols.w|"e") unit SI.kPa is "Maksymalne ciśnienie zewnętrzne od wiatru działające na powłokę"
    val wi = symbol(BasicSymbols.w|"i") unit SI.kPa is "Ciśnienie wewnętrzne od wiatru działające na powłokę"
    val ze = symbol(BasicSymbols.z|"e") is "Wysokość odniesienia dla ciśnienia zewnętrznego"
    val cpe = symbol(BasicSymbols.c|"p,e") acc 0.01 is "Współczynnik ciśnienia zewnętrznego"
    val cpi = symbol(BasicSymbols.c|"p,i") acc 0.01 is "Współczynnik ciśnienia wewnętrznego"
    
    //obciazenie wiatrem
    vbo := 22
    qbo := 0.30
    coz := 1.0
    cez := 2.3*((z/(10 unit SI.m))^0.24)
    qpz := cez*qbo
    cpi := -0.6
    ze := 20 unit SI.m
    we := 1.0*EvalAt(qpz,z -> ze)
    wi := cpi*EvalAt(qpz,z -> ze)
    w := we*1.5
    
    val P = BasicSymbols.P unit "kN/m" is "Obciążenie otworu kopuły" := (-1 unit "kN/m") - s*(r0/2)
    
    val Nφ = N|"φ" unit "kN/m" is "Siła południkowa"
    val NΘ = N|"Θ" unit "kN/m" is "Siła równoleżnikowa"
    
    val Nφg = N|"φ,g" args(φ) unit "kN/m" is "Funkcja siły południkowej od ciężaru własnego"
    val NΘg = N|"Θ,g" args(Θ) unit "kN/m" is "Funkcja siły równoleżnikowej od ciężaru własnego"
    
    val Nφs = N|"φ,s" args(φ) unit "kN/m" is "Funkcja siły południkowej od obciążenia śniegiem"
    val NΘs = N|"Θ,s" args(Θ) unit "kN/m" is "Funkcja siły równoleżnikowej od obciążenia śniegiem"
    
    val Nφw = N|"φ,w" args(φ,Θ) unit "kN/m" is "Funkcja siły południkowej od oddziaływania wiatru"
    val NφΘw = N|"φΘ,w" args(φ,Θ) unit "kN/m" is "Funkcja siły ścinającej od oddziaływania wiatru"
    val NΘw = N|"Θ,w" args(φ,Θ) unit "kN/m" is "Funkcja siły równoleżnikowej od oddziaływania wiatru"
    
    Nφg := -g*R*((cos(φ0)-cos(φ))/(sin(φ)^2))+P*(sin(φ0)/(sin(φ)^2)) // sila poludnikowa od ciezaru wlasnego
    NΘg := g*R*(((cos(φ0)-cos(φ))/(sin(φ)^2))-cos(φ))-P*(sin(φ0)/(sin(φ)^2)) // sila rownoleznikowa od ciezaru wlasnego
    
    Nφs := ((-s*R)/2)*(1-((sin(φ0)^2)/(sin(φ)^2))) // sila poludnikowa od obciazenia sniegiem
    NΘs := ((-s*R)/2)*(cos(2*φ)+((sin(φ0)^2)/(sin(φ)^2))) // sila rownoleznikowa od obciazenia sniegiem
    
    Nφw := w*R*(-frac(2,3)+cos(φ)-frac(1,3)*(cos(φ)^3))*(cos(φ)/(sin(φ)^3))*cos(Θ) // sila poludnikowa od obciazenia wiatrem
    NφΘw := w*R*(-frac(2,3)+cos(φ)-frac(1,3)*(cos(φ)^3))*(1/(sin(φ)^3))*sin(Θ) // sila scinajaca od obciazenia wiatrem
    NΘw := w*R*(frac(2,3)*cos(φ)-(sin(φ)^2)-frac(2,3)*(cos(φ)^4))*(1/(sin(φ)^3))*cos(Θ) // sila rownoleznikowa od obciazenia sniegiem
    
    val Nφ1 = Nφ(1) ## "od ciężaru własnego na krawędzi dolnej"
    val NΘ1 = NΘ(1) ## "od ciężaru własnego na krawędzi dolnej"
    
    val Nφ2 = Nφ(2) ## "od oddziaływania wiatru na krawędzi dolnej"
    val NΘ2 = NΘ(2) ## "od oddziaływania wiatru na krawędzi dolnej"
    
    val Nφ3 = Nφ(3) ## "od ciężaru własnego na poziomie linii przejściowej"
    val NΘ3 = NΘ(3) ## "od ciężaru własnego na poziomie linii przejściowej"
    
    val Nφ4 = Nφ(4) ## "od oddziaływania wiatru na poziomie linii przejściowej"
    val NΘ4 = NΘ(4) ## "od oddziaływania wiatru na poziomie linii przejściowej"
    
    val Nφ5 = Nφ(5) ## "od obciążenia śniegiem na poziomie linii przejściowej"
    val NΘ5 = NΘ(5) ## "od obciążenia śniegiem na poziomie linii przejściowej"
    
    val Nφ6 = Nφ(6) ## "od ciężaru własnego na krawędzi górnej"
    val NΘ6 = NΘ(6) ## "od ciężaru własnego na krawędzi górnej"
    
    val Nφ7 = Nφ(7) ## "od oddziaływania wiatru na krawędzi górnej"
    val NΘ7 = NΘ(7) ## "od oddziaływania wiatru na krawędzi górnej"
    
    val Nφ8 = Nφ(8) ## "od obciążenia śniegiem na krawędzi górnej"
    val NΘ8 = NΘ(8) ## "od obciążenia śniegiem na krawędzi górnej"
    
    Nφ1 := (Nφg at (φ -> φ1))
    NΘ1 := (NΘg at (φ -> φ1))
    
    Nφ2 := (Nφw at (φ -> φ1, Θ -> (180 unit SI.deg)))
    NΘ2 := (NΘw at (φ -> φ1, Θ -> (180 unit SI.deg)))
    
    Nφ3 := (Nφg at (φ -> φ2))
    NΘ3 := (NΘg at (φ -> φ2))
    
    Nφ4 := (Nφw at (φ -> φ2, Θ -> (180 unit SI.deg)))
    NΘ4 := (NΘw at (φ -> φ2, Θ -> (180 unit SI.deg)))
    
    Nφ5 := (Nφs at (φ -> φ2))
    NΘ5 := (NΘs at (φ -> φ2))
    
    Nφ6 := (Nφg at (φ -> φ0))
    NΘ6 := (NΘg at (φ -> φ0))
    
    Nφ7 := (Nφw at (φ -> φ0, Θ -> (180 unit SI.deg)))
    NΘ7 := (NΘw at (φ -> φ0, Θ -> (180 unit SI.deg)))
    
    Nφ8 := (Nφs at (φ -> φ0))
    NΘ8 := (NΘs at (φ -> φ0))
    
    val Sg = BasicSymbols.S|"g" is "Siła ściskająca w pierścieniu górnym" := P*cot(φ0)*r0
    

    val Δrg = "Δr"|"g" args(φ) unit SI.mm is "Funkcja zmiany promienia powłoki od ciężaru własnego"
    val Δχg = "Δχ"|"g" args(φ) is "Funkcja kąta odkształcenia powłoki od ciężaru własnego"
    
    val Δrs = "Δr"|"s" args(φ) unit SI.mm is "Funkcja zmiany promienia powłoki od obciążenia śniegiem"
    val Δχs = "Δχ"|"s" args(φ) is "Funkcja kąta odkształcenia powłoki od obciążenia śniegiem"
    
    val ξ1 = "ξ"|1 is "Współczynnik sztywności utwierdzenia krawędzi dolnej" := 0.5
    
    val kappa = BasicSymbols.kappa is "Współczynnik funkcji odkształcenia"
    val Hg = BasicSymbols.H|"g" unit "kN/m" is "Siła reakcji poziomej od utwierdzenia krawędzi dolnej"
    val Mg = BasicSymbols.M|"g" unit "kNm/m" is "Moment utwierdzenia na krawędzi dolnej"
    
    Δrg := (g*(R^2)*((cos(φ0)-cos(φ))/sin(φ)*(1+nu)-sin(φ)*cos(φ))-P*R*(sin(φ0)/sin(φ))*(1+nu))/(Ecm*h)
    Δχg := (-g*R*(2+nu)*sin(φ))/(Ecm*h)
    
    Δrs := (s*((R^2)*sin(φ))/2*((1-(sin(φ0)^2)/(sin(φ)^2))*(1+nu)-2*(cos(φ)^2)))/(Ecm*h)
    Δχs := (-s*R*(3+nu)*sin(φ)*cos(φ))/(Ecm*h)
    
    val Δrg1 = Δrg(1) ## "od ciężaru własnego na krawędzi dolnej"
    val Δχg1 = Δχg(1) ## "od ciężaru własnego na krawędzi dolnej"
    
    nu := 0.2
    Δrg1 := (Δrg at (φ -> φ1))
    Δχg1 := (Δχg at (φ -> φ1))
    kappa := sqrt(R/h)*((3*(1-(nu^2)))^0.25)
    Hg := (Δrg1*Ecm*h)/(2*kappa*r*sin(φ1))
    Mg := (Δχg1*Ecm*h*R)/(4*(kappa^3))
    
    val NΘt1 = NΘ|"t,1" unit "kN/m" is "Maksymalna siła rozciągająca" := max(NΘ1+NΘ2,NΘ3+NΘ4+NΘ5,NΘ6+NΘ7+NΘ8)
    val As1 = A|"s,1" unit "mm2" is "Minimalne wymagane pole przekroju zbrojenia ze względu na rozciąganie stali" := (NΘt1*(1 unit SI.m))/fyd
    val as = a|"s" unit "mm2" is "Pole przekroju pręta" := PI*((phis/2)^2)
    val nt1 = n|"t,1" is "Minimalna liczba prętów rozciąganych obwodowych ze względu na rozciąganie stali" := ceil(As1/as)
    val dt1 = d|"t,1" unit SI.cm is "Maksymalny rozstaw prętów rozciąganych obwodowych ze względu na rozciąganie stali" := (1 unit SI.m)/nt1
    
    val Ac = A|"c" unit "m2" is "Pole przekroju betonowego" := (1 unit SI.m)*h
    val sigt1 = BasicSymbols.sigma|"t,1" unit SI.MPa is "Maksymalne naprężenia obwodowe rozciągające w betonie" := (NΘt1*(1 unit SI.m))/Ac
    
    val Nφc1 = Nφ|"c,1" unit "kN/m" is "Maksymalna siła południkowa ściskająca" := min(Nφ1+Nφ2,Nφ3-Nφ4+Nφ5,Nφ6-Nφ7+Nφ8)
    val sigc1 = BasicSymbols.sigma|"c,1" unit SI.MPa is "Maksymalne naprężenia południkowe ściskające w betonie" := (Nφc1*(1 unit SI.m))/Ac
    
    val sigcr = BasicSymbols.sigma|"cr" unit SI.MPa is "Krytyczne naprężenie ściskające" := -(Ecm*h)/(R*sqrt(3*(1-(nu^2))))

    override val document = Document("",
        PredefinedStyles.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z przedmiotu \"Konstrukcje Betonowe\". Semestr letni 2011/2012."),
        		Section("Autor: Artur Opala, 61315. Prowadzący: prof. Andrzej Ubysz, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            Section(
                Section(styleTitle,"Projekt wstępny konstrukcji kopuły żelbetowej nad kaplicą cmentarną."),
                TableOfContents("Spis treści"),
                //PageBreak,
                NumSection("Założenia techniczne",
                       NumSection("Przeznaczenie i forma obiektu",
                               Section("Przedmiotem projektu jest konstrukcja pożegnalnej kaplicy cmentarnej. Nawa centralna kaplicy w formie jednoprzestrzennego prostopadłościanu o wym. 20m x 20m x 6m nakrytego kopułą pełną na pendentywach, całość zwieńczona niewielką latarnią.")
                       ), 
                       NumSection("Lokalizacja obiektu",
                               Section("Obiekt zlokalizowany jest na nowym cmentarzu miejskim w Legnicy przy ul. Nowocmentarnej, w centralnym punkcie założenia, na łagodnym wzniesieniu. Średnia wysokość terenu cmentarza nad poziomem morza wynosi +140m n.p.m. W otoczeniu brak zabudowy i zieleni wysokiej. Strefa wiatrowa I, kategoria wiatrowa terenu II. Strefa obciążenia śniegiem I. ")
                       ), 
                       NumSection("Warunki wodno-gruntowe",
                            Section("W miejsu lokalizacji kaplicy zalegają grunty niejodnorodne, wielowarstwowe, głównie piasek i glina piaszczysta. Teren cmentarza odwodniony, suchy. Lustro wody gruntowej nie przekracza rzędnej +135m n.p.m.")
                       ), 
                       NumSection("Normy i literatura",
                            Section(styleComment1,"[1] Norma PN-B-03264 \"Konstrukcje betonowe, żelbetowe i sprężone. Obliczenia statyczne i projektowanie.\""),
                            Section(styleComment1,"[2] \"Poradnik inżyniera i technika budowlanego\" Dąbrowski, Kolendowicz wyd. Arkady - Warszawa, 1983 "),
                            Section(styleComment1,"[3] \"Design of Reinforced Concrete Shells and Folded Plates\" P.C Varghese wyd. PHI Learning Pvt. Ltd."),
                            Section(styleComment1,"[4] \"Shell structures in civil and mechanical engineering: theory and closed-form analytical solutions\" Alphose Zingoni wyd. Thomas Telford, 1997")
                       )
                ),
                NumSection("Projekt budowlany",
                    NumSection("Opis techniczny rozwiązania",
                           Section("Kopuła żelbetowa, cieńkościenna o stałej grubości, bez usztywnień, wsparta na pierścieniu podporowym i powłoce przejściowej (pendentywach), z otworem na latarnię usztywnionym pierścieniem górnym."),
                           Section("Klasa konstrukcji S4, klasa ekspozycji XC2. Przyjęto beton klasy C30/37."),
                           Section("Zbrojenie powłoki prętami B500SP #8mm w formie siatki złożonej z prętów obwodowych ciągłych (zewnętrzne) i prętów radialnych (wewnętrzne) zakotwionych w pierścieniu dolnym. 1/2 zbrojenia radialnego doprowadzona do połowy wysokości kopuły, 1/4 do wysości 3/4 kopuły, 1/4 zakotwiona w pierścieniu górnym.")
                    ),
                    NumSection("Parametry konstrukcji",Evaluate(r,h,ζ,φ1,R,H,r0,φ0,φ2)),
                    NumSection("Właściwości materiałów konstrukcyjnych",
                        concrete.info,
                        reinforcement.info
                    ),
                    NumSection("Rozmieszczenie cięgien i otulina",Evaluate(phis,cminb,cmindur,cmin,dcdev,cnom)),
                    NumSection("Oddziaływania",
                        NumSection("Ciężar własny powłoki",Evaluate(gammac,g)),
                        NumSection("Oddziaływania od obciążenia śniegiem",Evaluate(HM,sk,mi1,Ce,Ct,sr,s)),
                        NumSection("Obciążenie latarnią",Evaluate(P)),
                        NumSection("Oddziaływania od obciążenia wiatrem",Evaluate(vbo,qbo,coz,cez,qpz,ze,we,cpi,wi,w))
                    ),
                    NumSection("Siły wewnętrzne w powłoce w stanie błonowym",
                        NumSection("Funkcje sił wewnętrznych",Expand(Nφg,NΘg,Nφs,NΘs,Nφw,NφΘw,NΘw)),
                        NumSection("Obliczenie sił na krawędzi dolnej",Evaluate(Nφ1,NΘ1,Nφ2,NΘ2)),
                        NumSection("Obliczenie sił na poziomie linii przejściowej",Evaluate(Nφ3,NΘ3,Nφ4,NΘ4,Nφ5,NΘ5)),
                        NumSection("Obliczenie sił na krawędzi górnej",Evaluate(Nφ6,NΘ6,Nφ7,NΘ7,Nφ8,NΘ8))
                    ),
                    NumSection("Siły w pierścieniu górnym",Evaluate(Sg)),
                    NumSection("Odkształcenie powłoki i reakcje utwierdzenia",
                        NumSection("Funkcje odkształceń",Expand(Δrg,Δχg,Δrs,Δχs)),
                        NumSection("Obliczenie odkształceń na krawędzi dolnej",Evaluate(Δrg1,Δχg1)),
                        NumSection("Siły reakcji od utwierdzenia krawędzi dolnej",Evaluate(kappa,Hg,Mg))
                    ),
                    NumSection("Wymiarowanie zbrojenia powłoki",
                        NumSection("Zbrojenie obwodowe rozciągane",
                                Evaluate(NΘt1,Ac,sigt1),
                                AssertionLE("naprężeń rozciągających w betonie",sigt1,fctd),
                                Section(styleComment1,"Naprężenia rozciągające nie przekraczają wytrzymałości obliczeniowej betonu na rozciąganie. " +
                                		"Sprawdzamy zbrojenie obwodowe #8mm jak dla warunku wytrzymałości stali zbrojeniowej:"),
                                Evaluate(As1,as,nt1,dt1),
                                Section(styleComment1,"Przyjmujemy zbrojenie obwodowe konstrukcyjne #8mm co 20cm")
                         ),
                                
                        NumSection("Zbrojenie południkowe ściskane ",Evaluate(Nφc1,sigc1),
                            AssertionGE("naprężeń ściskających w betonie",sigc1,-fcd),
                            Section(styleComment1,"Naprężenia ściskające nie przekraczają wytrzymałości obliczeniowej betonu na ściskanie. " +
                            		"Przyjmujemy zbrojenie południkowe konstrukcyjne #8mm co 20cm")
                        )
                        
                    ),
                    NumSection("Stateczność powłoki",Evaluate(sigcr),
                           AssertionGE("stateczności",sigc1,sigcr/4)
                    )
                )
            )
        )
    )

}