package org.encalmo.structures.eurocode.concrete

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.structures.Predefined
import org.encalmo.structures.Predefined._
import org.encalmo.structures.eurocode.actions.ActionsSymbols
import org.encalmo.structures.CalculationDocument

class DomeCalculationDocument extends CalculationDocument {
    
    override val name = "dome"
        
    import BasicSymbols._
    import ConcreteSymbols._
    import SI.{mm,mm2,cm,cm2,kN,MPa}
    import ReinforcingSteelSymbols.{fyd}
    
    val concrete = Concrete.C_40_50
    val reinforcement = ReinforcingSteel.B500SP
    calc add concrete
    calc add reinforcement
    
    val r = BasicSymbols.r unit SI.m is "Promień rzutu kopuły"
	val R = BasicSymbols.R unit SI.m is "Promień kopuły"
	val h = BasicSymbols.h unit SI.cm is "Grubość powłoki"
	val φ = BasicSymbols.phiv unit SI.deg is "Kąt pionowy"
	val Θ = BasicSymbols.theta unit SI.deg is "Kąt poziomy"
	
	val r0 = BasicSymbols.r|0 unit SI.m // promien otwarcia kopuly
	val φ0 = BasicSymbols.phiv|0 unit SI.deg // kat otwarcia kopuly
	val φ1 = BasicSymbols.phiv|1 unit SI.deg is "Kąt rozwarcia kopuły"
	
	val g = BasicSymbols.g unit "kN/m2" is "Obliczeniowy ciężar własny kopuły"
	val s = BasicSymbols.s unit "kN/m2" is "Obliczeniowe obciążenie śniegiem"
	val w = BasicSymbols.w unit SI.kPa is "Obliczeniowe parcie wiatru"
	
	val Hg = BasicSymbols.H|g
	val Hs = BasicSymbols.H|s
	val Hw = BasicSymbols.H|w
	
	val Sg = BasicSymbols.S|g
	val Ss = BasicSymbols.S|s
	val Sw = BasicSymbols.S|w
	
	calc(r) = 14 unit SI.m
	calc(φ1) = 90 unit SI.deg
	calc(h) = 10 unit SI.cm
	
	calc(R) = r/sin(φ1)
	calc(φ0) = arcsin(r0/R)
	calc(g) = gammac*h*1.35
    
    val sk = symbol(BasicSymbols.s|"k") unit "kN/m2" acc 0.1 is "Wartość charakterystyczna obciążenia gruntu śniegiem"
    val HM = symbol(BasicSymbols.H|"M") is "Wysokość nad poziomem morza dla lokalizacji obiektu"
    val mi1 = symbol(BasicSymbols.mu|"1") is "Współczynnik kształtu dachu"
    val Ce = symbol(BasicSymbols.C|"e") is "Współczynnik ekspozycji"
    val Ct = symbol(BasicSymbols.C|"t") is "Współczynnik termiczny"
    val sr = symbol(BasicSymbols.s|"r") unit "kN/m2" acc 0.1 is "Charakterystyczne obciążenie śniegiem powłoki"
    
    //obciazenie sniegiem
    calc(HM) = 140 unit SI.m
    calc(sk) = max(0.7,0.007*HM-1.4)
    calc(mi1) = 0.8
    calc(Ce) = 0.8
    calc(Ct) = 1.0
    calc(sr) = mi1*Ce*Ct*sk
    calc(s) = sr*1.5
    
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
    calc(vbo) = 22
    calc(qbo) = 0.30
    calc(coz) = 1.0
    calc(cez) = 2.3*((z/(10 unit SI.m))^0.24)
    calc(qpz) = cez*qbo
    calc(cpi) = -0.6
    calc(ze) = 20 unit SI.m
    calc(we) = 1.0*EvalAt(qpz,z -> ze)
    calc(wi) = cpi*EvalAt(qpz,z -> ze)
    calc(w) = we*1.5
    
    val Nφ0 = N|"φ0" unit SI.kN // sila brzegowa styczna w otwarciu
    
    val Nφ = N|"φ" unit SI.kN is "Siła południkowa"
    val NΘ = N|"Θ" unit SI.kN is "Siła równoleżnikowa"
    
    val Nφg = N|"φ,g" args(φ) unit SI.kN // sila poludnikowa od ciezaru wlasnego
    val NΘg = N|"Θ,g" args(Θ) unit SI.kN // sila rownoleznikowa od ciezaru wlasnego
    
    val Nφs = N|"φ,s" args(φ) unit SI.kN // sila poludnikowa od obciazenia sniegiem
    val NΘs = N|"Θ,s" args(Θ) unit SI.kN // sila rownoleznikowa od obciazenia sniegiem
    
    val Nφw = N|"φ,w" args(φ,Θ) unit SI.kN // sila poludnikowa od obciazenia wiatrem
    val NφΘw = N|"φΘ,w" args(φ,Θ) unit SI.kN // sila scinajaca od obciazenia wiatrem
    val NΘw = N|"Θ,w" args(φ,Θ) unit SI.kN // sila rownoleznikowa od obciazenia sniegiem
    
    calc(Nφg) = -g*R/(1+cos(φ)) // sila poludnikowa od ciezaru wlasnego
    calc(NΘg) = -g*R*(cos(φ) - (1/(1+cos(φ)))) // sila rownoleznikowa od ciezaru wlasnego
    
    calc(Nφs) = (-s*R)/2 // sila poludnikowa od obciazenia sniegiem
    calc(NΘs) = (-s*R)/2 * cos(2*φ) // sila rownoleznikowa od obciazenia sniegiem
    
    calc(Nφw) = w*R*(-frac(2,3)+cos(φ)-frac(1,3)*(cos(φ)^3))*(cos(φ)/(sin(φ)^3))*cos(Θ) // sila poludnikowa od obciazenia wiatrem
    calc(NφΘw) = w*R*(-frac(2,3)+cos(φ)-frac(1,3)*(cos(φ)^3))*(1/(sin(φ)^3))*sin(Θ) // sila scinajaca od obciazenia wiatrem
    calc(NΘw) = w*R*(frac(2,3)*cos(φ)-(sin(φ)^2)-frac(2,3)*(cos(φ)^4))*(1/(sin(φ)^3))*cos(Θ) // sila rownoleznikowa od obciazenia sniegiem
    
    val Nφ1 = Nφ(1) & " przy pierścieniu podporowym"
    val NΘ1 = NΘ(1) & " przy pierścieniu podporowym"
    
    calc(Nφ1) = (Nφg at (φ -> φ1))
    calc(NΘ1) = (NΘg at (φ -> φ1))
    
    calc(Nφ(2)) = (Nφw at (φ -> φ1, Θ -> (180 unit SI.deg)))
    calc(NΘ(2)) = (NΘw at (φ -> φ1, Θ -> (180 unit SI.deg)))
    
    
    val doc = Document("",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z przedmiotu \"Konstrukcje Betonowe\". Semestr letni 2011/2012."),
        		Section("Autor: Artur Opala, 61315. Prowadzący: prof. Andrzej Ubysz, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            Section(
                Section(styleTitle,"Projekt wstępny konstrukcji kopuły żelbetowej nad kaplicą cmentarną."),
                TableOfContents("Spis treści"),
                Section("Normy i literatura",
                	Section(styleComment1,"[1] Norma PN-B-03264 \"Konstrukcje betonowe, żelbetowe i sprężone. Obliczenia statyczne i projektowanie.\"")
                ),
                PageBreak,
                NumSection("Parametry zadania",Evaluate(calc,r,h,φ1,R)),
                NumSection("Dane materiałów konstrukcyjnych",
                        concrete.info,
                        reinforcement.info
                ),
                NumSection("Oddziaływania",
                        NumSection("Ciężar własny",Evaluate(calc,gammac,g)),
                        NumSection("Oddziaływania od obciążenia śniegiem",Evaluate(calc,HM,sk,mi1,Ce,Ct,sr,s)),
                        NumSection("Oddziaływania od obciążenia wiatrem",Evaluate(calc,vbo,qbo,coz,cez,qpz,ze,we,cpi,wi,w))
                ),
                NumSection("Siły wewnętrzne w powłoce",Resolve(calc,Nφg,NΘg,Nφs,NΘs,Nφw,NφΘw,NΘw),
                        NumSection("Kombinacja 1: Ciężar stały + wiatr",Evaluate(calc,Nφ1,NΘ1,Nφ(2),NΘ(2)))
                )
            )
        )
    )
       
    @Test
    override def printHtml:Unit = super.printHtml

}