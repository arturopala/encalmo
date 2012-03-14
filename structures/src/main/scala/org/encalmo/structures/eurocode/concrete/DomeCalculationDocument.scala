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
    
    val r = BasicSymbols.r unit SI.m // promien rzutu kopuly
	val R = BasicSymbols.R unit SI.m // promien sfery kopuly
	val h = BasicSymbols.h unit SI.cm // wysokosc przekroju
	val φ = BasicSymbols.phiv unit SI.deg // kat pionowy
	val Θ = BasicSymbols.theta unit SI.deg // kat poziomy
	
	val r0 = BasicSymbols.r|0 unit SI.m // promien otwarcia kopuly
	val φ0 = BasicSymbols.phiv|0 unit SI.deg // kat otwarcia kopuly
	val φ1 = BasicSymbols.phiv|1 unit SI.deg // kat podparcia kopuly
	
	val g = BasicSymbols.g unit "kN/m2" // ciezar wlasny kopuly
	val s = BasicSymbols.s unit "kN/m2" // obciazenie sniegiem
	val w = BasicSymbols.w unit SI.kPa // obciazenie wiatrem
	
	val Nφ0 = N|"φ0" unit SI.kN // sila brzegowa styczna w otwarciu
	
	val Nφg = N|"φ,g" args(φ) unit SI.kN // sila poludnikowa od ciezaru wlasnego
	val NΘg = N|"Θ,g" args(Θ) unit SI.kN // sila rownoleznikowa od ciezaru wlasnego
	
	val Nφs = N|"φ,s" args(φ) unit SI.kN // sila poludnikowa od obciazenia sniegiem
	val NΘs = N|"Θ,s" args(Θ) unit SI.kN // sila rownoleznikowa od obciazenia sniegiem
	
	val Nφw = N|"φ,w" args(φ,Θ) unit SI.kN // sila poludnikowa od obciazenia wiatrem
	val NφΘw = N|"φΘ,w" args(φ,Θ) unit SI.kN // sila scinajaca od obciazenia wiatrem
	val NΘw = N|"Θ,w" args(φ,Θ) unit SI.kN // sila rownoleznikowa od obciazenia sniegiem
	
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
	calc(g) = gammac*h
	
	calc(Nφg) = -g*R/(1+cos(φ)) // sila poludnikowa od ciezaru wlasnego
	calc(NΘg) = -g*R*(cos(φ) - (1/(1+cos(φ)))) // sila rownoleznikowa od ciezaru wlasnego
	
	calc(Nφs) = -(s*R)/2 // sila poludnikowa od obciazenia sniegiem
	calc(NΘs) = -(s*R)/2 * cos(2*φ) // sila rownoleznikowa od obciazenia sniegiem
	
	calc(Nφw) = w*R*(-2d/3+cos(φ)-1d/3*(cos(φ)^3))*(cos(φ)/(sin(φ)^3))*cos(Θ) // sila poludnikowa od obciazenia wiatrem
	calc(NφΘw) = w*R*(-2d/3+cos(φ)-1d/3*(cos(φ)^3))*(1/(sin(φ)^3))*sin(Θ) // sila scinajaca od obciazenia wiatrem
	calc(NΘw) = w*R*(2d/3+(sin(φ)^2)-2d/3*(cos(φ)^4))*(1/(sin(φ)^3))*cos(Θ) // sila rownoleznikowa od obciazenia sniegiem
	
	calc(Nφg(1)) = Nφg at (φ -> φ1)
    
    
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
                NumSection("Parametry zadania",Evaluate(calc,r,h,φ1,concrete(CLASS))),
                NumSection("Dane materiałów konstrukcyjnych",
                        concrete.info,
                        reinforcement.info
                ),
                NumSection("Siły wewnętrzne w kopule",Evaluate(calc,R,g,Nφg,NΘg,Nφs,NΘs,Nφw,NφΘw,NΘw))
            )
        )
    )
                

}