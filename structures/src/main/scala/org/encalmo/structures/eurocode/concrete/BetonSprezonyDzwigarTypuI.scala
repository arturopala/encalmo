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

class BetonSprezonyDzwigarTypuI extends CalculationDocument {
    
    override val name = "bks"
        
    import BasicSymbols._
    import ConcreteSymbols._
    
    val beton = Concrete.C_40_50
    calc add beton
    
    val gpk = g|"p,k" is "Charakterystyczny ciężar pokrycia dachu (płyt kanałowych i warstw izolacyjnych)" unit SI.kN/SI.m2; calc(gpk) = 4.0
    val qsk = q|"s,k" is "Charakterystyczne obciążenie dachu śniegiem" unit SI.kN/SI.m2; calc(qsk) = 0.72
    val qtk = q|"t,k" is "Charakterytyczne obciążenie technologiczne dźwigara" unit SI.kN/SI.m; calc(qtk) = 1.0
    
    val lr = l|r is "Rozstaw osi dźwigarów" unit SI.m; calc(lr) = 5.5
    val ld = l|d is "Długość dźwigara w osi podpór" unit SI.m; calc(ld) = 14
    
    val hd = h|d is "Wysokość przekroju dźwigara" unit SI.cm acc 5; calc(hd) = ld/20
	val bd = b|d is "Szerokość przekroju dźwigara" unit SI.cm acc 1; calc(bd) = hd/8
	val b1 = b|1 is "Szerokość środnika dźwigara" unit SI.cm acc 1; calc(b1) = bd/3
	val b2 = b|2 is "Szerokość półki dźwigara" unit SI.cm; calc(b2) = (bd-b1)/2
    val h1 = h|1 is "Wysokość półki górnej przekroju" unit SI.cm; calc(h1) = 10
    val h2 = h|2 is "Wysokość skosu półki górnej przekroju" unit SI.cm acc 1; calc(h2) = b2/2
	val h4 = h|4 is "Wysokość skosu półki dolnej przekroju" unit SI.cm acc 1; calc(h4) = b2/1.5
	val h5 = h|5 is "Wysokość półki dolnej przekroju" unit SI.cm; calc(h5) = 20
	val h3 = h|3 is "Wysokość środnika przekroju" unit SI.cm; calc(h3) = hd-h1-h2-h4-h5

    val Ac = A|c is "Pole przekroju betonowego" unit SI.cm2
	calc(Ac) = h1*bd+h5*bd+(h2+h3+h4)*b1+h2*b2+h4*b2
	val gwk = g|"c,k" is "Charakterystyczny ciężar własny dźwigara" unit SI.kN/SI.m
	calc(gwk) = Ac*gammac
	
    val qdmax = q|"d,max" is "Sumaryczne obliczeniowe obciążenie dźwigara (maksymalne)" unit SI.kN/SI.m acc 0.1; calc(qdmax) = gpk*lr*1.35 + qsk*lr*1.5 + qtk*1.5
    
    
    val doc = Document("",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z przedmiotu \"Betonowe Konstrukcje Sprężone\". Semestr zimowy 2011/2012."),
        		Section("Autor: XXX. Prowadzący: dr inż. Aleksy Łodo, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            NumSection("Zadanie",
                Section(styleComment,"Dźwigar dachowy z betonu sprężonego."),
                NumSection("Normy i literatura",
                	Section(styleComment," [1] Norma PN-EN 1992-1-1 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-1: Reguły ogólne i reguły dla budynków.\"")
                	
                ),
                NumSection("Parametry zadania",Evaluate(calc,ld,lr,beton(CLASS),gpk,qsk,qtk)),
                beton.info,
                NumSection("Geometria przekroju dźwigara",Evaluate(calc,hd,bd,b1,b2,h1,h2,h4,h5,h3,Ac,gwk)),
                NumSection("Wyznaczenie liczby cięgien sprężających",Evaluate(calc,qdmax)),
                NumSection("Wyznaczenie charakterystyk przekroju dźwigara",Evaluate(calc))
           ),
           
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: XXX")
        )
    )

}
