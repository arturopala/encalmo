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

class ZbiornikZelbetowyProstopadloscienny {
    
    import BasicSymbols._
    
    val calc = Calculation()
    
    val b1 = BasicSymbols.b|1 is "Długość prostokątnej podstawy silosu" unit "m"
    calc(b1) = 5.7
    val b2 = BasicSymbols.b|2 is "Szerokość prostokątnej podstawy silosu" unit "m"
    calc(b2) = 6.0
    val h1 = BasicSymbols.h|1 is "Wysokość ścian silosu" unit "m"
    calc(h1) = 12.0
    val h2 = BasicSymbols.h|2 is "Wysokość słupów wsporczych silosu" unit "m"
    calc(h2) = 4.2
    val daneGeometryczne = Seq(b1,b2,h1,h2)
    
    val gammal = BasicSymbols.gamma|"l" is "Ciężar jednostkowy (dolny)" unit "kN/m3"
    calc(gammal) = 8.0
    val gammau = BasicSymbols.gamma|"u" is "Ciężar jednostkowy (górny)" unit "kN/m3"
    calc(gammau) = 15.0
    val fic = BasicSymbols.phi|"c" is "Kąt stoku naturalnego" unit "°"
    calc(fic) = 41.0
    val fiim = BasicSymbols.phi|"im" is "Kąt tarcia wewnętrznego (średni)" unit "°"
    calc(fiim) = 35.0
    val afi = BasicSymbols.a|BasicSymbols.phi is "Parametr kąta tarcia wewnętrznego"
    calc(afi) = 1.16
    val Km = BasicSymbols.K|"m" is "Iloraz parcia bocznego (średni)"
    calc(Km) = 0.46
    val aK = BasicSymbols.a|BasicSymbols.K is "Parametr ilorazu parcia bocznego"
    calc(aK) = 1.20
    val mum = BasicSymbols.mu|"m" is "Współczynnik tarcia o ścianę typu D3"
    calc(mum) = 0.72
    val amu = BasicSymbols.a|BasicSymbols.mu is "Parametr współczynnika tarcia o ścianę"
    calc(amu) = 1.07
    val Cop = BasicSymbols.C|"op" is "Bazowy współczynnik parcia lokalnego"
    calc(Cop) = 0.5
    
    val K_u = BasicSymbols.K!"u" is "Iloraz parcia bocznego (górna wartość charakterystyczna)"
    calc(K_u) = Km*aK
    val K_l = BasicSymbols.K!"l" is "Iloraz parcia bocznego (dolna wartość charakterystyczna)"
    calc(K_l) = Km/aK
    
    val mu_u = BasicSymbols.mu!"u" is "Współczynnik tarcia o ścianę (górna wartość charakterystyczna)"
    calc(mu_u) = mum*amu
    val mu_l = BasicSymbols.mu!"l" is "Współczynnik tarcia o ścianę (dolna wartość charakterystyczna)"
    calc(mu_l) = mum/amu
    
    val fi_u = BasicSymbols.phi|("i","u") is "Kąt tarcia wewnętrznego (górna wartość charakterystyczna)"
    calc(fi_u) = fiim*afi
    val fi_l = BasicSymbols.phi|("i","l") is "Kąt tarcia wewnętrznego (dolna wartość charakterystyczna)"
    calc(fi_l) = fiim/afi
    
    
    
    
    val daneOsrodka = Seq(gammal,gammau,fic,fiim,afi,Km,aK,mum,amu,Cop)
    
    val charakterystyczneDaneOsrodka = Seq(K_u,K_l,mu_u,mu_l,fi_u,fi_l)
    
    
    val doc1 = Document(Predefined.style1,"",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z \"Konstrukcji Betonowych\". Semestr letni 2010/2011."),
        		Section("Autor: Artur Opala, 61315. Prowadzący: dr inż. Włodzimierz Wydra, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            NumSection("Zadanie",
                Section(styleComment,"Silos żelbetowy jednokomorowy na popiół lotny usytuowany wewnątrz hali przemysłowej.")
            ),
            NumSection("Bibliografia",
            	NumSection(styleComment," Norma PN-EN 1991-4 \"Eurokod 1. Oddziaływania na konstrukcje. Część 4: Silosy i zbiorniki.\""),
            	NumSection(styleComment," Norma PN-EN 1992-1-1 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-1: Reguły ogólne i reguły dla budynków.\""),
            	NumSection(styleComment," Norma PN-EN 1992-1-2 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-2: Reguły ogólne. Projektowanie z uwagi na warunki pożarowe.\""),
            	NumSection(styleComment," Norma PN-EN 1992-3 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 3: Silosy i zbiorniki na ciecze.\"")
            ),
            NumSection("Zadana geometria silosu",Evaluate(daneGeometryczne,calc)),
            NumSection("Właściwości składowanego materiału (popiołu lotnego)",Evaluate(daneOsrodka,calc)),
            NumSection("Wartości charakterystyczne dla składowanego materiału",Evaluate(charakterystyczneDaneOsrodka,calc))
           /*,
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")*/
        )
    )
    
    @Test def printPdf:Unit = {
        val layout = Predefined.layout
        val output:XslFoOutput = new XslFoOutput(layout, new java.util.Locale("PL"))
        output.open
        XslFoTextDocumentPrinter.print(doc1,output)
        output.close
        output.printConsole
        output.saveToFile(new java.io.File("target/test-results/kb-silos.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/kb-silos.pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc1,o)
        o.printConsole
    }

}