package org.encalmo.examples

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.examples.Predefined._

class ZespoloneStrop {
    
    import BasicSymbols._
    import SteelSymbols._
    import ConcreteSymbols._
    import ActionsSymbols._
    
    import CompositeSlabWithProfiledSheetingSymbols._
    import ProfiledSteelSheetSymbols._
    
    val zadanie1Statyka,zadanie1Eksploatacja,zadanie1Wymiarowanie:Seq[Expression] = Seq()
    val zadanie2Materialy,zadanie2Obciazenia,zadanie2Montaz,zadanie2Eksploatacja,zadanie2Wymiarowanie:Seq[Expression] = Seq()
    
    val zadanie = Calculation()
    val zadanie1 = Calculation()
    val zadanie2 = Calculation()
    
    //dane wejsciowe zadania
    val L1 = L|1 is "Rozpiętośc belki" unit "m"
    zadanie(L1) = 14
    val L2 = L|2 is "Rozstaw belek" unit "m"
    zadanie(L2) = 2.5
    val pc = p!c is "Obciążenie charakterystyczne" unit "N/m2"
    zadanie(pc) = 2500
    val daneWejsciowe = Seq(L1,L2,pc)
    
    val height:Expression = 0.12
    
    val blacha = FLORSTROP.T59_Z_100
    val beton = Concrete.C_50_60
    val plyta = new CompositeSlabWithProfiledSheeting(height,zadanie(L2),5,blacha,beton)
    val stal = Steel.S355
    
    zadanie1 add plyta
    
    plyta(Gsk) = 1E3
    plyta(qk) = zadanie(pc)
    plyta(Fk) = 10E3
    
    //wlasciwosci materialowe plyty
    plyta(gammaG) = 1.35
    plyta(gammaQ) = 1.5
    blacha.steel(gammaM0) = 1.0
    blacha.steel(gammaM1) = 1.0
    beton(gammaC) = 1.5
    
    plyta(dmesh) = 8
    plyta(sd) = 0.2
    
    
    val doc1 = Document(Predefined.style1,"",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z przedmiotu \"Kostrukcje Zespolone\". Autor: Artur Opala, album 61315."),
        		Section("Prowadzący: dr inż. Wojciech Lorenc, Instytut Budownictwa Politechniki Wrocławskiej. Semestr zimowy 2010/2011.")
        	),
            Section(""),
            NumSection("Zadanie",
                Section(styleComment,"Strop zespolony w parkingu wielopoziomowym."),
                Section(styleDescription),
                Section(styleDescription,"Normy i literatura przedmiotowa",
                	Section(styleDescription," [1] Norma PN-EN 1994-1-1:2010 \"Eurokod 4. Projektowanie konstrukcji zespolonych stalowo-betonowych. Część 1-1: Postanowienia ogólne. Reguły ogólne i reguły dotyczące budynków\""),
                	Section(styleDescription," [2] Norma PN-EN 1994-1-2:2010 \"Eurokod 4. Projektowanie zespolonych konstrukcji stalowo-betonowych. Część 1-2: Projektowanie z uwagi na warunki pożarowe.\""),
                	Section(styleDescription," [3] Norma PN-EN 1993-1-1:2006 \"Eurokod 3. Projektowanie konstrukcji stalowych. Część 1-1: Reguły ogólne i reguły dla budynków\""),
                	Section(styleDescription," [4] Norma PN-EN 1993-1-3:2006 \"Eurokod 3. Projektowanie konstrukcji stalowych. Część 1-3: Reguły ogólne. Reguły uzupełniające dla konstrukcji z kształtowników i blach profilowanych na zimno.\""),
                	Section(styleDescription," [5] Norma PN-EN 1992-1-1:2008 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-1: Reguły ogólne i reguły dla budynków\"")
                ),
                NumSection("Dane wejściowe",Evaluate(daneWejsciowe,zadanie))
           ),
           NumSection("Wymiarowanie płyty stropowej",
               NumSection("Przyjęte wymiary i właściwości materiałowe",
	               blacha.steel.info,blacha.info,beton.info,plyta.info
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie montażu",
	               plyta.LOAD1
	           ),
	           NumSection("Blacha trapezowa jako deskowanie",
	           	   Section(styleDescription,"""Schemat statyczny blachy trapezowej pełniącej funkcje deskowania przyjęto jako belkę pięcioprzęsłową. Siły wewnętrzne i reakcje obliczono korzystając z tablic w [6]."""),
	              plyta.ULS1,plyta.SLS1
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie eksploatacji",
	               plyta.LOAD2
	           ),
	           NumSection("Wymiarowanie płyty zespolonej w fazie eksploatacji",
	               plyta.ULS2,plyta.SLS2
	           )
           ),
           NumSection("Wymiarowanie belki stropowej",
	           NumSection("Materiały i przekrój poprzeczny",
	               Evaluate(zadanie2Materialy,zadanie2)
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie montażu",
	               Evaluate(zadanie2Obciazenia,zadanie2)
	           ),
	           NumSection("Wymiarowanie belki w fazie montażu",
	               Evaluate(zadanie2Montaz,zadanie2)
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie eksploatacji",
	               Evaluate(zadanie2Eksploatacja,zadanie2)
	           ),
	           NumSection("Wymiarowanie belki w fazie eksploatacji",
	               Evaluate(zadanie2Wymiarowanie,zadanie2)
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
        output.saveToFile(new java.io.File("target/test-results/kz-strop.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/kz-strop.pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc1,o)
        o.printConsole
    }

}