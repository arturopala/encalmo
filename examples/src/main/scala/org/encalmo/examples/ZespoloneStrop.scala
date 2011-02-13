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
    
    val plytaObciazenia,plytaMontaz,plytaEksploatacja,plytaWymiarowanie:Seq[Expression] = Seq()
    val belkaMaterialy,belkaObciazenia,belkaMontaz,belkaEksploatacja,belkaWymiarowanie:Seq[Expression] = Seq()
    
    val zadanie = Calculation()
    
    val plyta = Calculation()
    val blacha = FLORSTROP.T59_Z_075
    plyta add blacha
    val beton = Concrete.C_50_60
    plyta add beton
    
    val belka = Calculation()
    val stal = Steel.S355
    belka add stal
    
    //dane wejsciowe zadania
    val L1 = L|1 is "Rozpiętośc belki" unit "m"
    zadanie(L1) = 14
    val L2 = L|2 is "Rozstaw belek" unit "m"
    zadanie(L2) = 2.1
    val pc = p!c is "Obciążenie charakterystyczne" unit "N/m2"
    zadanie(pc) = 3000
    val daneWejsciowe = Seq(L1,L2,pc)
    
    //wlasciwo materialowe plyty
    blacha.steel(Steel.gammaM0) = 1.0
    blacha.steel(Steel.gammaM1) = 1.0
    beton(Concrete.gammaC) = 1.5
    
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
                NumSection("Normy i literatura",
                	Section(styleComment," [1] Norma PN-EN 1994-1-1:2010 \"Eurokod 4. Projektowanie konstrukcji zespolonych stalowo-betonowych. Część 1-1: Postanowienia ogólne. Reguły ogólne i reguły dotyczące budynków\"")
                ),
                NumSection("Dane wejściowe",Evaluate(daneWejsciowe,zadanie))
           ),
           NumSection("Wymiarowanie płyty stropowej",
               NumSection("Materiały konstrukcyjne i ich parametry",
	               blacha.steel.info,blacha.info,beton.info
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie montażu",
	               Evaluate(plytaObciazenia,plyta)
	           ),
	           NumSection("Blacha trapezowa jako deskowanie",
	               Evaluate(plytaMontaz,plyta)
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie eksploatacji",
	               Evaluate(plytaEksploatacja,plyta)
	           ),
	           NumSection("Wymiarowanie płyty zespolonej w fazie eksploatacji",
	               Evaluate(plytaWymiarowanie,plyta)
	           )
           ),
           NumSection("Wymiarowanie belki stropowej",
	           NumSection("Materiały i przekrój poprzeczny",
	               Evaluate(belkaMaterialy,belka)
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie montażu",
	               Evaluate(belkaObciazenia,belka)
	           ),
	           NumSection("Wymiarowanie belki w fazie montażu",
	               Evaluate(belkaMontaz,belka)
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie eksploatacji",
	               Evaluate(belkaEksploatacja,belka)
	           ),
	           NumSection("Wymiarowanie belki w fazie eksploatacji",
	               Evaluate(belkaWymiarowanie,belka)
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