package org.encalmo.examples

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.examples.Predefined._

class BetonSprezonyDzwigarTypuI {
    
    import BasicSymbols._
    import Concrete._
    
    val daneMaterialowe,zestawienieObciazen,geometriaDzwigara,charakterystykiGeometryczne:Seq[Expression] = Seq()
    
    val calc = Calculation()
    val beton = Concrete("C50/60")
    calc add beton
    
    val daneWejsciowe:Seq[Expression] = Seq(C_class,fck,Ecm)
    
    val doc1 = Document(Predefined.style1,"",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z przedmiotu \"Betonowe Konstrukcje Sprężone\". Semestr zimowy 2010/2011."),
        		Section("Autor: Artur Opala, album nr 61315. Prowadzący: dr inż. Aleksy Łodo, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            NumSection("Zadanie",
                Section(styleComment,"Dźwigar dachowy z betonu sprężonego."),
                NumSection("Normy i literatura",
                	Section(styleComment," [1] Norma PN-EN 1995-1-1:2010 \"Eurokod 5. Projektowanie konstrukcji drewnianych. Część 1-1: Postanowienia ogólne. Reguły ogólne i reguły dotyczące budynków\""),
                	Section(styleComment," [2] Aprobata ITB AT-15-7343/2007 \"Wkręty samowiercące SPAX&reg; do konstrukcji drewnianych\""),
                	Section(styleComment," [3] Norma PN-EN 338:2009")
                ),
                NumSection("Dane wejściowe",Evaluate(daneWejsciowe,calc))
           ),
           /*NumSection("Dane do obliczeń",
                NumSection("Przyjęte wymiary przekroju słupa",Evaluate(calc,przyjetaGeometria:_*)),
                NumSection("Przyjęte łączniki",Evaluate(calc,przyjeteLaczniki:_*)),
                NumSection("Właściwości geometryczne przekroju",Evaluate(calc,wlasciwosciGeometryczne:_*)),
                NumSection("Współczynniki",Evaluate(calc,wspolczynnikiCzesciowe:_*)),
                NumSection("Właściwości mechaniczne charakterystyczne dla drewna litego klasy C27 wg [3]",Evaluate(calc,wlasciwosciMechaniczneCharakterystyczne:_*)),
                NumSection("Właściwości mechaniczne obliczeniowe dla 2 klasy użytkowania i obciążeń długotrwałych",Evaluate(calc,wlasciwosciMechaniczneObliczeniowe:_*))),
           NumSection("Sprawdzenie stanów granicznych nośności wg PN-EN 1995-1-1",
                NumSection("Obliczenie sztywności zastępczej",Evaluate(calc,sztywnoscZastepcza:_*)),
                NumSection("Sprawdzenie nośności na ściskanie",Evaluate(calc,nosnoscObliczeniowa:_*)),
                Section(styleWarunek,"Warunek C.1 [1] jest spełniony: ",
                                Symb(sigmac0d),LE,Symb(kc),Symb(fc0d),ARROW,Result(calc,sigmac0d),LE,Result(calc,kc*fc0d)),
				NumSection("Sprawdzenie nośności środnika na ścinanie",Evaluate(calc,nosnoscScinanie:_*)),
				Section(styleWarunek,"Warunek 6.13 [1] jest spełniony: ",
                                Symb(tau2max),LE,Symb(fvd),ARROW,Result(calc,tau2max),LE,Result(calc,fvd)),
				NumSection("Sprawdzenie nośności łączników (wkrętów)",Evaluate(calc,nosnoscLacznikow:_*)),
				Section(styleWarunek,"Warunek nośności wkręta jest spełniony: ",
                                Symb(Fi),LE,Symb(FvRd),ARROW,Result(calc,Fi),LE,Result(calc,FvRd))
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
			),*/
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
        output.saveToFile(new java.io.File("target/test-results/ks-dzwigar.fo"))
        FOPHelper.buildPDF(output.getResult, "target/test-results/ks-dzwigar.pdf")
    }
    
    @Test def printText:Unit = {
        val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
        PlainTextDocumentPrinter.print(doc1,o)
        o.printConsole
    }

}