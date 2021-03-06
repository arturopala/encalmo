package org.encalmo.printer.document

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.style.DefaultFontStyle
import org.encalmo.style.DefaultStyle
import org.encalmo.style.StylesConfig

class PlainTextDocumentPrinterTest extends AssertionsForJUnit  {
	
	@Test def test1() {
		
		import BasicSymbols._
		
		val font1 = DefaultFontStyle.++.makeBold
		val font2 = DefaultFontStyle.fontSize(8).makeItalic
		
		val style1 = DefaultStyle.use(font1)
		val style2 = DefaultStyle.use(font2)
		
		implicit val calc1 = Calculation("1")
		val expr1 = a*b/c
		calc1 put (d -> expr1)
		
		val doc1 = Document("Test document",StylesConfig(style1),
    		Chapter(style1,"Test chapter",
				Section("header"),
				Section("footer"),
	    		Section(
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3"),
		            Evaluate(d),
		            "Test expression",
		            Evaluate(d)
	            ),
	            Section(style2,
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3")
	            ),
	            Section("Section test 1a"),
	            Section(style1,"Section test 2a"),
	            Section(style2,"Section test 3a"),
	            Section(
		            Section("Section test 1b"),
		            Section(style1,"Section test 2b"),
		            Section(style2,"Section test 3b")
	            ),
	            NumSection(
		            NumSection("Section test 1c"),
		            NumSection(style1,"Section test 2c"),
		            NumSection(style2,"Section test 3c",
	            		NumSection("Section test 1d"),
	            		NumSection(style1,"Section test 2d"),
	            		NumSection(style2,"Section test 3d")
	        		)
	            ),
	            EmptyDocumentComponent,
	            EmptyDocumentComponent
            )
       )

        val results = Reckoner.reckon
		val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
		PlainTextDocumentPrinter.print(doc1)(o)(results)
	}
	
	@Test def test2() {
		
		import BasicSymbols._
		
		implicit val c1 = Calculation("1")
		
	    val fi = phiv
	    val bf = b|f
	    val tf = t|f
	    val tw = t|w
	    val Nfcr = N|(fi+"cr") // withUnit SI.Newton
	    val Nxcr = N|"xcr" //withUnit SI.Newton
	    val NRc = N|"Rc" //withUnit SI.Newton
	    val Nmax = N|"max" //withUnit SI.Newton
	    val fd = f|d
	    val Ix = I|x
	    val Iy = I|y
	    val Io = I|o
	    val mix = mi|x
	    val io = i|o
	    val ix = i|x
	    val iy = i|y
	    val Iomega = I|omega
	    val Iomega2 = I|(omega+"2")
	    val mif = mi|fi
	    val It = I|t
	    val lambdax = lambda|x
	    val lambdaf = lambda|fi
	    val lambdad = lambda over ("_")
		
		c1 put (h -> 0.4)
	    c1 put (bf -> 0.155)
	    c1 put (tf -> 0.00144)
	    c1 put (tw -> 0.00216)
	    c1 put (A -> 233.9E-4)
	    c1 put (Ix -> 30370E-8)
	    c1 put (Iy -> Ix)
	    c1 put (ix -> ( sqrt(Ix/A) ))
	    c1 put (mix -> 1)
	    c1 put (mif -> 1)
	    c1 put (l -> 3)
	    c1 put (lambdax -> ( mix*l/ix ))
	    c1 put (lambdaf -> ( sqrt((Ix+Iy)/((Iomega/((mif*l)^2))+((G*It)/((PI^2)*E)))) ))
	    c1 put (E -> 205E9)
	    c1 put (G -> 80E9)
	    c1 put (fd -> 215E6)
	    c1 put (NRc -> ( A*fd ))
	    c1 put (Nxcr -> ( (pi^2)*E*Ix/((mix*l)^2) ))
	    c1 put (Io -> ( Ix+Iy ))
	    c1 put (io -> ( hypot(ix,ix) ))
	    c1 put (Iomega2 -> ( 2*(Ix*(h^2)/4) ))
	    c1 put (Iomega -> ( 2*415000E-12 ))
	    c1 put (It -> ( 2*183E-8 ))
	    c1 put (Nfcr -> ( 1/(io^2)*(((PI^2)*E*Iomega)/((mif*l)^2)+G*It) ))
	    c1 put (lambdad -> ( 1.15*sqrt(NRc/min(Nxcr,Nfcr)) ))
	    c1 put (n -> 1.2)
	    c1 put (fi -> ( (1+(lambdad^(2*n)))^(-(1/n)) ))
	    c1 put (Nmax -> ( fi*NRc ))
		
		val BOLD = DefaultStyle.fontBold
		val ITALIC = DefaultStyle.fontItalic
		
		val c2 = c1
		val c3 = c1
		val c4 = c1
		val c5 = c1
		val c6 = c1
		
		val doc1 = Document("",
		      Chapter("",
		      	  Section("Ćwiczenie z przedmiotu 'Cieńkościenne konstrukcje metalowe'. Słup ściskany osiowo - wyboczenie giętne i skrętne. Autorzy: Irmina Grudzień, Artur Opala."),
		      	  Section(""),
		          Section("W ćwiczeniu przyjęto słup krzyżowy o profilu +I400 (wg rysunku w załączniku) ze stali St3SX zamocowany przegubowo na obu końcach."),
		          Section("Obliczenia i wymiarowanie przeprowadzono kolejno dla 6 różnych długości słupa, l = 3, 4, 5, 6, 7 i 8m."),
		          
		      NumSection(BOLD,"Parametry zadania",  
		          Section(ITALIC,"Wszystkie parametry i wyniki podano w odpowiednich jednostkach SI (metrach i Newtonach)"),
		          NumSection(BOLD,"Parametry geometryczne:",
		            Section(Evaluate(h,bf,tf,tw,A)(c1))),
		          NumSection(BOLD,"Parametry wytrzymałościowe:",
		            Section(Evaluate(Ix,Iy,Io,ix,io,Iomega,It)(c1))),
		          NumSection(BOLD,"Parametry materiałowe:",
		            Section(Evaluate(E,G,fd)(c1))),
		          NumSection(BOLD,"Współczynniki długości wyboczeniowej:",
		            Section(Evaluate(mix,mif)(c1))),
		          NumSection(BOLD,"Nośność obliczeniowa przekroju przy osiowym ściskaniu:",
		            Section(Evaluate(NRc)(c1)))
		          
		      ),
		      NumSection(BOLD,"Obliczenie sił krytycznych wyboczenia giętnego i skrętnego dla zadanych przypadków.",
		          NumSection(BOLD,"Obliczenia dla l = 3m :",
		            NumSection("Smukłość wyboczenia giętnego i skrętnego:",
		              Section(Evaluate(lambdax,lambdaf)(c1))),
		            NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nxcr)(c1))),
		            NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nfcr)(c1)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 4m :",
		            NumSection("Smukłość wyboczenia giętnego i skrętnego:",
		              Section(Evaluate(lambdax,lambdaf)(c2))),
		            NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nxcr)(c2))),
		            NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nfcr)(c2)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 5m :",
		            NumSection("Smukłość wyboczenia giętnego i skrętnego:",
		              Section(Evaluate(lambdax,lambdaf)(c3))),
		            NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nxcr)(c3))),
		            NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nfcr)(c3)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 6m :",
		            NumSection("Smukłość wyboczenia giętnego i skrętnego:",
		              Section(Evaluate(lambdax,lambdaf)(c4))),
		            NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nxcr)(c4))),
		            NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nfcr)(c4)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 7m :",
		            NumSection("Smukłość wyboczenia giętnego i skrętnego:",
		              Section(Evaluate(lambdax,lambdaf)(c4))),
		            NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nxcr)(c4))),
		            NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nfcr)(c4)))
		          ),
		          
		          NumSection(BOLD,"K. Obliczenia dla l = 8m :",
		            NumSection("Smukłość wyboczenia giętnego i skrętnego:",
		              Section(Evaluate(lambdax,lambdaf)(c4))),
		            NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nxcr)(c4))),
		            NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nfcr)(c4)))
		          ),
		          
		          NumSection(BOLD,"Podsumowanie obliczeń :",
		            Section("dla l = 3m : ",Evaluate(Nxcr,lambdax,Nfcr,lambdaf)(c1)),
		            Section("dla l = 4m : ",Evaluate(Nxcr,lambdax,Nfcr,lambdaf)(c2)),
		            Section("dla l = 5m : ",Evaluate(Nxcr,lambdax,Nfcr,lambdaf)(c3)),
		            Section("dla l = 6m : ",Evaluate(Nxcr,lambdax,Nfcr,lambdaf)(c4)),
		            Section("dla l = 7m : ",Evaluate(Nxcr,lambdax,Nfcr,lambdaf)(c5)),
		            Section("dla l = 8m : ",Evaluate(Nxcr,lambdax,Nfcr,lambdaf)(c6))
		          ),
		          
		          NumSection(BOLD,"Wnioski z obliczeń :",
		             Section("Dla długości słupa krótszych od 7m siła krytyczna wyboczenia skrętnego jest mniejsza od siły krytycznej wyboczenia giętnego.")   
		          )
		      ),
		      
		      NumSection(BOLD,"Wymiarowanie słupa wg PN-90 B-03200.",
		          Section("Obliczenie maksymalnej osiowej siły ściskającej ",Evaluate(Nmax)(c1)," dla każdej z rozpatrywanych długości słupa."),
		          NumSection(BOLD,"Obliczenia dla l = 3m :",
		          NumSection("Smukłość względna pręta przy wyboczeniu:",
		            Section(Evaluate(lambdad)(c1))),
		          NumSection("Współczynnik wyboczeniowy wg krzywej c (",Evaluate(n)(c1),"):",
		            Section(Evaluate(fi)(c1))),
		          NumSection("Maksymalna osiowa siła ściskająca:",
		            Section(Evaluate(Nmax)(c1)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 4m :",
		          NumSection("Smukłość względna pręta przy wyboczeniu:",
		            Section(Evaluate(lambdad)(c2))),
		          NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(fi)(c2))),
		          NumSection("Maksymalna osiowa siła ściskająca:",
		            Section(Evaluate(Nmax)(c2)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 5m :",
		          NumSection("Smukłość względna pręta przy wyboczeniu:",
		            Section(Evaluate(lambdad)(c3))),
		          NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(fi)(c3))),
		          NumSection("Maksymalna osiowa siła ściskająca:",
		            Section(Evaluate(Nmax)(c3)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 6m :",
		          NumSection("Smukłość względna pręta przy wyboczeniu:",
		            Section(Evaluate(lambdad)(c4))),
		          NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(fi)(c4))),
		          NumSection("Maksymalna osiowa siła ściskająca:",
		            Section(Evaluate(Nmax)(c4)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 7m :",
		          NumSection("Smukłość względna pręta przy wyboczeniu:",
		            Section(Evaluate(lambdad)(c5))),
		          NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(fi)(c5))),
		          NumSection("Maksymalna osiowa siła ściskająca:",
		            Section(Evaluate(Nmax)(c5)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 8m :",
		          NumSection("Smukłość względna pręta przy wyboczeniu:",
		            Section(Evaluate(lambdad)(c6))),
		          NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(fi)(c6))),
		          NumSection("Maksymalna osiowa siła ściskająca:",
		            Section(Evaluate(Nmax)(c6)))
		        ),
		        NumSection(BOLD,"Podsumowanie wymiarowania :",
		            Section("dla l = 3m : ",Evaluate(fi,Nmax)(c1)),
		            Section("dla l = 4m : ",Evaluate(fi,Nmax)(c2)),
		            Section("dla l = 5m : ",Evaluate(fi,Nmax)(c3)),
		            Section("dla l = 6m : ",Evaluate(fi,Nmax)(c4)),
		            Section("dla l = 7m : ",Evaluate(fi,Nmax)(c5)),
		            Section("dla l = 8m : ",Evaluate(fi,Nmax)(c6))
		          )
		      ),
		      EmptySection,
		      EmptySection,
		      Section(ITALIC,"Literatura :",
		         Section("1. Norma PN-90 B-03200 \"Konstrukcje stalowe. Obliczenia statyczne i projektowanie.\""),
		         Section("2. Bogucki W., Żyburtowicz M. \"Tablice do projektowania konstrukcji metalowych\" Warszawa 1996"),
		         Section("3. Gosowski B. \"Skręcanie i zginanie otwartych, stężonych elementów konstrukcji metalowych\" Wrocław 2004"),
		         Section("4. Gosowski B., Kubica E. \"Badania laboratoryjne z konstrukcji metalowych\" Wrocław 2007")
		         
		      )
	      )
		)
        val results = Reckoner.reckon
		val output:TextOutput = new TextOutput(new java.util.Locale("US"))
		PlainTextDocumentPrinter.print(doc1)(output)(results)
	}

}