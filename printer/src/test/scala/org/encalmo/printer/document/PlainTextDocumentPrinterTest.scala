package org.encalmo.printer.document

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._

class PlainTextDocumentPrinterTest extends AssertionsForJUnit  {
	
	@Test def test1() {
		
		import BasicSymbols._
		
		val font1 = DefaultFontStyle.++.makeBold
		val font2 = DefaultFontStyle.withSize(8).makeItalic
		
		val style1 = DefaultStyle.use(font1)
		val style2 = DefaultStyle.use(font2)
		
		val calc1 = Calculation("1")
		val expr1 = a*b/c
		calc1 put (d -> expr1)
		
		val doc1 = Document(style1, "Test document",
    		Chapter(style1,"Test chapter",
				Section("header"),
				Section("footer"),
	    		Section(
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3"),
		            Expr(calc1,d),
		            "Test expression",
		            Expr(calc1,d)
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
		
		val o:TextOutput = new TextOutput(new java.util.Locale("PL"))
		PlainTextDocumentPrinter.print(doc1,o)
		o.printConsole
	}
	
	@Test def test2() {
		
		import BasicSymbols._
		
		val c1 = Calculation("1") 
		
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
		      	  Section("Æwiczenie z przedmiotu 'Cieñkoœcienne konstrukcje metalowe'. S³up œciskany osiowo - wyboczenie giêtne i skrêtne. Autorzy: Irmina Grudzieñ, Artur Opala."),
		      	  Section(""),
		          Section("W æwiczeniu przyjêto s³up krzy¿owy o profilu +I400 (wg rysunku w za³¹czniku) ze stali St3SX zamocowany przegubowo na obu koñcach."),
		          Section("Obliczenia i wymiarowanie przeprowadzono kolejno dla 6 ró¿nych d³ugoœci s³upa, l = 3, 4, 5, 6, 7 i 8m."),
		          
		      NumSection(BOLD,"Parametry zadania",  
		          Section(ITALIC,"Wszystkie parametry i wyniki podano w odpowiednich jednostkach SI (metrach i Newtonach)"),
		          NumSection(BOLD,"Parametry geometryczne:",
		            Section(Expr(c1,h,bf,tf,tw,A))),
		          NumSection(BOLD,"Parametry wytrzyma³oœciowe:",
		            Section(Expr(c1,Ix,Iy,Io,ix,io,Iomega,It))),
		          NumSection(BOLD,"Parametry materia³owe:",
		            Section(Expr(c1,E,G,fd))),
		          NumSection(BOLD,"Wspó³czynniki d³ugoœci wyboczeniowej:",
		            Section(Expr(c1,mix,mif))),
		          NumSection(BOLD,"Noœnoœæ obliczeniowa przekroju przy osiowym œciskaniu:",
		            Section(Expr(c1,NRc)))
		          
		      ),
		      NumSection(BOLD,"Obliczenie si³ krytycznych wyboczenia giêtnego i skrêtnego dla zadanych przypadków.",
		          NumSection(BOLD,"Obliczenia dla l = 3m :",
		            NumSection("Smuk³oœæ wyboczenia giêtnego i skrêtnego:",
		              Section(Expr(c1,lambdax,lambdaf))),
		            NumSection("Si³a krytyczna wyboczenia giêtnego przy œciskaniu osiowym:",
		              Section(Expr(c1,Nxcr))),
		            NumSection("Si³a krytyczna wyboczenia skrêtnego przy œciskaniu osiowym:",
		              Section(Expr(c1,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 4m :",
		            NumSection("Smuk³oœæ wyboczenia giêtnego i skrêtnego:",
		              Section(Expr(c2,lambdax,lambdaf))),
		            NumSection("Si³a krytyczna wyboczenia giêtnego przy œciskaniu osiowym:",
		              Section(Expr(c2,Nxcr))),
		            NumSection("Si³a krytyczna wyboczenia skrêtnego przy œciskaniu osiowym:",
		              Section(Expr(c2,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 5m :",
		            NumSection("Smuk³oœæ wyboczenia giêtnego i skrêtnego:",
		              Section(Expr(c3,lambdax,lambdaf))),
		            NumSection("Si³a krytyczna wyboczenia giêtnego przy œciskaniu osiowym:",
		              Section(Expr(c3,Nxcr))),
		            NumSection("Si³a krytyczna wyboczenia skrêtnego przy œciskaniu osiowym:",
		              Section(Expr(c3,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 6m :",
		            NumSection("Smuk³oœæ wyboczenia giêtnego i skrêtnego:",
		              Section(Expr(c4,lambdax,lambdaf))),
		            NumSection("Si³a krytyczna wyboczenia giêtnego przy œciskaniu osiowym:",
		              Section(Expr(c4,Nxcr))),
		            NumSection("Si³a krytyczna wyboczenia skrêtnego przy œciskaniu osiowym:",
		              Section(Expr(c4,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 7m :",
		            NumSection("Smuk³oœæ wyboczenia giêtnego i skrêtnego:",
		              Section(Expr(c4,lambdax,lambdaf))),
		            NumSection("Si³a krytyczna wyboczenia giêtnego przy œciskaniu osiowym:",
		              Section(Expr(c4,Nxcr))),
		            NumSection("Si³a krytyczna wyboczenia skrêtnego przy œciskaniu osiowym:",
		              Section(Expr(c4,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"K. Obliczenia dla l = 8m :",
		            NumSection("Smuk³oœæ wyboczenia giêtnego i skrêtnego:",
		              Section(Expr(c4,lambdax,lambdaf))),
		            NumSection("Si³a krytyczna wyboczenia giêtnego przy œciskaniu osiowym:",
		              Section(Expr(c4,Nxcr))),
		            NumSection("Si³a krytyczna wyboczenia skrêtnego przy œciskaniu osiowym:",
		              Section(Expr(c4,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Podsumowanie obliczeñ :",
		            Section("dla l = 3m : ",Expr(c1,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 4m : ",Expr(c2,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 5m : ",Expr(c3,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 6m : ",Expr(c4,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 7m : ",Expr(c5,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 8m : ",Expr(c6,Nxcr,lambdax,Nfcr,lambdaf))
		          ),
		          
		          NumSection(BOLD,"Wnioski z obliczeñ :",
		             Section("Dla d³ugoœci s³upa krótszych od 7m si³a krytyczna wyboczenia skrêtnego jest mniejsza od si³y krytycznej wyboczenia giêtnego.")   
		          )
		      ),
		      
		      NumSection(BOLD,"Wymiarowanie s³upa wg PN-90 B-03200.",
		          Section("Obliczenie maksymalnej osiowej si³y œciskaj¹cej ",Expr(c1,Nmax)," dla ka¿dej z rozpatrywanych d³ugoœci s³upa."),
		          NumSection(BOLD,"Obliczenia dla l = 3m :",
		          NumSection("Smuk³oœæ wzglêdna prêta przy wyboczeniu:",
		            Section(Expr(c1,lambdad))),
		          NumSection("Wspó³czynnik wyboczeniowy wg krzywej c (",Expr(c1,n),"):",
		            Section(Expr(c1,fi))),
		          NumSection("Maksymalna osiowa si³a œciskaj¹ca:",
		            Section(Expr(c1,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 4m :",
		          NumSection("Smuk³oœæ wzglêdna prêta przy wyboczeniu:",
		            Section(Expr(c2,lambdad))),
		          NumSection("Wspó³czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Expr(c2,fi))),
		          NumSection("Maksymalna osiowa si³a œciskaj¹ca:",
		            Section(Expr(c2,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 5m :",
		          NumSection("Smuk³oœæ wzglêdna prêta przy wyboczeniu:",
		            Section(Expr(c3,lambdad))),
		          NumSection("Wspó³czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Expr(c3,fi))),
		          NumSection("Maksymalna osiowa si³a œciskaj¹ca:",
		            Section(Expr(c3,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 6m :",
		          NumSection("Smuk³oœæ wzglêdna prêta przy wyboczeniu:",
		            Section(Expr(c4,lambdad))),
		          NumSection("Wspó³czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Expr(c4,fi))),
		          NumSection("Maksymalna osiowa si³a œciskaj¹ca:",
		            Section(Expr(c4,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 7m :",
		          NumSection("Smuk³oœæ wzglêdna prêta przy wyboczeniu:",
		            Section(Expr(c5,lambdad))),
		          NumSection("Wspó³czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Expr(c5,fi))),
		          NumSection("Maksymalna osiowa si³a œciskaj¹ca:",
		            Section(Expr(c5,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 8m :",
		          NumSection("Smuk³oœæ wzglêdna prêta przy wyboczeniu:",
		            Section(Expr(c6,lambdad))),
		          NumSection("Wspó³czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Expr(c6,fi))),
		          NumSection("Maksymalna osiowa si³a œciskaj¹ca:",
		            Section(Expr(c6,Nmax)))
		        ),
		        NumSection(BOLD,"Podsumowanie wymiarowania :",
		            Section("dla l = 3m : ",Expr(c1,fi,Nmax)),
		            Section("dla l = 4m : ",Expr(c2,fi,Nmax)),
		            Section("dla l = 5m : ",Expr(c3,fi,Nmax)),
		            Section("dla l = 6m : ",Expr(c4,fi,Nmax)),
		            Section("dla l = 7m : ",Expr(c5,fi,Nmax)),
		            Section("dla l = 8m : ",Expr(c6,fi,Nmax))
		          )
		      ),
		      EmptySection,
		      EmptySection,
		      Section(ITALIC,"Literatura :",
		         Section("1. Norma PN-90 B-03200 \"Konstrukcje stalowe. Obliczenia statyczne i projektowanie.\""),
		         Section("2. Bogucki W., ¯yburtowicz M. \"Tablice do projektowania konstrukcji metalowych\" Warszawa 1996"),
		         Section("3. Gosowski B. \"Skrêcanie i zginanie otwartych, stê¿onych elementów konstrukcji metalowych\" Wroc³aw 2004"),
		         Section("4. Gosowski B., Kubica E. \"Badania laboratoryjne z konstrukcji metalowych\" Wroc³aw 2007")
		         
		      )
	      )
		)
		
		val output:TextOutput = new TextOutput(new java.util.Locale("US"))
		PlainTextDocumentPrinter.print(doc1,output)
		output.printConsole
	}

}