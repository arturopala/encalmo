package org.encalmo.printer.document

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.fop.FOPHelper

class XslFoTextDocumentPrinterTest extends AssertionsForJUnit  {
	
	@Test def test1() {
		
		import BasicSymbols._
		
		val font1 = DefaultFontStyle.++.makeBold
		val font2 = DefaultFontStyle.makeItalic
		val font3 = DefaultFontStyle.withSize(9)
		val font4 = DefaultFontStyle.withSize(8).makeItalic
		
		val style1 = DefaultStyle.use(font1).useSpaceBefore(3)
		val style2 = DefaultStyle.use(font2).useColor(java.awt.Color.BLUE)
		val style3 = DefaultStyle.use(font3).marginLeft(10).useLetterSpacing("1pt")
		val style4 = DefaultStyle.use(font4).useColor(java.awt.Color.RED)
		
		val d1 = d|1
		val d2 = d|2
		val d3 = d|3
		
		val calc1 = Calculation("1")
		val expr1 = a*b/c
		calc1 put (d1 -> expr1)
		
		val a1 = Symbol1("Ar")
		val a2 = Symbol2("Ar",BasicSymbols.a)
		val a3 = Symbol3("Ar",BasicSymbols.a,BasicSymbols.beta)
		val a4 = Symbol4("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.gamma)
		val a5 = Symbol5("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.c,BasicSymbols.d)
		val dd = alpha!z
		val bb = a5|dd
		val expr2 = (a1+a2+a3+a4+a5)*(bb-1.3)+(sqrt(c^(2-r))/(sin(4.126)+phiv))+root(l*k,f-1)/cbrt(.123^f)+max(a1,a2,a3,a4)
		val expr3 = (a1+a2+a3+a4+a5)*(bb-1.3)
		
		calc1 put (d2 -> expr2)
		calc1 put (d3 -> expr3)
		
		calc1 put (a -> 5)
		calc1 put (b -> 3.21)
		calc1 put (c -> 0.57)
		
		val doc1 = Document(style1, "Test document",
			Enumerator(),
    		Chapter(style2,"Test chapter",
				Section("header"),
				Section(style2,"footer"),
				StyleList(style1,style2,style3,style4),
	    		Section(
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3")
	            ),
	    		Section(
		            Expr(calc1,d1)),
	    		Section(
		            Resolve(style1,style4,calc1,d1)),
	    		Section(
		            Evaluate(style1,style4,calc1,d1+sin(4.126))),
	    		Section(
		            Result(style1,calc1,d1)),
	            Section(style2,
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3"),
		            "Test expression output forms",
		            Expr(calc1,d2)),
	    		Section(
		            Resolve(calc1,d2)),
	    		Section(
		            Evaluate(calc1,d2)),
	    		Section(
		            Result(style1,calc1,d2)
	            ),
	            Section("Section test 1a"),
	            Section(style1,"Section test 2a"),
	            Section(style2,"Section test 3a"),
	            Section(
		            Section("Section test 1b"),
		            Section(style1,"Section test 2b"),
		            Section(style2,"Section test 3b")
	            ),
	            NumSection("NumSection test",
		            NumSection("NumSection test 1c"),
		            NumSection(style1,"NumSection test 2c with style1",style1.toString),
		            NumSection("NumSection test 3c",
	            		NumSection("NumSection test 1d"),
	            		NumSection(
            				"NumSection test 2d", 
            				Evaluate(calc1,d3)
        				),
	            		NumSection("NumSection test 3d"),
	            		"Test expression 1:",
	            		Resolve(calc1,d3),
	            		"Test expression 2:",
	            		Evaluate(calc1,expr1),
			            Section("Section test 1a"),
			            Section("Section test 2a"),
			            Section("Section test 3a"),
			            Section(
				            Section("Section test 1b"),
				            Section("Section test 2b"),
				            Section("Section test 3b")
			            )
	        		)
	            ),
	            EmptyDocumentComponent,
	            EmptyDocumentComponent,
	            Section(style2.hyphenateOn,
	            """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris id eros mi. Vestibulum id euismod purus. Quisque congue dignissim pretium. Ut tincidunt erat id justo tristique non scelerisque ante convallis. Nulla non arcu non nunc ultricies condimentum id et felis. Curabitur pellentesque faucibus erat, in porta erat consequat in. In sapien ipsum, venenatis eget semper id, eleifend in tortor. Aliquam pretium enim id neque mattis aliquam. Praesent bibendum venenatis venenatis. Cras imperdiet lacinia congue. Suspendisse tincidunt, est at viverra ultricies, ante lacus luctus justo, sit amet tristique erat purus quis lacus.
Aliquam erat volutpat. Nulla porta purus non tortor consectetur pharetra. Sed vel quam mi, sit amet tincidunt tellus. Quisque a varius elit. Aenean felis velit, consequat sed euismod ut, tempor eget ipsum. Nunc arcu leo, feugiat at congue ut, tristique ac nulla. Donec sed magna nisi, id ultrices mi. Cras ultrices, risus a mattis suscipit, purus mauris sollicitudin velit, id varius ipsum velit vitae tellus. Curabitur porta posuere sem. Curabitur mi urna, ultricies vel faucibus quis, dignissim ac dui. Aenean sed risus tellus, sed sagittis mi. Mauris pharetra dolor lobortis elit vestibulum sodales lacinia orci venenatis.
Curabitur sagittis volutpat sem, vitae vulputate elit rhoncus ut. Quisque sed elit quis lorem consectetur congue sed vehicula leo. Etiam scelerisque, urna sit amet rhoncus laoreet, neque risus pulvinar mi, non dapibus nibh nibh in ligula. Phasellus quis orci urna. Fusce placerat blandit nibh sit amet pellentesque. Nulla luctus, leo id scelerisque aliquam, est dolor vehicula elit, ac scelerisque sapien velit vitae tellus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin in metus in orci ornare scelerisque. Integer leo nisl, faucibus ut tristique eget, vestibulum et purus. Aliquam eget nunc lorem, quis scelerisque turpis.
Aenean risus felis, commodo et blandit vel, commodo at ipsum. Donec nisl nunc, facilisis ac aliquet quis, ultrices nec massa. Mauris ligula est, pulvinar eget pretium at, cursus quis mi. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Etiam pretium est quis elit aliquet sodales. Nullam et quam ac sapien blandit pulvinar. Quisque feugiat eleifend mauris, nec vestibulum eros posuere ut. Duis aliquet tristique ipsum et sagittis. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Aenean erat ante, posuere non sagittis eu, mattis sed dui. Vestibulum felis leo, volutpat ac pulvinar eu, facilisis ut risus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Ut aliquam dui vel sapien blandit fringilla id a sem. Nam iaculis, neque in laoreet congue, leo est dapibus massa, et fringilla odio dui id nibh. Praesent vel lectus ut ligula pellentesque pellentesque vitae sit amet odio. Curabitur in volutpat felis.
Mauris commodo consequat ligula mollis accumsan. Integer aliquet urna sed purus laoreet in congue ligula vehicula. Sed non erat sit amet lorem vehicula dapibus. Ut dapibus facilisis adipiscing. Nam vitae nisl vel diam laoreet rhoncus. Phasellus malesuada neque bibendum felis tincidunt venenatis. Phasellus ultricies aliquet turpis at tempor. Vivamus vel erat. """
            )),
            Chapter(style2,"Test chapter",
				Section("header"),
				Section("footer"),
            	Enumerator(),
	    		Section(
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3"),
		            Evaluate(calc1,d1)
	            ),
	            NumSection(
		            NumSection("Section test 1c"),
		            NumSection(style1,"Section test 2c"),
		            NumSection(style2,"Section test 3c",
	            		NumSection("Section test 1d"),
	            		NumSection(style1,"Section test 2d"),
	            		NumSection(style2,"Section test 3d"),
	            		"Test expression 1:",
	            		Evaluate(style2,calc1,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,calc1,expr1)
	        		)
	            ),
	            NumSection(
		            NumSection("Section test 1c"),
		            NumSection(style1,"Section test 2c"),
		            NumSection(style2,"Section test 3c",
	            		NumSection("Section test 1d"),
	            		NumSection(style1,"Section test 2d"),
	            		NumSection(style2,"Section test 3d"),
	            		"Test expression 1:",
	            		Evaluate(style2,calc1,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,calc1,expr1)
	        		)
	            )
            ),
            Chapter(style1,"Test chapter",
				Section("header"),
				Section("footer"),
	    		Section(
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3"),
		            Evaluate(calc1,d1)
	            ),
	            NumSection(
		            NumSection("Section test 1c"),
		            NumSection(style1,"Section test 2c"),
		            NumSection(style2,"Section test 3c",
	            		NumSection("Section test 1d"),
	            		NumSection(style1,"Section test 2d"),
	            		NumSection(style2,"Section test 3d"),
	            		"Test expression 1:",
	            		Evaluate(style2,calc1,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,calc1,expr1)
	        		)
	            ),
	            NumSection(style3,
		            NumSection("Lorem ipsum dolor sit amet"),
		            NumSection(style4,"Section test 2c"),
		            NumSection(style2,"Section test 3c",
	            		NumSection("Section test 1d"),
	            		NumSection(style1,"Section test 2d"),
	            		NumSection(style2,"Section test 3d"),
	            		"Test expression 1:",
	            		Evaluate(style2,calc1,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,calc1,expr1)
	        		)
	            )
            )
       )
		
		val output:XslFoOutput = new XslFoOutput(Layout(),new java.util.Locale("PL"))
		output.open
		XslFoTextDocumentPrinter.print(doc1,output)
		output.close
		output.printConsole
		output.saveToFile(new java.io.File("target/test-results/xslFoTextDocumentPrinterTest1.fo"))
		FOPHelper.buildPDF(output.getResult, "target/test-results/xslFoTextDocumentPrinterTest1.pdf")
	}
	
	def test2() {
		
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
		      	  Section("�wiczenie z przedmiotu 'Cie�ko�cienne konstrukcje metalowe'. S�up �ciskany osiowo - wyboczenie gi�tne i skr�tne. Autorzy: Irmina Grudzie�, Artur Opala."),
		      	  Section(""),
		          Section("W �wiczeniu przyj�to s�up krzy�owy o profilu +I400 (wg rysunku w za��czniku) ze stali St3SX zamocowany przegubowo na obu ko�cach."),
		          Section("Obliczenia i wymiarowanie przeprowadzono kolejno dla 6 r�nych d�ugo�ci s�upa, l = 3, 4, 5, 6, 7 i 8m."),
		          
		      NumSection(BOLD,"Parametry zadania",  
		          Section(ITALIC,"Wszystkie parametry i wyniki podano w odpowiednich jednostkach SI (metrach i Newtonach)"),
		          NumSection(BOLD,"Parametry geometryczne:",
		            Section(Evaluate(c1,h,bf,tf,tw,A))),
		          NumSection(BOLD,"Parametry wytrzyma�o�ciowe:",
		            Section(Evaluate(c1,Ix,Iy,Io,ix,io,Iomega,It))),
		          NumSection(BOLD,"Parametry materia�owe:",
		            Section(Evaluate(c1,E,G,fd))),
		          NumSection(BOLD,"Wsp�czynniki d�ugo�ci wyboczeniowej:",
		            Section(Evaluate(c1,mix,mif))),
		          NumSection(BOLD,"No�no�� obliczeniowa przekroju przy osiowym �ciskaniu:",
		            Section(Evaluate(c1,NRc)))
		          
		      ),
		      NumSection(BOLD,"Obliczenie si� krytycznych wyboczenia gi�tnego i skr�tnego dla zadanych przypadk�w.",
		          NumSection(BOLD,"Obliczenia dla l = 3m :",
		            NumSection("Smuk�o�� wyboczenia gi�tnego i skr�tnego:",
		              Section(Evaluate(c1,lambdax,lambdaf))),
		            NumSection("Si�a krytyczna wyboczenia gi�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c1,Nxcr))),
		            NumSection("Si�a krytyczna wyboczenia skr�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c1,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 4m :",
		            NumSection("Smuk�o�� wyboczenia gi�tnego i skr�tnego:",
		              Section(Evaluate(c2,lambdax,lambdaf))),
		            NumSection("Si�a krytyczna wyboczenia gi�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c2,Nxcr))),
		            NumSection("Si�a krytyczna wyboczenia skr�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c2,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 5m :",
		            NumSection("Smuk�o�� wyboczenia gi�tnego i skr�tnego:",
		              Section(Evaluate(c3,lambdax,lambdaf))),
		            NumSection("Si�a krytyczna wyboczenia gi�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c3,Nxcr))),
		            NumSection("Si�a krytyczna wyboczenia skr�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c3,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 6m :",
		            NumSection("Smuk�o�� wyboczenia gi�tnego i skr�tnego:",
		              Section(Evaluate(c4,lambdax,lambdaf))),
		            NumSection("Si�a krytyczna wyboczenia gi�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c4,Nxcr))),
		            NumSection("Si�a krytyczna wyboczenia skr�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c4,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Obliczenia dla l = 7m :",
		            NumSection("Smuk�o�� wyboczenia gi�tnego i skr�tnego:",
		              Section(Evaluate(c4,lambdax,lambdaf))),
		            NumSection("Si�a krytyczna wyboczenia gi�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c4,Nxcr))),
		            NumSection("Si�a krytyczna wyboczenia skr�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c4,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"K. Obliczenia dla l = 8m :",
		            NumSection("Smuk�o�� wyboczenia gi�tnego i skr�tnego:",
		              Section(Evaluate(c4,lambdax,lambdaf))),
		            NumSection("Si�a krytyczna wyboczenia gi�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c4,Nxcr))),
		            NumSection("Si�a krytyczna wyboczenia skr�tnego przy �ciskaniu osiowym:",
		              Section(Evaluate(c4,Nfcr)))
		          ),
		          
		          NumSection(BOLD,"Podsumowanie oblicze� :",
		            Section("dla l = 3m : ",Evaluate(c1,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 4m : ",Evaluate(c2,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 5m : ",Evaluate(c3,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 6m : ",Evaluate(c4,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 7m : ",Evaluate(c5,Nxcr,lambdax,Nfcr,lambdaf)),
		            Section("dla l = 8m : ",Evaluate(c6,Nxcr,lambdax,Nfcr,lambdaf))
		          ),
		          
		          NumSection(BOLD,"Wnioski z oblicze� :",
		             Section("Dla d�ugo�ci s�upa kr�tszych od 7m si�a krytyczna wyboczenia skr�tnego jest mniejsza od si�y krytycznej wyboczenia gi�tnego.")   
		          )
		      ),
		      
		      NumSection(BOLD,"Wymiarowanie s�upa wg PN-90 B-03200.",
		          Section("Obliczenie maksymalnej osiowej si�y �ciskaj�cej ",Evaluate(c1,Nmax)," dla ka�dej z rozpatrywanych d�ugo�ci s�upa."),
		          NumSection(BOLD,"Obliczenia dla l = 3m :",
		          NumSection("Smuk�o�� wzgl�dna pr�ta przy wyboczeniu:",
		            Section(Evaluate(c1,lambdad))),
		          NumSection("Wsp�czynnik wyboczeniowy wg krzywej c (",Evaluate(c1,n),"):",
		            Section(Evaluate(c1,fi))),
		          NumSection("Maksymalna osiowa si�a �ciskaj�ca:",
		            Section(Evaluate(c1,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 4m :",
		          NumSection("Smuk�o�� wzgl�dna pr�ta przy wyboczeniu:",
		            Section(Evaluate(c2,lambdad))),
		          NumSection("Wsp�czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(c2,fi))),
		          NumSection("Maksymalna osiowa si�a �ciskaj�ca:",
		            Section(Evaluate(c2,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 5m :",
		          NumSection("Smuk�o�� wzgl�dna pr�ta przy wyboczeniu:",
		            Section(Evaluate(c3,lambdad))),
		          NumSection("Wsp�czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(c3,fi))),
		          NumSection("Maksymalna osiowa si�a �ciskaj�ca:",
		            Section(Evaluate(c3,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 6m :",
		          NumSection("Smuk�o�� wzgl�dna pr�ta przy wyboczeniu:",
		            Section(Evaluate(c4,lambdad))),
		          NumSection("Wsp�czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(c4,fi))),
		          NumSection("Maksymalna osiowa si�a �ciskaj�ca:",
		            Section(Evaluate(c4,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 7m :",
		          NumSection("Smuk�o�� wzgl�dna pr�ta przy wyboczeniu:",
		            Section(Evaluate(c5,lambdad))),
		          NumSection("Wsp�czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(c5,fi))),
		          NumSection("Maksymalna osiowa si�a �ciskaj�ca:",
		            Section(Evaluate(c5,Nmax)))
		        ),
		        NumSection(BOLD,"Obliczenia dla l = 8m :",
		          NumSection("Smuk�o�� wzgl�dna pr�ta przy wyboczeniu:",
		            Section(Evaluate(c6,lambdad))),
		          NumSection("Wsp�czynnik wyboczeniowy wg krzywej c (n=1,2):",
		            Section(Evaluate(c6,fi))),
		          NumSection("Maksymalna osiowa si�a �ciskaj�ca:",
		            Section(Evaluate(c6,Nmax)))
		        ),
		        NumSection(BOLD,"Podsumowanie wymiarowania :",
		            Section("dla l = 3m : ",Evaluate(c1,fi,Nmax)),
		            Section("dla l = 4m : ",Evaluate(c2,fi,Nmax)),
		            Section("dla l = 5m : ",Evaluate(c3,fi,Nmax)),
		            Section("dla l = 6m : ",Evaluate(c4,fi,Nmax)),
		            Section("dla l = 7m : ",Evaluate(c5,fi,Nmax)),
		            Section("dla l = 8m : ",Evaluate(c6,fi,Nmax))
		          )
		      ),
		      EmptySection,
		      EmptySection,
		      Section(ITALIC,"Literatura :",
		         Section("1. Norma PN-90 B-03200 \"Konstrukcje stalowe. Obliczenia statyczne i projektowanie.\""),
		         Section("2. Bogucki W., �yburtowicz M. \"Tablice do projektowania konstrukcji metalowych\" Warszawa 1996"),
		         Section("3. Gosowski B. \"Skr�canie i zginanie otwartych, st�onych element�w konstrukcji metalowych\" Wroc�aw 2004"),
		         Section("4. Gosowski B., Kubica E. \"Badania laboratoryjne z konstrukcji metalowych\" Wroc�aw 2007")
		         
		      )
	      )
		)
		
		val output:XslFoOutput = new XslFoOutput(Layout(), new java.util.Locale("US"))
		output.open
		XslFoTextDocumentPrinter.print(doc1,output)
		output.close
		output.printConsole
		output.saveToFile(new java.io.File("target/test-results/xslFoTextDocumentPrinterTest2.fo"))
		FOPHelper.buildPDF(output.getResult, "target/test-results/xslFoTextDocumentPrinterTest2.pdf")
	}

}