package org.encalmo.printer.document

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.fop.FOPHelper
import org.encalmo.style.DefaultFontStyle
import org.encalmo.style.DefaultStyle
import org.encalmo.style.StylesConfig

class HtmlTextDocumentPrinterTest extends AssertionsForJUnit  {
	
	@Test def test1() {
		
		import BasicSymbols._

        val font1 = DefaultFontStyle.++.makeBold
		val font2 = DefaultFontStyle.makeItalic
		val font3 = DefaultFontStyle.fontSize(9)
		val font4 = DefaultFontStyle.fontSize(8).makeItalic
		
		val style1 = DefaultStyle.use(font1).useSpaceBefore(3)
		val style2 = DefaultStyle.use(font2).useColor(java.awt.Color.BLUE)
		val style3 = DefaultStyle.use(font3).marginLeft(10).useLetterSpacing("1pt")
		val style4 = DefaultStyle.use(font4).useColor(java.awt.Color.RED)
		
		val stylesConfig = StylesConfig()
		
		val d1 = d|1
		val d2 = d|2
		val d3 = d|3
		
		implicit val calc1 = Calculation("1")

		val expr1 = (a*b/c + c/b + (a^3))^2
		val expr4 = a*b/c + c/b + (a^3)
		
		calc1 put (d1 -> expr1)
		
		val a1 = Symbol("Ar")
		val a2 = Symbol("Ar",BasicSymbols.a)
		val a3 = Symbol("Ar",BasicSymbols.a,BasicSymbols.beta)
		val a4 = Symbol("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.gamma)
		val a5 = Symbol("Ar",BasicSymbols.a,BasicSymbols.b,BasicSymbols.c,BasicSymbols.d)
		val dd = alpha!z
		val bb = a5|dd
		val expr2 = (a1+a2+a3+a4+a5)*(bb-1.3)+(sqrt(c^(2-r))/(sin(4.126)+phiv))+root(l*k,f-1)/cbrt(.123^f)+max(a1,a2,a3,a4)
		val expr3 = (a1+a2+a3+a4+a5)*(bb-1.3)
		
		calc1 put (d2 -> expr2)
		calc1 put (d3 -> expr3)
		
		calc1 put (a -> 5)
		calc1 put (b -> 3.21)
		calc1 put (c -> 0.57)
		
		
		val doc1 = Document("Test document",StylesConfig(style2),
			Enumerator(),
    		Chapter(style2,"Test chapter",
				Section("header"),
				Section(style2,"footer"),
	    		Section(
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3")
	            ),
	    		Section(
		            Expand(d1)(calc1)),
	    		Section(
		            Evaluate(d1+sin(4.126)),
		            Evaluate(style1,d1+sin(4.126)),
		            Evaluate(style2,d1+sin(4.126)),
		            Evaluate(style2.fontBold,style2,d1+sin(4.126))),
		        Section(
		            Evaluate(expr4)(calc1)),
	    		Section(
		            Result(d1)),
	            Section(style2,
		            Text("test1"),
		            Text(style1,"test2"),
		            Text(style2,"test3"),
		            "Test expression output forms",
		            Expand(d2)),
	    		Section(
		            Expand(d2)),
	    		Section(
		            Evaluate(d2)),
	    		Section(
		            Result(style1,d2)
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
            				Evaluate(d3)
        				),
	            		NumSection("NumSection test 3d"),
	            		"Test expression 1:",
	            		Expand(d3),
	            		"Test expression 2:",
	            		Evaluate(expr1),
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
		            Evaluate(d1)
	            ),
	            NumSection(
		            NumSection("Section test 1c"),
		            NumSection(style1,"Section test 2c"),
		            NumSection(style2,"Section test 3c",
	            		NumSection("Section test 1d"),
	            		NumSection(style1,"Section test 2d"),
	            		NumSection(style2,"Section test 3d"),
	            		"Test expression 1:",
	            		Evaluate(style2,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,expr1)
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
	            		Evaluate(style2,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,expr1)
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
		            Evaluate(d1)
	            ),
	            NumSection(
		            NumSection("Section test 1c"),
		            NumSection(style1,"Section test 2c"),
		            NumSection(style2,"Section test 3c",
	            		NumSection("Section test 1d"),
	            		NumSection(style1,"Section test 2d"),
	            		NumSection(style2,"Section test 3d"),
	            		"Test expression 1:",
	            		Evaluate(style2,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,expr1)
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
	            		Evaluate(style2,d3),
	            		"Test expression 2:",
	            		Evaluate(style2,expr1)
	        		)
	            )
            )
       )

        val results = Reckoner.reckon
		val output:HtmlOutput = new HtmlOutput(Layout(),new java.util.Locale("PL"))
		output.open()
		HtmlTextDocumentPrinter.print(doc1)(output)(results)
		output.close()
		output.saveToFile(new java.io.File("target/test-results/htmlTextDocumentPrinterTest1.html"))
	}
	
	@Test def test2() {
		
		import BasicSymbols._
		import org.encalmo.style.StylesConfigSymbols._
		
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
		val eNxcr = (PI^2)*E*Ix/((mix*l)^2) 
	    c1 put (Nxcr -> eNxcr)
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
		
		val style1 = DefaultStyle.fontSize(11).useSpaceBefore(2).useSpaceAfter(2)
		val BOLD = style1.fontBold
		val ITALIC = style1.fontItalic
		
		val stylesConfig = StylesConfig(style1)
		stylesConfig(EXPRESSION) = style1.marginLeft(15)
		stylesConfig(EXPR_SYMBOL) = style1.paddingRight(2)
		stylesConfig(EXPR_UNRESOLVED) = style1.fontSmaller
		stylesConfig(EXPR_SUBSTITUTED) = style1.fontSmaller
		stylesConfig(EXPR_PARTIALLY_EVALUATED) = style1.fontSmaller
		stylesConfig(EXPR_NUMBERS) = style1.useColor(java.awt.Color.BLUE)
		stylesConfig(EXPR_EVALUATED) = style1.fontBold
		stylesConfig(NUMSECT_LEVEL0) = style1.fontBold.useSpaceBefore(9)
		stylesConfig(NUMSECT_LEVEL1) = style1.useSpaceBefore(7)
		stylesConfig(NUMSECT_LEVEL2) = style1.fontSmaller
		stylesConfig(NUMSECT_LEVEL3) = style1.fontSmaller.fontItalic
		stylesConfig(NUMSECT_LEVEL4) = style1.fontSmaller.fontSmaller.fontItalic
		
		val doc1 = Document("",
		     stylesConfig,
			 Chapter("",
		      	  Section("Ćwiczenie z przedmiotu 'Cieńkościenne konstrukcje metalowe'. Słup ściskany osiowo - wyboczenie giętne i skrętne. Autorzy: Irmina Grudzień, Artur Opala."),
		      	  Section(""),
		          Section("W ćwiczeniu przyjęto słup krzyżowy o profilu +I400 (wg rysunku w załączniku) ze stali St3SX zamocowany przegubowo na obu końcach."),
		          Section("Obliczenia i wymiarowanie przeprowadzono kolejno dla 6 różnych długości słupa, l = 3, 4, 5, 6, 7 i 8m."),
		          
		      NumSection("Parametry zadania",  
		          Section(ITALIC,"Wszystkie parametry i wyniki podano w odpowiednich jednostkach SI (metrach i Newtonach)"),
		          NumSection("Parametry geometryczne:",
		            Section(Evaluate(h,bf,tf,tw,A))),
		          NumSection("Parametry wytrzymałościowe:",
		            Section(Evaluate(Ix,Iy,Io,ix,io,Iomega,It))),
		          NumSection("Parametry materiałowe:",
		            Section(Evaluate(E,G,fd))),
		          NumSection("Współczynniki długości wyboczeniowej:",
		            Section(Evaluate(mix,mif))),
		          NumSection("Nośność obliczeniowa przekroju przy osiowym ściskaniu:",
		            Section(Evaluate(NRc)))
		          
		      ),
		      NumSection("Obliczenie sił krytycznych wyboczenia giętnego i skrętnego dla zadanych przypadków.",
		          NumSection("Obliczenia dla l = 3m :",
		            NumSection("Smukłość wyboczenia giętnego i skrętnego:",
		              Section(Evaluate(lambdax,lambdaf))),
		            NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nxcr))),
		            NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
		              Section(Evaluate(Nfcr)))
		          ),
		          NumSection("Podsumowanie obliczeń :",
		            Section("dla l = 3m : ",Evaluate(Nxcr,lambdax,Nfcr,lambdaf))
		          ),
		          
		          NumSection("Wnioski z obliczeń :",
		             Section("Dla długości słupa krótszych od 7m siła krytyczna wyboczenia skrętnego jest mniejsza od siły krytycznej wyboczenia giętnego.")   
		          )
		      ),
		      
		      NumSection("Wymiarowanie słupa wg PN-90 B-03200.",
		          Section("Obliczenie maksymalnej osiowej siły ściskającej ",Evaluate(Nmax)," dla każdej z rozpatrywanych długości słupa."),
		          NumSection("Obliczenia dla l = 3m :",
		          NumSection("Smukłość względna pręta przy wyboczeniu:",
		            Section(Evaluate(lambdad))),
		          NumSection("Współczynnik wyboczeniowy wg krzywej c (",Evaluate(n),"):",
		            Section(Evaluate(fi))),
		          NumSection("Maksymalna osiowa siła ściskająca:",
		            Section(Evaluate(Nmax)))
		        ),
		        NumSection("Podsumowanie wymiarowania :",
		            Section("dla l = 3m : ",Evaluate(fi,Nmax))
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
        val output:HtmlOutput = new HtmlOutput(Layout(),new java.util.Locale("PL"))
        output.open
        HtmlTextDocumentPrinter.print(doc1)(output)(results)
        output.close
        output.saveToFile(new java.io.File("target/test-results/htmlTextDocumentPrinterTest2.html"))
	}

}