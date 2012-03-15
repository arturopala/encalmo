package org.encalmo.document

import org.encalmo.common._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.style.DefaultFontStyle
import org.encalmo.style.DefaultStyle
import org.encalmo.style.StylesConfig

/**
 * Document test
 * @author artur.opala
 */
class DocumentTest extends AssertionsForJUnit {
    
    @Test def testDocument1() {
        
        import BasicSymbols._
        
        val font1 = DefaultFontStyle.++.makeBold
        val font2 = DefaultFontStyle.fontSize(8).makeItalic
        
        val style1 = DefaultStyle.use(font1)
        val style2 = DefaultStyle.use(font2)
        
        val calc1 = Calculation("1")
        val Evaluate1 = a*b/c
        calc1 put (d -> Evaluate1)
        
        val sc:StylesConfig = StylesConfig(style1)
        
        val doc1 = Document("Test document",sc,
            Chapter(style1,"Test chapter",
                Section("header"),
                Section("footer"),
                Section(
                    Text("test1"),
                    Text(style1,"test2"),
                    Text(style2,"test3"),
                    Evaluate(calc1,d),
                    "Text Evaluateession",
                    Evaluate(calc1,d)
                ),
                Section(style2,
                    Text("test1"),
                    Text(style1,"test2"),
                    Text(style2,"test3")
                ),
                Section("Section test 1"),
                Section(style1,"Section test 2"),
                Section(style2,"Section test 3"),
                Section(
                    Section("Section test 1"),
                    Section(style1,"Section test 2"),
                    Section(style2,"Section test 3")
                ),
                NumSection(
                    NumSection("Section test 1"),
                    NumSection(style1,"Section test 2"),
                    NumSection(style2,"Section test 3",
                        NumSection("Section test 1"),
                        NumSection(style1,"Section test 2"),
                        NumSection(style2,"Section test 3")
                    )
                ),
                EmptyDocumentComponent,
                EmptyDocumentComponent
            )
       )
        
       doc1.dumpTreeToConsole
       
       val sr1 = doc1.select(0).getOrElse(fail()).asInstanceOf[Document]
       assertSame(sr1, doc1);
       val sr2 = doc1.select(0,0,0,2).getOrElse(fail()).asInstanceOf[Text]
       assertEquals(sr2.text, "test3");
       assertFalse(sr2.text=="test2")
       val sr3 = doc1.select(0,0,6,2,3,0).getOrElse(fail()).asInstanceOf[Text]
       assertEquals(sr3.text, "Section test 3");
       assertFalse(sr3.text=="test2")
       val sr4 = doc1.select(0,0,6,2,1).getOrElse(fail()).asInstanceOf[NumSection]
       assertSame(sr4.style, style2);
       val sr5 = doc1.select(0,0,0,2).getOrElse(fail()).asInstanceOf[Text]
       assertSame(sr5.style, style2);
       val sr6 = sr4.parentOfType[Document](classOf[Document])
       assertSame(doc1, sr6.get);
       val pred1 = (o:Document) => {true}
       val pred2 = (o:NumSection) => {true}
       val i1 = sr4.countParentsOfTypeUntil[Document](classOf[Document],pred1)
       val i2 = sr4.countParentsOfTypeUntil[NumSection](classOf[NumSection],pred2)
       assertEquals(1, i1);
       assertEquals(2, i2);
       val sr7 = doc1.select(0,0,6,2,0)
       val sr8 = sr4.parentOrSiblingOfType[Text](classOf[Text])
       assertSame(sr7.get, sr8.get);
       val i3 = sr4.enumeratorLevel
       assertEquals(2, i3)
       
       val i4 = doc1.countTreeLeafs
       assertEquals(38, i4)
       val i5 = sr4.countTreeLeafs
       assertEquals(0, i5)
    }
    
    @Test def testDocument2() {
        
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
        
        val doc = Document("",
              Chapter("",
                  Section("Ćwiczenie z przedmiotu 'Cieńkościenne konstrukcje metalowe'. Słup ściskany osiowo - wyboczenie giętne i skrętne. Autorzy: Irmina Grudzień, Artur Opala."),
                  Section(""),
                  Section("W ćwiczeniu przyjęto słup krzyżowy o profilu +I400 (wg rysunku w załączniku) ze stali St3SX zamocowany przegubowo na obu końcach."),
                  Section("Obliczenia i wymiarowanie przeprowadzono kolejno dla 6 różnych długości słupa, l = 3, 4, 5, 6, 7 i 8m."),
                  
              NumSection(BOLD,"Parametry zadania",  
                  Section(ITALIC,"Wszystkie parametry i wyniki podano w odpowiednich jednostkach SI (metrach i Newtonach)"),
                  NumSection(BOLD,"Parametry geometryczne:",
                    Section(Evaluate(c1,h,bf,tf,tw,A))),
                  NumSection(BOLD,"Parametry wytrzymałościowe:",
                    Section(Evaluate(c1,Ix,Iy,Io,ix,io,Iomega,It))),
                  NumSection(BOLD,"Parametry materiałowe:",
                    Section(Evaluate(c1,E,G,fd))),
                  NumSection(BOLD,"Współczynniki długości wyboczeniowej:",
                    Section(Evaluate(c1,mix,mif))),
                  NumSection(BOLD,"Nośność obliczeniowa przekroju przy osiowym ściskaniu:",
                    Section(Evaluate(c1,NRc)))
                  
              ),
              NumSection(BOLD,"Obliczenie sił krytycznych wyboczenia giętnego i skrętnego dla zadanych przypadków.",
                  NumSection(BOLD,"Obliczenia dla l = 3m :",
                    NumSection("Smukłość wyboczenia giętnego i skrętnego:",
                      Section(Evaluate(c1,lambdax,lambdaf))),
                    NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c1,Nxcr))),
                    NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c1,Nfcr)))
                  ),
                  
                  NumSection(BOLD,"Obliczenia dla l = 4m :",
                    NumSection("Smukłość wyboczenia giętnego i skrętnego:",
                      Section(Evaluate(c2,lambdax,lambdaf))),
                    NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c2,Nxcr))),
                    NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c2,Nfcr)))
                  ),
                  
                  NumSection(BOLD,"Obliczenia dla l = 5m :",
                    NumSection("Smukłość wyboczenia giętnego i skrętnego:",
                      Section(Evaluate(c3,lambdax,lambdaf))),
                    NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c3,Nxcr))),
                    NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c3,Nfcr)))
                  ),
                  
                  NumSection(BOLD,"Obliczenia dla l = 6m :",
                    NumSection("Smukłość wyboczenia giętnego i skrętnego:",
                      Section(Evaluate(c4,lambdax,lambdaf))),
                    NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c4,Nxcr))),
                    NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c4,Nfcr)))
                  ),
                  
                  NumSection(BOLD,"Obliczenia dla l = 7m :",
                    NumSection("Smukłość wyboczenia giętnego i skrętnego:",
                      Section(Evaluate(c4,lambdax,lambdaf))),
                    NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c4,Nxcr))),
                    NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c4,Nfcr)))
                  ),
                  
                  NumSection(BOLD,"K. Obliczenia dla l = 8m :",
                    NumSection("Smukłość wyboczenia giętnego i skrętnego:",
                      Section(Evaluate(c4,lambdax,lambdaf))),
                    NumSection("Siła krytyczna wyboczenia giętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c4,Nxcr))),
                    NumSection("Siła krytyczna wyboczenia skrętnego przy ściskaniu osiowym:",
                      Section(Evaluate(c4,Nfcr)))
                  ),
                  
                  NumSection(BOLD,"Podsumowanie obliczeń :",
                    Section("dla l = 3m : ",Evaluate(c1,Nxcr,lambdax,Nfcr,lambdaf)),
                    Section("dla l = 4m : ",Evaluate(c2,Nxcr,lambdax,Nfcr,lambdaf)),
                    Section("dla l = 5m : ",Evaluate(c3,Nxcr,lambdax,Nfcr,lambdaf)),
                    Section("dla l = 6m : ",Evaluate(c4,Nxcr,lambdax,Nfcr,lambdaf)),
                    Section("dla l = 7m : ",Evaluate(c5,Nxcr,lambdax,Nfcr,lambdaf)),
                    Section("dla l = 8m : ",Evaluate(c6,Nxcr,lambdax,Nfcr,lambdaf))
                  ),
                  
                  NumSection(BOLD,"Wnioski z obliczeń :",
                     Section("Dla długości słupa krótszych od 7m siła krytyczna wyboczenia skrętnego jest mniejsza od siły krytycznej wyboczenia giętnego.")   
                  )
              ),
              
              NumSection(BOLD,"Wymiarowanie słupa wg PN-90 B-03200.",
                  Section("Obliczenie maksymalnej osiowej siły ściskającej ",Evaluate(c1,Nmax)," dla każdej z rozpatrywanych długości słupa."),
                  NumSection(BOLD,"Obliczenia dla l = 3m :",
                  NumSection("Smukłość względna pręta przy wyboczeniu:",
                    Section(Evaluate(c1,lambdad))),
                  NumSection("Współczynnik wyboczeniowy wg krzywej c (",Evaluate(c1,n),"):",
                    Section(Evaluate(c1,fi))),
                  NumSection("Maksymalna osiowa siła ściskająca:",
                    Section(Evaluate(c1,Nmax)))
                ),
                NumSection(BOLD,"Obliczenia dla l = 4m :",
                  NumSection("Smukłość względna pręta przy wyboczeniu:",
                    Section(Evaluate(c2,lambdad))),
                  NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
                    Section(Evaluate(c2,fi))),
                  NumSection("Maksymalna osiowa siła ściskająca:",
                    Section(Evaluate(c2,Nmax)))
                ),
                NumSection(BOLD,"Obliczenia dla l = 5m :",
                  NumSection("Smukłość względna pręta przy wyboczeniu:",
                    Section(Evaluate(c3,lambdad))),
                  NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
                    Section(Evaluate(c3,fi))),
                  NumSection("Maksymalna osiowa siła ściskająca:",
                    Section(Evaluate(c3,Nmax)))
                ),
                NumSection(BOLD,"Obliczenia dla l = 6m :",
                  NumSection("Smukłość względna pręta przy wyboczeniu:",
                    Section(Evaluate(c4,lambdad))),
                  NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
                    Section(Evaluate(c4,fi))),
                  NumSection("Maksymalna osiowa siła ściskająca:",
                    Section(Evaluate(c4,Nmax)))
                ),
                NumSection(BOLD,"Obliczenia dla l = 7m :",
                  NumSection("Smukłość względna pręta przy wyboczeniu:",
                    Section(Evaluate(c5,lambdad))),
                  NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
                    Section(Evaluate(c5,fi))),
                  NumSection("Maksymalna osiowa siła ściskająca:",
                    Section(Evaluate(c5,Nmax)))
                ),
                NumSection(BOLD,"Obliczenia dla l = 8m :",
                  NumSection("Smukłość względna pręta przy wyboczeniu:",
                    Section(Evaluate(c6,lambdad))),
                  NumSection("Współczynnik wyboczeniowy wg krzywej c (n=1,2):",
                    Section(Evaluate(c6,fi))),
                  NumSection("Maksymalna osiowa siła ściskająca:",
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
                 Section("2. Bogucki W., Żyburtowicz M. \"Tablice do projektowania konstrukcji metalowych\" Warszawa 1996"),
                 Section("3. Gosowski B. \"Skręcanie i zginanie otwartych, stężonych elementów konstrukcji metalowych\" Wrocław 2004"),
                 Section("4. Gosowski B., Kubica E. \"Badania laboratoryjne z konstrukcji metalowych\" Wrocław 2007")
                 
              )
          )
        )
        
    }
	
	
	@Test def testTranslator1() {
		val locale = new java.util.Locale("PL")
		val dict = "document"
		assertEquals("spełniony",Translator.translate("verified",locale,dict).get)
		assertEquals("NIE spełniony",Translator.translate("not verified",locale,dict).get)
		assertEquals("NIEOKREŚLONY",Translator.translate("unknown",locale,dict).get)
	}
    
}