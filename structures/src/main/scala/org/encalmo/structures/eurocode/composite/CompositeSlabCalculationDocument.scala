package org.encalmo.structures.eurocode.composite

import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.fop.FOPHelper
import org.encalmo.structures.Predefined
import org.encalmo.structures.Predefined._
import org.encalmo.structures.eurocode.concrete.ReinforcingSteel
import org.encalmo.structures.eurocode.concrete.Concrete
import org.encalmo.structures.eurocode.concrete.ConcreteSymbols
import org.encalmo.structures.eurocode.actions.ActionsSymbols
import org.encalmo.structures.eurocode.steel.SteelSymbols
import org.encalmo.structures.eurocode.steel.IPESection
import org.encalmo.structures.eurocode.steel.HeadedStud
import org.encalmo.structures.eurocode.steel.Steel
import org.encalmo.structures.eurocode.steel.FLORSTROP
import scalax.file.Path
import org.encalmo.structures.CalculationDocument

class CompositeSlabCalculationDocument extends CalculationDocument {
    
    import BasicSymbols._
    import SteelSymbols.{gammaM0,gammaM1}
    import ConcreteSymbols.{gammaC}
    import ActionsSymbols.{gammaG,gammaQ}
    
    import CompositeSlabWithProfiledSheetingSymbols.{Gsk,qk,Fk,dmesh,sd}
    
    override val name = "kz-strop"
    
    val zadanie1Statyka,zadanie1Eksploatacja,zadanie1Wymiarowanie:Seq[Expression] = Seq()
    val zadanie2Materialy,zadanie2Obciazenia,zadanie2Montaz,zadanie2Eksploatacja,zadanie2Wymiarowanie:Seq[Expression] = Seq()
    
    val zadanie = Calculation()
    val zadanie1 = Calculation()
    
    //dane wejsciowe zadania
    val L1 = L|1 is "Rozpiętośc belki" unit "m"
    zadanie(L1) = 18
    val L2 = L|2 is "Rozstaw belek" unit "m"
    zadanie(L2) = 2.8
    val pc = p!c is "Obciążenie charakterystyczne" unit "N/m2"
    zadanie(pc) = 3000
    val daneWejsciowe = Seq(L1,L2,pc)
    
    //przyjete materialy i wymiary
    val height:Expression = 12 unit SI.cm
    val blacha = FLORSTROP.T59_Z_125
    val beton = Concrete.C_20_25
    val stalZbrojeniowa = ReinforcingSteel.B500SP
    val stal = Steel.S355
    val profil = IPESection.IPE_600
    val sworzen = HeadedStud.NELSON_S3L_19_100
    
    val plyta = new CompositeSlabWithProfiledSheeting(height,zadanie(L2),5,blacha,beton,stalZbrojeniowa)
    
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
    plyta(sd) = 0.15
    
    val zadanie2 = Calculation()
    val belka = new BeamOfCompositeSlab(zadanie(L1),profil,stal,plyta,sworzen)
    belka(gammaG) = 1.35
    belka(gammaQ) = 1.5
    belka.steel(gammaM0) = 1.0
    belka.steel(gammaM1) = 1.0
    zadanie2 add belka
    
    
    val doc = Document("",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z przedmiotu \"Konstrukcje Zespolone\". Autor: Artur Opala, album 61315."),
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
                NumSection("Dane wejściowe",Evaluate(daneWejsciowe,zadanie),Evaluate(zadanie,stal.label)),
                NumSection("Przyjęto do obliczeń",Evaluate(zadanie,plyta(CompositeSlabWithProfiledSheetingSymbols.h),beton.label,blacha.label,profil.label,sworzen.label))
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
	           belka.info,
	           NumSection("Obciążenia i schemat statyczny w fazie montażu",
	               belka.LOAD1
	           ),
	           NumSection("Wymiarowanie belki w fazie montażu",
	               belka.ULS1,belka.SLS1
	           ),
	           NumSection("Obciążenia i schemat statyczny w fazie eksploatacji",
	               belka.LOAD2
	           ),
	           NumSection("Wymiarowanie belki w fazie eksploatacji",
	               belka.ULS2,belka.SLS2
	           )
           ),
           NumSection("Podsumowanie",
           		Evaluate(belka,BeamOfCompositeSlabSymbols.mS)
           ),
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")
        )
    )
    
    @Test override def printHtml:Unit = super.printHtml

}
