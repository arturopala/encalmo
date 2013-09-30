package org.encalmo.structures.miscellaneous

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.style.PredefinedStyles
import PredefinedStyles._
import org.encalmo.structures.eurocode.concrete.ReinforcingSteel
import org.encalmo.structures.eurocode.concrete.Concrete
import org.encalmo.structures.eurocode.steel._
import org.encalmo.structures.Worksheet
import org.encalmo.document.TableOfContents
import org.encalmo.structures.eurocode.fasteners.HeadedStud
import org.encalmo.structures.eurocode.composite.{BeamOfCompositeSlab, CompositeConcreteSlabOnProfiledSteelSheeting}

class AOMTWorksheet extends Worksheet("aomt") {

    import BasicSymbols._

    //dane wejsciowe zadania
    val L1 = L|1 is "Rozpiętośc belki" unit SI.m := 15
    val L2 = L|2 is "Rozstaw belek" unit SI.m := 2.5
    val pc = p!c is "Obciążenie charakterystyczne" unit "kN/m2" := 2.5 unit "kN/m2"
    
    //przyjete materialy i wymiary
    val height:Expression = 11 unit SI.cm
    val blacha = ProfiledSteelSheet.COFRAPLUS_60_100
    val beton = Concrete.C_20_25
    val stalZbrojeniowa = ReinforcingSteel.B500SP
    val stal = Steel.S355
    val profil = IPESteelSection.IPE_450
    val sworzen = HeadedStud.NELSON_S3L_19_100

    val plyta = new CompositeConcreteSlabOnProfiledSteelSheeting(
        name = "Płyta stropowa zespolona beton + blacha trapezowa",
        height = height,
        length = this(L2),
        spans = 5,
        sheet = blacha,
        concrete = beton,
        reinforcingSteel = stalZbrojeniowa,
        p_gammaG = 1.35,
        p_gammaQ = 1.5,
        p_Gsk = 1 unit "kN/m2",
        p_qk = this(pc),
        p_Fk =  10 unit SI.kN,
        p_dmesh = 8 unit SI.mm,
        p_sd = 150 unit SI.mm,
        p_ss = profil(profil.b)
    )

    val belka = new BeamOfCompositeSlab(
        name = "Belka stalowa zespolona z płytą stropową zespoloną",
        length = this(L1),
        section = profil,
        steel = stal,
        slab = plyta,
        stud = sworzen,
        p_gammaG = 1.35,
        p_gammaQ = 1.5
    )

    this add plyta
    this add belka
    
    override val document = Document("",
        PredefinedStyles.stylesConfig,
        Chapter("",
        	Section(
            	Section("Praca magisterska \"Konstrukcja stropu zespolonego garażu wielopoziomowego\". Autor: Artur Opala, album 61315."),
        		Section("Prowadzący: dr hab. inż. Wojciech Lorenc, Instytut Budownictwa Politechniki Wrocławskiej. Semestr letni 2012/2013.")
        	),
            Section(""),
            Section(styleTitle,"Strop zespolony w parkingu wielopoziomowym - wariant 1."),
           NumSection("Parametry zadania",
               NumSection("Dane wejściowe",Evaluate(L1,L2,pc)),
               NumSection("Przyjęto do obliczeń",Evaluate(plyta(plyta.h),stal.label,beton.label,blacha.label,profil.label,sworzen.label)(plyta))
           ),
           NumSection("Podsumowanie wyników",
               Checklist(10),
               Evaluate(belka.mS)(belka)
           ),
           NumSection("Obliczenia szczegółowe",
               TableOfContents("Spis treści", 4),
               NumSection("Wymiarowanie płyty stropowej",
                   NumSection("Przyjęte wymiary i właściwości materiałowe",
                       blacha.steel.info,beton.info,blacha.info,plyta.info
                   ),
                   NumSection("Obciążenia i schemat statyczny w fazie montażu",
                       Section(styleDescription,
                           """Schemat statyczny blachy trapezowej pełniącej funkcje deskowania przyjęto jak dla belki ciągłej pięcioprzęsłowej, wolnopodpartej, o stałej sztywności EI i stałym rozstawie przęseł.
                              Siły wewnętrzne w przęśle i na podporze oraz reakcje podporowe obliczono dla obciążenia ciągłego, równomiernie rozłożonego, korzystając z tablic w [6]."""),
                       plyta.LOAD1
                   ),
                   NumSection("Blacha trapezowa jako deskowanie",
                      plyta.ULS1,plyta.SLS1
                   ),
                   NumSection("Obciążenia i schemat statyczny w fazie eksploatacji",
                       Section(styleDescription,
                           """Schemat statyczny blachy trapezowej zespolonej z betonem przyjęto jak dla belki swobodnie podpartej.
                              Siły wewnętrzne w przęśle i na podporze obliczono dla obciążenia ciągłego, równomiernie rozłożonego.
                              Nad podporami pośrednimi zaprojektowano zbrojenie przeciwdziałające zarysowaniu betonu w formie siatki zbrojeniowej."""),
                       plyta.LOAD2
                   ),
                   NumSection("Wymiarowanie płyty zespolonej w fazie eksploatacji",
                       plyta.ULS2,plyta.SLS2
                   )
               ),
               NumSection("Wymiarowanie belki stropowej",
                   belka.info,
                   NumSection("Obciążenia i schemat statyczny w fazie montażu",
                       Section(styleDescription,
                           """Schemat statyczny belki w fazie montażu przyjęto jak dla belki jednoprzęsłowej, wolnopodpartej, obciążonej równomiernie na całej długości."""),
                       belka.LOAD1
                   ),
                   NumSection("Wymiarowanie belki w fazie montażu",
                       belka.ULS1,belka.SLS1
                   ),
                   NumSection("Obciążenia i schemat statyczny w fazie eksploatacji",
                       Section(styleDescription,
                           """Schemat statyczny belki w fazie eksploatacji przyjęto jak dla belki jednoprzęsłowej, wolnopodpartej, obciążonej równomiernie na całej długości."""),
                       belka.LOAD2
                   ),
                   NumSection("Wymiarowanie belki w fazie eksploatacji",
                       belka.ULS2,belka.SLS2
                   )
               )
           ),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")
        )
    )

}
