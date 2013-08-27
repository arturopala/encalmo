package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.document._
import org.encalmo.structures.Predefined
import org.encalmo.structures.Predefined._
import org.encalmo.structures.eurocode.actions.silos.ParticulateSolid
import org.encalmo.structures.eurocode.actions.silos.ThinWalledCircularSlenderSiloWithSteepHopper
import org.encalmo.structures.Worksheet

/**
 * Calculation of the circular steel silos with steep hopper
 */
class CircularSteelSiloCalculation extends Worksheet("km-silos") {
   
    val particulateSolid = ParticulateSolid.Cement
    val steel = Steel.S275
    val silos = new ThinWalledCircularSlenderSiloWithSteepHopper(
            diameter = 4.0 unit SI.m,
            heightOfChamber = 11.0 unit SI.m,
            heightOfHopper = 3.0 unit SI.m,
            thicknessOfChamberWall = 10 unit SI.mm,
            thicknessOfHopperWall = 15 unit SI.mm,
            thicknessOfRing = 20 unit SI.mm,
            heightOfRing = 390 unit SI.mm,
            widthOfColumn = 300 unit SI.mm,
            numberOfColumns = 4,
            diameterOfOutlet = 30 unit SI.cm,
            particulateSolid = particulateSolid,
            wallType = 1,
            steel = Steel.S275
    )
    
    this add silos

    override val document = Document("",
        Predefined.stylesConfig,
        Chapter("",
        	Section(
            	Section("Ćwiczenie projektowe z \"Konstrukcji Metalowych\". Semestr zimowy 2011/2012."),
        		Section("Autor: Artur Opala, 61315. Prowadzący: dr inż. Jacek Dudkiewicz, Instytut Budownictwa Politechiki Wrocławskiej.")
        	),
            Section(""),
            Section(
                Section(styleTitle,"Stalowy silos walcowy jednokomorowy na cement. \r\n Sprawdzenie nośności powłoki komory."),
                TableOfContents("Spis treści"),
                Section(styleComment,"Wykaz materiałów źródłowych",
                    Section(styleComment1,"[1991-1-3] Norma PN-EN 1991-1-3 \"Eurokod 1. Oddziaływania na konstrukcje. Część 1-3: Oddziaływania ogólne. Obciążenia śniegiem.\""),
                    Section(styleComment1,"[1991-1-4] Norma PN-EN 1991-1-4 \"Eurokod 1. Oddziaływania na konstrukcje. Część 1-4: Oddziaływania ogólne. Oddziaływania wiatru.\""),
                	Section(styleComment1,"[1991-4] Norma PN-EN 1991-4 \"Eurokod 1. Oddziaływania na konstrukcje. Część 4: Silosy i zbiorniki.\""),
                	Section(styleComment1,"[1993-1-1] Norma PN-EN 1993-1-1 \"Eurokod 3. Projektowanie konstrukcji stalowych. Część 1-1: Reguły ogólne dla budynków.\""),
                	Section(styleComment1,"[1993-1-6] Norma PN-EN 1993-1-6 \"Eurokod 3. Projektowanie konstrukcji stalowych. Część 1-6: Wytrzymałosć i stateczność konstrukcji powłokowych.\""),
                	Section(styleComment1,"[1993-4] Norma PN-EN 1993-4 \"Eurokod 3. Projektowanie konstrukcji stalowych. Część 4-1: Silosy.\"")
                ),
                PageBreak,
                NumSection("Dane do projektowania",
                        silos.inputGeometry,
                        steel.info,
                        particulateSolid.properties,
                        particulateSolid.characteristicValues,
                        silos.calculatedGeometry,
                        silos.volumes
                ),
                NumSection("Oddziaływania na silos",
                    silos.ciezarWlasny,
                    silos.obciazenieUzytkowe,
                    silos.obciazenieSniegiem,
                    silos.obciazenieWiatrem,
                    NumSection("Oddziaływania na silos przy napełnianiu i opróżnianiu",
                        silos.fillingSymmetricalLoad,
                        silos.fillingPatchLoad,
                        silos.dischargeSymmetricalLoad,
                        silos.dischargePatchLoad,
                        silos.fillingHopperLoad,
                        silos.dischargeHopperLoad
                    )
                ),
                NumSection("Analiza stateczności powłoki silosu",
                    NumSection("Sprawdzenie geometrii powłoki",
                        silos.statecznosc
                    ),
                    NumSection("Obliczenie naprężeń krytycznych",
                        NumSection("Naprężenia krytyczne przy ściskaniu południkowym",silos.naprezeniaKrytycznePoludnikowe),
                        NumSection("Naprężenia krytyczne przy ściskaniu równoleżnikowym",silos.naprezeniaKrytyczneRownoleznikowe),
                        NumSection("Naprężenia krytyczne przy ścinaniu",silos.naprezeniaKrytyczneScinajace)
                    ),
                    NumSection("Sprawdzenie warunku stateczności powłoki przy obciążeniu wiatrem pustego silosu (obc. stałe (płaszcz,dach,pomost) + użytkowe + wiatr)",
                        silos.statecznoscKombinacja1
                    ),
                    NumSection("Sprawdzenie warunku stateczności powłoki przy opróżnianiu silosu (obc. stałe (płaszcz,dach,pomost) + użytkowe + śnieg + opróżnianie)",
                        silos.statecznoscKombinacja2
                    ),
                    NumSection("Sprawdzenie stateczności lokalnej płaszcza w rejonie podpory",
                        silos.statecznoscLokalnaPodpory
                    )
                )
            )
           ,
			Section(style1.marginTop(20),""),
			Section("Koniec obliczeń."),
			Section(style1.marginTop(20),""),
			Section(style1.useAlign("right"),"Opracował: Artur Opala")
        )
    )

}
