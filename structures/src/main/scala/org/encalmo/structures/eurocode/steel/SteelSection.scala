package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.structures.common.section.Section

trait SteelSectionSymbols extends SymbolConfigurator {

    val steelSectionDict = "steelSection"

    val phi = symbol(BasicSymbols.phiv) unit "mm" dict steelSectionDict //Minimalna średnica śruby

    val epsi = symbol(BasicSymbols.epsi) dict steelSectionDict //Współczynnik korekcyjny klasy stali
    val alphaw = symbol(BasicSymbols.alpha|BasicSymbols.w) dict steelSectionDict //Współczynnik proporcji naprężeń plastycznych w środniku
    val alphaf = symbol(BasicSymbols.alpha|BasicSymbols.f) dict steelSectionDict //Współczynnik proporcji naprężeń plastycznych w półce
    val Thetaw = symbol(BasicSymbols.Theta|BasicSymbols.w) dict steelSectionDict //Współczynnik proporcji naprężeń sprężystych w środniku
    val ksigma = symbol(BasicSymbols.k|BasicSymbols.sigmav) dict steelSectionDict //EN 1993-1-5
    //Classification ENV 1993-1-1
    val C1 = symbol("C"|"1") dict steelSectionDict //Klasa przekroju zginanego
    val C2 = symbol("C"|"2") dict steelSectionDict //Klasa przekroju ściskanego
    val C3 = symbol("C"|"3") dict steelSectionDict //Klasa przekroju ściskanego i zginanego
    val wy = symbol(BasicSymbols.w|BasicSymbols.y) dict steelSectionDict //Krzywa wyboczenia z płaszczyzny y-y
    val wz = symbol(BasicSymbols.w|BasicSymbols.z) dict steelSectionDict //Krzywa wyboczenia z płaszczyzny z-z
    
}

abstract class SteelSection(name: String) extends Section(name) with SteelSectionSymbols
