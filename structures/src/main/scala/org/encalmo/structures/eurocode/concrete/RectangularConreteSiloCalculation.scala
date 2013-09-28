package org.encalmo.structures.eurocode.concrete

import org.encalmo.expression._
import org.encalmo.document._
import org.encalmo.structures.{Worksheet}
import org.encalmo.style.PredefinedStyles
import PredefinedStyles._
import org.encalmo.structures.eurocode.actions.silos.ParticulateSolid
import org.encalmo.structures.eurocode.actions.silos.RectangularSlenderSiloWithFlatBottom

class RectangularConreteSiloCalculation extends Worksheet("kb-silos") {

    val solid = ParticulateSolid.Flyash
    val concrete = Concrete.C_20_25
    val reinforcement = ReinforcingSteel.B500SP
    val silos = new RectangularSlenderSiloWithFlatBottom("Silos prostokątny żelbetowy",5.7, 6.0, 12, 4.2, 0.2, 0.4, 0.25, solid, 3)

    this add silos

    override val document = Document("",
        PredefinedStyles.stylesConfig,
        Chapter("",
            Section(
                Section("Ćwiczenie projektowe z \"Konstrukcji Betonowych\". Semestr letni 2010/2011."),
                Section("Autor: XXX. Prowadzący: dr inż. Włodzimierz Wydra, Instytut Budownictwa Politechiki Wrocławskiej.")
            ),
            Section(""),
            Section(
                Section(styleTitle, "Silos żelbetowy jednokomorowy na popiół lotny.")
            ),
            NumSection("Wykaz materiałów źródłowych",
                NumSection(styleComment, " Norma PN-EN 1991-4 \"Eurokod 1. Oddziaływania na konstrukcje. Część 4: Silosy i zbiorniki.\""),
                NumSection(styleComment, " Norma PN-EN 1992-1-1 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-1: Reguły ogólne i reguły dla budynków.\""),
                NumSection(styleComment, " Norma PN-EN 1992-1-2 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 1-2: Reguły ogólne. Projektowanie z uwagi na warunki pożarowe.\""),
                NumSection(styleComment, " Norma PN-EN 1992-3 \"Eurokod 2. Projektowanie konstrukcji z betonu. Część 3: Silosy i zbiorniki na ciecze.\"")
            ),
            silos.inputGeometry,
            solid.properties,
            concrete.info,
            reinforcement.info,
            silos.calculatedGeometry,
            solid.characteristicValues,
            silos.volumes,
            silos.fillingSymmetricalLoad,
            silos.fillingPatchLoad,
            silos.dischargeSymmetricalLoad,
            silos.dischargePatchLoad,
            silos.loadsOnSiloBottom,
            Section(style1.marginTop(20), ""),
            Section("Koniec obliczeń."),
            Section(style1.marginTop(20), ""),
            Section(style1.useAlign("right"), "Opracował: XXX")
        )
    )

}
