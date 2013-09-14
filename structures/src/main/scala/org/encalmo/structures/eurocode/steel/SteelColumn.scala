package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Calculation
import org.encalmo.document._
import org.encalmo.structures.common.section.Section

trait SteelColumnSymbols extends SymbolConfigurator {
    import BasicSymbols._

    val ID = symbol("ID").makeNonPrintable
    val Ned = symbol(N|"ed") unit SI.kN
    val NbRd = symbol(N|"b,Rd") unit SI.kN
    val Lcry = symbol(L|"cr,y") unit SI.m
    val Lcrz = symbol(L|"cr,z") unit SI.m
    val lambday = symbol(lambda|y over ("_"))
    val lambdaz = symbol(lambda|z over ("_"))
    val lambdal = symbol(lambda|l)
    val Xi = symbol(BasicSymbols.Xi)
    val Xiy = symbol(BasicSymbols.Xi|y)
    val Xiz = symbol(BasicSymbols.Xi|z)
    val alphay = symbol(BasicSymbols.alpha|y)
    val Phiy = symbol(BasicSymbols.Phi|y)
    val alphaz = symbol(BasicSymbols.alpha|z)
    val Phiz = symbol(BasicSymbols.Phi|z)

}

class SteelColumn(name:String, val steel:Steel, val section:SteelSection, val p_Lcry: Expression, val p_Lcrz: Expression, val p_Ned: Expression)
extends Calculation(name,"steelColumn") with SteelColumnSymbols {

    import steel.{epsi,fy,gammaM1}
    import section.{A,iy,iz,C2,wy,wz}

    this add steel
    this add section

    ID := text(name)

    Lcry := p_Lcry
    Lcrz := p_Lcrz
    Ned := p_Ned

    alphay := mapChoice(wy,Map(text("a0")->Number(0.13),text("a")->Number(0.21),text("b")->Number(0.34),text("c")->Number(0.49),text("d")->Number(0.76)))
    alphaz := mapChoice(wz,Map(text("a0")->Number(0.13),text("a")->Number(0.21),text("b")->Number(0.34),text("c")->Number(0.49),text("d")->Number(0.76)))

    lambdal := 93.9*epsi
    lambday := Lcry/(iy*lambdal)
    lambdaz := Lcrz/(iz*lambdal)
    Phiy := 0.5*(1+alphay*(lambday-0.2)+(lambday^2))
    Phiz := 0.5*(1+alphaz*(lambdaz-0.2)+(lambdaz^2))
    Xiy := 1/(Phiy+sqrt((Phiy^2)-(lambday^2)))
    Xiz := 1/(Phiz+sqrt((Phiz^2)-(lambdaz^2)))
    Xi := min(Xiy,Xiz)
    NbRd := (Xi*A*fy)/gammaM1

    override def label = this(ID)

    def info = NumSection(TextToTranslate("SteelColumn",dictionary),name,
        Evaluate(steel.label,section.label,C2,A,iy,iz,Lcry,Lcrz)
    )

}
