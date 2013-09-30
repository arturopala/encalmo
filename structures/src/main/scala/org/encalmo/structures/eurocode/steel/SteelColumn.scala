package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.Calculation
import org.encalmo.document._

trait SteelColumnSymbols extends SymbolConfigurator {
    import BasicSymbols._

    val ID = symbol("ID").makeNonPrintable
    val Ned = symbol(N|"ed") unit SI.kN
    val NbRd = symbol(N|"b,Rd") unit SI.kN
    val Lcry = symbol(L|"cr,y") unit SI.m
    val Lcrz = symbol(L|"cr,z") unit SI.m
    val lambday = symbol(lambda|y over "_")
    val lambdaz = symbol(lambda|z over "_")
    val lambdal = symbol(lambda|l)
    val chi = symbol(BasicSymbols.chi)
    val chiy = symbol(BasicSymbols.chi|y)
    val chiz = symbol(BasicSymbols.chi|z)
    val alphay = symbol(BasicSymbols.alpha|y)
    val Phiy = symbol(BasicSymbols.Phi|y)
    val alphaz = symbol(BasicSymbols.alpha|z)
    val Phiz = symbol(BasicSymbols.Phi|z)

}

/**
 * Steel Column
 * @param name element name
 * @param steel steel type
 * @param section section type
 * @param p_Lcry critical length along y-y plane
 * @param p_Lcrz critical length along z-z plane
 * @param p_Ned axis compression load
 */
class SteelColumn(name:String, val steel:Steel, val section:SteelSection, val p_Lcry: Expression, val p_Lcrz: Expression, val p_Ned: Expression)
extends Calculation(name,"steelColumn") with SteelColumnSymbols {

    import steel.{epsi,fy,gammaM1}
    import section.{A,iy,iz,C2,wy,wz,h,b}

    this add steel
    this add section

    section(section.epsi) = steel(steel.epsi)

    ID := text(name)

    Lcry := p_Lcry
    Lcrz := p_Lcrz
    Ned := p_Ned

    alphay := mapChoice(wy,text("a0")->Number(0.13),text("a")->Number(0.21),text("b")->Number(0.34),text("c")->Number(0.49),text("d")->Number(0.76))
    alphaz := mapChoice(wz,text("a0")->Number(0.13),text("a")->Number(0.21),text("b")->Number(0.34),text("c")->Number(0.49),text("d")->Number(0.76))

    lambdal := 93.9*epsi
    lambday := Lcry/(iy*lambdal)
    lambdaz := Lcrz/(iz*lambdal)
    Phiy := 0.5*(1+alphay*(lambday-0.2)+(lambday^2))
    Phiz := 0.5*(1+alphaz*(lambdaz-0.2)+(lambdaz^2))
    chiy := 1/(Phiy+sqrt((Phiy^2)-(lambday^2)))
    chiz := 1/(Phiz+sqrt((Phiz^2)-(lambdaz^2)))
    chi := min(chiy,chiz)
    NbRd := (chi*A*fy)/gammaM1

    val R_ULS1 = require(abs(Ned/NbRd)<1,"Nośność na ściskanie z uwzględnieniem wyboczenia wg PN-EN 1993-1-1 (6.16)")

    override def label = this(ID)

    def info = NumSection(Text("Steel column",dictionary),name,section.name,
        NumSection(Text("Inputs",dictionary),
            Evaluate(steel.label,section.label,Lcrz,Lcrz,Ned)
        ),
        NumSection(Text("Section data",dictionary),
            Evaluate(h,b,epsi,C2,A,lambdal)
        ),
        NumSection(Text("Reduction factor for the z-z",dictionary), "PN-EN 1993-1-1 6.3.1.2, 6.3.1.3",
            Evaluate(Lcrz,iz,wz,alphaz,lambdaz,Phiz,chiz)
        ),
        NumSection(Text("Reduction factor for the y-y",dictionary),"PN-EN 1993-1-1 6.3.1.2, 6.3.1.3",
            Evaluate(Lcry,iy,wy,alphay,lambday,Phiy,chiy)
        ),
        NumSection(Text("Design buckling resistance of a compression member",dictionary), " PN-EN 1993-1-1 6.3.1.1",
            Evaluate(chi,NbRd)
        ),
        Check(R_ULS1)
    )

}
