package org.encalmo.structures.eurocode.steel

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.Worksheet
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document.{NumSection, Evaluate, Section}

class ProfiledSteelSheetUnitTest extends AssertionsForJUnit {

    @Test
    def testProfiledSteelSheet(): Unit = {
        val profiledSteelSheet = ProfiledSteelSheet.COFRAPLUS_60_100
        def worksheet = new Worksheet("Profiled Steel Sheet Test") {
            import org.encalmo.expression.BasicSymbols._

            import profiledSteelSheet.{br,hw,bb,fyb,t,hp,sw,MRdm,MRdp}
            this add profiledSteelSheet

            val Wplus = symbol(W!"+") unit SI.cm3
            val eplus = symbol(e!"+") unit SI.mm
	        
            I := 57.2 unit SI.cm4
            eplus := 33.3
            Wplus := M / fyb
            I := Wplus*eplus
            z := (br*hp+2*sw*hp/2)/(br+bb+2*sw)

            override val document = defaultDocument(
                Section(profiledSteelSheet.info,Section(Evaluate(MRdp,MRdm,M,Wplus,I,eplus,z)),profiledSteelSheet.shear)
            )
        }
        val results = worksheet.results
        worksheet.printHtml(results)("target/test-results/" + worksheet.name + ".html")
        worksheet.printXslFo(results)("target/test-results/" + worksheet.name + ".fo")
        worksheet.printPdf(results)("target/test-results/" + worksheet.name + ".pdf")
    }

    @Test
    def test2(): Unit = {
        def worksheet = new Worksheet("Assert Test") {
            import org.encalmo.expression.BasicSymbols._

            val steel = Steel.S355
            this add steel
            val ipe = IPESteelSection.IPE_450
            this add ipe

            val ΓV = Gamma|V
            val MRd = M|"Rd" unit SI.kNm

            l := 15 unit SI.m
            q := 2.5 unit "kN/m2"
            s := 3 unit SI.m
            M := q*s*sq(l)/8
            MRd := steel.fyd*ipe.Wy

            ΓV := assertLessThenOrEqualTo(abs(M/MRd),1)

            override val document = defaultDocument(
                Evaluate(l,q,M,MRd,ΓV)
            )
        }
        val results = worksheet.results
        worksheet.printHtml(results)("target/test-results/" + worksheet.name + ".html")
        worksheet.printXslFo(results)("target/test-results/" + worksheet.name + ".fo")
        worksheet.printPdf(results)("target/test-results/" + worksheet.name + ".pdf")
    }


}
