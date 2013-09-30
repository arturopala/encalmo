package org.encalmo.structures.eurocode.steel

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.encalmo.structures.Worksheet
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.expression.sqrt
import org.encalmo.expression.abs

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
    }

    @Test
    def test2(): Unit = {
        def worksheet = new Worksheet("Assert Test") {
            import org.encalmo.expression.BasicSymbols._

            val steel = Steel.S355
            this add steel
            val ipe = IPESteelSection.IPE_450
            this add ipe

            val MRd = M|"Rd" unit SI.kNm is "Nośność obliczeniowa na zginanie"
            val VRd = V|"Rd" unit SI.kN is "Nośność obliczeniowa na ścinanie"
            val qb = q|b unit "kN/m" is "Obciążenie obliczeniowe"

            l := 15 unit SI.m
            q := 2.5 unit "kN/m2"
            s := 3 unit SI.m
            g := (ipe.m*GRAV) unit "kN/m"
            qb := 1.5*q*s + 1.35*g
            M := qb*sq(l)/8
            V := qb*l/2
            MRd := steel.fyd*ipe.Wy
            VRd := ipe.AVz*(steel.fyd/sqrt(3))
            val r1 = require(abs(V / VRd) < 1,"Nośność na ścinanie przy podporze")
            val r2 = require(abs(M / MRd) < 0.1,"Nośność na zginanie w przęśle")

            override val document = defaultDocument(
                NumSection("Obliczenia",
                    TableOfContents("Spis treści"),
                    Checklist(),
                    NumSection("Belka stropowa swobodnie podparta",
                        Evaluate(l,q,g,qb,M,MRd,V,VRd),
                        Check(r1,r2)
                    ),
                    Evaluate(ipe.Iy)
                )
            )
        }
        val results = worksheet.results
        worksheet.printHtml(results)("target/test-results/" + worksheet.name + ".html")
        worksheet.printXslFo(results)("target/test-results/" + worksheet.name + ".fo")
        worksheet.printPdf(results)("target/test-results/" + worksheet.name + ".pdf")
    }


}
