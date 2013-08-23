package org.encalmo.printer.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document.Evaluate

class ExpressionToPrintUnitTest extends AssertionsForJUnit {

    @Test def shouldPrepareExpressionToPrint() {
        //given
        import BasicSymbols._
        implicit val calc = Calculation()
        a := b * (c - d)
        b := c + 10
        c := hypot(-e, abs(f))
        d := h * (e / 10)
        e := sin(z) - 0.3
        val element = Evaluate(a, b, c, d, e)
        //when
        val toPrint = ExpressionToPrint.prepare(element, new FormulaSetCache {})
        //then
        assertTrue(toPrint.size == 5)
        val r1 = toPrint(0)
        assertTrue(r1.size == 3)
        assertTrue(r1(0).expression == a)
        assertTrue(r1(0).prefix == null)
        assertTrue(r1(0).suffix == null)
        assertTrue(r1(1).expression == (b * (c - d)))
        assertTrue(r1(1).prefix == "=")
        assertTrue(r1(1).suffix == null)
        assertTrue(r1(2).expression == (10 + (hypot(-(sin(z) - 0.3), abs(f)))) * ((hypot(-(sin(z) - 0.3), abs(f))) - (h * ((sin(z) - 0.3) / 10))))
        assertTrue(r1(2).prefix == "=")
        assertTrue(r1(2).suffix == null)
        val r2 = toPrint(1)
        assertTrue(r2.size == 4)
        assertTrue(r2(0).expression == b)
        assertTrue(r2(0).prefix == null)
        assertTrue(r2(0).suffix == null)
        assertTrue(r2(1).expression == c + 10)
        assertTrue(r2(1).prefix == "=")
        assertTrue(r2(1).suffix == null)
        assertTrue(r2(2).expression == hypot(-(sin(z) - 0.3), abs(f)) + 10)
        assertTrue(r2(2).prefix == "=")
        assertTrue(r2(2).suffix == null)
        val r3 = toPrint(2)
        assertTrue(r3.size == 3)
        assertTrue(r3(0).expression == c)
        assertTrue(r3(0).prefix == null)
        assertTrue(r3(0).suffix == null)
        assertTrue(r3(1).expression == hypot(-e, abs(f)))
        assertTrue(r3(1).prefix == "=")
        assertTrue(r3(1).suffix == null)
        assertTrue(r3(2).expression == hypot(-(sin(z) - 0.3), abs(f)))
        assertTrue(r3(2).prefix == "=")
        assertTrue(r3(2).suffix == null)
        val r4 = toPrint(3)
        assertTrue(r4.size == 3)
        assertTrue(r4(0).expression == d)
        assertTrue(r4(0).prefix == null)
        assertTrue(r4(0).suffix == null)
        assertTrue(r4(1).expression == h * (e / 10))
        assertTrue(r4(1).prefix == "=")
        assertTrue(r4(1).suffix == null)
        assertTrue(r4(2).expression == h * ((sin(z) - 0.3) / 10))
        assertTrue(r4(2).prefix == "=")
        assertTrue(r4(2).suffix == null)
        val r5 = toPrint(4)
        assertTrue(r5.size == 2)
        assertTrue(r5(0).expression == e)
        assertTrue(r5(0).prefix == null)
        assertTrue(r5(0).suffix == null)
        assertTrue(r5(1).expression == sin(z) - 0.3)
        assertTrue(r5(1).prefix == "=")
        assertTrue(r5(1).suffix == null)
    }

}