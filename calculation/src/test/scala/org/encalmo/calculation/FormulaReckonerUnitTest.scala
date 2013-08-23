package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.expression.hypot
import org.encalmo.expression.abs
import org.encalmo.expression.sin

class FormulaReckonerUnitTest extends AssertionsForJUnit {

    @Test def shouldReckonSingleExpression = {
        //given
        import BasicSymbols._
        implicit val calc = Calculation()
        a := b * (c - d)
        b := c + 10
        c := hypot(-e, abs(f))
        d := h * (e / 10)
        e := sin(z) - 0.3
        k := 8
        //when
        val formula = FormulaReckoner.reckon(a)
        //then
        val expected = (10 + (hypot(-(sin(z) - 0.3), abs(f)))) * ((hypot(-(sin(z) - 0.3), abs(f))) - (h * ((sin(z) - 0.3) / 10)))
        assertEquals(3, formula.parts.size)
        assertTrue(formula.parts.head.expression.isInstanceOf[Symbol])
        assertTrue(formula.parts.head.relation == FormulaPartRelation.NONE)
        assertTrue(formula.parts.head.position == FormulaPosition.LEFT)
        assertTrue(formula.parts(1).expression == (b * (c - d)))
        assertTrue(formula.parts(1).relation == FormulaPartRelation.EQUAL)
        assertTrue(formula.parts(1).position == FormulaPosition.EXPR_UNRESOLVED)
        assertTrue(formula.parts(2).expression == expected)
        assertTrue(formula.parts(2).relation == FormulaPartRelation.EQUAL)
        assertTrue(formula.parts(2).position == FormulaPosition.RIGHT)
    }

    @Test def shouldReckonAllExpressions = {
        //given
        import BasicSymbols._
        implicit val calc = Calculation()
        a := b * (c - d)
        b := c + 10
        c := hypot(-e, abs(f))
        d := h * (e / 10)
        e := sin(z) - 0.3
        k := 8
        val cache = new ResultsCache()
        //when
        val formulaSet = FormulaReckoner.reckonAll(calc, cache)
        //then
        assertTrue(formulaSet.size == 9)
        assertEquals(3, formulaSet.get(a).get.parts.size)
        assertTrue(formulaSet.get(b).get.parts.size == 4)
        assertTrue(formulaSet.get(c).get.parts.size == 3)
        assertTrue(formulaSet.get(d).get.parts.size == 3)
        assertTrue(formulaSet.get(e).get.parts.size == 2)
        assertTrue(formulaSet.get(f).get.parts.size == 1)
        assertTrue(formulaSet.get(h).get.parts.size == 1)
        assertTrue(formulaSet.get(z).get.parts.size == 1)
        assertTrue(formulaSet.get(k).get.parts.size == 2)
        assertTrue(cache.contains(a))
        assertTrue(cache.contains(b))
        assertTrue(cache.contains(c))
        assertTrue(cache.contains(d))
        assertTrue(cache.contains(e))
        assertTrue(cache.contains(f))
        assertTrue(cache.contains(h))
        assertTrue(cache.contains(z))
        assertTrue(cache.contains(k))
    }

    @Test def shouldReckonAllExpressions2 = {
        //given
        import BasicSymbols._
        implicit val calc = Calculation()
        l := m
        m := n
        n := f + e
        val cache = new ResultsCache()
        //when
        val formulaSet = FormulaReckoner.reckonAll(calc, cache)
        //then
        assertEquals(5, formulaSet.size)
        assertEquals(4, formulaSet.get(l).get.parts.size)
        assertEquals(l, formulaSet.get(l).get.parts(0).expression)
        assertEquals(m, formulaSet.get(l).get.parts(1).expression)
        assertEquals(n, formulaSet.get(l).get.parts(2).expression)
        assertEquals(f + e, formulaSet.get(l).get.parts(3).expression)
        assertEquals(3, formulaSet.get(m).get.parts.size)
        assertEquals(2, formulaSet.get(n).get.parts.size)
    }

}
