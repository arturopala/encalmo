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
    //when
    val formula = FormulaReckoner.reckon(a)
    //then
    val expected = (10 + (hypot(-(sin(z) - 0.3), abs(f)))) * ((hypot(-(sin(z) - 0.3), abs(f))) - (h * ((sin(z) - 0.3) / 10)))
    assertTrue(formula.parts.size == 3)
    assertTrue(formula.parts.head.expression.isInstanceOf[Symbol])
    assertTrue(formula.parts.head.relation == FormulaPartRelation.NONE)
    assertTrue(formula.parts(1).expression == (b * (c - d)))
    assertTrue(formula.parts(1).relation == FormulaPartRelation.EQUAL)
    assertTrue(formula.parts(2).expression == expected)
    assertTrue(formula.parts(2).relation == FormulaPartRelation.EQUAL)
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
    //when
    val formulaSet = FormulaReckoner.reckonAll
    //then
    assertTrue(formulaSet.size == 8)
    assertTrue(formulaSet.get(a).get.parts.size == 3)
    assertTrue(formulaSet.get(b).get.parts.size == 4)
    assertTrue(formulaSet.get(c).get.parts.size == 3)
    assertTrue(formulaSet.get(d).get.parts.size == 3)
    assertTrue(formulaSet.get(e).get.parts.size == 2)
    assertTrue(formulaSet.get(f).get.parts.size == 1)
    assertTrue(formulaSet.get(h).get.parts.size == 1)
    assertTrue(formulaSet.get(z).get.parts.size == 1)
  }

}
