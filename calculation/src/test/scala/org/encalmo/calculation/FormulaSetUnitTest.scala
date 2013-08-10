package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class FormulaSetUnitTest extends AssertionsForJUnit {

  @Test def shouldAppendFormulasToFormulaSet = {
    import BasicSymbols._
    //given
    val formulaSet = new FormulaSet()
    //when
    val f1: Formula = Formula(Seq(FormulaPart(a, FormulaPosition.LEFT), FormulaPart(b, FormulaPosition.RIGHT)))
    formulaSet += f1
    val f2: Formula = Formula(Seq(FormulaPart(c, FormulaPosition.LEFT), FormulaPart(d, FormulaPosition.RIGHT)))
    formulaSet += f2
    //then
    assertTrue(formulaSet.contains(a))
    assertFalse(formulaSet.contains(b))
    assertTrue(formulaSet.contains(c))
    assertFalse(formulaSet.contains(d))
    assertTrue(formulaSet.get(a) == Some(f1))
    assertTrue(formulaSet.get(b) == None)
    assertTrue(formulaSet.get(c) == Some(f2))
    assertTrue(formulaSet.get(d) == None)
  }

}
