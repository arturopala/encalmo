package org.encalmo.calculation

import org.encalmo.expression._

case class FormulaPart(expression: Expression, position: FormulaPosition.Value, relation: FormulaPartRelation.Value)

object FormulaPart {

    def apply(expression: Expression, position: FormulaPosition.Value): FormulaPart = FormulaPart(expression,position,FormulaPart.discover(expression))

    def discover(expression: Expression): FormulaPartRelation.Value = {
        import org.encalmo.calculation.FormulaPartRelation._
        expression match {
            case n: Number if (n.isRounded) => APPROXIMATELY_EQUAL
            case tv: TextValue => NONE
            case _ => EQUAL
        }
    }

}
