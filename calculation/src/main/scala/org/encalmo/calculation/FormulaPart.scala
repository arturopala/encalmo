package org.encalmo.calculation

import org.encalmo.expression._

case class FormulaPart(expression: Expression, position: FormulaPosition.Value, relation: Relation.Value)

object FormulaPart {

    def apply(expression: Expression, position: FormulaPosition.Value): FormulaPart = FormulaPart(expression,position,FormulaPart.discover(expression))

    def discover(expression: Expression): Relation.Value = {
        import Relation._
        expression match {
            case n: Number if n.isRounded => APPROXIMATELY_EQUAL
            case tv: TextValue => NONE
            case _ => EQUAL
        }
    }

}
