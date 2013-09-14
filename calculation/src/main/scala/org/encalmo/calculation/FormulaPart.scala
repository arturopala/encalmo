package org.encalmo.calculation

import org.encalmo.expression._

case class FormulaPart(expression: Expression, position: FormulaPosition.Value, relation: Relation.Value, index: Int = 0)

object FormulaPart {

    def apply(expression: Expression, position: FormulaPosition.Value, index: Int): FormulaPart = FormulaPart(expression,position,FormulaPart.discover(expression,index),index)

    def discover(expression: Expression, index: Int): Relation.Value = {
        import Relation._
        expression match {
            case n: Number if n.isRounded => APPROXIMATELY_EQUAL
            case tv: TextValue if index == 1 => NONE
            case _ => EQUAL
        }
    }

}
