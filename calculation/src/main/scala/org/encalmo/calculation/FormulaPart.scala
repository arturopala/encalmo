package org.encalmo.calculation

import org.encalmo.expression._

case class FormulaPart(expression: Expression, position: FormulaPosition.Value, relation: FormulaPartRelation.Value = FormulaPartRelation.EQUAL)
