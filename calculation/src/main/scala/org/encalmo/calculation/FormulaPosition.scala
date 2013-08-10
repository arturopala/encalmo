package org.encalmo.calculation

object FormulaPosition extends Enumeration {

  type FormulaPosition = Value
  val LEFT,EXPR_UNRESOLVED,EXPR_SUBSTITUTED,EXPR_PARTIALLY_EVALUATED,RIGHT = Value

}
