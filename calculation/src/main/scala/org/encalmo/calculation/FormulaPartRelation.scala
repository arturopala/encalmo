package org.encalmo.calculation

object FormulaPartRelation extends Enumeration {

  type FormulaPartRelation = Value
  val NONE, IDENTICAL, EQUAL, APPROXIMATELY_EQUAL, ASYMPTOTICALLY_EQUAL, LESS, GREATER, LESS_OR_EQUAL, GREATER_OR_EQUAL, MUCH_LESS, MUCH_GREATER, PROPORTIONAL, LIMIT = Value

}
