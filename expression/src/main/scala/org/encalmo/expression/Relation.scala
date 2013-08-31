package org.encalmo.expression

object Relation extends Enumeration {

  type Relation = Value
  val NONE, IDENTICAL, EQUAL, NOT_EQUAL, APPROXIMATELY_EQUAL, ASYMPTOTICALLY_EQUAL, LESS, GREATER, LESS_OR_EQUAL, GREATER_OR_EQUAL, MUCH_LESS, MUCH_GREATER, PROPORTIONAL, LIMIT = Value

}
