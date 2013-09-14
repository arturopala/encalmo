package org.encalmo.expression

object Relation extends Enumeration {

  type Relation = Value
  val NONE, IDENTICAL, EQUAL, NOT_EQUAL, APPROXIMATELY_EQUAL, ASYMPTOTICALLY_EQUAL, LESS, GREATER, LESS_OR_EQUAL, GREATER_OR_EQUAL, MUCH_LESS, MUCH_GREATER, PROPORTIONAL, LIMIT = Value

  def faceOf(relation: Relation): String = {
      relation match {
          case Relation.APPROXIMATELY_EQUAL => "≈"
          case Relation.ASYMPTOTICALLY_EQUAL => "≃"
          case Relation.EQUAL => "="
          case Relation.GREATER => ">"
          case Relation.GREATER_OR_EQUAL => ">="
          case Relation.IDENTICAL => "="
          case Relation.LESS => "<"
          case Relation.LESS_OR_EQUAL => "<="
          case Relation.LIMIT => "->"
          case Relation.MUCH_GREATER => ">>"
          case Relation.MUCH_LESS => "<<"
          case Relation.NONE => ""
          case Relation.PROPORTIONAL => "~"
      }
  }

}
