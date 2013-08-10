package org.encalmo.calculation

import scala.collection.mutable.Map
import org.encalmo.expression._

case class FormulaSet() {

  private var formulaSet = Vector[Formula]()
  private val formulaMap = Map[Expression, Formula]()

  def +=(formula: Formula): FormulaSet = {
    formulaSet = formulaSet :+ formula
    formulaMap(formula.left.expression) = formula
    this
  }

  def items:Seq[Formula] = formulaSet
  def size = items.size
  def get(expression:Expression):Option[Formula] = formulaMap.get(expression)
  def contains(expression:Expression) = formulaMap.contains(expression)
}
