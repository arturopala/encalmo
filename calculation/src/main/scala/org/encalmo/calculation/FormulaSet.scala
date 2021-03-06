package org.encalmo.calculation

import org.encalmo.expression._
import scala.collection.mutable

case class FormulaSet() {

    private val formulaMap = mutable.LinkedHashMap[Expression, Formula]()

    private[calculation] def put(formula: Formula): FormulaSet = {
        formulaMap(formula.left.expression) = formula
        this
    }

    def items: Iterable[Formula] = formulaMap.values

    def size = items.size

    def get(expression: Expression): Option[Formula] = formulaMap.get(expression)

    /*def getOrReckon(expression: Expression, context: Context, results: Results): Formula = {
        get(expression) getOrElse {
            val formula = Reckoner.reckonExpression(expression,context,results)
            this put formula
            formula
        }
    }*/

    def contains(expression: Expression) = formulaMap.get(expression).isDefined
}
