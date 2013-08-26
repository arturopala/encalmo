package org.encalmo.calculation

import org.encalmo.expression._
import scala.collection.mutable

case class FormulaSet(contextId: String) {

    private val formulaMap = mutable.Map[Expression, Formula]()

    val cache = new ResultsCache()

    private[calculation] def put(formula: Formula): FormulaSet = {
        formulaMap(formula.left.expression) = formula
        this
    }

    def items: Iterable[Formula] = formulaMap.values

    def size = items.size

    def get(expression: Expression): Option[Formula] = formulaMap.get(expression)

    def getOrReckon(expression: Expression, context: Context, results: Results): Formula = {
        get(expression) getOrElse {
            val formula = Reckoner.reckonExpression(expression, results, cache)(context)
            this put formula
            formula
        }
    }

    def contains(expression: Expression) = formulaMap.get(expression).isDefined
}
