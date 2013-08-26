package org.encalmo.calculation

import org.encalmo.expression.Expression

/**
 * Calculation runtime results: evaluated formulas and values cache
 */
class Results {

    private val formulaSetMap = scala.collection.mutable.Map[Context,FormulaSet]()

    private[calculation] def put(context: Context, formulaSet: FormulaSet):Results = {
        formulaSetMap.put(context, formulaSet)
        this
    }

    def formulaSetFor(context: Context): FormulaSet = {
        formulaSetMap.get(context) getOrElse {
            Reckoner.reckon(this)(context)
            formulaSetMap.get(context).get
        }
    }

    def get(context: Context): Option[FormulaSet] = formulaSetMap.get(context)
    def get(context: Context, expression: Expression): Option[Formula] = formulaSetMap.get(context).flatMap(_.get(expression))

    def formulaSet(implicit context: Context): FormulaSet = formulaSetFor(context)

}

object Results {

    def createFromImplicitContext(implicit context: Context): Results = {
        Reckoner.reckon(new Results())(context)
    }

}
