package org.encalmo.calculation

trait FormulaSetCache {

    private val formulaSetCache = scala.collection.mutable.Map[ExpressionResolver,FormulaSet]()
    private val resultsCache = new ResultsCache()

    def formulaSetFor(context: ExpressionResolver): FormulaSet = {
        if(context!=null) formulaSetCache.getOrElseUpdate(context, FormulaReckoner.reckonAll(context, resultsCache))
        else FormulaSet.echo
    }

    def resultCacheFor(context: ExpressionResolver): ResultsCache = {
        resultsCache
    }

}
