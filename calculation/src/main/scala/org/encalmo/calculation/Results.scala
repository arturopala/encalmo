package org.encalmo.calculation

import org.encalmo.graph.Graph
import org.encalmo.expression.Symbol


/**
 * Calculation runtime results: evaluated formulas and values cache
 */
class Results(val graph: Graph[Symbol] = Graph(), val formulaSet: FormulaSet = new FormulaSet(), val cache: ResultsCache = new ResultsCache())
