package org.encalmo.calculation

import org.encalmo.expression.{ZERO, Value, Expression, Symbol, Number}
import scala.collection.mutable

class ResultsCache {

    private val cache = new mutable.LinkedHashMap[Symbol,Expression]

    /**
     * Puts expression evaluation to cache
     */
    def put(symbol: Symbol, expression: Expression): Unit = cache.put(symbol,expression)

    /**
     * Gets evaluated expression's result, if any.
     */
    def get(symbol: Symbol): Option[Expression] = cache.get(symbol)

    def contains(symbol: Symbol): Boolean = cache.contains(symbol)

    def apply(symbol: Symbol):Number = {
        get(symbol) match {
            case Some(d:Number) => d
            case _ => ZERO
        }
    }

}
