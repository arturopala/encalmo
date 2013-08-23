package org.encalmo.calculation

import org.encalmo.expression.{ZERO, Value, Expression, Symbol, Number}
import scala.collection.mutable

class ResultsCache(val accuracy:Option[Double] = None) {

    private val cache = new mutable.LinkedHashMap[Symbol,Expression]

    /**
     * Puts expression evaluation to cache
     */
    def put(s:Symbol, e:Expression): Unit = {
        val ec = e match {
            case v:Value => v.convertTo(s.unit,
                if(s.accuracy.isDefined)
                    s.accuracy
                else
                    accuracy)
            case _ => e
        }
        cache.put(s,ec)
    }

    /**
     * Gets evaluated expression's result, if any.
     * @param s symbol
     * @return evaluated expression
     */
    def get(s: Symbol): Option[Expression] = {
        cache.get(s)
    }

    def contains(s:Symbol): Boolean = {
        cache.contains(s)
    }

    def apply(s:Symbol):Number = {
        get(s) match {
            case Some(d:Number) => d
            case _ => ZERO
        }
    }

}
