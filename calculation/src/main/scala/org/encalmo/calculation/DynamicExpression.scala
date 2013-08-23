package org.encalmo.calculation

import org.encalmo.expression.{Expression, Symbol}

case class DynamicExpression(symbols: Seq[Symbol], body: (ResultsCache) => Expression) extends Expression {

    def f(cache: ResultsCache) = {
        body(cache)
    }

}