package org.encalmo.calculation

import org.encalmo.expression._
import java.util.concurrent.atomic.AtomicInteger

trait MutableContext extends Context {

    //implicit self-reference required for :=
    implicit val ctx: this.type = this

    /**
     * Map expression to the symbol in this context
     */
    def update(symb: Symbol, expression: Expression): Unit

    /** Maps expressions to the symbols in this context */
    def put(ts: (Symbol, Expression)*): this.type

    private val idseq = new AtomicInteger
    def require(expression: Expression, description: String): Symbol = {
        val symbol: Symbol = Symbol(Context.REQUIREMENT_SYMBOL_PREFIX + idseq.incrementAndGet(), id, description)
        expression match {
            case a: Assert => {
                update(symbol,a.required)
            }
            case _ => update(symbol,expression)
        }
        symbol

    }

}