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

	def require(expression: Expression, description: String): Symbol = require(expression,EmptyUnitOfValue,description)
	def require(expression: Expression, unit: UnitOfValue, description: String): Symbol = {
        val symbol: Symbol = Symbol(Context.REQUIREMENT_SYMBOL_PREFIX + idseq.incrementAndGet(), id, description)
        expression match {
            case a: Assert => {
                update(symbol,a.required.unit(unit))
            }
            case _ => throw new IllegalArgumentException("require needs Assert arg")
        }
        symbol
    }

	def check(expression: Expression, description: String): Symbol = check(expression,EmptyUnitOfValue,description)
	def check(expression: Expression, unit: UnitOfValue, description: String): Symbol = {
		val symbol: Symbol = Symbol(Context.CHECK_SYMBOL_PREFIX + idseq.incrementAndGet(), id, description)
		expression match {
			case a: Assert => {
				update(symbol,a.unit(unit))
			}
			case _ => throw new IllegalArgumentException("require needs Assert arg")
		}
		symbol

	}

}