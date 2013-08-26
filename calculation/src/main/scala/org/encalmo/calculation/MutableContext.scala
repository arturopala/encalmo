package org.encalmo.calculation

import org.encalmo.expression._

trait MutableContext extends Context {

    //implicit self-reference required for :=
    implicit val ctx: this.type = this

    /**
     * Map expression to the symbol in this context
     */
    def update(symb: Symbol, expression: Expression): Unit

    /** Maps expressions to the symbols in this context */
    def put(ts: (Symbol, Expression)*): this.type

}