package org.encalmo.calculation

import org.encalmo.expression._

/** Symbol class functionality extension */
case class SymbolExt(symbol:Symbol) {
    
    def := (expression:Expression)(implicit context:MutableContext):Symbol = {
        context(symbol) = expression
        symbol
    }

}