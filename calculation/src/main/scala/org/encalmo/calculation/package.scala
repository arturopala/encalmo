package org.encalmo

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.DynamicExpression

package object calculation {
    
    def dynamic(symbols: Symbol*)(f: (ResultsCache)=>Expression) = DynamicExpression(symbols,f)
    
    /** Symbol class functionality extension */
    implicit class SymbolExt(symbol:Symbol) {
        
        def := (expression:Expression)(implicit context:MutableContext):Symbol = {
            context(symbol) = expression
            symbol
        }
    
    }
    
    /** Expression trait functionality extension */
    implicit class ExpressionExt(expression:Expression) {
    
        def at(map:Map[Symbol,Expression]):EvalAt = new EvalAt(expression, Context(map))
        def at(er:ExpressionResolver):EvalAt = new EvalAt(expression, er)
        def at(args:(Symbol,Expression)*):EvalAt = new EvalAt(expression, Context(args:_*))
        
    }
}