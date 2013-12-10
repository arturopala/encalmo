package org.encalmo

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.calculation.DynamicExpression

package object calculation {
    
    def dynamic(symbols: Symbol*)(f: (ResultsCache)=>Expression) = DynamicExpression(symbols,f)
    
    /** Symbol class functionality extension */
    implicit class SymbolExt(val symbol:Symbol) extends AnyVal {
        
        def := (expression:Expression)(implicit context:MutableContext):Symbol = {
            context(symbol) = expression
            symbol
        }
    
    }
    
    /** Expression trait functionality extension */
    implicit class ExpressionExt(val expression:Expression) extends AnyVal {
    
        def at(map:Map[Symbol,Expression]):EvalAt = new EvalAt(expression, MapContext(map))
        def at(er:Context):EvalAt = new EvalAt(expression, er)
        def at(args:(Symbol,Expression)*):EvalAt = new EvalAt(expression, MapContext(args:_*))
        
    }
}