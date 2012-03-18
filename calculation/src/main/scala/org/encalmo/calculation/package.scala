package org.encalmo

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol

package object calculation {
    
    def dynamic(f: =>Expression) = DynamicExpression(f _)
    
    implicit def expressionToExpressionExt(expr:Expression):ExpressionExt = ExpressionExt(expr)
    implicit def symbolToSymbolExt(symbol:Symbol):SymbolExt = SymbolExt(symbol)
}