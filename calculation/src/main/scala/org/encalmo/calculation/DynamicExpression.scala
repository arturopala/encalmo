package org.encalmo.calculation

import org.encalmo.expression._

case class DynamicExpression(symbol:Symbol,f:()=>Expression) extends Expression with SymbolLike {
    
}