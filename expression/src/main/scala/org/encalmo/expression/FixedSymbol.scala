package org.encalmo.expression

/**
 * Fixed, unresolvable symbol
 */
case class FixedSymbol(symbol:Symbol) extends Expression with SymbolLike