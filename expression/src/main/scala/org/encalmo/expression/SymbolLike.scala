package org.encalmo.expression

/**
 * Trait for expressions behaving as Symbol
 * @author artur.opala
 */
trait SymbolLike extends Expression {
	
	def symbol:Symbol
	
	override def printable:Boolean = symbol.printable
}