package org.encalmo.expression

/**
 * Trait for expressions behaving as symbols
 * @author artur.opala
 */
trait SymbolLike extends Expression {
	
	def symbol:Symbol
	
	override def printable:Boolean = symbol.printable

    override def face = symbol.face
}