package org.encalmo.document

import org.encalmo.expression._

/**
 * DescriptionOfSymbol component class
 */
class DescriptionOfSymbol(myStyle:Style, val symbol:Symbol) 
extends DocumentComponent(myStyle) with TextContent {
	
	override def toString = "DescriptionOfSymbol("+myStyle+","+symbol+")"
	
	override def textContent:String = symbol match {
	    case sd:SymbolWithDescription => sd.description
	    case _ => ""
	}
	
}

/**
 * DescriptionOfSymbol class companion object
 * @author artur.opala
 */
object DescriptionOfSymbol {
	
	def apply(mystyle:Style, symbol:Symbol) = {
		new DescriptionOfSymbol(mystyle,symbol)
	}
	
	def apply(symbol:Symbol) = {
		new DescriptionOfSymbol(null,symbol)
	}
	
	def unapply(t:DescriptionOfSymbol) = Some((t.myStyle,t.symbol))
	
}