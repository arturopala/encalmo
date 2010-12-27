package org.encalmo.expression

/**
 * Expresion structure travel node
 * @author artur.opala
 */
case class Node(parent:Node, expr:Expression, position:Int = 0) {
	
	val level:Int = parent match {
		case null => 0
		case _ => parent.level+1
	}
	
}