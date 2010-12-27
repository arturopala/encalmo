package org.encalmo.expression

/**
 * Expression structure traveler
 * @author artur.opala
 */
trait Traveler {
	
	def onEnter(node:Node):Unit = Unit
	
	def onBeforeChildEnter(node:Node, position:Int, child:Expression):Unit = Unit
	
	def onBetweenChildren(node:Node, leftChild:Expression, rightChild:Expression):Unit = Unit
	
	def onAfterChildExit(node:Node, position:Int, child:Expression):Unit = Unit
	
	def onExit(node:Node):Unit = Unit

}