package org.encalmo.common

/**
 * Tree structure traveler
 * @author artur.opala
 */
trait Traveler[A<:AnyRef] {
	
	def onEnter(node:Node[A]):Unit = Unit
	
	def onBeforeChildEnter(node:Node[A], position:Int, child:A):Unit = Unit
	
	def onBetweenChildren(node:Node[A], leftChild:A, rightChild:A):Unit = Unit
	
	def onAfterChildExit(node:Node[A], position:Int, child:A):Unit = Unit
	
	def onExit(node:Node[A]):Unit = Unit

}