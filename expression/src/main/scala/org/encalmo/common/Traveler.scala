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

case class AdHocTraveler[A<:AnyRef] (
    
    val onEnterFx:Option[Node[A]=>Unit] = None,
    val onExitFx:Option[Node[A]=>Unit] = None,
    val onBeforeChildEnterFx:Option[(Node[A],Int,A)=>Unit] = None,
    val onAfterChildExitFx:Option[(Node[A],Int,A)=>Unit] = None,
    val onBetweenChildrenFx:Option[(Node[A],A,A)=>Unit] = None
    
) extends Traveler[A] {
    
    override def onEnter(node:Node[A]):Unit = onEnterFx.map(_(node))
	
	override def onBeforeChildEnter(node:Node[A], position:Int, child:A):Unit = onBeforeChildEnterFx.map(_(node,position,child))
	
	override def onBetweenChildren(node:Node[A], leftChild:A, rightChild:A):Unit = onBetweenChildrenFx.map(_(node,leftChild,rightChild))
	
	override def onAfterChildExit(node:Node[A], position:Int, child:A):Unit = onAfterChildExitFx.map(_(node,position,child))
	
	override def onExit(node:Node[A]):Unit = onExitFx.map(_(node))
    
}