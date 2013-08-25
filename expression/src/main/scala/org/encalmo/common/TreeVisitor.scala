package org.encalmo.common

/**
 * Tree structure visitor
 * @author artur.opala
 */
trait TreeVisitor[A<:AnyRef] {
	
	def onEnter(node:Node[A]):Unit = Unit
	
	def onBeforeChildEnter(node:Node[A], position:Int, child:A):Unit = Unit
	
	def onBetweenChildren(node:Node[A], leftChild:A, rightChild:A):Unit = Unit
	
	def onAfterChildExit(node:Node[A], position:Int, child:A):Unit = Unit
	
	def onExit(node:Node[A]):Unit = Unit

	def mapElement(a:A):A = a
	
}

case class AdHocVisitor[A<:AnyRef] (

                                       onEnterFx:Option[Node[A]=>Unit] = None,
    onExitFx:Option[Node[A]=>Unit] = None,
    onBeforeChildEnterFx:Option[(Node[A],Int,A)=>Unit] = None,
    onAfterChildExitFx:Option[(Node[A],Int,A)=>Unit] = None,
    onBetweenChildrenFx:Option[(Node[A],A,A)=>Unit] = None,
    mapElementFx:Option[A=>A] = None
    
) extends TreeVisitor[A] {
    
    override def onEnter(node:Node[A]):Unit = onEnterFx.map(_(node))
	
	override def onBeforeChildEnter(node:Node[A], position:Int, child:A):Unit = onBeforeChildEnterFx.map(_(node,position,child))
	
	override def onBetweenChildren(node:Node[A], leftChild:A, rightChild:A):Unit = onBetweenChildrenFx.map(_(node,leftChild,rightChild))
	
	override def onAfterChildExit(node:Node[A], position:Int, child:A):Unit = onAfterChildExitFx.map(_(node,position,child))
	
	override def onExit(node:Node[A]):Unit = onExitFx.map(_(node))
	
	override  def mapElement(a:A):A = if(mapElementFx.isDefined) mapElementFx.map(_(a)).get else super.mapElement(a)
    
}