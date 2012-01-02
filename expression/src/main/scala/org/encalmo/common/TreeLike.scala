package org.encalmo.common

import annotation.tailrec

/**
 * Tree like structure trait
 * @author artur.opala
 */
trait TreeLike[A<:TreeLike[A]] extends Travelable[A] {
    
    self:A =>
	
	val MAX_MAP_ALL_LOOP_COUNT:Int = 256
    
    /** Children components */
    def children:Seq[A] = Seq.empty
    
	/**
	 * Maps this structure once with tranformation function. 
	 * Subtypes should return own copy with custom arguments after transformation
	 * @param f transformate
	 * @return tranformed structure
	 */
	def map(f:A=>A):A = f(this)
	
	/**
	 * Maps this structure with tranformation function in loop
	 * while returns non identical results or if loop count exceeds max allowed number (256)
	 * Subtypes should return own copy with custom arguments after transformation
	 * @param f transformate
	 * @return fully tranformed structure or lasty transformed in case of max loop count overflow
	 */
	@tailrec
	final def mapAll(f:A=>A, c:Int = 0, source:A = this):A = {
		val e:A = map(f)
		if(c>=MAX_MAP_ALL_LOOP_COUNT){
			throw new CycleDetectedException[A](source,source.toString+" -> "+e.toString)
		}else{
			if(this.eq(e)) {
				return e
			}else{
				e.mapAll(f, c+1, source)
			}
		}
	}
    
    /**
     * Travels internal structure of the expression 
     * @param t traveler
     */
  	final def travel(parentNode:Node[A] = null, traveler:Traveler[A], position:Int=0):Unit = {
  	    val element = traveler.mapElement(this)
		val node = Node(parentNode,element,position)
		traveler.onEnter(node)
		val children = element.children
		if(!children.isEmpty){
			children.size match {
				case 1 => travelChild(node,children(0),traveler)
				case 2 => travel2Children(node,children(0),children(1),traveler)
				case _ => travelNChildren(node,children,traveler)
			}
			
		}
		traveler.onExit(node)
  	}
  	
  	private def travelChild(node:Node[A],child:A,traveler:Traveler[A],position:Int = 0):Unit = {
		traveler.onBeforeChildEnter(node,position,child)
		child.travel(node,traveler,position)
		traveler.onAfterChildExit(node,position,child)
  	}
  	
  	private def travel2Children(node:Node[A],child1:A,child2:A,traveler:Traveler[A]):Unit = {
		travelChild(node,child1,traveler,0)
		traveler.onBetweenChildren(node,child1,child2)
		travelChild(node,child2,traveler,1)
  	}
  	
  	private def travelNChildren(node:Node[A],children:Seq[A],traveler:Traveler[A]):Unit = {
		travelChild(node,children(0),traveler,0)
		for(x <- 1 to children.size-1){
			traveler.onBetweenChildren(node,children(x-1),children(x))
			travelChild(node,children(x),traveler,x)
	  	}
  	}
  	
  	/**
  	 * Counts tree leafs starting at this node
  	 */
  	final lazy val countTreeLeafs:Int = {
  	    if(children.isEmpty){
  	        0
  	    }else{
  	        children.size + children.map(_.countTreeLeafs).sum
  	    }
  	}

}