package org.encalmo.common

import annotation.tailrec

/**
 * Node of tree-like structure.
 * @author artur.opala
 */
trait TreeNode[A<:TreeNode[A]] extends Visitable[A] {
    
    self:A =>
	
	val MAX_MAP_ALL_LOOP_COUNT:Int = 256
    
    /** Children components */
    def children:Seq[A] = Seq.empty
    
	/**
	 * Maps this structure once with given tranformation function. 
	 * Subtypes should apply this transformation to the children and return itself or copy if something has changed.
	 * @param f transformate
	 * @return tranformed structure
	 */
	def map(f:A=>A):A = f(this)
	
	/**
	 * Maps this structure with tranformation function in a loop
	 * while returns non identical results
	 * Subtypes should return own copy with custom arguments after transformation
	 * @param f transformate
	 * @return fully tranformed structure
	 * @throws CycleDetectedException if loop count exceeds max allowed number (256)
	 */
	@tailrec
	final def mapAll(f:A=>A, c:Int = 0, source:A = this):A = {
		val e:A = map(f)
		if(false) Console.println(e)
		if(c>=MAX_MAP_ALL_LOOP_COUNT){
			throw new CycleDetectedException[A](source,source.toString+" -> "+e.toString)
		}else{
			if(this.eq(e)) {
				e
			}else{
				e.mapAll(f, c+1, source)
			}
		}
	}
    
    /**
     * Travels internal structure of the expression
     */
  	final override def visit(visitor:TreeVisitor[A], parentNode:Node[A] = null, position:Int=0):Unit = {
  	    val element = visitor.mapElement(this)
		val node = Node(parentNode,element,position)
		visitor.onEnter(node)
		val children = element.children
		if(!children.isEmpty){
			children.size match {
				case 1 => visitChild(node,children.head,visitor)
				case 2 => visit2Children(node,children.head,children.tail.head,visitor)
				case _ => visitNChildren(node,children,visitor)
			}
			
		}
		visitor.onExit(node)
  	}

    final override def visitChildren(visitor:TreeVisitor[A], parentNode:Node[A] = null, position:Int=0):Unit = {
        val element = this
        val node = Node(parentNode,element,position)
        var p = position
        for(child <- element.children) {
            child.visit(visitor,node,p)
            p = p+1
        }


    }
  	
  	private def visitChild(node:Node[A],child:A,visitor:TreeVisitor[A],position:Int = 0):Unit = {
		visitor.onBeforeChildEnter(node,position,child)
		child.visit(visitor,node,position)
		visitor.onAfterChildExit(node,position,child)
  	}
  	
  	private def visit2Children(node:Node[A],child1:A,child2:A,visitor:TreeVisitor[A]):Unit = {
		visitChild(node,child1,visitor,0)
		visitor.onBetweenChildren(node,child1,child2)
		visitChild(node,child2,visitor,1)
  	}
  	
  	private def visitNChildren(node:Node[A],children:Seq[A],visitor:TreeVisitor[A]):Unit = {
		visitChild(node,children.head,visitor,0)
		for(x <- 1 to children.size-1){
			visitor.onBetweenChildren(node,children(x-1),children(x))
			visitChild(node,children(x),visitor,x)
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
  	
  	/** Collection of all nested level's children of the type A */
    final def allNestedChildrenOfType[B <: TreeNode[A]](t:Class[B]):Seq[B] = {
        children.filter(e => t.isAssignableFrom(e.getClass)).map(_.asInstanceOf[B]) ++ children.flatMap(_.childrenOfType[B](t))
    }
    
    /** Collection of children of the type A */
    final def childrenOfType[B <: TreeNode[A]](t:Class[B]):Seq[B] = {
        children.filter(e => t.isAssignableFrom(e.getClass)).map(_.asInstanceOf[B])
    }

}