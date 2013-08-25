package org.encalmo.common

import annotation.tailrec

/**
 * Node of the tree-like structure with back-reference to the owning parent node.
 * @author artur.opala
 */
trait TreeNodeWithParentRef[A<:TreeNodeWithParentRef[A]] extends TreeNode[A] {
    
    self:A =>
	
	/** Parent component */
    var parent:Option[A] = None
    
    for (f <- children) {
        f.parent = Some(this)
    }
    
    /** Nearest parent component of the type A */
    @tailrec
    final def parentOfType[B](t:Class[B]):Option[B] = {
    	if(!parent.isDefined) None else {
    		val p = parent.get
    		p match {
    			case o if t.isAssignableFrom(o.getClass) => {
    				Some(o.asInstanceOf[B])
    			}
    			case o => o.parentOfType[B](t)
    		}
    	}
    }
    
    /** Nearest parent or sibling component of the type A */
    @tailrec
    final def parentOrSiblingOfType[B](t:Class[B]):Option[B] = {
    	if(!parent.isDefined) None else {
    		val p = parent.get
    		p match {
    			case o if t.isAssignableFrom(o.getClass) => {
    				Some(o.asInstanceOf[B])
    			}
    			case o => {
    				if(!o.children.isEmpty){
    					val r = o.children.takeWhile(x => x.ne(this)).find(x => t.isAssignableFrom(x.getClass))
    					if(r.isDefined){
    						Some(r.get.asInstanceOf[B])
    					}else{
    						o.parentOrSiblingOfType[B](t)
    					}
    				}else{
    					o.parentOrSiblingOfType[B](t)
    				}
    			}
    		}
    	}
    }
    
    /** Count parent components of the type A until predicate is true */
    @tailrec
    final def countParentsOfTypeUntil[B](t:Class[B],until:B=>Boolean,c:Int=0):Int = {
    	if(!parent.isDefined) c else {
    		val p = parent.get
    		p match {
    			case o if t.isAssignableFrom(o.getClass) => {
    				if(until(o.asInstanceOf[B])){
    					o.countParentsOfTypeUntil[B](t,until,c+1)
    				}else{
    					c
    				}
    			}
    			case o => o.countParentsOfTypeUntil[B](t,until,c)
    		}
    	}
    }
    
    
  	
  	/**
  	 * Checks if this is the first children of type B
  	 */
  	final def isFirstChildrenOfType[B](t:Class[B]):Boolean = {
  	    parent match {
  	    	case None => true
  	    	case Some(p) => {
  	    		p.children.takeWhile(
    				x => x.ne(this)
				).forall(
					z => !t.isAssignableFrom(t.getClass)
				)
  	    	}
  	    }
  	}

}