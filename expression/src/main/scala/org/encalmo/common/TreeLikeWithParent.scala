package org.encalmo.common

import annotation.tailrec

/**
 * Tree like structure trait
 * @author artur.opala
 */
trait TreeLikeWithParent[A<:TreeLikeWithParent[A]] extends TreeLike[A] {
	
	/** Parent component */
    var parent:Option[A] = None
    
    for (f <- children) {
        f.parent = Some(this.asInstanceOf[A])
    }
    
    /** Nearest parent component of the type A */
    //@tailrec
    final def parentOfType[B](t:Class[B]):Option[B] = {
    	if(!parent.isDefined) None else {
    		val p = parent.get
    		p match {
    			case o if (t.isAssignableFrom(o.getClass)) => {
    				Some(o.asInstanceOf[B])
    			}
    			case o => o.parentOfType[B](t)
    		}
    	}
    }
    
    /** Nearest parent or sibling component of the type A */
    //@tailrec
    final def parentOrSiblingOfType[B](t:Class[B]):Option[B] = {
    	if(!parent.isDefined) None else {
    		val p = parent.get
    		p match {
    			case o if (t.isAssignableFrom(o.getClass)) => {
    				Some(o.asInstanceOf[B])
    			}
    			case o => {
    				if(!o.children.isEmpty){
    					val r = o.children.find(x => t.isAssignableFrom(x.getClass))
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

}