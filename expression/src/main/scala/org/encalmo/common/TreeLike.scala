package org.encalmo.common

import annotation.tailrec

/**
 * Tree like structure trait
 * @author artur.opala
 */
trait TreeLike[A<:TreeLike[A]] extends Travelable[A] {
	
	/** Parent component */
    var parent:Option[A] = None
    
    /** Children components */
    def children:Seq[A] = Seq.empty
    
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
    
    /**
     * Travels internal structure of the expression 
     * @param t traveler
     */
  	final override def travel(parentNode:Node[A] = null, traveler:Traveler[A], position:Int=0):Unit = {
		val n = Node(parentNode,this.asInstanceOf[A],position)
		traveler.onEnter(n)
		if(!children.isEmpty){
			val zip:Seq[((A, A), Int)] = (children.+:(null.asInstanceOf[A])).zip(children.:+(null.asInstanceOf[A])).zipWithIndex
			zip.foreach(z => {
				val e = z._1._2 //current node
				val pe = z._1._1 //previous node
				val i = z._2 //position index
				if(e!=null){
					if(pe!=null){
						traveler.onBetweenChildren(n,pe,e)
					}
					traveler.onBeforeChildEnter(n,i,e)
					e.travel(n,traveler,i)
					traveler.onAfterChildExit(n,i,e)
				}
			})
		}
		traveler.onExit(n)
  	}

}