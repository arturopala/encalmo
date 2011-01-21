package org.encalmo.common

import annotation.tailrec

/**
 * Tree like structure trait
 * @author artur.opala
 */
trait TreeLike[A<:TreeLike[A]] extends Travelable[A] {
    
    /** Children components */
    def children:Seq[A] = Seq.empty
    
	/**
	 * Maps this structure with tranformation function. 
	 * Subtypes should return own copy with custom arguments after transformation
	 * @param f transformate
	 * @return tranformed structure
	 */
	def map(f:A=>A):A = f(this.asInstanceOf[A])
    
    /**
     * Travels internal structure of the expression 
     * @param t traveler
     */
  	override def travel(parentNode:Node[A] = null, traveler:Traveler[A], position:Int=0):Unit = {
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