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
	final def mapAll(f:A=>A, c:Int = 0):A = {
		val e:A = map(f)
		if(c>=MAX_MAP_ALL_LOOP_COUNT){
			e
		}else{
			if(this.eq(e)) {
				return e
			}else{
				e.mapAll(f, c+1)
			}
		}
	}
    
    /**
     * Travels internal structure of the expression 
     * @param t traveler
     */
  	override def travel(parentNode:Node[A] = null, traveler:Traveler[A], position:Int=0):Unit = {
		val n = Node(parentNode,this,position)
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