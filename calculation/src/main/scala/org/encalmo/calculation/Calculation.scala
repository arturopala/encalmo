package org.encalmo.calculation

import scala.collection.mutable.{Map,LinkedHashSet,LinkedHashMap}
import org.encalmo.expression._
import scala.annotation.tailrec

/** 
 * Calculation is a hierarchical, mutable context for expressions.
 */
class Calculation(val id:Option[String] = None) extends MutableContext with MutableExpressionResolverSeq {
	
	private val context = new MapContext(id)
	
	override val map = context.map
	
	add(context)
	
	/** Maps symbols to the expressions */
	def put(ts:(Symbol,Expression)*):Unit = {
		if(opened) for((s,e) <- ts){
		    update(s,e)
		} else throwContextAlreadyLockedException
	}
	
	/** Inserts new ExpressionResolver */
	override def add(er:ExpressionResolver):Unit = {
		if(opened) super.add(er) else throwContextAlreadyLockedException
	}
	
	def label:Expression = id.map(text).getOrElse(null)
	
    private var accuracy:Option[Double] = None
    def acc(d:Double):Calculation = {
        accuracy = Some(d)
        this
    }

}

/** 
 * Calculation factory. 
 */
object Calculation {
	
	def apply():Calculation = new Calculation()
	
	def apply(index:String):Calculation = new Calculation(Option(index))
	
	def apply(vmap:scala.collection.Map[Symbol,Expression]) = {
	    val c = new Calculation()
	    c.put(vmap.toSeq:_*)
	    c
	}
	
	def apply(entries:(Symbol,Expression)*) = {
		val c = new Calculation()
	    c.put(entries:_*)
	    c
	}
	
}