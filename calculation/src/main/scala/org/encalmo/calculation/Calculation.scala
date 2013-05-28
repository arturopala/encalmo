package org.encalmo.calculation

import scala.collection.mutable.{Map,LinkedHashSet,LinkedHashMap}
import org.encalmo.expression._
import scala.annotation.tailrec

/** 
 * Calculation. Hierarchical mutable context caching evaluted expressions.
 */
class Calculation(val id:Option[String] = None) extends MutableContext with MutableExpressionResolverSeq {
	
	private val context = new MapContext(id)
	private val cache = new LinkedHashMap[Symbol,Expression]
	
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
	
	/**
	 * Resolves and evaluates all symbols to values
	 * Uses and updates internal cache of resolved symbols.
	 */
	override def evaluate(e:Expression):Expression = {
		val expr = super.evaluate(e)
		e match {
			case s:Symbol => addToCache(s,expr)
			case _ => expr
		}
	}
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getExpression(s:Symbol):Option[Expression] = {
	    val c = cache.get(s)
	    if(c.isDefined) c else super.getExpression(s)
	}
    
    /**
     * Returns true if exists expression mapped to that symbol
     */
    override def hasExpression(s:Symbol):Boolean = {
        cache.get(s).isDefined || super.hasExpression(s)
    }
	
	/**
	 * Adds resolved espresion to cache
	 */
	private def addToCache(s:Symbol,e:Expression) = {
	    val ec = e match {
	        case v:Value => v.convertTo(s.unit, 
	                if(s.accuracy.isDefined) 
	                    s.accuracy 
                    else 
                        accuracy)
	        case _ => e
        }
		cache.put(s,ec)
		ec
	}
	
	def label:Expression = id.map(text(_)).getOrElse(null)
	
    private var accuracy:Option[Double] = None
    def acc(d:Double):Calculation = {
        accuracy = Some(d)
        this
    }


}

/** 
 * Calculation factory. 
 */
object Calculation{
	
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