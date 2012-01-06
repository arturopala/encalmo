package org.encalmo.calculation

import scala.collection.mutable.{Map,LinkedHashSet,LinkedHashMap}
import org.encalmo.expression._

/** 
 * Calculation. Mutable context with evaluated expression's cache.
 */
class Calculation(val id:Option[String] = None) extends ContextSet with MutableContext {
	
	override val set:LinkedHashSet[ExpressionResolver] = LinkedHashSet[ExpressionResolver]()
	
	private val context = new MapContext(id)
	private val cache = new LinkedHashMap[Symbol,Expression]
	
	val map = context.map
	
	set.add(context)
	
	/** Maps symbols to the expressions */
	def put(ts:(Symbol,Expression)*):Unit = {
		if(opened) for(t <- ts){
		    update(t._1,t._2)
		} else throwException
	}
	
	/** Inserts new ExpressionResolver */
	def add(er:ExpressionResolver):Unit = {
		if(opened) set.add(er) else throwException
	}
	
	/**
	 * Resolves and evaluates all symbols to values
	 * Uses and updates internal cache of resolved symbols.
	 */
	override def evaluate(e:Expression):Expression = {
	    /*val expr = e match {
	        case s:Symbol => cache.get(s).getOrElse(
	                super.evaluate(e)
            )
	        case _ => super.evaluate(e)
	    }*/
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
		/*cache.get(s).orElse(
	        findExpression(s,set.elements)
        )*/
	    val c = cache.get(s)
	    if(c.isDefined) c
	    else {
	        /*val e =*/ findExpression(s,set.elements)
	        /*if(e.isDefined) Some(addToCache(s,e.get)) else None*/
	    }
	}
	
	/**
	 * Resolves symbol in nested contexts
	 */
	def findExpression(s:Symbol,it:Iterator[ExpressionResolver]):Option[Expression] = {
		if(it.hasNext){
			it.next.getExpression(s).orElse(findExpression(s,it))
		}else{
			None
		}
	}
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getRawExpression(s:Symbol):Option[Expression] = {
		findRawExpression(s,set.elements)
	}
	
	/**
	 * Resolves symbol in nested contexts
	 */
	def findRawExpression(s:Symbol,it:Iterator[ExpressionResolver]):Option[Expression] = {
		if(it.hasNext){
			it.next.getRawExpression(s).orElse(findRawExpression(s,it))
		}else{
			None
		}
	}
	
	/**
	 * Adds resolved espresion to cache
	 */
	private def addToCache(s:Symbol,e:Expression) = {
	    val ec = e match {case v:Value => v.convertTo(s.unit); case _ => e}
		cache.put(s,ec)
		ec
	}
	
	/**
	 * Returns true if exists expression mapped to that symbol
	 */
	override def hasExpression(s:Symbol):Boolean = {
		cache.get(s).isDefined || set.find(c => c.hasExpression(s)).isDefined
	}
	
	def label:Expression = id.map(text(_)).getOrElse(null)


}

/** 
 * Calculation factory. 
 */
object Calculation{
	
	def apply():Calculation = new Calculation()
	
	def apply(index:String):Calculation = new Calculation(Option(index))
	
	def apply(vmap:Map[Symbol,Expression]) = {
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