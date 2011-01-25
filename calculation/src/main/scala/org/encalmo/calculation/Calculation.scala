package org.encalmo.calculation

import scala.collection.Map
import scala.collection.mutable.{LinkedHashSet,LinkedHashMap}
import org.encalmo.expression._

/** 
 * Calculation context set
 */
class Calculation(val id:String) extends LinkedHashSet[ExpressionResolver] with ContextSet {
	
	val context = new DefaultContext(id)
	val cache = new LinkedHashMap[Symbol,Expression]
	
	this.add(context)
	
	/**
	 * Maps symbol to expression in the internal default context
	 */
	def put(s:Symbol,e:Expression) = context.put(s,e)
	
	/**
	 * Maps symbol to expression in the internal default context
	 */
	def put(t:(Symbol,Expression)) = context.put(t._1 ,t._2)
	
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
		cache.get(s).orElse(findExpression(s,this.elements))
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
		findRawExpression(s,this.elements)
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
	def addToCache(s:Symbol,e:Expression) = {
		cache.put(s,e)
		e
	}
	
	/**
	 * Returns true if exists expression mapped to that symbol
	 */
	override def hasExpression(s:Symbol):Boolean = {
		cache.get(s).isDefined || this.find(c => c.hasExpression(s)).isDefined
	}


}

object Calculation{
	
	def apply(id:String) = new Calculation(id)
	
}