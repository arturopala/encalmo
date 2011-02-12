package org.encalmo.calculation

import scala.collection.mutable.{Map,LinkedHashSet,LinkedHashMap}
import org.encalmo.expression._

/** 
 * Calculation
 */
class Calculation(val id:Option[String] = None) extends ContextSet {
	
	override val set:LinkedHashSet[ExpressionResolver] = LinkedHashSet[ExpressionResolver]()
	
	private val context = new MapContext(id)
	private val cache = new LinkedHashMap[Symbol,Expression]
	
	set.add(context)
	
	/** Maps symbol to the expression */
	def update(s:Symbol,e:Expression):Unit = {
		context(s) = e
	}
	
	/** Maps symbols to the expressions */
	def put(ts:(Symbol,Expression)*):Unit = {
		for(t <- ts){
			context(t._1) = t._2
		}
	}
	
	/** Inserts new ExpressionResolver */
	def add(er:ExpressionResolver):Unit = {
		set.add(er)
	}
	
	/** Returns future expression */
	def apply(s:Symbol):FutureExpression = FutureExpression(this,s)
	
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
		cache.get(s).orElse(findExpression(s,set.elements))
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
	def addToCache(s:Symbol,e:Expression) = {
		cache.put(s,e)
		e
	}
	
	/**
	 * Returns true if exists expression mapped to that symbol
	 */
	override def hasExpression(s:Symbol):Boolean = {
		cache.get(s).isDefined || set.find(c => c.hasExpression(s)).isDefined
	}


}

object Calculation{
	
	def apply():Calculation = new Calculation()
	def apply(index:String):Calculation = new Calculation(Option(index))
	
}