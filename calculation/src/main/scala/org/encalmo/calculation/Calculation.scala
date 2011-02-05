package org.encalmo.calculation

import scala.collection.Map
import scala.collection.mutable.{LinkedHashSet,LinkedHashMap}
import org.encalmo.expression._

/** 
 * Calculation
 */
class Calculation(val id:Option[String] = None) extends LinkedHashSet[ExpressionResolver] with ContextSet {
	
	val context = new DefaultContext(id)
	val cache = new LinkedHashMap[Symbol,Expression]
	
	this.add(context)
	
	/**
	 * Maps the symbol to the expression in the internal context
	 */
	def put(s:Symbol,e:Expression):Option[Expression] = context.put(symbol(s),e)
	
	def put(t:(Symbol,Expression)):Option[Expression] = put(t._1 ,t._2)
	
	def update(s:Symbol,e:Expression) = put(s,e)
	
	/**
	 * Returns expression which will evaluate 
	 * this expression in the context of this calculation in the future
	 */
	def future(s:Symbol):FutureEvaluation = FutureEvaluation(this,s)
	
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
	
	/**
	 * Returns symbol with context index
	 */
	def symbol(s:Symbol):Symbol = {
		id match {
			case Some(i) => {
				s.contextId match {
					case Some(currId) => {
						if(!currId.contains(i)){
							s.at(i)
						}else{
							s
						}
					}
					case None => s.at(i)
				}
			}
			case None => s
		}
	}


}

object Calculation{
	
	def apply():Calculation = new Calculation()
	def apply(index:String):Calculation = new Calculation(Option(index))
	
}