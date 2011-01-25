package org.encalmo.calculation

import scala.collection.{Map}
import org.encalmo.expression._

/** 
 * Context trait
 */
trait Context extends Map[Symbol,Expression] with ExpressionResolver{
	
	def id:String
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getExpression(s:Symbol):Option[Expression] = this.get(s)
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getRawExpression(s:Symbol):Option[Expression] = this.get(s)

	/**
	 * Returns true if exists expression mapped to that symbol
	 */
	override def hasExpression(s:Symbol):Boolean = this.contains(s)
  
}