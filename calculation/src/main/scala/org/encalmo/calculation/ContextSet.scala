package org.encalmo.calculation

import scala.collection.{Set}

/** 
 * Context set
 */
trait ContextSet extends ExpressionResolver {
	
	def set:Set[ExpressionResolver]
	
}