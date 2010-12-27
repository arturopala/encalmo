package org.encalmo.calculation

import scala.collection.{Set}

/** 
 * Context set
 */
trait ContextSet extends Set[ExpressionResolver] with ExpressionResolver {
	
}