package org.encalmo.calculation

import org.encalmo.expression._

/**
 * Calculation with symbols indexing
 * @author artur.opala
 */
class IndexedCalculation(id:String) extends Calculation(id) {
	
	/** Maps symbol to the expression in the internal context */
	override def put(s:Symbol,e:Expression):Option[Expression] = {
		context.put(s.at(id),e)
	}

}

object IndexedCalculation {
	
	def apply(id:String) = new IndexedCalculation(id)
	
}