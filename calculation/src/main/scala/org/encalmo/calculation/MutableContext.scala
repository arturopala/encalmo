package org.encalmo.calculation

import scala.collection.mutable.Map
import org.encalmo.expression._

/** 
 * Mutable Context trait
 */
trait MutableContext extends Context {
  
  override def map:Map[Symbol,Expression]
  
  def update(s:Symbol, e:Expression) = {
		if(!opened) throwException else map.put(symbol(s),e)
  }

}