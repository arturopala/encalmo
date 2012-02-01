package org.encalmo.calculation

import scala.collection.mutable.Map
import org.encalmo.expression._

/** 
 * Mutable (updatable) Context trait
 */
trait MutableContext extends Context {
  
  override def map:Map[Symbol,Expression]
  
  /**
   * Maps expression to the symbol in this context
   */
  def update(s:Symbol, e:Expression):Unit = {
		if(!opened) throwException else {
		    val symb = symbol(s)
		    val expr:Expression = e match {
		        case v:Value => v.convertTo(s.unit)
		        case _ => e
		    }
		    map.put(symb,expr)
		}
  }

}