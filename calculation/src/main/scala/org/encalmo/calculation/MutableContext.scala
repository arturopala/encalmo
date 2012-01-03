package org.encalmo.calculation

import scala.collection.mutable.Map
import org.encalmo.expression._

/** 
 * Mutable Context trait
 */
trait MutableContext extends Context {
  
  override def map:Map[Symbol,Expression]
  
  def update(s:Symbol, e:Expression) = {
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