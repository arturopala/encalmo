package org.encalmo.calculation

import scala.collection.mutable.{Map,LinkedHashMap}
import org.encalmo.expression._
import scala.collection.mutable

/** 
 * Base  implementation
 */
class MapContext(val id:Option[String] = None) extends MutableContext {
	
	override val map:mutable.Map[Symbol,Expression] = mutable.LinkedHashMap[Symbol,Expression]()
	
	override def equals(a:Any) = {
		a match {
			case d:MapContext => this.eq(d)
			case _ => false
		}
	}
	
}

object MapContext {
	
	def apply(id:String) = new MapContext(Option(id))
	
	def apply(id:Option[String] = None) = new MapContext(id)
	
	def unapply(d:MapContext) = Some(d.id)
}