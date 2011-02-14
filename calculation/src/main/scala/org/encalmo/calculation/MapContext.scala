package org.encalmo.calculation

import scala.collection.mutable.{LinkedHashMap}
import org.encalmo.expression._

/** 
 * Base {@link org.encalmo.calculation.Context} implementation
 */
class MapContext(val id:Option[String] = None) extends Context {
	
	override val map:LinkedHashMap[Symbol,Expression] = LinkedHashMap[Symbol,Expression]()
	
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