package org.encalmo.calculation

import scala.collection.mutable.{LinkedHashMap}
import org.encalmo.expression._

/** 
 * Default context class
 */
class DefaultContext(val id:Option[String] = None) extends LinkedHashMap[Symbol,Expression] with Context {
	
	override def equals(a:Any) = {
		a match {
			case d:DefaultContext => this.eq(d)
			case _ => false
		}
	}
	
}

object DefaultContext {
	
	def apply(id:Option[String] = None) = new DefaultContext(id)
	
	def unapply(d:DefaultContext) = Some(d.id)
}