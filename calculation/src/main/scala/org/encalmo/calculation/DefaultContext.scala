package org.encalmo.calculation

import scala.collection.mutable.{LinkedHashMap}
import org.encalmo.expression._

/** 
 * Default context class
 */
class DefaultContext(val id:String) extends LinkedHashMap[Symbol,Expression] with Context {
	
	override def equals(a:Any) = {
		a match {
			case d:DefaultContext => this.eq(d)
			case _ => false
		}
	}
	
}

object DefaultContext {
	
	def apply(id:String) = new DefaultContext(id)
	
	def unapply(d:DefaultContext) = Some(d.id)
}