package org.encalmo.calculation

import annotation.tailrec
import scala.collection.mutable.Map
import org.encalmo.expression._

/** 
 * Context trait
 */
trait Context extends ExpressionResolver{
	
	def id:Option[String]
	def map:Map[Symbol,Expression]
	
	protected var opened:Boolean = true
	
	/** Locks context content */
	def lock = {opened = false}
	
	def throwException = throw new IllegalStateException("This context has been locked.")
	
	def update(s:Symbol, e:Expression) = {
		if(opened) map.put(symbol(s),e) else throwException
	}
	
	/**
	 * Returns symbol with context's index added
	 */
	def symbol(s:Symbol):Symbol = {
		id match {
			case Some(i) => {
				s.contextId match {
					case Some(currId) => {
						if(!currId.contains(i)){
							s.at(i)
						}else{
							s
						}
					}
					case None => s.at(i)
				}
			}
			case None => s
		}
	}
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getExpression(s:Symbol):Option[Expression] = {
		val oe:Option[Expression] = map.get(s) 
		oe match {
			case Some(x) => oe
			case None => {
				if(id.isDefined){
					s.contextId match {
						case Some(currId) => {
							if(!currId.contains(id.get)){
								getExpression(s.at(id.get))
							}else{
								None
							}
						}
						case None => getExpression(s.at(id.get))
					}
				} else {
					None
				}
				
			}
		}
	}
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getRawExpression(s:Symbol):Option[Expression] = {
		val oe:Option[Expression] = map.get(s) 
		oe match {
			case Some(x) => oe
			case None => {
				if(id.isDefined){
					s.contextId match {
						case Some(currId) => {
							if(!currId.contains(id.get)){
								getRawExpression(s.at(id.get))
							}else{
								None
							}
						}
						case None => getRawExpression(s.at(id.get))
					}
				}else{
					None
				}
				
			}
		}
	}

	/**
	 * Returns true if exists expression mapped to that symbol
	 */
	override def hasExpression(s:Symbol):Boolean = {
		map.contains(s) || (id.isDefined && (s.contextId match {
			case Some(currId) => {
				if(!currId.contains(id.get)){
					map.contains(s.at(id.get))
				}else{
					false
				}
			}
			case None => {
				map.contains(s.at(id.get))
			}
		}))
	}
  
}

object Context {
	
	def apply() = new MapContext()
	
	def apply(id:String) = new MapContext(Option(id))
	
}