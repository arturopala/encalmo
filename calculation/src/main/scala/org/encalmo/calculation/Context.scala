package org.encalmo.calculation

import annotation.tailrec
import scala.collection.{Map}
import org.encalmo.expression._

/** 
 * Context trait
 */
trait Context extends Map[Symbol,Expression] with ExpressionResolver{
	
	def id:String
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getExpression(s:Symbol):Option[Expression] = {
		val oe:Option[Expression] = this.get(s) 
		oe match {
			case Some(x) => oe
			case None => {
				s.contextId match {
					case Some(currId) => {
						if(!currId.contains(id)){
							getExpression(s.at(id))
						}else{
							None
						}
					}
					case None => getExpression(s.at(id))
				}
				
			}
		}
	}
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getRawExpression(s:Symbol):Option[Expression] = {
		val oe:Option[Expression] = this.get(s) 
		oe match {
			case Some(x) => oe
			case None => {
				s.contextId match {
					case Some(currId) => {
						if(!currId.contains(id)){
							getRawExpression(s.at(id))
						}else{
							None
						}
					}
					case None => getRawExpression(s.at(id))
				}
				
			}
		}
	}

	/**
	 * Returns true if exists expression mapped to that symbol
	 */
	override def hasExpression(s:Symbol):Boolean = {
		this.contains(s) || (s.contextId match {
				case Some(currId) => this.contains(s.at(currId+","+id))
				case None => this.contains(s.at(id))
			})
	}
  
}