package org.encalmo.calculation

import annotation.tailrec
import scala.collection.{Map}
import org.encalmo.expression._

/** 
 * Context trait
 */
trait Context extends Map[Symbol,Expression] with ExpressionResolver{
	
	def id:Option[String]
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getExpression(s:Symbol):Option[Expression] = {
		val oe:Option[Expression] = this.get(s) 
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
		val oe:Option[Expression] = this.get(s) 
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
		this.contains(s) || (id.isDefined && (s.contextId match {
			case Some(currId) => {
				if(!currId.contains(id.get)){
					this.contains(s.at(id.get))
				}else{
					false
				}
			}
			case None => {
				this.contains(s.at(id.get))
			}
		}))
	}
  
}