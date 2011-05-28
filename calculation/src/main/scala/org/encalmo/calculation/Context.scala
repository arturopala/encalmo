package org.encalmo.calculation

import annotation.tailrec
import scala.collection.Map
import org.encalmo.expression._

/** 
 * Context trait
 */
trait Context extends ExpressionResolver {
	
	def id:Option[String]
	def map:Map[Symbol,Expression]
	
	protected var opened:Boolean = true
	
	/** Locks context content */
	def lock = {opened = false}
	
	protected def throwException = throw new IllegalStateException("This context has been locked.")
	
	/** Returns future expression */
	def apply(s:Symbol):FutureExpression = FutureExpression(this,s)
	
	/** Returns function evaluation for some arguments */
	def apply(expr:Expression, args:(Symbol,Expression)*):Eval = {
	    if(args.size>0){
		    val c = Calculation()
		    c add this
		    c put (args:_*)
		    Eval(expr, c)
	    }else{
	        Eval(expr, this)
	    }
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
	
	override def toSeq = map.toSeq
  
}

object Context {
	
	def apply() = new MapContext()
	
	def apply(id:String) = new MapContext(Option(id))
	
	def apply(vmap:Map[Symbol,Expression]) = new Context(){
		val id = None
		val map = vmap
	}
	
	def apply(entry:(Symbol,Expression)*) = new Context(){
		val id = None
		val map = Map(entry:_*)
	}
	
}