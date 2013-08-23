package org.encalmo.calculation

import scala.collection.Map
import org.encalmo.expression._

/** 
 * Context is a Map-based ExpressionResolver with identity
 */
trait Context extends ExpressionResolver {
	
	def id:Option[String]
	def map:Map[Symbol,Expression]
	
	/** Returns future expression */
	def apply(s:Symbol):PinnedExpression = PinnedExpression(this,s)
	
	/** Returns function evaluation for some arguments */
	def apply(expr:Expression, args:(Symbol,Expression)*):EvalAt = {
	    if(args.size>0){
		    val c = Calculation()
		    c add this
		    c put (args:_*)
		    EvalAt(expr, c)
	    }else{
	        EvalAt(expr, this)
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
							s.id(i)
						}else{
							s
						}
					}
					case None => s.id(i)
				}
			}
			case None => s
		}
	}
	
	/**
	 * Returns expression mapped to that symbol or None
	 */
	override def getRawExpression(s:Symbol):Option[Expression] = {
		map.get(s) orElse {
            if(id.isDefined){
                s.contextId match {
                    case Some(currId) => {
                        if(!currId.contains(id.get)){
                            getRawExpression(s.id(id.get))
                        }else{
                            None
                        }
                    }
                    case None => getRawExpression(s.id(id.get))
                }
            }else{
                None
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
					map.contains(s.id(id.get))
				}else{
					false
				}
			}
			case None => {
				map.contains(s.id(id.get))
			}
		}))
	}
	
	override def listMappings: Seq[(Symbol,Expression)] = map.toSeq
	override def listSymbols: Seq[Symbol] = map.keySet.toSeq
  
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